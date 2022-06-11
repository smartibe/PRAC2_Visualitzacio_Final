###                                    ###
###                                    ###
### My Anime List                      ###
### PRAC2 - Visualització de dades     ###    
###                                    ### 
###  Sergi Marti                       ### 
###                                    ###

###              ###
#### Llibreries ####
###              ###


library(data.table)
library(magrittr)
library(tidyverse)


###              ###
#### Paràmetres ####
###              ###

data_path <- paste0(getwd(),"/01 - Data")
out_path  <- paste0(getwd(),"/03 - Output")
file      <- "/mal_top2000_anime.csv"


###                   ###
#### Lectura          ####
###                   ###

anime_data <- data.table::fread(file = paste0(data_path,file) , sep = ",", dec = ".")

###                   ###
#### Preparació       ####
###                   ###

###                         ###
#### ... 1. Live bar chart ####
###                        ###

anime_data[,V1 := .I, Name]
setnames(old= "V1",new = "index", anime_data)

anime_by_year <- copy(anime_data)

# Live in wide

for(i in 1:nrow(anime_by_year)){
    
  years_air <- anime_by_year[i][,trimws(gsub("[0-9]{1,2}, |[A-Z]|[a-z]", "",`Air Date`))]
  years_air <- tstrsplit(years_air, " ")
  
  if(length(years_air) == 4){
    years_air <- as.integer(years_air[1]):as.integer(years_air[4])
  }
  
  if(length(years_air) == 3){
    years_air <- as.integer(years_air[1]):2022  
  }
  
  for(j in years_air)
    anime_by_year[index == i,as.character(j):= 1]
  
}

anime_by_year[is.na(anime_by_year)] <- 0
    

# data table live years by genre

anime_by_gen_year <- data.table::data.table()

years <- grep("[0-9]",names(anime_by_year), value =T)

for(i in 1:nrow(anime_by_year)){
  
  genres <- anime_by_year[index == i, strsplit((Genres), ",")][,unique(toupper(trimws(V1)))]
  genres <- gsub("[^A-Z]", "",genres)
  
  for(j in genres){
    
    aux <- data.table::data.table(V1 = j,  anime_by_year[index == i, .SD, .SDcols = years])
    anime_by_gen_year <- rbind(anime_by_gen_year,aux)
  }
}

setnames(anime_by_gen_year, old = "V1", new = "Genres")



resultat_1<- anime_by_gen_year %>% 
                  group_by(Genres) %>% 
                  summarise_at(c(sort(names(anime_by_gen_year)[-1])), sum, na.rm = TRUE)


data.table::fwrite(resultat_1, 
                   file = paste0(out_path, "/resultat_1_live_bar_chart.csv"),
                   sep = ";",
                   dec = ",")

 
###                           ###
#### ... 2. Word clouds       ####
###                           ###

theme_data <- copy(anime_by_year)

# Decades

theme_data[,total:= rowSums(.SD), .SDcols = c(years)]
theme_data[,dec_70_80:= rowSums(.SD), .SDcols = c(grep("197|198" ,years, value =T))]
theme_data[,dec_90_00:= rowSums(.SD), .SDcols = c(grep("199|200" ,years, value =T))]
theme_data[,dec_10_20:= rowSums(.SD), .SDcols = c(grep("201|202" ,years, value =T))]

theme_data[total > 1, total := 1]
theme_data[dec_70_80 > 1, dec_70_80 := 1]
theme_data[dec_90_00 > 1, dec_90_00 := 1]
theme_data[dec_10_20 > 1, dec_10_20 := 1]

# Generem dataset amb els themes desagregats

resultat_themes <- data.table::data.table()

for(i in 1:nrow(theme_data)){
  
  aux <- theme_data[index == i, strsplit(trimws(gsub("[]]|[[]","",`Theme(s)`)), ",")]
  
  for(j in aux$V1){
    aux_row <- theme_data[index == i, theme_desagregat := j][index == i]
    resultat_themes <- rbind(resultat_themes, aux_row)
  }
}

theme_data[,theme_desagregat:=NULL]

resultat_themes[,theme_desagregat := trimws(gsub("[']","",theme_desagregat ))]

resultat_2 <-  resultat_themes[,.(`Total històric`     = sum(total),
                                 `'70'-'80`            = sum(dec_70_80),
                                 `'90-'00`             = sum(dec_90_00),
                                 `'10-'20`             = sum(dec_10_20)), by = theme_desagregat]


resultat_2 <- resultat_2[!(theme_desagregat == "None")]

data.table::fwrite(resultat_2, 
                   file = paste0(out_path, "/resultat_2_word_clouds.csv"),
                   sep = ";",
                   dec = ",")



###                                           ###
#### ... 3. Sunburst i TOP productors         ####
###                                           ###

anime_by_gen_studio <- data.table::data.table()

for(i in 1:nrow(anime_data)){
  
  genres <- anime_data[index == i, strsplit((Genres), ",")][,unique(toupper(trimws(V1)))]
  studio <- anime_data[index == i, strsplit((Studio), ",")][,unique(toupper(trimws(V1)))]
  genres <- gsub("[^A-Z]", "",genres)
  studio <- gsub("[^A-Z]", "",studio)
  
  for(j in genres){
    for(k in studio){
      aux_row <- anime_data[index == i, .(index,Name,genere_desagregat = j, studi_desagregat = k, Demographic, Score) ]
      
      
      anime_by_gen_studio <- rbind(anime_by_gen_studio, aux_row)
    }
  }
}


resultat_aux <- anime_by_gen_studio[, uniqueN(index), by = .(studi_desagregat, genere_desagregat )]

resultat_3 <- resultat_aux[,sum(V1), studi_desagregat] %>% 
                setorder(.,-V1) %>% 
                head(5)

anime_by_gen_studio[studi_desagregat %in% resultat_3[,studi_desagregat], mean(Score), studi_desagregat  ]

resultat_4 <- resultat_aux[studi_desagregat %in% resultat_3[,unique(studi_desagregat)]] %>%
                setorderv(., c("studi_desagregat", "V1"), c(1,-1))



data.table::fwrite(resultat_3, 
                   file = paste0(out_path, "/resultat_3_top_estudis.csv"),
                   sep = ";",
                   dec = ",")



data.table::fwrite(resultat_4, 
                   file = paste0(out_path, "/resultat_4_sunburst.csv"),
                   sep = ";",
                   dec = ",")


###                                           ###
#### ... 4. Marimekko                        ####
###                                           ###

anime_by_gen_studio[Demographic == "Josei", Demo_cat := "Dones adultes"]
anime_by_gen_studio[Demographic == "Kids", Demo_cat := "Nens/es"]
anime_by_gen_studio[Demographic == "Seinen", Demo_cat := "Homes adults joves"]
anime_by_gen_studio[Demographic == "Shoujo", Demo_cat := "Dones adolescents"]
anime_by_gen_studio[Demographic == "Shounen", Demo_cat := "Homes adolescents"]
anime_by_gen_studio[Demo_cat %like% "Home", Demo_cat_gen := "Masculí"]
anime_by_gen_studio[Demo_cat %like% "Dones", Demo_cat_gen := "Femení"]
anime_by_gen_studio[Demo_cat %like% "Nen", Demo_cat_gen := "Nens/es"]
anime_by_gen_studio[Demo_cat %like% "adolescents", Demo_cat_edat := "Adolescents"]
anime_by_gen_studio[Demo_cat %like% "adults", Demo_cat_edat := "Adults"]
anime_by_gen_studio[Demo_cat %like% "Nen", Demo_cat_edat := "Nens/es"]


resultat_5 <- table(anime_by_gen_studio[Demographic != "None", Demo_cat_edat], anime_by_gen_studio[Demographic != "None", genere_desagregat])
resultat_6 <- table(anime_by_gen_studio[Demographic != "None", Demo_cat_gen], anime_by_gen_studio[Demographic != "None", genere_desagregat])

data.table::fwrite(resultat_5, 
                   file = paste0(out_path, "/resultat_5_marimekko_edat.csv"),
                   sep = ";",
                   dec = ",")

data.table::fwrite(resultat_6, 
                   file = paste0(out_path, "/resultat_6_marimekko_genere.csv"),
                   sep = ";",
                   dec = ",")
