#Date: August 17, 2022
#Author: Cristiana Aparecida Nogueira Couto
#This script calculates the richness of the municipality of Caucaia
#Both considering the whole city, as well as dividing by the different
#vegetation, according to the registered data collected from the herbariums. 
#This script imports the data from the script 02_cleaning_data_II.R

#Importing the data
flora_table <- read.csv("data/output/02_flora_table.csv", header=TRUE)

#Calculating the richness in Caucaia -------------------
#We can see that some of the entries in the name of the city are NA
sum(flora_table$municipio == "Caucaia", na.rm = TRUE)

#But we know from the group who complied the dataset that all the rows refer to 
#species found in Caucaia, so we add the name for all of them
flora_table$municipio[which(is.na(flora_table$municipio))] <- "Caucaia"

#Richness by genre -------------
sort(unique(flora_table$genero))

#Now we check the genre rows with the taxonomists names, we want to combine them with the rows 
#with only the genre name
taxonomists_genre <- "L.$|Vahl$|Aubl.$|Jacq.$|R.Br$|ex A.Juss.$|Mart. ex Klotzsch$"
unique(flora_table$genero[which(stri_detect_regex(flora_table$genero, taxonomists_genre))])

genre_regex <- c("Cyperus", "Actinostemon", "Ditaxis", "Lycium", "Trigonia", "Tamonea", 
                 "Fimbristylis", "Croton", "Jatropha", "Schwenckia", "Lantana", "Hybanthus", 
                 "Dioscorea", "Dalechampia", "Chrysophyllum", "Jacquinia", "Stachytarpheta",
                 "Eleocharis", "Manihot")

flora_genre_aux <- flora_table %>%
  mutate(genero = 
           case_when(
              stri_detect_regex(genero, genre_regex[1]) ~ genre_regex[1],
              stri_detect_regex(genero, genre_regex[2]) ~ genre_regex[2],
              stri_detect_regex(genero, genre_regex[3]) ~ genre_regex[3],
              stri_detect_regex(genero, genre_regex[4]) ~ genre_regex[4],
              stri_detect_regex(genero, genre_regex[5]) ~ genre_regex[5],
              stri_detect_regex(genero, genre_regex[6]) ~ genre_regex[6],
              stri_detect_regex(genero, genre_regex[7]) ~ genre_regex[7],
              stri_detect_regex(genero, genre_regex[8]) ~ genre_regex[8],
              stri_detect_regex(genero, genre_regex[9]) ~ genre_regex[9],
              stri_detect_regex(genero, genre_regex[10]) ~ genre_regex[10],
              stri_detect_regex(genero, genre_regex[11]) ~ genre_regex[11],
              stri_detect_regex(genero, genre_regex[12]) ~ genre_regex[12],
              stri_detect_regex(genero, genre_regex[13]) ~ genre_regex[13],
              stri_detect_regex(genero, genre_regex[14]) ~ genre_regex[14],
              stri_detect_regex(genero, genre_regex[15]) ~ genre_regex[15],
              stri_detect_regex(genero, genre_regex[16]) ~ genre_regex[16],
              stri_detect_regex(genero, genre_regex[17]) ~ genre_regex[17],
              stri_detect_regex(genero, genre_regex[18]) ~ genre_regex[18],
              stri_detect_regex(genero, genre_regex[19]) ~ genre_regex[19],
              
              TRUE ~ genero
             ))

#Now, we calculate the richness by genre
flora_by_genre <- flora_genre_aux %>% 
  group_by(genero) %>%
  summarise(richness_by_genre = n())
  
#Exporting the flora table by genre
write.table(flora_by_genre, file = "data/output/03_flora_by_genre.csv", sep = ",")
