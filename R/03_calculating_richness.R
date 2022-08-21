#Date: August 17, 2022
#Author: Cristiana Aparecida Nogueira Couto
#This script calculates the richness of the municipality of Caucaia
#according to the registered data collected from the herbariums. 
#This script imports the data from the script 02_cleaning_data_II.R

#Importing the data
flora_table <- read.csv("data/output/02_flora_table.csv", header=TRUE)

#Calculating the richness in Caucaia -------------------
#We can see that some of the entries in the name of the city are NA
sum(flora_table$municipio == "Caucaia", na.rm = TRUE)

#But we know from the group who complied the dataset that all the rows refer to 
#species found in Caucaia, so we add the name for all of them
flora_table$municipio[which(is.na(flora_table$municipio))] <- "Caucaia"

#Richness by genus -------------
sort(unique(flora_table$genero))
flora_table$genero <-  trimws(flora_table$genero)

#Now we check the genus rows with the taxonomists names, we want to combine them with the rows 
#with only the genus name
taxonomists_genus <- "L.$|Vahl$|Aubl.$|Jacq.$|R.Br$|ex A.Juss.$|Mart. ex Klotzsch$"
unique(flora_table$genero[which(stri_detect_regex(flora_table$genero, taxonomists_genus))])

genus_regex <- c("Cyperus", "Actinostemon", "Ditaxis", "Lycium", "Trigonia", "Tamonea", 
                 "Fimbristylis", "Croton", "Jatropha", "Schwenckia", "Lantana", "Hybanthus", 
                 "Dioscorea", "Dalechampia", "Chrysophyllum", "Jacquinia", "Stachytarpheta",
                 "Eleocharis", "Manihot")

flora_genus_aux <- flora_table %>%
  mutate(genero = 
           case_when(
              stri_detect_regex(genero, genus_regex[1]) ~ genus_regex[1],
              stri_detect_regex(genero, genus_regex[2]) ~ genus_regex[2],
              stri_detect_regex(genero, genus_regex[3]) ~ genus_regex[3],
              stri_detect_regex(genero, genus_regex[4]) ~ genus_regex[4],
              stri_detect_regex(genero, genus_regex[5]) ~ genus_regex[5],
              stri_detect_regex(genero, genus_regex[6]) ~ genus_regex[6],
              stri_detect_regex(genero, genus_regex[7]) ~ genus_regex[7],
              stri_detect_regex(genero, genus_regex[8]) ~ genus_regex[8],
              stri_detect_regex(genero, genus_regex[9]) ~ genus_regex[9],
              stri_detect_regex(genero, genus_regex[10]) ~ genus_regex[10],
              stri_detect_regex(genero, genus_regex[11]) ~ genus_regex[11],
              stri_detect_regex(genero, genus_regex[12]) ~ genus_regex[12],
              stri_detect_regex(genero, genus_regex[13]) ~ genus_regex[13],
              stri_detect_regex(genero, genus_regex[14]) ~ genus_regex[14],
              stri_detect_regex(genero, genus_regex[15]) ~ genus_regex[15],
              stri_detect_regex(genero, genus_regex[16]) ~ genus_regex[16],
              stri_detect_regex(genero, genus_regex[17]) ~ genus_regex[17],
              stri_detect_regex(genero, genus_regex[18]) ~ genus_regex[18],
              stri_detect_regex(genero, genus_regex[19]) ~ genus_regex[19],
              
              TRUE ~ genero
             ))

#Now, we calculate the richness by genus
flora_by_genus <- flora_genus_aux %>% 
  group_by(genero) %>%
  summarise(richness_by_genus = n())
  
#Exporting the flora table by genus
write.table(flora_by_genus, file = "data/output/03_flora_by_genus.csv", sep = ",")
