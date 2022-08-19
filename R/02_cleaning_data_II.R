#Date: August 17, 2022
#Author: Cristiana Aparecida Nogueira Couto
#This script reads the output data from 01_reading_and_cleaning_data.R script to continue
#the cleaning of the dataset, now focusing on solving the problems with different formats of the 
#names on the rows.

library(readr)
library(dplyr)
library(stringi)
library(tidyr)

#Importing data ----------------------
load(file = "data/output/01_flora_table.rdata")

#Data cleaning -------------------------
#Now we see that some of the lines with the identifier number has a abbreviation
#regarding the collection where the dry plant is found
#So we create a new column for this abbreviation and leave the "nº.tombo" column just 
#with the number
flora_table_final <- flora_table_final %>%
  mutate(colecao.abrev = stri_extract_first_regex(nº.tombo, "^[A-Z]+"),
         .after = colecao,
         nº.tombo = stri_extract_first_regex(nº.tombo, "[0-9]+"))


#We delete the lines where the collection name is NA
flora_table_final <- flora_table_final %>% filter(!(is.na(colecao)))

#Now we check which rows still don't have the abbreviation and extract them from the collection name
flora_table_final <- flora_table_final %>%
  mutate(colecao.abrev = replace_na(stri_extract_first_regex(colecao, "[A-Z]{2,}")))

#We delete the lines where the collection abbreviation is NA
flora_table_final <- flora_table_final %>% filter(!(is.na(colecao.abrev)))

#Now we check if the abbreviation in colecao.abrev matches the abbreviation in the colecao column
#We do this to see if the abbreviation with the identifier number matches the one in the collection
#name
flora_table_final <- flora_table_final[stri_detect_regex(flora_table_final$colecao, 
                                                      flora_table_final$colecao.abrev),]


#Some of the other errors identified were regarding the names of the collection
#The same herbarium appears with different writings throughout the dataset
#So we change it to only one name 
#The names where checked in the official websites of the institutions
unique(flora_table_final$colecao)

flora_table_herbarium_names_updated <- flora_table_final %>%
  mutate(colecao = 
           case_when(
             stri_detect_regex(colecao, "EAC") ~ "Herbário Prisco Bezerra",
             stri_detect_regex(colecao, "UFG") ~ "Herbário da Universidade Federal de Goiás",
             stri_detect_regex(colecao, "JPB") ~ "Herbário Lauro Pires Xavier",
             stri_detect_regex(colecao, "UFP") ~ "Herbário Geraldo Mariz",
             stri_detect_regex(colecao, "BHZB") ~ "Herbário do Jardim Botânico da Fundação de Parques Municipais e Zoobotânica",
             stri_detect_regex(colecao, "UB") ~ "Herbário da Universidade de Brasília",
             stri_detect_regex(colecao, "NYBG") ~ "The New York Botanical Garden",
             stri_detect_regex(colecao, "ASE") ~ "Herbário da Universidade Federal de Sergipe",
             stri_detect_regex(colecao, "HCDAL") ~ "Herbário Caririense Dárdano de Andrade-Lima",
             stri_detect_regex(colecao, "UEFS") ~ "Herbário da Universidade Estadual de Feira de Santana",
             stri_detect_regex(colecao, "CEN") ~ "Herbário da Embrapa Recursos Genéticos e Biotecnologia",
             stri_detect_regex(colecao, "UEC") ~ "Herbário da Universidade Estadual de Campinas",
             stri_detect_regex(colecao, "MOSS") ~ "Herbário Dárdano de Andrade Lima - MOSS",
             stri_detect_regex(colecao, "SP-Fanerogamas") ~ "Herbário do Estado Maria Eneyda P. Kaufmann Fidalgo",
             stri_detect_regex(colecao, "BHCB") ~ "Herbário da UFMG",
             stri_detect_regex(colecao, "ICN") ~ "Herbário do Instituto de Ciências Naturais",
             stri_detect_regex(colecao, "IPA") ~ "Herbário IPA - Dárdano de Andrade Lima",
             stri_detect_regex(colecao, "UFRN") ~ "Herbário da UFRN",
             stri_detect_regex(colecao, "US") ~ "Smithsonian Department of Botany - South American records",
             stri_detect_regex(colecao, "NY") ~ "The New York Botanical Garden",
             stri_detect_regex(colecao, "UPCB") ~ "Herbário da Universidade Federal do Paraná",
             stri_detect_regex(colecao, "RB") ~ "Jardim Botânico do Rio de Janeiro",
             stri_detect_regex(colecao, "HEPH") ~ "Herbário Ezechias Paulo Heringer",
             stri_detect_regex(colecao, "HDJF") ~ "Herbário Dendrológico Jeanine Felfil",
             
             TRUE ~ colecao
           ))

#Checking the current unique values for the herbarium name column
unique(flora_table_herbarium_names_updated$colecao)

#Another issue spotted was information placed in the wrong column, 
#for example the popular name of a plant is on the column that refers to the conservation
#status
unique(flora_table_herbarium_names_updated$status.de.conservacao)
index_aux <- which(stri_detect_regex(flora_table_herbarium_names_updated$status.de.conservacao, "Genipapo-bravo"))

flora_table_herbarium_names_updated$nome.popular[index_aux] <- flora_table_herbarium_names_updated$status.de.conservacao[index_aux]
flora_table_herbarium_names_updated$status.de.conservacao[index_aux] <- NA

#Exporting the data as a csv ----------
write.table(flora_table_herbarium_names_updated, file = "data/output/02_flora_table.csv", sep = ",")
