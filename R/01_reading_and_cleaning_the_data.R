#Date: August 16, 2022
#Author: Cristiana Aparecida Nogueira Couto
#This script reads the raw data in xlsx format to produce a tibble object in R
#Subsequently it cleans the data removing duplicates and empty lines etc.
#This is the step one of the analysis.

library(readxl)
library(dplyr)
library(stringi)
library(tidyr)

#Reading data -------------

#The data was collected by three people, so the information is across three different
#xlxs files
flora_table1 <- read_excel("data/raw/Planilha de Dados Botânicos Parte 1- Lucas.xlsx")
flora_table2 <- read_excel("data/raw/Planilha de Dados Botânicos Parte 2- Isabela.xlsx")
flora_table3 <- read_excel("data/raw/Planilha de Dados Botânicos Parte 3.xlsx")

#Now we combine the datasets all in one single table
flora_table_complete <- rbind(flora_table1, flora_table2, flora_table3)

#Cleaning the data --------------

#We start cleaning the empty lines in the table
flora_table_clean <- flora_table_complete %>% filter_all(any_vars(!is.na(.)))
anti_join(flora_table_complete, flora_table_clean)

#Now we check for duplicate lines and exclude them
flora_table_clean <- flora_table_clean %>% distinct()

#We want to avoid naming columns using names that contain spaces
flora_table_clean <- flora_table_clean %>%
  rename_with(~ tolower(gsub(" ", ".", .x, fixed = TRUE)))

#Removing accents from columns names
flora_table_clean <- flora_table_clean %>%
  rename_with(~ stri_trans_general(.x, "Latin-ASCII"))

#Now we have to exclude the lines where the first column is a manually insertion of
#the classification and the others are NA, as for example "BRIÓFITAS E PTERIDÓFITAS, NA, ..."
flora_table_final <- flora_table_clean[!(stri_detect_regex(
  stri_trans_general(flora_table_clean$colecao, "Latin-ASCII"), "^[A-Z ]+$") 
                     & is.na(flora_table_clean$nº.tombo)),]

#We check to see if we didn't exclude any relevant lines in the previous change in the table.
#We can see that one of the lines doesn't have an identifier, we can suppose that in this case
#it was a error in the manual typing 
diff_tables <- anti_join(flora_table_clean, flora_table_final)

#For some reason we see that the previous manipulation add two lines with only NAs
#We again remove these empty lines
flora_table_final <- flora_table_final %>% filter_all(any_vars(!is.na(.)))

#We output this table in the data folder
if (!dir.exists("data/output")) dir.create("data/output")
save(flora_table_final, file="data/output/01_flora_table.rdata")
