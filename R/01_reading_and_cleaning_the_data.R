#This script reads the raw data in xlsx format to produce a tible object in R
#Subsequently it cleans the data, removing repeated lines etc
#This is the step one of the analysis

#Reading data -------------
library(readxl)
library(dplyr)

#The data was collected by three people, so the information is across three different
#xlxs files
flora_table1 <- read_excel("data/raw/Planilha de Dados Botânicos Parte 1- Lucas.xlsx")
flora_table2 <- read_excel("data/raw/Planilha de Dados Botânicos Parte 2- Isabela.xlsx")
flora_table3 <- read_excel("data/raw/Planilha de Dados Botânicos Parte 3.xlsx")


#Now we combine the datasets all in one single table
flora_table_complete <- rbind(flora_table1, flora_table2, flora_table3)

#Cleaning the data --------------

#We start cleaning the empty line in the table
flora_table_clean <- flora_table_complete %>% filter_all(any_vars(!is.na(.)))

#After checking the complete table, it was possible to see that some of them have duplicate 
#lines, so we are going to exclude them
flora_table_clean <- flora_table_complete %>% distinct()

