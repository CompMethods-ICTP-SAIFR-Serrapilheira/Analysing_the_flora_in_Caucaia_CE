#Date: August 17, 2022
#Author: Cristiana Aparecida Nogueira Couto
#This script reads the output data from 01_reading_and_cleaning_data.R script to continue
#the cleaning of the dataset, now focusing on solving the problems with different format of the 
#names on the rows.

library(readr)

#Importing data ----------
X01_flora_table <- read_csv("data/output/01_flora_table.csv")

#Now we see that some of the lines with the identifier number has a abbreviation
#regarding the collection where the dry plant is found
#So we create a new column for this abbreviation and leave the "nº.tombo" column just 
#with the number
X01_flora_table <- X01_flora_table %>%
  mutate(colecao.abrev = stri_extract_first_regex(nº.tombo, "^[A-Z]+"),
         .after = colecao,
         nº.tombo = stri_extract_first_regex(nº.tombo, "[0-9]+"))


#Now we check which rows don't have the abbreviation and extract them from the collection name
X01_flora_table <- X01_flora_table %>%
  mutate(colecao.abrev = replace_na(stri_extract_first_regex(colecao, "[A-Z]{2,}")))

#Now we check if the abbreviation in colecao.abrev matches the abbreviation in the colecao column
#We do this to see if the abbreviation with the identifier number matches the one in the collection
#name
X01_flora_table <- X01_flora_table[stri_detect_regex(X01_flora_table$colecao, 
                                                      X01_flora_table$colecao.abrev),]


#Some of the other errors identified were regarding the names of the collection
#Some rows contains the abbreviation, others the complete name of the herbarium
#So we change it to have only one format


#Another issue spotted was that some information is placed in the wrong column, 
#for example the popular name of a plant is on the column that refers to the conservation
#status

