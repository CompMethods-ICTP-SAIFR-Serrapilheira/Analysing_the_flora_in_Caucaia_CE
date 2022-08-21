#Date: August 20, 2022
#Author: Cristiana Aparecida Nogueira Couto
#This script plots the richness by genus in Caucaia

library(ggplot2)
library(tidyverse)

#Importing the data
flora_genus <- read.csv("data/output/03_flora_by_genus.csv", header=TRUE)

#Plotting the data ----------------
flora_genus_aux <- flora_genus %>%
  arrange(desc(richness_by_genus)) %>% 
  filter_all(any_vars(!is.na(genero)))

flora_genus_first_30 <- flora_genus_aux[1:30,]

#The number of genus found was 312
length(flora_genus_aux$genero)

#Plotting the data
p <- ggplot(data = flora_genus_first_30, aes(x = reorder(genero, -richness_by_genus), y = richness_by_genus)) + 
  geom_bar(stat = "identity", fill="#a2de70") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Genus", y = "Number of occurences") +
  theme(plot.title = element_text(hjust = 0.5))

#Richness by genus in Caucaia-CE
p

#Exporting figure
ggsave(path = "figs", 
       filename = "04_richness_by_genus.png", 
       plot = p, device = 'png')
