# Analysing the flora in Caucaia-CE

This short project proposes a initial data analysis of a dataset including all plants register in a herbarium database collected in the municipality of Caucaia in Ceará. Data used for this project was a compilation of the datasets collected using the Specieslink, GBif and reflora database. The dataset contains the herbarium where the dried plant was deposited, identifier number of herbarium specimen, taxonomic classification, the type of vegetation, substrate where the species is found and the geographic coordinates of the collection site. 


## Project structure

Above we have the project structure folder, where you can find all the scripts used, the raw data and processed output, besides the figures and reports that can be found in the docs folder.
```
Analysing_the_flora_in_Caucaia_CE/
*    ├── data/
*    │   ├── raw
*    │   └── output
     ├── docs/
*    ├── figs/
     ├── R/
*    ├── citation/
*    └── README.md
```

## Requirements
The R packages required to reproduce the scripts are: readxl, dplyr, stringi, tidyr, ggplot2, tidyverse and readr.
