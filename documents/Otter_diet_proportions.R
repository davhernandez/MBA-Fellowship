#setup

library(ggplot2)
library(dplyr)
library(readxl)

prey_species <- read_xlsx("~/Desktop/Grad School/github/MBA Fellowship/data/SeaOtter_Diet_data_DavidH.xlsx", sheet = 3)

prey_species$dungeness <- apply(prey_species, 1, function(x) ifelse(x["Prey Species"] == "dun", "Dungeness", "Other"))

#stacked histograms based on area and then prey species
prey_species %>%
  filter(average != 0) %>%
  ggplot(aes(x = Area, y = average, fill = dungeness)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Proportion of Diet")