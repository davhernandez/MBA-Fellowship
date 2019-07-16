#setup

library(ggplot2)
library(dplyr)
library(readxl)

prey_species <- read_xlsx("~/Desktop/Grad School/github/MBA Fellowship/data/SeaOtter_Diet_data_DavidH.xlsx", sheet = 3)
prey_dictionary <- read_xlsx("~/Desktop/Grad School/github/MBA Fellowship/data/SeaOtter_Diet_data_DavidH.xlsx", sheet = 4)

prey_species$dungeness <- apply(prey_species, 1, function(x) ifelse(x["Prey Species"] == "dun", "Dungeness", "Other"))

prey_species <- left_join(prey_species, prey_dictionary, by = c("Prey Species" = "Prey")) %>%
  filter(average != 0)


#stacked histograms based on area and then prey species
ggplot(prey_species, aes(x = Area, y = average, fill = dungeness)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Proportion of Diet")

ggplot(prey_species, aes(x = Area, y = average, fill = ClassName)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Proportion of Diet")
