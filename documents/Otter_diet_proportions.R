#setup

library(ggplot2)
library(dplyr)
library(readxl)

prey_species <- read_xlsx("~/Desktop/Grad School/github/MBA Fellowship/data/SeaOtter_Diet_data_DavidH.xlsx", sheet = 3)
prey_dictionary <- read_xlsx("~/Desktop/Grad School/github/MBA Fellowship/data/SeaOtter_Diet_data_DavidH.xlsx", sheet = 4)

prey_species <- left_join(prey_species, prey_dictionary, by = c("Prey Species" = "Prey")) %>%
  filter(average != 0)

#make a column identifying Dungeness, unidentified crab, and all other prey
prey_species$dungeness <- apply(prey_species, 1, function(x)
  if(x["Prey Species"] == "dun"){
    x["dungeness"] <- "Dungeness" 
  } else if(x["ClassName"] == "crab_other"){
    x["dungeness"] <- "Unidentified Crab"
  } else {
    x["dungeness"] <- "Other"
  }
)

#reassign Class Name of Dungeness crabs from 'cancer_crab' to 'dungeness'
prey_species$ClassName <- apply(prey_species, 1, function(x) ifelse(x["Prey Species"] == "dun", "Dungeness", x["ClassName"]))

#stacked histograms based on area and then prey species
ggplot(prey_species, aes(x = Area, y = average, fill = factor(dungeness, levels = c("Dungeness", "Unidentified Crab", "Other")))) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Proportion of Diet") +
  labs(fill = "Prey Item")

#color values for the stacked histogram
cols <- c("Dungeness" = "#0044FF",
          "cancer_crab" = "#003FED",
          "kelp_crab" = "#003ADB",
          "crab_other" = "#0036C9",
          "abalone" = "#FF9966",
          "chiton" = "#FF9560",
          "clam" = "#FF915B",
          "cucumber" = "#FF8E55",
          "fish" = "#FF8A50",
          "fishegg" = "#FF874B",
          "mussel" = "#FF8345",
          "octopus" = "#FF7F40",
          "sand_crab" = "#FF7C3B",
          "sand_dollar" = "#FF7835",
          "small_crustacean" = "#FF7530",
          "small_kelp_invert" = "#FF712A",
          "small_mollusk" = "#FF6E25",
          "snail" = "#FF6A20",
          "sponge" = "#FF661A",
          "squid"= "#FF6315",
          "star" ="#FF5F10",
          "tunicate" = "#FF5C0A",
          "urchin" = "#FF5805",
          "worm" = "#FF5500")
#defining order of the levels for the stacked histogram
factor_levels <- c("Dungeness", "cancer_crab", "kelp_crab", "crab_other", "abalone", "chiton", "clam", "cucumber", "fish", "fishegg", "mussel", "octopus", "sand_crab", "sand_dollar", "small_crustacean", "small_kelp_invert", "small_mollusk", "snail", "sponge", "squid", "star", "tunicate", "urchin", "worm")

#stacked histogram of all classes
ggplot(prey_species, aes(x = Area, y = average, fill = factor(ClassName, levels = factor_levels))) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Proportion of Diet") +
  scale_fill_manual(values = cols) +
  labs(fill="Prey Items") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"))

