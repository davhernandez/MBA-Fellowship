#setup --------------------------------------

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

#determine numerical proportions ------------------------------------------

#Elkhorn Slough
prey_species %>%
  filter(Area == "ES") %>%
  group_by(ClassName) %>%
  summarize(amount = sum(average))

#Monterey both areas combined and averaged
prey_species %>%
  filter(Area != "ES") %>%
  group_by(ClassName) %>%
  summarize(amount = sum(average)/2)

#New legend -----------------------------------------------------

#dictionary for find and replace
  #the find column is in the form that gsub takes for OR arguments
find_and_replace <- matrix(data = c(#"cancer_crab", "crab_other",
                "abalone|chiton|clam|mussel|small_mollusk|snail", "mollusk",
                "cucumber|sand_dollar|star|urchin", "echinoderm",
                "fish|fishegg", "fish or fish egg",
                "octopus|squid", "cephalopod",
                "sand_crab|small_crustacean", "other_crustacean"),
                nrow = 5, ncol = 2, byrow = TRUE,
                dimnames = list(c(), c("find", "replace"))
                )
#for each pair, find and replace
for(i in 1:nrow(find_and_replace)){
  prey_species$ClassName <- gsub(find_and_replace[i,"find"], find_and_replace[i,"replace"], prey_species$ClassName)
}

#separate the two locations and make a new composite long dataframe
composite <- rbind(
  #Elkhorn Slough
  (prey_species %>%
    filter(Area == "ES") %>%
    group_by(ClassName) %>%
    summarize(amount = sum(average))%>%
     mutate(location = "Elkhorn Slough"))
  ,
#Monterey both areas combined and averaged
  (prey_species %>%
    filter(Area != "ES") %>%
    group_by(ClassName) %>%
    summarize(amount = sum(average)/2) %>%
     mutate(location = "Monterey Bay"))
)

legend_factors <- c("Dungeness", "cancer_crab", "kelp_crab", "crab_other", "other_crustacean", "echinoderm", "mollusk", "sponge", "fish or fish egg", "cephalopod", "worm", "tunicate", "small_kelp_invert")

#plot
ggplot(composite, aes(x = location, y = amount, fill = factor(ClassName, levels = legend_factors))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(colorRampPalette(c("#3c0164", "#d78fff"))(4), colorRampPalette(c("#b4fff3", "#00342c"))(9))) +
  scale_y_continuous(labels = function(x) paste(x*100, "%", sep = "")) + #make y-axis ticks percentage
  labs(fill="Prey Items") +
  theme_classic() +
  ylab("biomass percentage") +
  ggtitle("Sea Otter Diet Composition") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        axis.title.x = element_blank(), #drop "location" from the x-axis
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")), #move y axis label out
        axis.text.x = element_text(margin = margin(c(0.5,0.2), unit = "cm"), size = 10, color = "black"), #make the locations look the same as the -axis label
        axis.ticks.length=unit(-0.1, "cm"))

#sandbox -----------------------------------------------------------
for(i in 1:6){
  dummy$ClassName[which(dummy$ClassName == find_and_replace[i,"find"])] <- find_and_replace[i,"replace"]
}

for(i in 1:6){
if(prey_species$ClassName[which()] %in% find_and_replace[i,"find"])
  dummy <- find_and_replace[i,"replace"]
}

dummy <- prey_species
dummy$ClassName[which(dummy$ClassName == find_and_replace[2,"find"])] <- find_and_replace[2,"replace"]


apply(prey_species[,6], 1, function(x) gsub(find, replace, x))




apply(find_and_replace, 1, function(x) gsub(x["find"], x["replace"], prey_species$ClassName)) %>% View

replace <- function(prey_species) {
  
  prey_species$ClassName = ifelse(prey_species$ClassName == find_and_replace[,1],
                                  find_and_replace[,2],
                                  prey_species$ClassName) 
}

apply(find_and_replace, 1, replace)

dummy <- prey_species

for(j in 1:nrow(prey_species)){
  for(i in 1:nrow(find_and_replace)){
    dummy$ClassName[j] <- ifelse(prey_species$ClassName[j] == find_and_replace[i,"find"],
                                 gsub(find_and_replace[i,"find"], find_and_replace[i,"replace"], prey_species$ClassName[j]),
                                 prey_species$ClassName[j])
  }
}

ifelse(prey_species$ClassName[j] == find_and_replace[i,"find"],
       gsub(find_and_replace[i,"find"], find_and_replace[i,"replace"], prey_species$ClassName[j]),
       prey_species$ClassName[j])

matrix(c("Cook", "Family 1",
     "Rivera|Rogers", "Family 2",
     "Torres", "Family 3"),
     nrow = 3, ncol =2, byrow = TRUE,
     dimnames = list(c(), c("find", "replace")))

dummy <- prey_species$ClassName
for(i in 1:6){
  dummy <- gsub(find_and_replace[i,"find"], find_and_replace[i,"replace"], dummy)
}

apply(find_and_replace, 1, function(x){
  dummy <- gsub(x["find"], x["replace"], dummy)
}) %>% View
