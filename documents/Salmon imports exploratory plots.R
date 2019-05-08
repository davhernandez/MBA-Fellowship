library(dplyr)
library(ggplot2)

setwd("~/Desktop/Grad School/github/MBA Fellowship")

#import data ------------------------------------------
salmon_raw <- read.csv("data/Atlantic Salmon Imports 1995-2019.csv")
salmon_raw <- salmon_raw[grep("FARMED", salmon_raw$Product.Name),] #grab rows with partial match for `FARMED` in `Product Name`
salmon_raw <- rename(salmon_raw, Product = Product.Name)
#combine dates into single column & calculate price per kilo
salmon_raw <- mutate(salmon_raw,
                     Date = as.Date(paste(salmon_raw$Year, salmon_raw$Month, "1", sep = "/"), format = "%Y/%m/%d"),
                     Price_per_Kilo = salmon_raw$Kilo/salmon_raw$Dollars)
#remove 2019 entries because the data is incomplete
salmon_raw <- filter(salmon_raw, Year != "2019")

#Save directory ------------------------------------------------------------
setwd("~/Desktop/Grad School/github/MBA Fellowship/plots/Farmed Salmon")

#Exploratoy plots --------------------------------------------------

#all products produced by most productive countries
salmon_raw %>%
  group_by(Date, Country) %>%
  summarise(Kilos = sum(Kilos)) %>%
  filter(Country %in% c("CANADA", "CHILE", "FAROE IS.", "NORWAY", "UNITED KINGDOM")) %>%
  { #wrap the pipe in `{}` and the data will only be piped to where you call `.`
    ggplot(. , aes(x = Date, y = Kilos, colour = Country)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Farmed Atlantic Salmon Product Imports") +
      theme_classic()
  }
#ggsave("All Farmed Imports from Most Productive Countries.pdf")

#Canadian imports by product type
salmon_raw %>%
  filter(Country == "CANADA") %>%
  group_by(Product, Date) %>%
  summarise(Kilos = sum(Kilos)) %>%
  {
    ggplot(. , aes(x = Date, y = Kilos, colour = Product)) +
      geom_point() +
      geom_line() +
      ggtitle("Atlantic Salmon Imports from Canada by Product") +
      theme_classic()
  }  
#ggsave("Imports from Canada by Product Type.pdf")

#Chilean imports by product type
salmon_raw %>%
  filter(Country == "CHILE") %>%
  group_by(Product, Date) %>%
  summarise(Kilos = sum(Kilos)) %>%
  {
    ggplot(. , aes(x = Date, y = Kilos, colour = Product)) +
      geom_point() +
      geom_line() +
      ggtitle("Atlantic Salmon Imports from Chile by Product") +
      theme_classic()
  }
#ggsave("Imports from Chile by Product Type.pdf")

#Norwegian imports by product type
salmon_raw %>%
  filter(Country == "NORWAY") %>%
  group_by(Product, Date) %>%
  summarise(Kilos = sum(Kilos)) %>%
  {
    ggplot(. , aes(x = Date, y = Kilos, colour = Product)) +
      geom_point() +
      geom_line() +
      ggtitle("Atlantic Salmon Imports from Norway by Product") +
      theme_classic()
  }
#ggsave("Imports from Norway by Product Type.pdf")

#price per kilo worldwide
salmon_raw %>%
  filter(Country %in% c("CANADA", "CHILE", "FAROE IS.", "NORWAY", "UNITED KINGDOM")) %>%
  group_by(Date, Country) %>%
  summarise(Price_per_Kilo = sum(Price_per_Kilo)) %>%
  {
    ggplot(., aes(x = Date, y = Price_per_Kilo, colour = Country)) +
      geom_point() +
      geom_line() +
      ggtitle("Atlantic Salmon Imports Price per Kilo by Country") +
      ylab("Price per Kilo") +
      theme_classic()
  }

#price per kilo worldwide; smoothed
salmon_raw %>%
  filter(Country %in% c("CANADA", "CHILE", "FAROE IS.", "NORWAY", "UNITED KINGDOM")) %>%
  group_by(Date, Country) %>%
  summarise(Price_per_Kilo = sum(Price_per_Kilo)) %>%
  {
    ggplot(., aes(x = Date, y = Price_per_Kilo, colour = Country)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Atlantic Salmon Imports Price per Kilo by Country") +
      ylab("Price per Kilo") +
      theme_classic()
  }
#ggsave("Price per Kilo by Country.pdf")

#price per kilo worldwide by year
salmon_raw %>%
  filter(Country %in% c("CANADA", "CHILE", "FAROE IS.", "NORWAY", "UNITED KINGDOM")) %>%
  group_by(Year, Country) %>%
  summarise(Price_per_Kilo = sum(Price_per_Kilo)) %>%
  {
    ggplot(., aes(x = Year, y = Price_per_Kilo, colour = Country)) +
      geom_point() +
      geom_line() +
      ggtitle("Atlantic Salmon Imports Price per Kilo per Year by Country") +
      ylab("Price per Kilo") +
      theme_classic()
  }
#ggsave("Price per Kilo per Year by Country.pdf")

#price per kilo by product from Canda
salmon_raw %>%
  filter(Country == "CANADA") %>%
  group_by(Date, Product) %>%
  summarise(Price_per_Kilo = sum(Price_per_Kilo)) %>%
  {
    ggplot(. , aes(x = Date, y = Price_per_Kilo, colour = Product)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Price per Kilo of Canadian Imports by Product") +
      ylab("Price per Kilo") +
      theme_classic()
  }
#ggsave("Price per Kilo by Product from Canada.pdf")

#RAW MONEY!!!!!!!
salmon_raw %>%
  filter(Country %in% c("CANADA", "CHILE", "FAROE IS.", "NORWAY", "UNITED KINGDOM")) %>%
  group_by(Year, Country) %>%
  summarise(Dollars = sum(Dollars)) %>%
  {
    ggplot(., aes(x = Year, y = Dollars, colour = Country)) +
      geom_point() +
      geom_line() +
      ggtitle("Atlantic Salmon Imports Price per Year by Country") +
      ylab("Price ($)") +
      theme_classic()
  }
#ggsave("Raw money.pdf")  
