###################Jointure 
library(readxl)
library(lubridate)
library(fixest)
library(dplyr)
library(writexl)
##Data_ Importatuion

ERA <- read_excel("C:/Users/OumyDIONE/Desktop/Travaux _withThierry/Data/RX5day_France_2000_2024.xlsx")
SPEI <- read_excel("C:/Users/OumyDIONE/Desktop/Travaux _withThierry/Data/SPEI6_france_2000_2024.xlsx")
WIND <-read_excel("C:/Users/OumyDIONE/Desktop/Travaux _withThierry/Data/WindExtreme_France_2000_2024.xlsx")

names(ERA)
names(SPEI)
names(WIND)

str(ERA$date)
str(SPEI$date)
str(WIND$date)

ERA  <- ERA  %>% mutate(date = as.Date(date))
SPEI <- SPEI %>% mutate(date = as.Date(date))
WIND <- WIND %>% mutate(date = as.Date(date))


ERA <- ERA %>%
  mutate(date = ceiling_date(date, "month") - days(1))

SPEI <- SPEI %>%
  mutate(date = ceiling_date(date, "month") - days(1))

WIND <- WIND %>%
  mutate(date = ceiling_date(date, "month") - days(1))



head(ERA$date)
head(SPEI$date)
head(WIND$date)

Climate <- ERA %>%
  left_join(SPEI, by = "date") %>%
  left_join(WIND, by = "date")

names(Climate)

head(Climate)

colSums(is.na(Climate))

library(writexl)
write_xlsx(
  Climate,
  "C:/Users/OumyDIONE/Desktop/Travaux _withThierry/Data/Climate_2000_2024.xlsx"
)
