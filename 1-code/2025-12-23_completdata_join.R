library(readxl)
library(lubridate)
library(fixest)
library(dplyr)
library(writexl)

economic <- read_excel("C:/Users/OumyDIONE/Desktop/Travaux _with/Data/Economic_data.xlsx")
climate  <- read_excel("C:/Users/OumyDIONE/Desktop/Travaux _with/Data/Climate_2000_2024.xlsx")

economic$Période <- as.character(economic$Période)
climate$date <- as.character(climate$date)

economic$Période <- parse_date_time(
  economic$Période,
  orders = c("Y-m-d", "d/m/Y", "d-m-Y", "Y/m/d", "Y-m", "m/Y")
)

climate$date <- parse_date_time(
  climate$date,
  orders = c("Y-m-d", "d/m/Y", "d-m-Y", "Y/m/d", "Y-m", "m/Y")
)

economic <- economic %>%
  mutate(time = format(as.Date(Période), "%Y-%m"))

climate <- climate %>%
  mutate(time = format(as.Date(date), "%Y-%m"))

data_all <- inner_join(
  economic %>% select(-Période),
  climate %>% select(-date),
  by = "time"
)

names(data_all)
head(data_all)


###########################
data_all <- data_all %>%
  select(
    time,
    `CAC 40`,
    IPI_growth,
    Inflation,
    ShortRate,
    LongRate,
    RX5day,
    SPEI,
    WindExtreme
  )



library(writexl)

write_xlsx(
  data_all,
  "C:/Users/OumyDIONE/Desktop/Travaux _with/Data/data_GSE_2000_2024.xlsx"
)

