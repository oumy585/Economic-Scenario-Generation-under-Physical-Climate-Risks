##########################  Cac40 treatment 

library(readxl)
library(dplyr)
library(lubridate)
library(xts)
library(writexl)
#"C:/Users/OumyDIONE/Downloads/CAC40 Index.xlsx"
CAC <- read_excel(choose.files())

CAC <- CAC %>%
  arrange(Date)

CAC_xts <- xts(CAC$PX_LAST, order.by = CAC$Date)

CAC_month_price <- CAC_xts[endpoints(CAC_xts, on = "months")]

CAC_month_return <- diff(log(CAC_month_price)) * 100

CAC_month <- data.frame(
  Date = index(CAC_month_return),
  CAC_return = coredata(CAC_month_return)
)

CAC_month <- CAC_month %>%
  filter(Date >= as.Date("2000-01-01"),
         Date <= as.Date("2024-12-31"))

write_xlsx(
  CAC_month,
  "CAC40_log_return_monthly_2000_2024.xlsx"
)

