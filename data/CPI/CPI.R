library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(openxlsx)

# 1. 파일 불러오기
powerCPI_raw <- read_excel("전기료지수_품목별_소비자물가지수_품목성질별_2020100__20250625135505.xlsx", sheet = "데이터")
totalCPI_raw <- read_excel("총지수_품목별_소비자물가지수_품목성질별_2020100__20250625134938.xlsx", sheet = "데이터")  



# 1. 전기료 CPI
powerCPI_long <- powerCPI_raw %>%
  mutate(시도별 = str_squish(시도별)) %>%
  pivot_longer(cols = matches("^\\d{4}\\.\\d{2}$"),
               names_to = "ym", values_to = "powerCPI") %>%
  mutate(
    ym = str_remove_all(ym, "`"),
    powerCPI = as.numeric(powerCPI)  
  )

# 2. 전체 CPI
totalCPI_long <- totalCPI_raw %>%
  mutate(시도별 = str_squish(시도별)) %>%
  pivot_longer(cols = matches("^\\d{4}\\.\\d{2}$"),
               names_to = "ym", values_to = "totalCPI") %>%
  mutate(
    ym = str_remove_all(ym, "`"),
    totalCPI = as.numeric(totalCPI)
  )

# 3. 실질 전력물가지수 계산 (지역 및 시점 기준 결합)
real_powerCPI <- powerCPI_long %>%
  left_join(totalCPI_long, by = c("시도별", "ym")) %>%
  mutate(
    powerCPI = as.numeric(powerCPI),
    totalCPI = as.numeric(totalCPI),
    real_powerCPI = powerCPI / totalCPI
  ) %>%
  mutate(
    year = as.integer(str_extract(ym, "^\\d{4}")),
    month = as.integer(str_extract(ym, "(?<=\\.)\\d{2}"))
  ) %>%
  select(시도 = 시도별, ym, year, month, real_powerCPI)


  

write.csv(real_powerCPI, "real_powerCPI.csv", fileEncoding = c("EUC-KR"))



