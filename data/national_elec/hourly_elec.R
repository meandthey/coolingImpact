library(tidyverse)
library(lubridate)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)

# 파일 목록 불러오기
file_list <- list.files(path = "./", pattern = "hourly_elecUse.*\\.csv$", full.names = TRUE)

# 각 파일을 읽어서 하나로 합치기
all_data <- file_list %>%
  map_dfr(~ read_csv(.x, locale = locale(encoding = "EUC-KR")))


# 데이터 정리: 시간대 컬럼을 long format으로 변환
elec_long <- all_data %>%
  pivot_longer(cols = -날짜, names_to = "hour", values_to = "demand") %>%
  mutate(
    hour = parse_number(hour),
    datetime = ymd(날짜) + hours(hour - 1)
  )


ggplot(elec_long, aes(x = datetime, y = demand)) +
  geom_point(alpha = 0.3, size = 0.2) +
  labs(title = "Hourly Electricity Demand (Nationwide)", x = "Time", y = "Demand (MWh)") +
  theme_minimal()
