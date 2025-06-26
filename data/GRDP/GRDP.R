library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(openxlsx)

# 1. 파일 불러오기
grdp_raw <- read_excel("GRDP_시_군_구__20250618093735.xlsx", sheet = "데이터")
index_raw <- read_excel("시도_산업별_광공업생산지수_2020100__20250625124201.xlsx", sheet = "데이터")  # 소매판매액지수 시트로 조정



  
# 2. 시군구 GRDP 전처리: '지역', '연도', 'GRDP' 형태로 변환
grdp_long <- grdp_raw %>%
  slice(-1) %>%
  pivot_longer(cols = c(-1, -2), names_to = "year", values_to = "grdp") %>%
  mutate(year = as.integer(year),
         grdp = as.numeric(grdp)) %>%
  mutate(`행정구역별(2)` = case_when(
    
    `행정구역별(1)` == "충청북도"&`행정구역별(2)` == "통합청주시" ~ "청주시",
    `행정구역별(1)` == "충청북도"&`행정구역별(2)` == "청원군" ~ "청주시",
    `행정구역별(1)` == "경상남도"&`행정구역별(2)` == "통합창원시" ~ "창원시",
    TRUE ~ `행정구역별(2)`
  )) %>%
  mutate(grdp = replace_na(grdp,0)) %>%
  group_by(`행정구역별(1)`, `행정구역별(2)`, year) %>% summarize(grdp = sum(grdp)) %>%
  rename(시도 = `행정구역별(1)`, 시군구 = `행정구역별(2)`) %>%
  mutate(unit = "백만원_2015년 기준가격",
         시도_시군구 = paste(시도, 시군구, sep = "_"))

# 3. 생산지수: 시도, 연도, 월별 인덱스
index_long <- index_raw %>%
  slice(-1) %>%
  pivot_longer(
    cols = starts_with("20"),  # "2010.01", "2010.02", ...
    names_to = "ym",
    values_to = "index"
  ) %>%
# 2. 'ym' → year, month 분리
  mutate(
    ym = str_remove_all(ym, "`"),  # 백틱 제거
    year = str_extract(ym, "^\\d{4}"),
    month = str_extract(ym, "\\.(\\d{2})$") %>% str_remove("\\."),  # 월만 추출
    year = as.integer(year),
    month = as.integer(month),
    index = as.numeric(index)
  ) %>%
  select(-산업별) %>%
  rename(시도 = 시도별)

####### 쪼개기 ####### 

# 1. 월별 산업지수 비율 계산 (연도별·시도별 총합 대비 비중)
index_ratio <- index_long %>%
  filter(!is.na(month)) %>%  # NA 제외 (예: p), 예측치 포함 시 판단 필요
  group_by(시도, year) %>%
  mutate(month_ratio = index / sum(index, na.rm = TRUE)) %>%
  ungroup()

# 2. 연간 GRDP와 산업지수 비중 결합
grdp_monthly <- grdp_long %>%
  left_join(index_ratio, by = c("시도", "year")) %>%
  mutate(grdp_month = grdp * month_ratio) %>%
  select(시도, 시군구, year, month, grdp_month)

# 3. 정렬 및 확인
grdp_monthly <- grdp_monthly %>%
  arrange(시도, 시군구, year, month)

# 결과 
#write.csv(grdp_monthly, "grdp_monthly.csv", fileEncoding = 'EUC-kr')
