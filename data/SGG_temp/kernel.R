# 필요한 패키지 로딩
library(readr)   # read_csv
library(dplyr)   # 데이터 조작
library(purrr)   # map_dfr
library(tibble)
library(ggplot2)
library(stringr)
library(broom)
library(tidyr)
library(readxl)
library(timeDate)
library(lubridate)

## sido name ##

# 시도 이름 리스트
sido_list <- c("서울특별시", "부산광역시", "대구광역시", "인천광역시",
               "광주광역시", "대전광역시", "울산광역시", "세종특별자치시",
               "경기도", "강원도", "충청북도", "충청남도",
               "전라북도", "전라남도", "경상북도", "경상남도", "제주특별자치도")

sido_mapping <- tibble(
  old = c("강원특별자치도", "전북특별자치도", "경기도", "서울특별시", "부산광역시",
          "대구광역시", "인천광역시", "광주광역시", "대전광역시", "울산광역시",
          "세종특별자치시", "충청북도", "충청남도", "전라남도", "전라북도",
          "경상북도", "경상남도", "제주특별자치도"),
  new = c("강원도", "전라북도", "경기도", "서울특별시", "부산광역시",
          "대구광역시", "인천광역시", "광주광역시", "대전광역시", "울산광역시",
          "세종특별자치시", "충청북도", "충청남도", "전라남도", "전라북도",
          "경상북도", "경상남도", "제주특별자치도")
)
# 1. 폴더 경로 지정
folder_path <- "./tempData/"  # 현재 작업 디렉토리 (필요시 수정)

# 2. 폴더 내 모든 .csv 파일 목록 가져오기
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# 3. 파일 이름을 이름으로 붙여줌 (소스 추적용)
names(file_list) <- basename(file_list)

# 4. 모든 파일 읽어서 병합 (EUC-KR 인코딩 지정)
all_temperature_data <- map_dfr(file_list, ~ read_csv(.x, locale = locale(encoding = "euc-kr")), .id = "source_file")

# 5. 일시 변수 변환 (POSIXct), 정렬
all_temperature_data <- all_temperature_data %>%
  mutate(일시 = as.POSIXct(일시, format = "%Y-%m-%d %H:%M")) %>%
  arrange(지점, 일시)


# 기온 데이터의 지점 >> 전력 데이터의 지역으로 변환 #
power_ma_regionMapping <- readr::read_csv("./mappingRegion/SGG_mapping.csv", locale = locale(encoding = "euc-kr")) %>%
  rename(지점명 = 기상청지역)


all_temperature_data_rename <- all_temperature_data %>%
  left_join(power_ma_regionMapping, by = c("지점명")) %>%
  mutate(한전_지역명 = paste0(한전_시도, 한전_시군구)) %>%
  filter(한전_지역명 != "NANA")


  

## temperature data check ##
# 지점별 시계열 시작일, 종료일, 총 개수 요약
# time_coverage_summary <- all_temperature_data_rename %>%
#   group_by(지점명, 한전_시군구, 한전_시도) %>%
#   summarise(
#     시작일 = min(일시, na.rm = TRUE),
#     종료일 = max(일시, na.rm = TRUE),
#     총시간수 = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(시작일)
# 
# # 결과 확인
# print(time_coverage_summary)



### Kernel

# s값 표준화 포함
temp_data_std <- all_temperature_data_rename %>%
  mutate(
    s = (`기온(°C)` + 20) / 60,
    year = as.integer(format(일시, "%Y")),
    month = as.integer(format(일시, "%m"))
  )

# 지점별-연도별-월별 커널 분포 추정
kernel_by_jym <- temp_data_std %>%
  group_by(한전_지역명, 한전_시도, 한전_시군구, year, month) %>%
  group_split() %>%
  keep(~ nrow(.x) >= 10) %>%   # ⚠️ 이 줄 추가
  set_names(map_chr(., ~ paste0(unique(.x$한전_지역명), "_", unique(.x$year)*100 + unique(.x$month)))) %>%
  map(~ density(.x$s, bw = "nrd0", kernel = "gaussian"))


# 기존 kernel_by_jym의 key: "수원시_200401" 형태
contract_types <- c("일반용", "주택용")

kernel_by_jym_extended <- map_dfr(names(kernel_by_jym), function(base_name) {
  kde <- kernel_by_jym[[base_name]]
  
  # 시군구, 연월 분리
  parts <- str_match(base_name, "^(.+?)_(\\d{6})$")
  if (any(is.na(parts))) return(NULL)
  
  지역 <- parts[2]
  연월 <- parts[3]
  
  # 계약종별별로 복제
  map_dfr(contract_types, function(ct) {
    tibble(
      key = paste0(지역, "_", ct, "_", 연월),
      kde = list(kde)
    )
  })
})

# 리스트로 변환
kernel_by_jym_final <- set_names(kernel_by_jym_extended$kde, kernel_by_jym_extended$key)


#################################################################
###########################  시각화   ###########################
#################################################################


#############################  시각화 전지점 예제 #############################
# # 2020년 해당하는 커널 객체만 필터
# kernels_2020 <- kernel_by_jym[str_detect(names(kernel_by_jym), "_2020")]
# 
# # 커널 객체 → long-format 데이터프레임으로 변환
# kdf_2020 <- map2_dfr(
#   kernels_2020,
#   names(kernels_2020),
#   function(kde, label) {
#     tibble(
#       s = kde$x,
#       density = kde$y,
#       month = str_sub(label, -2),  # 마지막 2자리: 월
#       station = str_remove(label, "_2020[0-9][0-9]")  # 지점명 추출
#     )
#   }
# )
# 
# ggplot(kdf_2020, aes(x = s, y = density, color = station)) +
#   geom_line(alpha = 0.8) +
#   facet_wrap(~ month, ncol = 4) +
#   labs(
#     title = "2020년 월별 각 지점 기온분포 (커널 밀도)",
#     x = "표준화 기온 s ∈ [0,1]",
#     y = "밀도",
#     color = "지점"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")  # 너무 많을 경우 범례 생략







#################################################################
###########################  regression   ###########################
#################################################################

# 1. 표준화 기온 그리드
s_grid <- seq(0, 1, length.out = 200)
ds <- diff(s_grid)[1]

# 2. 커널 → 적분 벡터
kernel_df <- map2_dfr(kernel_by_jym_final, names(kernel_by_jym_final), function(kde, label) {
  f_s <- approx(kde$x, kde$y, xout = s_grid, rule = 2)$y
  
  tibble(
    key = label,
    x1 = sum(s_grid * f_s) * ds,
    x2 = sum(s_grid^2 * f_s) * ds,
    x3 = sum(cos(2 * pi * s_grid) * f_s) * ds,
    x4 = sum(sin(2 * pi * s_grid) * f_s) * ds
  )
}) %>%
  mutate(
    시군구 = str_extract(key, "^[^_]+"),
    계약종별 = str_match(key, "^[^_]+_([^_]+)_")[,2],
    연월 = str_extract(key, "\\d{6}"),
    연도 = as.integer(substr(연월, 1, 4)),
    월 = as.integer(substr(연월, 5, 6))
  ) %>%
  select(-key)



###############################################################################
###########################  Moving Average power   ###########################
###############################################################################
power_ma <- readr::read_csv("../SGG_elec/power_ma_trim.csv", locale = locale(encoding = "euc-kr"))



# 병합: 시군구 + 연도 + 월 기준
reg_df <- power_ma %>%
  mutate(시군구 = paste0(시도, 시군구)) %>%
  inner_join(kernel_df, by = c("시군구", "계약종별", "연도", "월")) %>%
  mutate(
    T_total = max(t),
    t_T = t / T_total,
    z0 = t_T,
    z1 = t_T * x1,
    z2 = t_T * x2,
    z3 = t_T * x3,
    z4 = t_T * x4
  ) %>%
  filter(!is.na(y_ts))



###############################################################################
# 시군구, 계약종별 회귀식
###############################################################################
region_type_models <- reg_df %>%
  group_by(시군구, 계약종별) %>%
  group_split() %>%
  set_names(map_chr(., ~ paste0(unique(.x$시군구), "_", unique(.x$계약종별)))) %>%
  map(~ lm(y_ts ~ x1 + x2 + x3 + x4 + z0 + z1 + z2 + z3 + z4, data = .x))

# 각 지역 회귀 결과 요약
model_summaries <- map_dfr(
  region_type_models,
  ~ glance(.x),
  .id = "region_type"
)
head(model_summaries)

model_coefficients <- map_dfr(
  region_type_models,
  ~ tidy(.x),
  .id = "region_type"
)


###############################################################################
# 시각화 
###############################################################################
# # 저장 루트 폴더
# root_folder <- "./reaction_curves_realtemp"
# 
# # 표준화 기온과 실제 기온
# s_grid <- seq(0, 1, length.out = 200)
# T_grid <- s_grid * 60 - 20
# 
# # 그래프 생성 및 저장
# walk2(
#   region_type_models,
#   names(region_type_models),
#   function(model, region_type) {
#     coefs <- coef(model)
#     if (!all(c("x1", "x2", "x3", "x4") %in% names(coefs))) return(NULL)
#     
#     # 반응함수 계산
#     f_s <- coefs["x1"] * s_grid +
#       coefs["x2"] * s_grid^2 +
#       coefs["x3"] * cos(2 * pi * s_grid) +
#       coefs["x4"] * sin(2 * pi * s_grid)
#     
#     df_plot <- tibble(temp_C = T_grid, f_s = f_s)
#     
#     # 계약종별 분리
#     parts <- str_split(region_type, "_", simplify = TRUE)
#     if (ncol(parts) < 2) return(NULL)
#     시군구 <- parts[1]
#     계약종별 <- parts[2]
#     
#     # 곡선 형태 판별 (bell vs U)
#     f_min <- min(f_s, na.rm = TRUE)
#     f_max <- max(f_s, na.rm = TRUE)
#     center_val <- f_s[which.min(abs(T_grid))]  # T=0°C 근처 값
#     shape <- if ((f_s[1] > center_val) & (f_s[length(f_s)] > center_val)) {
#       "U자형"
#     } else if ((f_s[1] < center_val) & (f_s[length(f_s)] < center_val)) {
#       "Bell형"
#     } else {
#       "기타"
#     }
#     
#     # 폴더 경로 구성
#     subfolder <- file.path(root_folder, 계약종별, shape)
#     if (!dir.exists(subfolder)) dir.create(subfolder, recursive = TRUE)
#     
#     # 안전한 파일명
#     filename_safe <- str_replace_all(region_type, "[^[:alnum:]_]", "_")
#     output_file <- file.path(subfolder, paste0(filename_safe, ".png"))
#     
#     # 그래프 저장
#     g <- ggplot(df_plot, aes(x = temp_C, y = f_s)) +
#       geom_line(color = "darkblue", size = 1) +
#       labs(
#         title = paste0("기온 반응함수: ", region_type, " [", shape, "]"),
#         x = "기온 (°C)",
#         y = "반응함수 f(T)"
#       ) +
#       theme_bw(base_size = 12) +
#       theme(
#         panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white", color = NA),
#         panel.grid.major = element_line(color = "gray90"),
#         panel.grid.minor = element_blank()
#       )
#     
#     ggsave(output_file, plot = g, width = 6, height = 4, dpi = 300)
#   }
# )

###############################################################################
# 결과 요약 테이블
###############################################################################
# 3. 반응함수 곡선 형태 판별 함수
determine_shape <- function(coef_df) {
  # x1 ~ x4 계수만 추출
  b <- coef_df %>%
    filter(term %in% c("x1", "x2", "x3", "x4")) %>%
    arrange(term) %>%
    pull(estimate)
  
  if (length(b) != 4 || any(is.na(b))) return(NA)
  
  s_grid <- seq(0, 1, length.out = 200)
  f_s <- b[1] * s_grid +
    b[2] * s_grid^2 +
    b[3] * cos(2 * pi * s_grid) +
    b[4] * sin(2 * pi * s_grid)
  
  # 최저점 기준
  s_star <- s_grid[which.min(f_s)]
  left <- f_s[1]
  right <- f_s[length(f_s)]
  center <- f_s[which.min(abs(s_grid - 0.5))]
  
  # U자형: 양쪽 끝 > 중심
  if (left > center && right > center) {
    "U자형"
  } else {
    "Bell형"
  }
}

# 4. region_type 별 shape 계산
shape_df <- model_coefficients %>%
  group_by(region_type) %>%
  group_split() %>%
  set_names(map_chr(., ~ unique(.x$region_type))) %>%
  map_chr(determine_shape) %>%
  tibble(region_type = names(.), shape = .)

### 메모용 요약 테이블 ###
model_table <- model_summaries %>%
  left_join(shape_df, by = "region_type") %>%
  separate(region_type, into = c("지역", "계약종별"), sep = "_") %>%
  select(지역, 계약종별, adj.r.squared, shape)


### 4. 요약 정보 병합 + 조건 필터링
valid_regions <- model_summaries %>%
  left_join(shape_df, by = "region_type") %>%
  filter(adj.r.squared >= 0.4, shape == "U자형") %>%
  pull(region_type)

### 5. 유효한 지역만 다시 회귀
region_type_models_filtered <- reg_df %>%
  mutate(region_type = paste0(시군구, "_", 계약종별)) %>%
  filter(region_type %in% valid_regions) %>%
  group_by(region_type) %>%
  group_split() %>%
  set_names(map_chr(., ~ unique(.x$region_type))) %>%
  map(~ lm(y_ts ~ x1 + x2 + x3 + x4 + z0 + z1 + z2 + z3 + z4, data = .x))

### 6. 결과 확인
model_summaries_filtered <- map_dfr(
  region_type_models_filtered,
  ~ glance(.x),
  .id = "region_type"
)

##############################
# 전제: s_grid <- seq(0, 1, length.out = 200)
ds <- diff(s_grid)[1]

calculate_TE_components <- function(model, kde, id_label) {
  b <- coef(model)[c("x1", "x2", "x3", "x4")]
  if (length(b) != 4 || any(is.na(b))) return(NULL)
  
  k_s <- b[1]*s_grid + b[2]*s_grid^2 + b[3]*cos(2*pi*s_grid) + b[4]*sin(2*pi*s_grid)
  s_star <- s_grid[which.min(k_s)]
  k_star <- min(k_s)
  
  f_hat <- approx(kde$x, kde$y, xout = s_grid, rule = 2)$y
  
  TE_c <- sum(((k_s - k_star) * f_hat)[s_grid > s_star]) * ds
  TE_h <- sum(((k_s - k_star) * f_hat)[s_grid < s_star]) * ds
  baseline <- k_star
  TE_total <- TE_c + TE_h + baseline
  
  tibble(
    id = id_label,
    s_star = s_star,
    T_star = s_star * 60 - 20,
    TE_total = TE_total,
    TE_cooling = TE_c,
    TE_heating = TE_h,
    baseline = baseline
  )
}


# 전체 계산

te_components_df <- map_dfr(
  names(kernel_by_jym_final),
  function(full_id) {
    parts <- str_match(full_id, "^(.+?)_(.+?)_(\\d{6})$")
    if (any(is.na(parts))) return(NULL)
    
    지역명 <- parts[2]  # ex: "서울특별시종로구"
    계약종별 <- parts[3]
    연월 <- parts[4]
    
    base_id <- paste0(지역명, "_", 계약종별)
    
    if (!(base_id %in% names(region_type_models_filtered))) return(NULL)
    
    model <- region_type_models_filtered[[base_id]]
    kde <- kernel_by_jym_final[[full_id]]
    
    if (is.null(kde) || length(kde$x) < 2 || length(kde$y) < 2 || any(is.na(kde$x)) || any(is.na(kde$y))) return(NULL)
    
    result <- tryCatch({
      calculate_TE_components(model, kde, base_id)
    }, error = function(e) return(NULL))
    
    if (is.null(result)) return(NULL)
    
    result %>%
      mutate(
        시군구 = 지역명,
        계약종별 = 계약종별,
        연월 = 연월,
        연도 = as.integer(substr(연월, 1, 4)),
        월 = as.integer(substr(연월, 5, 6))
      )
  }
) %>%
  rename(시도시군구 = 시군구) %>%
  mutate(
    시도 = str_extract(시도시군구, paste0("^(", paste(sido_list, collapse = "|"), ")")),
    시군구 = str_remove(시도시군구, paste0("^(", paste(sido_list, collapse = "|"), ")"))
  )

####################
## Power CPI Data ##
####################
powerCPI_byMonth <- readr::read_csv("../CPI/real_powerCPI.csv", locale = locale(encoding = "euc-kr"))


###############
## GRDP Data ##
###############
grdp_byMonth <- readr::read_csv("../GRDP/grdp_monthly.csv", locale = locale(encoding = "euc-kr"))


############################
## Power Consumption Data ##
############################
file_list <- list.files(path = "../SGG_elec/", pattern = "\\.xlsx$", full.names = TRUE)

# 1. 단일 파일 처리 함수
process_file <- function(file) {
  year <- str_extract(file, "20\\d{2}")  # 연도 추출
  
  readxl::read_excel(file, sheet = "계약종별", skip = 2) %>%   # ✅ 시트 지정
    select(연도, 시도, 시군구, 계약종별, `1월`:`12월`) %>%
    filter(계약종별 %in% c("일반용", "주택용")) %>%
    pivot_longer(cols = `1월`:`12월`, names_to = "월", values_to = "사용량(MWh)") %>%
    mutate(
      연도 = as.integer(year),
      월 = str_remove(월, "월") %>% as.integer()
    )
}

# 2. 모든 파일을 병합하여 하나의 데이터프레임으로
power_data <- map_dfr(file_list, process_file) %>%
  mutate(`사용량(MWh)` = case_when(
    
    연도 >= 2014 ~ `사용량(MWh)` / 1000,
    TRUE ~ `사용량(MWh)`
    
  )) %>%
  mutate(`사용량(MWh)` = case_when(
    
    `사용량(MWh)` < 0 ~ `사용량(MWh)` * c(-1), 
    TRUE ~ `사용량(MWh)`
    
  ))

######################################
## MED: Monthly Effective Day  Data ##
######################################

# 예시: 2004년 1월부터 2024년 12월까지의 유효일수 계산
dates <- seq.Date(as.Date("2004-01-01"), as.Date("2024-12-31"), by = "day")
calendar <- tibble(
  date = dates,
  year = year(dates),
  month = month(dates),
  wday = wday(dates, label = TRUE),
  holiday = isHoliday(as.timeDate(dates), "KR")  # 한국 공휴일
)

# 유효일 가중치 예시 (일반용)
calendar <- calendar %>%
  mutate(weight = case_when(
    holiday ~ 0.84,
    wday %in% c("Sat") ~ 0.93,
    wday %in% c("Sun") ~ 0.84,
    TRUE ~ 1.00
  ))

med_monthly <- calendar %>%
  group_by(year, month) %>%
  summarise(MED = sum(weight), .groups = "drop") %>%
  rename(연 = year, 월 = month) %>%
  mutate(연월 = paste0(연, str_pad(월, 2, pad = "0")))



##############################
########## Renamed ##########
##############################
# 시도, 시군구, 시도시군구, 
# 계약종,
# 연, 월, 연월
te_components_df_Renamed <- te_components_df %>%
  rename(계약종 = 계약종별,
         연 = 연도)

power_data_Renamed <- power_data %>%
  mutate(시도시군구 = paste0(시도, 시군구),
         연월 = paste0(연도, str_pad(월, 2, pad = "0"))) %>%
  rename(계약종 = 계약종별,
         연 = 연도)

powerCPI_byMonth_Renamed <- powerCPI_byMonth %>%
  rename(연 = year,
         월 = month) %>%
  mutate(연월 = paste0(연, str_pad(월, 2, pad = "0")))

  
grdp_byMonth_Renamed <- grdp_byMonth %>%
  left_join(sido_mapping, by = c("시도" = "old")) %>%
  mutate(시도 = coalesce(new, 시도)) %>%  # new가 NA면 원래 값 유지
  select(-new) %>%
  rename(연 = year, 월 = month) %>%
  mutate(시도시군구 = paste0(시도, 시군구),
         연월 = paste0(연, str_pad(월, 2, pad = "0")))



###################################
########## All left_join ##########
###################################
allData <- te_components_df_Renamed %>%
  left_join(power_data_Renamed, by = c("시도", "시군구", "시도시군구", "계약종", "연", "월", "연월")) %>%
  left_join(grdp_byMonth_Renamed, by = c("시도", "시군구", "시도시군구",         "연", "월", "연월")) %>%
  left_join(powerCPI_byMonth_Renamed, by = c("시도", "연", "월", "연월")) %>%
  left_join(med_monthly, by = c("연","월","연월")) %>%
  drop_na() %>%
  filter(grdp_month != 0) %>%
  mutate(log_demand_per_day = log(`사용량(MWh)` / MED))


######################################
########## Final Regression ##########
######################################

##############
# Simple OLS #
##############

# [1]
# TE_(c), TE_(h) seperate version

# 1. id별 그룹화 후 회귀 실행
models_split_TE <- allData %>%
  group_by(id) %>%
  group_split() %>%
  set_names(map_chr(., ~ unique(.x$id))) %>%
  map(~ lm(log_demand_per_day ~ log(grdp_month) + log(real_powerCPI) + TE_cooling + TE_heating, data = .x))

# 2. 결과 요약
models_split_TE_coeffs <- map_dfr(models_split_TE, tidy, .id = "id")
models_split_TE_summary <- map_dfr(models_split_TE, glance, .id = "id")


# 3. 각 계수 추출 및 조건 필터링
grdp_valid <- models_split_TE_coeffs %>%
  filter(term == "log(grdp_month)", estimate > 0, p.value < 0.05) %>%
  select(id)

price_valid <- models_split_TE_coeffs %>%
  filter(term == "log(real_powerCPI)", estimate < 0, p.value < 0.05) %>%
  select(id)

cooling_valid <- models_split_TE_coeffs %>%
  filter(term == "TE_cooling", estimate > 0, p.value < 0.05) %>%
  select(id)

heating_valid <- models_split_TE_coeffs %>%
  filter(term == "TE_heating", estimate > 0, p.value < 0.05) %>%
  select(id)

# 3. 전체 회귀 적합도 조건 (설명력 & 유의성)
summary_valid <- models_split_TE_summary %>%
  filter(adj.r.squared >= 0.4, p.value < 0.05) %>%
  select(id)

# 4. 위 모든 조건을 만족하는 id만 교집합 추출
final_valid_ids <- Reduce(intersect, list(
  grdp_valid$id,
  price_valid$id,
  cooling_valid$id,
  heating_valid$id,
  summary_valid$id
))



######################################
# Calculate Cooling & Heating Demand #
######################################


# 냉방수요/난방수요 계산 함수
calculate_cooling_heating_demand <- function(df, model) {
  coefs <- coef(model)
  
  # 1. 회귀모형으로부터 예측값 계산 (진짜 y_hat)
  y_hat <- predict(model, newdata = df)
  
  # 2. 기온 기준분포에서의 baseline y 예측값 (TE = 0 가정)
  y_bar <- coefs["(Intercept)"] +
    coefs["log(grdp_month)"] * log(df$grdp_month) +
    coefs["log(real_powerCPI)"] * log(df$real_powerCPI)
  
  # 3. 기온효과로 인한 초과 수요 계산
  delta_y <- exp(y_hat) - exp(y_bar)
  
  # 4. 냉/난방 비율 계산을 위한 TE 합
  total_TE <- df$TE_cooling + df$TE_heating
  total_TE[total_TE == 0] <- NA  # 0으로 나누기 방지
  
  # 5. 냉/난방 수요 계산
  cooling <- df$MED * delta_y * (df$TE_cooling / total_TE)
  heating <- df$MED * delta_y * (df$TE_heating / total_TE)
  
  # 6. 최종 테이블 반환
  tibble(
    id = df$id[1],
    연 = df$연,
    월 = df$월,
    baseline_demand = exp(y_bar) * df$MED,
    cooling_demand = cooling,
    heating_demand = heating,
    total_predicted = exp(y_hat) * df$MED
  )
}

# 전체 id에 적용
results_demand <- map_dfr(names(models_split_TE), function(id) {
  model <- models_split_TE[[id]]
  df <- allData %>% filter(id == !!id)
  calculate_cooling_heating_demand(df, model)
})




