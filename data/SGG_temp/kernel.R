# 필요한 패키지 로딩
library(readr)   # read_csv
library(dplyr)   # 데이터 조작
library(purrr)   # map_dfr
library(tibble)
library(ggplot2)
library(stringr)

# 1. 폴더 경로 지정
folder_path <- "."  # 현재 작업 디렉토리 (필요시 수정)

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
  left_join(power_ma_regionMapping, by = c("지점명"))


# all_temperature_data_rename %>%
#   group_by(한전_시도, 한전_시군구) %>%
#   summarise(지점명_개수 = n_distinct(지점명), .groups = "drop") %>%
#   filter(지점명_개수 > 1)
# 
# whoNA <- all_temperature_data_rename %>% filter(is.na(한전_시군구)) 
# 
# list <- all_temperature_data_rename %>% filter(한전_시군구 == "강릉시")
# unique(list$지점명)

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
  group_by(지점명, year, month) %>%
  group_split() %>%
  keep(~ nrow(.x) >= 10) %>%   # ⚠️ 이 줄 추가
  set_names(map_chr(., ~ paste0(unique(.x$지점명), "_", unique(.x$year)*100 + unique(.x$month)))) %>%
  map(~ density(.x$s, bw = "nrd0", kernel = "gaussian"))



#################################################################
###########################  시각화   ###########################
#################################################################

#############################  시각화 강릉 예제 #############################
# # 강릉 2020년 1~12월 커널 객체 필터
# gangneung_kernels <- kernel_by_jym[names(kernel_by_jym) %in% paste0("강릉_", 202001:202012)]
# 
# # 각 월별 커널 객체를 데이터프레임으로 변환
# gangneung_df <- map2_dfr(gangneung_kernels, names(gangneung_kernels), function(kde, label) {
#   tibble(
#     s = kde$x,
#     density = kde$y,
#     month = substr(label, 8, 9)  # 월 정보 추출
#   )
# })
# 
# # 그래프 출력
# ggplot(gangneung_df, aes(x = s, y = density, color = month)) +
#   geom_line(size = 1) +
#   labs(title = "강릉 2020년 월별 표준화 기온분포 (커널 추정)",
#        x = "표준화 기온 s ∈ [0,1]",
#        y = "밀도",
#        color = "월") +
#   theme_minimal()
# 
# 
# 
# #############################  시각화 전지점 예제 #############################
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
kernel_df <- map2_dfr(kernel_by_jym, names(kernel_by_jym), function(kde, label) {
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
  rename(시군구 = 시군구, 연도 = 연도, 월 = 월) %>%
  inner_join(kernel_df, by = c("시군구", "연도", "월")) %>%
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





model <- lm(y_ts ~ x1 + x2 + x3 + x4 + z0 + z1 + z2 + z3 + z4, data = reg_df)
summary(model)


# model_simple <- lm(y_ts ~ x1 + x2 + x3 + x4, data = reg_df)
# summary(model_simple)


###############################################################################
###########################  k_(t,s): 기온반응함수   ###########################
###############################################################################
# 기존 s_grid
s_grid <- seq(0, 1, length.out = 200)

# 변환: 실제 기온 (섭씨)
T_C <- 60 * s_grid - 20

# 계수 추출
coefs <- coef(model)
t_T <- 0.5  # 예: 중간 시점

# 반응함수 계산
k_t_s <- coefs["(Intercept)"] +
  coefs["x1"] * s_grid +
  coefs["x2"] * s_grid^2 +
  coefs["x3"] * cos(2 * pi * s_grid) +
  coefs["x4"] * sin(2 * pi * s_grid) +
  t_T * (
    coefs["z0"] +
      coefs["z1"] * s_grid +
      coefs["z2"] * cos(2 * pi * s_grid) +
      coefs["z3"] * sin(2 * pi * s_grid) +
      coefs["z4"] * s_grid^2
  )

# 시각화 (기온축으로)
plot_df <- tibble(T_C = T_C, k_ts = k_t_s)

ggplot(plot_df, aes(x = T_C, y = k_ts)) +
  geom_line(size = 1) +
  labs(
    title = "기온 반응함수 k_t(s), x축: 실제 기온(℃)",
    x = "기온 (°C)",
    y = "반응도"
  ) +
  theme_minimal()



###############################################################################
###########################  k_(t,s): 기온반응함수 (2)   ###########################
###############################################################################

# 추정된 계수
coefs <- coef(model)
T_total <- max(reg_df$t)
s_grid <- seq(0, 1, length.out = 200)
T_C <- 60 * s_grid - 20  # 실제 기온

# 시점 예: 2004년, 2010년, 2016년
selected_years <- c(2010, 2015, 2020)

# 각 연도에 해당하는 평균 t값 계산
t_lookup <- reg_df %>%
  group_by(연도) %>%
  summarise(t_avg = mean(t)) %>%
  filter(연도 %in% selected_years)

# 반응함수 생성
plot_df <- t_lookup %>%
  mutate(t_T = t_avg / T_total) %>%
  rowwise() %>%
  mutate(data = list(tibble(
    T_C = T_C,
    k_ts = coefs["(Intercept)"] +
      coefs["x1"] * s_grid +
      coefs["x2"] * s_grid^2 +
      coefs["x3"] * cos(2 * pi * s_grid) +
      coefs["x4"] * sin(2 * pi * s_grid) +
      t_T * (
        coefs["z0"] +
          coefs["z1"] * s_grid +
          coefs["z2"] * cos(2 * pi * s_grid) +
          coefs["z3"] * sin(2 * pi * s_grid) +
          coefs["z4"] * s_grid^2
      ),
    연도 = 연도
  ))) %>%
  pull(data) %>%
  bind_rows()

ggplot(plot_df, aes(x = T_C, y = k_ts, group = 연도, color = as.factor(연도), linetype = as.factor(연도))) +
  geom_line(size = 1) +
  scale_color_manual(values = c("gray", "black", "lightgreen")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    title = "시점별 기온 반응함수 비교",
    x = "기온 (°C)",
    y = "반응도",
    color = "연도",
    linetype = "연도"
  ) +
  coord_cartesian(xlim = c(-10, 35)) +
  theme_minimal()





##############################
# t_T_values <- c(0.1, 0.5, 0.9)
# 
# plot_df <- map_dfr(t_T_values, function(t_T) {
#   tibble(
#     T_C = 60 * s_grid - 20,
#     k_ts = coefs["(Intercept)"] +
#       coefs["x1"] * s_grid +
#       coefs["x2"] * s_grid^2 +
#       coefs["x3"] * cos(2 * pi * s_grid) +
#       coefs["x4"] * sin(2 * pi * s_grid) +
#       t_T * (
#         coefs["z0"] +
#           coefs["z1"] * s_grid +
#           coefs["z2"] * cos(2 * pi * s_grid) +
#           coefs["z3"] * sin(2 * pi * s_grid) +
#           coefs["z4"] * s_grid^2
#       ),
#     t_T = t_T
#   )
# })
# 
# ggplot(plot_df, aes(x = T_C, y = k_ts, color = as.factor(t_T))) +
#   geom_line(size = 1.2) +
#   labs(title = "기온 반응함수: 시점(t/T)별 비교",
#        x = "기온 (°C)", y = "반응도", color = "t/T") +
#   theme_minimal()
# 
# 


t_T_values <- c(0.1, 0.5, 0.9)

plot_df <- map_dfr(t_T_values, function(t_T) {
  tibble(
    T_C = 60 * s_grid - 20,
    k_ts = coefs["(Intercept)"] +
      coefs["x1"] * s_grid +
      coefs["x2"] * s_grid^2 +
      coefs["x3"] * cos(2 * pi * s_grid) +
      coefs["x4"] * sin(2 * pi * s_grid) +
      t_T * (
        coefs["z0"] +
          coefs["z1"] * s_grid +
          coefs["z2"] * cos(2 * pi * s_grid) +
          coefs["z3"] * sin(2 * pi * s_grid) +
          coefs["z4"] * s_grid^2
      ),
    t_T = paste0("t/T=", t_T)
  )
})

ggplot(plot_df, aes(x = T_C, y = k_ts, color = t_T)) +
  geom_line(size = 1.2) +
  labs(
    title = "기온 반응함수 k_t(s): 시점(t/T)별 비교",
    x = "기온 (°C)",
    y = "반응도",
    color = "시점"
  ) +
  theme_minimal()

