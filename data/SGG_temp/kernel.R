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

### Kernel

# s값 표준화 포함
temp_data_std <- all_temperature_data %>%
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

## S grid 생성 ##
s_grid <- seq(0, 1, length.out = 200)   # 표준화된 기온의 격자
ds <- diff(s_grid)[1]                  # 간격




