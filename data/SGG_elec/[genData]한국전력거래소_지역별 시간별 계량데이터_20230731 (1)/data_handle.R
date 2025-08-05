library(tidyverse)
library(lubridate)
library(stringr)

# 파일들이 있는 폴더 경로
data_folder <- "./"  # 예: "./data/"
output_folder <- "./outputGraph"
dir.create(output_folder, showWarnings = FALSE)

# 연도 목록
years <- 2013:2023

# 모든 연도 데이터 불러오기
all_data <- map_dfr(years, function(y) {
  file_path <- paste0(data_folder, y, "_광역시별_도별_계량값_1시간(GWh).csv")
  if (!file.exists(file_path)) return(NULL)
  
  read_csv(file_path, locale = locale(encoding = "CP949")) %>%
    mutate(
      연도 = y,
      일시 = ymd_h(paste(거래일, 시간)),
      지역 = str_trim(지역)
    )
})

# 지역 목록
지역목록 <- unique(all_data$지역)

# 지역별 그래프 생성
for (rgn in 지역목록) {
  df_sub <- all_data %>% filter(지역 == rgn)
  obs_count <- sum(!is.na(df_sub$계량값))
  
  p <- ggplot(df_sub, aes(x = 일시, y = 계량값)) +
    geom_point(color = "darkblue", size = 0.3) +
    labs(
      title = paste0(rgn, " 전력수요 시계열 (관측치: ", obs_count, ")"),
      x = "시간", y = "전력수요 (GWh)"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  file_out <- paste0(output_folder, "/", rgn, "_전력수요_2013-2023.png")
  ggsave(file_out, plot = p, width = 12, height = 4, bg = "white")
}
