library(tidyverse)
library(lubridate)
library(stringr)

# 데이터 폴더 경로 설정
data_folder <- "./"

# 측정 요소 매핑 (열 이름 → 파일명 접미어 및 폴더명)
elements <- list(
  "기온(°C)" = "기온",
  "강수량(mm)" = "강수",
  "풍속(m/s)" = "풍속",
  "습도(%)" = "습도",
  "일사(MJ/m2)" = "일사",
  "적설(cm)" = "적설"
)

# 연도 반복
for (year in 2013:2024) {
  file_path <- paste0(data_folder, year, ".csv")
  if (!file.exists(file_path)) next
  
  df <- read_csv(file_path, locale = locale(encoding = "EUC-KR")) %>%
    rename(지점번호 = 지점)
  
  지점들 <- unique(df$지점명)
  
  for (지점 in 지점들) {
    df_sub <- df %>% filter(지점명 == 지점)
    
    for (colname in names(elements)) {
      if (!colname %in% colnames(df_sub)) next
      
      element_name <- elements[[colname]]
      obs_count <- sum(!is.na(df_sub[[colname]]))
      
      # 하위 폴더 생성
      dir.create(paste0("./outputGraph/", element_name), showWarnings = FALSE, recursive = TRUE)
      
      # 점 그래프 생성
      p <- ggplot(df_sub, aes(x = 일시, y = .data[[colname]])) +
        geom_point(color = "steelblue", size = 0.5) +
        labs(
          title = paste0(지점, " - ", year, "년 ", element_name, " 시계열 (관측치: ", obs_count, ")"),
          x = "시간", y = element_name
        ) +
        theme_minimal()
      
      # 파일 저장
      file_out <- paste0("./outputGraph/", element_name, "/", 지점, "_", year, "_", element_name, ".png")
      ggsave(filename = file_out, plot = p, width = 10, height = 4)
    }
  }
}


