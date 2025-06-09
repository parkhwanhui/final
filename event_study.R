#필요한 패키지 
library(tidyverse)
library(lubridate)
library(showtext)

# 폰트 설정
font_add_google("Nanum Gothic", "nanum")
showtext_auto()

# 사건 정보
event_info <- tibble(
  기업명 = c("현대차", "YG ENT", "SK하이닉스", "SPC 삼립"),
  사건일 = as.Date(c("2022-02-24", "2019-02-26", "2022-10-26", "2022-10-15"))
)

# 주가 데이터
stock_df <- read_csv("~/Desktop/all_stock_data.csv", show_col_types = FALSE) %>%
  rename(날짜 = Date, 종가 = Close) %>%
  mutate(날짜 = as.Date(날짜)) %>%
  rename(기업명 = name)

# 코스피 데이터
kospi_df <- read_csv("~/Desktop/kospi_market_data.csv", show_col_types = FALSE) %>%
  rename(날짜 = Date, KOSPI = KOSPI_Close) %>%
  mutate(날짜 = as.Date(날짜))

# 이벤트 스터디 수행
for (i in 1:nrow(event_info)) {
  name <- event_info$기업명[i]
  event_date <- event_info$사건일[i]
  
  start_date <- event_date - 7
  end_date <- event_date + 7
  
  stock_window <- stock_df %>%
    filter(기업명 == name, 날짜 >= start_date, 날짜 <= end_date) %>%
    arrange(날짜)
  
  kospi_window <- kospi_df %>%
    filter(날짜 >= start_date, 날짜 <= end_date) %>%
    arrange(날짜)
  
  if (nrow(stock_window) < 3 || nrow(kospi_window) < 3) {
    message(paste(name, ": 유효한 데이터 부족"))
    next
  }
  
  # 수익률 계산 (단순 수익률, %)
  stock_window <- stock_window %>%
    mutate(수익률 = (종가 / lag(종가) - 1) * 100)
  
  kospi_window <- kospi_window %>%
    mutate(코스피수익률 = (KOSPI / lag(KOSPI) - 1) * 100)
  
  df <- stock_window %>%
    left_join(kospi_window %>% select(날짜, 코스피수익률), by = "날짜") %>%
    mutate(
      AR = 수익률 - 코스피수익률,
      CAR = cumsum(replace_na(AR, 0))
    )
  
  # 시각화
  print(
    ggplot(df, aes(x = 날짜)) +
      geom_col(aes(y = AR), fill = "steelblue", alpha = 0.8) +
      geom_line(aes(y = CAR), color = "darkred", size = 1.2) +
      geom_vline(xintercept = as.numeric(event_date), linetype = "dashed", color = "black") +
      annotate("text", x = event_date, y = max(df$CAR, na.rm = TRUE),
               label = "사건일", vjust = -1, family = "nanum") +
      labs(
        title = paste0(name, " 사건 전후 비정상 수익률 및 CAR"),
        subtitle = paste0("사건일: ", event_date, " / 분석 구간: ±7일"),
        x = "날짜", y = "수익률 (%)"
      ) +
      theme_minimal(base_family = "nanum")
  )
}

