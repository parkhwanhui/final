#필요한 패키지
library(tidyverse)
library(lubridate)
library(zoo)
library(showtext)

# 한글 폰트 설정 
font_add(family = "apple", regular = "/System/Library/Fonts/AppleSDGothicNeo.ttc")
showtext_auto()
theme_set(theme_minimal(base_family = "apple"))

# 사건 기업 리스트 
companies <- tibble(
  기업명 = c("SPC 삼립", "YG ENT", "현대차", "SK하이닉스"),
  그룹 = c("윤리적", "윤리적", "경제적", "경제적"),
  사건일 = as.Date(c("2022-10-15", "2019-02-26", "2022-02-24", "2022-10-26"))
)

# 주가 데이터 불러오기
prices <- read_csv("~/Desktop/all_stock_data.csv", show_col_types = FALSE) %>%
  rename(날짜 = Date, 기업명 = name, price = Close) %>%
  mutate(날짜 = as.Date(날짜))

# 분석 파라미터
연속일수 <- 3
최종관찰일 <- as.Date("2025-05-31")

# 결과 저장 초기화
recovery_df <- data.frame()

# 회복 분석 루프
for (i in 1:nrow(companies)) {
  name <- companies$기업명[i]
  event_date <- companies$사건일[i]
  group <- companies$그룹[i]
  
  df <- prices %>%
    filter(기업명 == name, 날짜 <= 최종관찰일) %>%
    arrange(날짜)
  
  # 기준가: 사건일 전 20거래일 중 최근 10일 평균
  pre_event_df <- df %>% filter(날짜 < event_date) %>% tail(20)
  if (nrow(pre_event_df) < 10) next
  기준가 <- mean(tail(pre_event_df$price, 10), na.rm = TRUE)
  
  # 최고 하락일: 사건일 이후 15일 내 최저가 날짜
  이후15일 <- df %>% filter(날짜 >= event_date, 날짜 <= event_date + 15)
  if (nrow(이후15일) == 0) next
  min_row <- 이후15일 %>%
    filter(price == min(price, na.rm = TRUE)) %>%
    slice(1)
  최저가 <- min_row$price
  최고하락일 <- min_row$날짜
  
  # 회복 기준가
  하락폭 <- 기준가 - 최저가
  회복기준 <- 최저가 + 하락폭 * 0.6
  
  # 회복 여부 판단 (3일 연속 유지)
  이후 <- df %>% filter(날짜 > 최고하락일)
  이후$회복여부 <- zoo::rollapply(
    이후$price >= 회복기준,
    width = 연속일수,
    FUN = all,
    fill = NA,
    align = "left"
  )
  
  회복행 <- 이후 %>% filter(회복여부 == TRUE) %>% slice(1)
  회복일 <- if (nrow(회복행) == 0) NA else 회복행$날짜
  회복기간 <- if (is.na(회복일)) NA else as.numeric(회복일 - 최고하락일)
  
  # 결과 저장
  recovery_df <- bind_rows(recovery_df, tibble(
    기업명 = name,
    그룹 = group,
    사건일 = event_date,
    기준가 = round(기준가, 2),
    최저가 = round(최저가, 2),
    하락률 = round((기준가 - 최저가) / 기준가 * 100, 2),
    회복기준 = round(회복기준, 2),
    회복일 = 회복일,
    회복기간 = 회복기간,
    회복여부 = !is.na(회복일)
  ))
}

# 시각화 
recovery_df$그룹 <- factor(recovery_df$그룹, levels = c("경제적", "윤리적"))

# 회복 여부 확인
print(recovery_df %>%
        select(기업명, 사건일, 회복여부, 회복일, 회복기간, 기준가, 최저가, 회복기준))

# 하락률 그래프
ggplot(recovery_df, aes(x = reorder(기업명, -하락률), y = 하락률, fill = 그룹)) +
  geom_col(width = 0.6) +
  labs(title = "기업별 주가 하락률", x = "기업명", y = "하락률 (%)")

# 회복기간 그래프
ggplot(recovery_df, aes(x = reorder(기업명, -회복기간), y = 회복기간, fill = 그룹)) +
  geom_col(width = 0.6, na.rm = FALSE) +
  geom_text(aes(label = ifelse(is.na(회복기간), "미회복", 회복기간)), vjust = -0.5, size = 4) +
  labs(title = "기업별 회복기간", x = "기업명", y = "회복까지 걸린 일수")


