# Part C : 영화 흥행 예측

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)
library(lubridate)
library(ggpubr)
library(gridExtra)

source("/Users/hwangsuyeon/Desktop/석사/석사 4학기/이통2/Project 1/functions.R")

df_movie1 <- read.csv("/Users/hwangsuyeon/Desktop/석사/석사 4학기/이통2/Project 1/왕과 사는 남자.csv", skip=3, header = TRUE) %>%
  select(날짜, 관객수, 누적관객수)  %>%
  mutate(
    날짜 = as.Date(날짜,'%Y.%m.%d'),
    관객수 = as.numeric(gsub(",", "", 관객수)),
    누적관객수 = as.numeric(gsub(",", "", 누적관객수))
  ) %>%
  filter(날짜 >= as.Date("2026-02-04"))

head(df_movie1)

options(scipen = 999)
plot(df_movie1$날짜, df_movie1$관객수,
  type = "l", col = "black", lwd = 1.5,
  xlab = "개봉 후 일수", ylab = "일별 관객수",
  main = "왕과 사는 남자 일별 관객수")

grid(col = "lightgray", lty = "dashed")

plot(
  df_movie1$날짜,
  df_movie1$누적관객수,
  type = "l",
  col = "black",
  lwd = 2,
  xlab = "일자",
  ylab = "누적 관객수",
  main = "왕과 사는 남자 누적 관객수"
)
grid(col = "lightgray", lty = "dashed")

# 2026년 9월까지의 한국의 공휴일
holiday_2026 <- as.Date(c(
  "2026-02-16", "2026-02-17", 
  "2026-02-18", "2026-03-01",
  "2026-03-02", "2026-05-05", 
  "2026-05-24",  "2026-05-25", 
  "2026-06-03", "2026-06-06",
  "2026-08-15", "2026-08-17",
  "2026-09-24", "2026-09-25",  
  "2026-09-26"
))

pre_audience <- df_movie1$누적관객수[1] - df_movie1$관객수[1]

# 1. 주말/공휴일 지정
df_movie1 <- df_movie1 %>%
  mutate(
    isHoliday = ifelse(
      weekdays(날짜) %in% c("Saturday", "Sunday") |
        날짜 %in% holiday_2026,
      1, 0
    ),
    # 휴일이면 관객수 절반
    관객수2 = ifelse(
      isHoliday == 1,
      관객수 / 2,
      관객수
    ),
    날짜 = as.character(날짜)
  )

# 2. 휴일 행을 하나 더 생성
df2_movie1 <- df_movie1 %>%
  filter(isHoliday == 1) %>%
  mutate(날짜 = paste0(날짜, "A"))

# 3. 기존 데이터 + 복제된 휴일 데이터
df_movie1_adj <- rbind(df_movie1, df2_movie1) %>%
  arrange(날짜) %>%
  mutate(
    t = row_number(),
    누적관객수2 = pre_audience + cumsum(관객수2)
  )

# 분석용 데이터
df_movie1_adj <- df_movie1_adj %>%
  arrange(t) %>%
  mutate(
    Y = lag(누적관객수2),
    Y = ifelse(t == 1, pre_audience, Y)
  )

head(df_movie1_adj)

fit_diffusion <- function(df) {
  
  # Bass
  fit_bass <- lm(관객수2 ~ Y + I(Y^2), data = df)
  
  a <- fit_bass$coef[1]
  b <- fit_bass$coef[2]
  c <- fit_bass$coef[3]
  
  m_bass <- (-b - sqrt(b^2 - 4*a*c)) / (2*c)
  p_bass <- a / m_bass
  q_bass <- b + p_bass
  
  # Logistic
  fit_logistic <- lm(관객수2 ~ Y + I(Y^2) - 1, data = df)
  
  a <- fit_logistic$coef[1]
  b <- fit_logistic$coef[2]
  
  q_logistic <- a
  m_logistic <- -q_logistic / b
  

  ## Gumbel
  fit_gumbel <- lm(관객수2 ~ Y + I(Y * log(Y)) - 1, data = df)
  
  a <- fit_gumbel$coef[1]
  b <- fit_gumbel$coef[2]
  
  q_gumbel <- -b
  m_gumbel <- exp(a / q_gumbel)
  
  ## Exponential
  fit_exp <- lm(관객수2 ~ Y, data = df)
  
  a <- fit_exp$coef[1]
  b <- fit_exp$coef[2]
  
  p_exp <- -b
  m_exp <- a / p_exp
  
  ## 결과
  result <- data.frame(
    Model = c("Bass", "Logistic", "Gumbel", "Exponential"),
    
    m = c(m_bass, m_logistic, m_gumbel, m_exp),
    p = c(p_bass, NA, NA, p_exp ),
    q = c(q_bass, q_logistic, q_gumbel, NA)
  )
  
  return(result)
}

df_movie1_adj <- df_movie1_adj %>%
  mutate(실제날짜 = as.Date(substr(날짜, 1, 10)))

# 개봉일
start_date <- as.Date("2026-02-04")

# 1주
df_1week <- df_movie1_adj %>%
  filter(실제날짜 >= start_date,
         실제날짜 < start_date + 7)

# 2주
df_2week <- df_movie1_adj %>%
  filter(실제날짜 >= start_date,
         실제날짜 < start_date + 14)

# 4주
df_4week <- df_movie1_adj %>%
  filter(실제날짜 >= start_date,
         실제날짜 < start_date + 28)

result_1week <- fit_diffusion(df_1week)
result_2week <- fit_diffusion(df_2week)
result_4week <- fit_diffusion(df_4week)

result_m <- data.frame(
  기간 = c("1주", "2주", "4주"),
  Bass = c(
    result_1week$m[result_1week$Model == "Bass"],
    result_2week$m[result_2week$Model == "Bass"],
    result_4week$m[result_4week$Model == "Bass"]
  ),
  Logistic = c(
    result_1week$m[result_1week$Model == "Logistic"],
    result_2week$m[result_2week$Model == "Logistic"],
    result_4week$m[result_4week$Model == "Logistic"]
  ),
  Gumbel = c(
    result_1week$m[result_1week$Model == "Gumbel"],
    result_2week$m[result_2week$Model == "Gumbel"],
    result_4week$m[result_4week$Model == "Gumbel"]
  ),
  Exponential = c(
    result_1week$m[result_1week$Model == "Exponential"],
    result_2week$m[result_2week$Model == "Exponential"],
    result_4week$m[result_4week$Model == "Exponential"]
  )
)

result_m

actual_m <- max(df_movie1$누적관객수, na.rm = TRUE)

result_error <- result_m %>%
  pivot_longer(
    cols = c(Bass, Logistic, Gumbel, Exponential),
    names_to = "Model",
    values_to = "m_hat"
  ) %>%
  mutate(
    actual_m = actual_m,
    relative_error = 100 * (m_hat - actual_m) / actual_m,
    abs_relative_error = abs(relative_error)
  ) %>%
  arrange(Model, 기간)


result_error

best_by_period <- result_error %>%
  filter(!is.na(abs_relative_error)) %>%
  group_by(기간) %>%
  slice_min(abs_relative_error, n = 1) %>%
  ungroup()

best_by_period

# Bass
p1 <- df_movie1_adj %>% 
  arrange(t) %>% 
  mutate(U = 누적관객수2/(actual_m+1), bass = log((1+U)/(1-U))) %>%
  ggplot(aes(bass, t)) + geom_point(size=2) + geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..)) + ggtitle("Bass Q-Q Plot")

# Gumbel
p2 <- df_movie1_adj %>% 
  arrange(t) %>% 
  mutate(U = 누적관객수2/(actual_m+1), logis = qlogis(U)) %>%
  ggplot(aes(logis, t)) + geom_point(size=2) + geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..)) + ggtitle("Logistic Q-Q Plot")

# Exponential
p3 <- df_movie1_adj %>% 
  arrange(t) %>% 
  mutate(U = 누적관객수2/(actual_m+1), gumbel = -log(-log(U))) %>%
  ggplot(aes(gumbel, t)) + geom_point(size=2) + geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..)) + ggtitle("Gumbel Q-Q Plot")


# Logistic
p4 <- df_movie1_adj %>% 
  arrange(t) %>% 
  mutate(U = 누적관객수2/(actual_m+1), exp = qexp(U)) %>%
  ggplot(aes(exp, t)) + geom_point(size=2) + geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..)) + ggtitle("Exponential Q-Q Plot")

grid.arrange(p1, p2, p3, p4, ncol=2)

df_movie2 <- read.csv("/Users/hwangsuyeon/Desktop/석사/석사 4학기/이통2/Project 1/오디세이.csv", skip=3, header = TRUE) %>%
  select(날짜, 관객수, 누적관객수)  %>%
  mutate(
    날짜 = as.Date(날짜,'%Y.%m.%d'),
    관객수 = as.numeric(gsub(",", "", 관객수)),
    누적관객수 = as.numeric(gsub(",", "", 누적관객수))
  ) %>%
  filter(날짜 >= as.Date("2026-08-05"))

head(df_movie2)

options(scipen = 999)
plot(df_movie2$날짜, df_movie2$관객수,
  type = "l", col = "black", lwd = 1.5,
  xlab = "개봉 후 일수", ylab = "일별 관객수",
  main = "오디세이자 일별 관객수")

grid(col = "lightgray", lty = "dashed")

plot(
  df_movie2$날짜,
  df_movie2$누적관객수,
  type = "l",
  col = "black",
  lwd = 2,
  xlab = "일자",
  ylab = "누적 관객수",
  main = "오디세이 누적 관객수"
)
grid(col = "lightgray", lty = "dashed")

pre_audience <- df_movie2$누적관객수[1] - df_movie2$관객수[1]

# 1. 주말/공휴일 지정
df_movie2 <- df_movie2 %>%
  mutate(
    isHoliday = ifelse(
      weekdays(날짜) %in% c("Saturday", "Sunday") |
        날짜 %in% holiday_2026,
      1, 0
    ),
    # 휴일이면 관객수 절반
    관객수2 = ifelse(
      isHoliday == 1,
      관객수 / 2,
      관객수
    ),
    날짜 = as.character(날짜)
  )

# 2. 휴일 행을 하나 더 생성
df2_movie2 <- df_movie2 %>%
  filter(isHoliday == 1) %>%
  mutate(날짜 = paste0(날짜, "A"))

# 3. 기존 데이터 + 복제된 휴일 데이터
df_movie2_adj <- rbind(df_movie2, df2_movie2) %>%
  arrange(날짜) %>%
  mutate(
    t = row_number(),
    누적관객수2 = pre_audience + cumsum(관객수2)
  )

# 분석용 데이터
df_movie2_adj <- df_movie2_adj %>%
  arrange(t) %>%
  mutate(
    Y = lag(누적관객수2),
    Y = ifelse(t == 1, pre_audience, Y)
  )

head(df_movie2_adj)

df_movie2_adj <- df_movie2_adj %>%
  mutate(실제날짜 = as.Date(substr(날짜, 1, 10)))

# 개봉일
start_date <- as.Date("2026-08-05")

# 1주
df2_1week <- df_movie2_adj %>%
  filter(실제날짜 >= start_date,
         실제날짜 < start_date + 7)

# 2주
df2_2week <- df_movie2_adj %>%
  filter(실제날짜 >= start_date,
         실제날짜 < start_date + 14)

# 4주
df2_4week <- df_movie2_adj %>%
  filter(실제날짜 >= start_date,
         실제날짜 < start_date + 28)

result2_1week <- fit_diffusion(df2_1week)
result2_2week <- fit_diffusion(df2_2week)
result2_4week <- fit_diffusion(df2_4week)

result_m2 <- data.frame(
  기간 = c("1주", "2주", "4주"),
  Bass = c(
    result2_1week$m[result2_1week$Model == "Bass"],
    result2_2week$m[result2_2week$Model == "Bass"],
    result2_4week$m[result2_4week$Model == "Bass"]
  ),
  Logistic = c(
    result2_1week$m[result2_1week$Model == "Logistic"],
    result2_2week$m[result2_2week$Model == "Logistic"],
    result2_4week$m[result2_4week$Model == "Logistic"]
  ),
  Gumbel = c(
    result2_1week$m[result2_1week$Model == "Gumbel"],
    result2_2week$m[result2_2week$Model == "Gumbel"],
    result2_4week$m[result2_4week$Model == "Gumbel"]
  ),
  Exponential = c(
    result2_1week$m[result2_1week$Model == "Exponential"],
    result2_2week$m[result2_2week$Model == "Exponential"],
    result2_4week$m[result2_4week$Model == "Exponential"]
  )
)

result_m2

actual_m2 <- max(df_movie2$누적관객수, na.rm = TRUE)

result_error2 <- result_m2 %>%
  pivot_longer(
    cols = c(Bass, Logistic, Gumbel, Exponential),
    names_to = "Model",
    values_to = "m_hat"
  ) %>%
  mutate(
    actual_m2 = actual_m2,
    relative_error = 100 * (m_hat - actual_m2) / actual_m2,
    abs_relative_error = abs(relative_error)
  ) %>%
  arrange(Model, 기간)


result_error2

best_by_period2 <- result_error2 %>%
  filter(!is.na(abs_relative_error)) %>%
  group_by(기간) %>%
  slice_min(abs_relative_error, n = 1) %>%
  ungroup()

best_by_period2

# Bass
p1 <- df_movie2_adj %>% 
  arrange(t) %>% 
  mutate(U = 누적관객수2/(actual_m2+1), bass = log((1+U)/(1-U))) %>%
  ggplot(aes(bass, t)) + geom_point(size=2) + geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..)) + ggtitle("Bass Q-Q Plot")

# Logistic
p2 <- df_movie2_adj %>% 
  arrange(t) %>% 
  mutate(U = 누적관객수2/(actual_m2+1), logis = qlogis(U)) %>%
  ggplot(aes(logis, t)) + geom_point(size=2) + geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..)) + ggtitle("Logistic Q-Q Plot")

# Gumbel
p3 <- df_movie2_adj %>% 
  arrange(t) %>% 
  mutate(U = 누적관객수2/(actual_m2+1), gumbel = -log(-log(U))) %>%
  ggplot(aes(gumbel, t)) + geom_point(size=2) + geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..)) + ggtitle("Gumbel Q-Q Plot")

# Exponential
p4 <- df_movie2_adj %>% 
  arrange(t) %>% 
  mutate(U = 누적관객수2/(actual_m2+1), exp = qexp(U)) %>%
  ggplot(aes(exp, t)) + geom_point(size=2) + geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..)) + ggtitle("Exponential Q-Q Plot")

grid.arrange(p1, p2, p3, p4, ncol=2)
