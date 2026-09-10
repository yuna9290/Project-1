# Part A

# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

# load data
# 첫 번째 칼럼명과 값의 위치가 맞지 않는 문제는 엑셀 파일 내에서 직접 해결함
dram_data = read_excel('DRAM_AIDS_data.xls',sheet=1)
dram_data = dram_data[,1:5]

# check data
head(dram_data)

# visualization
ggplot(data=dram_data, aes(x=`T(시점)`, y=`256KD`))+
  geom_line()+labs(title='DRAM 선적량 추이', x='시점', y='')+
  theme_minimal(base_family='AppleGothic')

# ---------------------------------------------------------------------
# 1. 데이터 전처리 (누적 선적량 Y_t 생성)
# ---------------------------------------------------------------------
# '256KD' 컬럼 사용, NA 제거 및 누적합(Y_t) 계산
dram_processed <- dram_data %>%
  select(T = `T(시점)`, S_t = `256KD`) %>%
  filter(!is.na(S_t)) %>%
  mutate(
    Y_t = cumsum(S_t),          # 현재 시점 누적량
    Y_lag = lag(Y_t, default = 0) # t-1 시점 누적량 (Y_{t-1})
  )

# ---------------------------------------------------------------------
# 2. 모형별 모수 추정 함수 정의
# ---------------------------------------------------------------------

# A. Bass 모형 추정 함수
fit_bass <- function(data) {
  # OLS 회귀분석: S_t = a + b*Y_lag + c*Y_lag^2
  fit <- lm(S_t ~ Y_lag + I(Y_lag^2), data = data)
  coefs <- coef(fit)
  
  a <- coefs[1]
  b <- coefs[2]
  c <- coefs[3]
  
  # 계수로부터 (m, p, q) 역산
  # c = -q/m, b = q - p, a = p*m
  # m 구하기 (2차 방정식 근의 공식)
  m <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * c)
  p <- a / m
  q <- -c * m
  
  return(c(p = unname(p), q = unname(q), m = unname(m)))
}

# B. Logistic 모형 추정 함수
fit_logistic <- function(data) {
  # OLS 회귀분석 (절편 없음): S_t = a*Y_lag + b*Y_lag^2
  fit <- lm(S_t ~ 0 + Y_lag + I(Y_lag^2), data = data)
  coefs <- coef(fit)
  
  a <- coefs[1]
  b <- coefs[2]
  
  q <- a
  m <- -q / b
  
  return(c(p = 0, q = unname(q), m = unname(m)))
}

# C. Gumbel 모형 추정 함수
## 절편 없는 OLS
fit_gumbel <- function(data) {
  # Y_lag = 0 인 첫 열은 제외 (독립변수에 ln(Y_t-1) 사용됨)
  data_gumb = data %>% filter(Y_lag >0)
  
  fit <- lm(S_t ~ 0 + Y_lag + I(Y_lag*log(Y_lag)), data = data_gumb)
  coefs <- coef(fit)
  
  a <- coefs[1]
  b <- coefs[2]
  # 계수 매핑
  ## q=-b, m = exp(a/q)
  q_gumb <- -b
  m_gumb <- exp(-a/b)
  
  return(c(q = unname(q_gumb), m = unname(m_gumb)))
}

# ---------------------------------------------------------------------
# 3. N = 15, 30, 51 분할 학습 및 결과 수집
# ---------------------------------------------------------------------
n_list <- c(15, 30, 51)
results <- list()

for (n in n_list) {
  train_data <- dram_processed %>% filter(T <= n)
  
  bass_res <- fit_bass(train_data)
  logi_res <- fit_logistic(train_data)
  gumb_res <- fit_gumbel(train_data)
  
  results[[as.character(n)]] <- tibble(
    N = n,
    Model = c("Bass", "Logistic", "Gumbel"),
    p = c(bass_res["p"], logi_res["p"], NA),
    q = c(bass_res["q"], logi_res["q"], gumb_res["q"]), # Gumbel의 g를 q 위치에 매핑
    m = c(bass_res["m"], logi_res["m"], gumb_res["m"])
  )
}

# 결과 통합 및 출력
final_summary <- bind_rows(results)
print(final_summary)

# ---------------------------------------------------------------------
# 1. 실제 총수요 (m_true) 정의
# ---------------------------------------------------------------------
# N=51일 때의 총 누적판매량(Y_t[51])보다 약간 큰 값(예: 5% 여유)으로 설정
m_true <- dram_processed$Y_t[51] * 1.05 

# ---------------------------------------------------------------------
# 2. 상대오차 계산 및 표 정리
# ---------------------------------------------------------------------
# 기존 final_summary 테이블(이전 코드 결과)을 받아서 계산
relative_error_summary <- final_summary %>%
  mutate(
    m_true = m_true,
    # 상대오차 수식: 100 * (m_hat - m) / m
    relative_error_pct = round(100 * (m - m_true) / m_true, 2)
  ) %>%
  select(N, Model, m_hat = m, m_true, `Relative_Error(%)` = relative_error_pct)

# 결과 출력
print(relative_error_summary)

# ---------------------------------------------------------------------
# 모형 평가 및 Q-Q Plot R^2 산출 종합 함수
# ---------------------------------------------------------------------
evaluate_models <- function(data) {
  n_obs <- nrow(data)
  
  # A. Bass Model Fit
  fit_bass <- lm(S_t ~ Y_lag + I(Y_lag^2), data = data)
  coef_b <- coef(fit_bass)
  a_b <- coef_b[1]; b_b <- coef_b[2]; c_b <- coef_b[3]
  m_b <- (-b_b - sqrt(b_b^2 - 4 * a_b * c_b)) / (2 * c_b)
  p_b <- a_b / m_b; q_b <- -c_b * m_b
  pred_bass <- predict(fit_bass)
  
  # B. Logistic Model Fit
  fit_logi <- lm(S_t ~ 0 + Y_lag + I(Y_lag^2), data = data)
  coef_l <- coef(fit_logi)
  b_l <- coef_l[1]; c_l <- coef_l[2]
  m_l <- -b_l / c_l; q_l <- b_l
  pred_logi <- predict(fit_logi)
  
  # C. Gumbel Model Fit
  g_data <- data %>% filter(Y_lag > 0)
  fit_gumb <- lm(S_t ~ 0 + Y_lag + I(Y_lag*log(Y_lag)), data = g_data)
  coef_g <- coef(fit_gumb)
  a_g <- coef_g[1]; b_g <- coef_g[2]
  q_g <- -b_g; m_g <- exp(-a_g / b_g)
  pred_gumb <- predict(fit_gumb)
  
  # -------------------------------------------------------------------
  # 1) MSE 계산 (실제 S_t vs 예측 S_t)
  # -------------------------------------------------------------------
  mse_bass <- mean((data$S_t - pred_bass)^2, na.rm = TRUE)
  mse_logi <- mean((data$S_t - pred_logi)^2, na.rm = TRUE)
  mse_gumb <- mean((g_data$S_t - pred_gumb)^2, na.rm = TRUE)
  
  # -------------------------------------------------------------------
  # 2) Q-Q Plot 평가 (구매시간 분위수 u_r = r / (n + 1) 이용)
  # X_{(r)} ~ mu + sigma * G^{-1}(u_r) 의 R^2 측정
  # -------------------------------------------------------------------
  r <- 1:n_obs
  u_r <- r / (n_obs + 1)
  X_r <- data$T # 순서통계량(시점)
  
  # Bass 이론 분위수 inverse F(t)
  # F(t) = (1 - exp(-kt)) / (1 + (q/p)*exp(-kt)) -> t에 대해 역산
  k_b <- p_b + q_b
  inv_F_bass <- -(1 / k_b) * log((1 - u_r) / (1 + (q_b / p_b) * u_r))
  qq_bass_r2 <- summary(lm(X_r ~ inv_F_bass))$r.squared
  
  # Logistic 이론 분위수
  inv_F_logi <- (1 / q_l) * log(u_r / (1 - u_r))
  qq_logi_r2 <- summary(lm(X_r ~ inv_F_logi))$r.squared
  
  # Gumbel 이론 분위수
  inv_F_gumb <- -log(-log(u_r))
  qq_gumb_r2 <- summary(lm(X_r ~ inv_F_gumb))$r.squared
  
  tibble(
    Model = c("Bass", "Logistic", "Gumbel"),
    MSE = c(mse_bass, mse_logi, mse_gumb),
    QQ_R2 = c(qq_bass_r2, qq_logi_r2, qq_gumb_r2)
  )
}

# ---------------------------------------------------------------------
# 3. N=15, 30, 51 별 최적 모형 자동선택 실행
# ---------------------------------------------------------------------
n_list <- c(15, 30, 51)
evaluation_results <- list()

for (n in n_list) {
  train_data <- dram_processed %>% filter(T <= n)
  eval_res <- evaluate_models(train_data)
  
  # MSE 최소, Q-Q R^2 최대 지표 기준으로 최적 모형 선정
  best_mse_model <- eval_res %>% arrange(MSE) %>% slice(1) %>% pull(Model)
  best_qq_model  <- eval_res %>% arrange(desc(QQ_R2)) %>% slice(1) %>% pull(Model)
  
  evaluation_results[[as.character(n)]] <- eval_res %>%
    mutate(
      N = n,
      Selected_by_MSE = ifelse(Model == best_mse_model, "★ 최적", ""),
      Selected_by_QQ  = ifelse(Model == best_qq_model, "★ 최적", "")
    )
}

# 최종 정리 및 출력
final_eval_table <- bind_rows(evaluation_results) %>%
  select(N, Model, MSE, Selected_by_MSE, QQ_R2, Selected_by_QQ)

print(final_eval_table)

# ---------------------------------------------------------------------
# 1. 실제 총수요(m) 정의
# ---------------------------------------------------------------------
# N=51일 때의 총 누적판매량(Y_t[51])보다 약간 큰 값(예: 5% 여유)으로 설정
m_true <- dram_processed$Y_t[51] * 1.05 

# ---------------------------------------------------------------------
# 2. 모형 평가, m_hat 추정 및 Q-Q 데이터 수집 종합 함수
# ---------------------------------------------------------------------
evaluate_models_enhanced <- function(data, m_true) {
  n_obs <- nrow(data)
  
  # A. Bass Model
  fit_bass <- lm(S_t ~ Y_lag + I(Y_lag^2), data = data)
  coef_b <- unname(coef(fit_bass))
  a_b <- coef_b[1]; b_b <- coef_b[2]; c_b <- coef_b[3]
  m_b <- (-b_b - sqrt(b_b^2 - 4 * a_b * c_b)) / (2 * c_b)
  p_b <- a_b / m_b; q_b <- -c_b * m_b
  pred_bass <- predict(fit_bass)
  
  # B. Logistic Model
  fit_logi <- lm(S_t ~ 0 + Y_lag + I(Y_lag^2), data = data)
  coef_l <- unname(coef(fit_logi))
  b_l <- coef_l[1]; c_l <- coef_l[2]
  m_l <- -b_l / c_l; q_l <- b_l
  pred_logi <- predict(fit_logi)
  
  # C. Gumbel Model
  g_data <- data %>% filter(Y_lag > 0)
  fit_gumb <- lm(S_t ~ 0 + Y_lag + I(Y_lag * log(Y_lag)), data = g_data)
  coef_g <- unname(coef(fit_gumb))
  a_g <- coef_g[1]; b_g <- coef_g[2]
  q_g <- -b_g; m_g <- exp(-a_g / b_g)
  pred_gumb <- predict(fit_gumb)
  
  # -------------------------------------------------------------------
  # 1) MSE 계산
  # -------------------------------------------------------------------
  mse_bass <- mean((data$S_t - pred_bass)^2, na.rm = TRUE)
  mse_logi <- mean((data$S_t - pred_logi)^2, na.rm = TRUE)
  mse_gumb <- mean((g_data$S_t - pred_gumb)^2, na.rm = TRUE)
  
  # -------------------------------------------------------------------
  # 2) Q-Q Plot 좌표 및 R^2 계산
  # -------------------------------------------------------------------
  r <- 1:n_obs
  u_r <- r / (n_obs + 1)
  X_r <- data$T # 표본 분위수 (관측 시점)
  
  # 이론적 분위수 (x축)
  k_b <- p_b + q_b
  inv_F_bass <- -(1 / k_b) * log((1 - u_r) / (1 + (q_b / p_b) * u_r))
  inv_F_logi <- (1 / q_l) * log(u_r / (1 - u_r))
  inv_F_gumb <- -log(-log(u_r))
  
  qq_bass_r2 <- summary(lm(X_r ~ inv_F_bass))$r.squared
  qq_logi_r2 <- summary(lm(X_r ~ inv_F_logi))$r.squared
  qq_gumb_r2 <- summary(lm(X_r ~ inv_F_gumb))$r.squared
  
  # -------------------------------------------------------------------
  # 3) 총수요(m) 상대오차 (%) 계산
  # -------------------------------------------------------------------
  rel_err_bass <- abs(m_b - m_true) / m_true * 100
  rel_err_logi <- abs(m_l - m_true) / m_true * 100
  rel_err_gumb <- abs(m_g - m_true) / m_true * 100
  
  # -------------------------------------------------------------------
  # 결과 데이터 프레임 구성
  # -------------------------------------------------------------------
  eval_table <- tibble(
    Model = c("Bass", "Logistic", "Gumbel"),
    m_hat = c(m_b, m_l, m_g),
    Rel_Error_Pct = c(rel_err_bass, rel_err_logi, rel_err_gumb),
    MSE = c(mse_bass, mse_logi, mse_gumb),
    QQ_R2 = c(qq_bass_r2, qq_logi_r2, qq_gumb_r2)
  )
  
  qq_plot_df <- tibble(
    T_obs = rep(X_r, 3),
    Theoretical_Quantile = c(inv_F_bass, inv_F_logi, inv_F_gumb),
    Model = factor(rep(c("Bass", "Logistic", "Gumbel"), each = n_obs), 
                   levels = c("Bass", "Logistic", "Gumbel"))
  )
  
  return(list(summary = eval_table, qq_data = qq_plot_df))
}

# ---------------------------------------------------------------------
# 3. N = 15, 30, 51 반복 실행 및 결과 통합
# ---------------------------------------------------------------------
n_list <- c(15, 30, 51)
summary_results <- list()
qq_results <- list()

for (n in n_list) {
  train_data <- dram_processed %>% filter(T <= n)
  res <- evaluate_models_enhanced(train_data, m_true)
  
  # 평가 결과 요약
  best_mse_model <- res$summary %>% arrange(MSE) %>% slice(1) %>% pull(Model)
  best_qq_model  <- res$summary %>% arrange(desc(QQ_R2)) %>% slice(1) %>% pull(Model)
  
  summary_results[[as.character(n)]] <- res$summary %>%
    mutate(
      N = n,
      m_true = m_true,
      Selected_by_MSE = ifelse(Model == best_mse_model, "★ 최적", ""),
      Selected_by_QQ  = ifelse(Model == best_qq_model, "★ 최적", "")
    )
  
  # Q-Q Plot 시각화용 데이터 수집
  qq_results[[as.character(n)]] <- res$qq_data %>% mutate(N = paste0("N = ", n))
}

# 최종 요약 표 출력
final_eval_table <- bind_rows(summary_results) %>%
  select(N, Model, m_hat, m_true, Rel_Error_Pct, MSE, Selected_by_MSE, QQ_R2, Selected_by_QQ)

print("=== 모형 평가 및 총수요(m) 상대오차 요약표 ===")
print(final_eval_table)

# ---------------------------------------------------------------------
# 4. N 및 모형별 Q-Q Plot 시각화
# ---------------------------------------------------------------------
all_qq_data <- bind_rows(qq_results)

ggplot(all_qq_data, aes(x = Theoretical_Quantile, y = T_obs)) +
  geom_point(aes(color = Model), alpha = 0.7, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.6) +
  facet_grid(N ~ Model, scales = "free_x") +
  labs(
    title = "Q-Q Plots across Models and Sample Sizes (N)",
    subtitle = "Dotted line: Fitted OLS regression line (Linearity = High R^2)",
    x = "Theoretical Quantile G^(-1)(u_r)",
    y = "Observed Purchase Time (T)"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)
  )

# ---------------------------------------------------------------------
# 5. Q-Q 확률지(probability plot) 기반 m̃ 재추정 — OLS m̂과 비교
# ---------------------------------------------------------------------
qq_reestimate_m <- function(data, m_true) {
  n_obs <- nrow(data)
  r <- 1:n_obs
  u_r <- r / (n_obs + 1)
  X_r <- data$T
  T_last <- max(data$T)
  Y_last <- data$Y_t[data$T == T_last]

  # ---- OLS 모수 (evaluate_models_enhanced와 동일 로직) ----
  fit_bass <- lm(S_t ~ Y_lag + I(Y_lag^2), data = data)
  cb <- unname(coef(fit_bass)); a_b<-cb[1]; b_b<-cb[2]; c_b<-cb[3]
  m_b <- (-b_b - sqrt(b_b^2 - 4*a_b*c_b)) / (2*c_b)
  p_b <- a_b / m_b; q_b <- -c_b * m_b

  fit_logi <- lm(S_t ~ 0 + Y_lag + I(Y_lag^2), data = data)
  cl <- unname(coef(fit_logi)); b_l<-cl[1]; c_l<-cl[2]
  q_l <- b_l; m_l <- -b_l / c_l

  g_data <- data %>% filter(Y_lag > 0)
  fit_gumb <- lm(S_t ~ 0 + Y_lag + I(Y_lag*log(Y_lag)), data = g_data)
  cg <- unname(coef(fit_gumb)); a_g<-cg[1]; b_g<-cg[2]
  q_g <- -b_g; m_g <- exp(-a_g/b_g)

  # ---- 이론적 분위수 (기존 코드와 동일) ----
  k_b <- p_b + q_b
  inv_F_bass <- -(1/k_b) * log((1-u_r)/(1+(q_b/p_b)*u_r))
  inv_F_logi <- (1/q_l) * log(u_r/(1-u_r))
  inv_F_gumb <- -log(-log(u_r))

  # ---- Q-Q 회귀(확률지 적합): X_r = alpha + beta * inv_F(u_r) ----
  fit_qq_bass <- lm(X_r ~ inv_F_bass)
  fit_qq_logi <- lm(X_r ~ inv_F_logi)
  fit_qq_gumb <- lm(X_r ~ inv_F_gumb)

  # ---- 순방향 CDF (inv_F의 역함수) ----
  F_bass <- function(x, k, ratio) (1 - exp(-k*x)) / (1 + ratio*exp(-k*x))
  F_logi <- function(x, q)        1 / (1 + exp(-q*x))
  F_gumb <- function(x)           exp(-exp(-x))

  # ---- 회귀직선으로 보정된 마지막 시점(T_last)의 확률(u) 역산 ----
  z_b <- (T_last - coef(fit_qq_bass)[1]) / coef(fit_qq_bass)[2]
  z_l <- (T_last - coef(fit_qq_logi)[1]) / coef(fit_qq_logi)[2]
  z_g <- (T_last - coef(fit_qq_gumb)[1]) / coef(fit_qq_gumb)[2]

  u_hat_b <- F_bass(z_b, k_b, q_b/p_b)
  u_hat_l <- F_logi(z_l, q_l)
  u_hat_g <- F_gumb(z_g)

  tibble(
    Model      = c("Bass", "Logistic", "Gumbel"),
    m_hat_OLS  = c(m_b, m_l, m_g),
    m_tilde_QQ = c(Y_last/u_hat_b, Y_last/u_hat_l, Y_last/u_hat_g),
    QQ_R2      = c(summary(fit_qq_bass)$r.squared,
                    summary(fit_qq_logi)$r.squared,
                    summary(fit_qq_gumb)$r.squared)
  ) %>%
    mutate(
      Rel_Error_OLS_Pct = 100*(m_hat_OLS - m_true)/m_true,
      Rel_Error_QQ_Pct  = 100*(m_tilde_QQ - m_true)/m_true
    )
}

qq_reest_results <- map_dfr(n_list, function(n) {
  train_data <- dram_processed %>% filter(T <= n)
  qq_reestimate_m(train_data, m_true) %>% mutate(N = n, .before = 1)
})

print("=== OLS m̂ vs Q-Q 확률지 기반 m̃ 비교 ===")
print(qq_reest_results)

# 최종 best model: Q-Q 기반 |상대오차|가 가장 작은 (N, Model) 조합
final_best_qq <- qq_reest_results %>% slice_min(abs(Rel_Error_QQ_Pct))
print("=== 최종 Best Model (Q-Q 기반) ===")
print(final_best_qq)

# ---------------------------------------------------------------------
# 1. 1M-DRAM 데이터 전처리 (N=40 고정)
# ---------------------------------------------------------------------
df_1m <- dram_data %>%
  select(T = `T(시점)`, S_t = `1MD`) %>%
  filter(!is.na(S_t)) %>%
  slice(1:40) %>%
  mutate(
    Y_t = cumsum(S_t),
    Y_lag = lag(Y_t, default = 0)
  )

# 참값 m_true 가정: N=40 누적판매량의 1.10배 (약 10% 추가 수요 존재 가정)
Y_40 <- last(df_1m$Y_t)
m_true <- Y_40 * 1.10

# ---------------------------------------------------------------------
# 2. 모형별 N=40 OLS 추정 및 m값 비교
# ---------------------------------------------------------------------

# A. Bass 모형
fit_bass <- lm(S_t ~ Y_lag + I(Y_lag^2), data = df_1m)
cb <- coef(fit_bass)
m_bass <- (-cb[2] - sqrt(cb[2]^2 - 4 * cb[1] * cb[3])) / (2 * cb[3])
p_bass <- cb[1] / m_bass
q_bass <- -cb[3] * m_bass

# B. Logistic 모형
fit_logi <- lm(S_t ~ 0 + Y_lag + I(Y_lag^2), data = df_1m)
cl <- coef(fit_logi)
q_logi <- cl[1]
m_logi <- -cl[1] / cl[2]

# C. Gumbel 모형
g_data <- df_1m %>% filter(Y_lag > 0)
fit_gumb <- lm(S_t ~ 0 + Y_lag + I(Y_lag*log(Y_lag)), data = g_data)
cg <- coef(fit_gumb)
q_gumb <- -cg[2]
m_gumb <- exp(cg[1] / q_gumb)

# ---------------------------------------------------------------------
# 3. m값 및 상대오차 결과 표 출력
# ---------------------------------------------------------------------
comparison_table <- tibble(
  N = 40,
  Model = c("Bass", "Logistic", "Gumbel"),
  m_hat = c(unname(m_bass), unname(m_logi), unname(m_gumb)),
  m_true = m_true,
  `Relative_Error(%)` = round(100 * (m_hat - m_true) / m_true, 2)
)

cat("=== 1M-DRAM (N=40) 모형별 m 추정치 및 상대오차 비교 ===\n")
print(comparison_table)

# ---------------------------------------------------------------------
# 4. Q-Q Plot 시각화 (R^2 수치 표시 추가)
# ---------------------------------------------------------------------
u_r <- (1:40) / (40 + 1)
X_r <- df_1m$T

# 이론적 분위수 G^(-1)(u_r) 계산
quantile_bass <- -(1 / (p_bass + q_bass)) * log((1 - u_r) / (1 + (q_bass / p_bass) * u_r))
quantile_logi <- (1 / q_logi) * log(u_r / (1 - u_r))
quantile_gumb <- -log(-log(u_r))

# 1) 각 모형별 Q-Q Plot R^2 산출
r2_bass <- round(summary(lm(X_r ~ quantile_bass))$r.squared, 4)
r2_logi <- round(summary(lm(X_r ~ quantile_logi))$r.squared, 4)
r2_gumb <- round(summary(lm(X_r ~ quantile_gumb))$r.squared, 4)

# 2) R^2 수치가 포함된 라벨 생성
model_labels <- c(
  "Bass" = paste0("Bass (R² = ", r2_bass, ")"),
  "Logistic" = paste0("Logistic (R² = ", r2_logi, ")"),
  "Gumbel" = paste0("Gumbel (R² = ", r2_gumb, ")")
)

# 3) Plot 데이터프레임 구성
qq_data <- tibble(
  T_obs = rep(X_r, 3),
  Theoretical_Quantile = c(quantile_bass, quantile_logi, quantile_gumb),
  Model_Raw = factor(rep(c("Bass", "Logistic", "Gumbel"), each = 40), levels = c("Bass", "Logistic", "Gumbel"))
) %>%
  mutate(Model = factor(model_labels[Model_Raw], levels = model_labels))

# 4) Q-Q Plot 그리드 그래프 생성
ggplot(qq_data, aes(x = Theoretical_Quantile, y = T_obs)) +
  geom_point(aes(color = Model), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  facet_wrap(~ Model, scales = "free_x") +
  labs(
    title = "Q-Q Plot for 1M-DRAM Diffusion Models (N=40)",
    subtitle = "Evaluated by Linearity (R²) between Theoretical Quantiles and Observed Time",
    x = "Theoretical Quantiles G^(-1)(u_r)",
    y = "Observed Purchase Time (T)"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11) # 패싯 라벨 굵게 표시
  )
