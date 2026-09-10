## =========================================================================
## Part B-2 (2) : 국내(내국인) HIV/AIDS 확산예측 (Logistic / Gumbel 대안모형)
##  - 자료 : 질병관리청 감염병연보, 연도별 "내국인" HIV 신규감염자수 S(t), 1985~2024
##  - 모형/추정식 출처 : "Bass-모형.pptx" (slide 7, 8, 19) + 문제지 부록
##
##  [Logistic 모형]  (p = 0)
##     dY(t) = q*Y(t)/m * (m - Y(t)) dt
##     d lnY(t) = dY(t)/Y(t) = (q/m)*(m - Y(t)) dt          <- 선형화
##     Y(t) = m / (1 + e^{-q(t-mu)})
##
##  [Gumbel 모형]  (q = 0인 Exponential과는 다른 대안모형)
##     dY(t) = q*Y(t)*(ln m - ln Y(t)) dt
##     d lnY(t) = dY(t)/Y(t) = q*(ln m - ln Y(t)) dt        <- 선형화
##     Y(t) = m * exp[-exp(-q(t-mu))]
##
##  공통:  sigma = 1/q ,  Y(t) = m*G((t-mu)/sigma)
##         S(t) = dY(t)/dt = m * sigma^{-1} * g((t-mu)/sigma)
##         g1(x) = e^{-x}/(1+e^{-x})^2         (Logistic pdf)
##         g2(x) = e^{-x} * e^{-e^{-x}}        (Gumbel   pdf)
## =========================================================================

## ---- 0. 준비 ------------------------------------------------------------
# install.packages("readxl")   # 최초 1회만 실행
library(readxl)

## ---- 0-1. 데이터 불러오기 -------------------------------------------------
## 파일 : "국내_HIV_AIDS_연도별_성별_현황_1985_2025.xlsx"
## 시트 : "연도별성별HIV감염현황"
## 구조 : 1행 제목, 2행 공백, 3~4행 병합 헤더(전체/내국인/외국인 x 계·남자·여자),
##        5행부터 데이터 (A열=연도, B~D열=전체, E~G열=내국인, H~J열=외국인)
##
## ** 이 문제는 "국내(내국인)" 자료만 사용 -> E열("내국인 소계")을 S(t)로 사용 **

path <- "국내_HIV_AIDS_연도별_성별_현황_1985_2025.xlsx"   # 본인 파일 경로로 수정

raw <- read_excel(path, sheet = "연도별성별HIV감염현황",
                   skip = 4, col_names = FALSE)            # 4개 헤더행을 건너뛰고 데이터만 읽음

## 1열=연도, 5열=내국인 "소계" (내국인 신규감염자수, S(t)로 사용)
dat <- data.frame(year = as.numeric(raw[[1]]),
                   St   = as.numeric(raw[[5]]))
dat <- dat[!is.na(dat$year) & !is.na(dat$St), ]
dat <- dat[order(dat$year), ]

dat <- dat[dat$year <= 2024, ]        ## 문제 지정대로 t <= 2024 자료만 사용 (2025는 제외)

dat$Yt <- cumsum(dat$St)              # 누적 감염자수 Y(t)  (내국인 누적)
n <- nrow(dat)                        # n = 40 (1985~2024)

## =========================================================================
## a) 시계열도표 : S(t), t <= 2024
## =========================================================================
par(family = "AppleGothic")     # Mac 기준 (Windows는 "맑은 고딕" 등으로 교체)
plot(dat$year, dat$St, type = "o", pch = 16, col = "steelblue",
     xlab = "연도", ylab = "S(t) : 연간 내국인 HIV 신규감염자수",
     main = "국내(내국인) 연도별 HIV 신규감염자수 (1985~2024)", family = "AppleGothic")
grid()

## =========================================================================
## b) (dlnYt vs Yt), (dlnYt vs lnYt) 산점도 + OLS로 (m,q) 추정
##
##   dlnY_t = dY_t/Y_t (연간 로그증가율) 는 Δt=1(년)로 이산근사하면
##            dlnY_t ≈ S_t / Y_(t-1)          (t = 2,...,n)
##
##   Logistic:  dlnY_t = q - (q/m)*Y_(t-1) + e_t      (Y_(t-1)에 대해 선형)
##   Gumbel  :  dlnY_t = q*ln(m) - q*ln(Y_(t-1)) + e_t   (ln Y_(t-1)에 대해 선형)
## =========================================================================
Ylag  <- dat$Yt[-n]                    # Y_1,...,Y_(n-1)  ( = Y_(t-1), t=2,...,n )
dlnY  <- dat$St[-1] / Ylag             # dlnY_t,  t = 2,...,n
year2 <- dat$year[-1]

## ---- 방법1 : 전체 자료로 OLS ---------------------------------------------
fit_logistic_OLS <- function(Ylag, dlnY) {
  df  <- data.frame(dlnY = dlnY, Ylag = Ylag)
  fit <- lm(dlnY ~ Ylag, data = df)
  q_hat <- unname(coef(fit)[1])                 # 절편 = q
  m_hat <- -q_hat / unname(coef(fit)[2])         # 기울기 = -q/m  ->  m = -q/slope
  list(fit = fit, m = m_hat, q = q_hat)
}

fit_gumbel_OLS <- function(Ylag, dlnY) {
  df  <- data.frame(dlnY = dlnY, lnYlag = log(Ylag))
  fit <- lm(dlnY ~ lnYlag, data = df)
  q_hat  <- -unname(coef(fit)[2])                # 기울기 = -q
  lnm    <- unname(coef(fit)[1]) / q_hat          # 절편 = q*ln(m)  ->  ln(m) = 절편/q
  m_hat  <- exp(lnm)
  list(fit = fit, m = m_hat, q = q_hat)
}

res_logi_full <- fit_logistic_OLS(Ylag, dlnY)
res_gumb_full <- fit_gumbel_OLS(Ylag, dlnY)

cat(sprintf("[전체자료] Logistic OLS : m=%.0f, q=%.5f, R^2=%.3f\n",
            res_logi_full$m, res_logi_full$q, summary(res_logi_full$fit)$r.squared))
cat(sprintf("[전체자료] Gumbel   OLS : m=%.0f, q=%.5f, R^2=%.3f\n",
            res_gumb_full$m, res_gumb_full$q, summary(res_gumb_full$fit)$r.squared))
cat(sprintf("(참고) 2024년까지 실제 누적감염자수 Y_2024 = %.0f 명 -> m추정치가 이보다 작으면 모형이 부적합함을 의미\n",
            dat$Yt[n]))

## ---- 방법2 : 최근 10년(2015~2024) 자료만 사용 -----------------------------
recent <- year2 >= (max(dat$year) - 9)
res_logi_recent <- fit_logistic_OLS(Ylag[recent], dlnY[recent])
res_gumb_recent <- fit_gumbel_OLS(Ylag[recent], dlnY[recent])

cat(sprintf("[최근10년] Logistic OLS : m=%.0f, q=%.5f, R^2=%.3f\n",
            res_logi_recent$m, res_logi_recent$q, summary(res_logi_recent$fit)$r.squared))
cat(sprintf("[최근10년] Gumbel   OLS : m=%.0f, q=%.5f, R^2=%.3f\n",
            res_gumb_recent$m, res_gumb_recent$q, summary(res_gumb_recent$fit)$r.squared))

## ---- 산점도 (선형성 검토) --------------------------------------------------
par(mfrow = c(1, 2), family = "AppleGothic")
plot(Ylag, dlnY, pch = 16, col = "darkorange",
     xlab = "Y_(t-1)", ylab = "dlnY_t = S_t / Y_(t-1)",
     main = "Logistic 검토: dlnYt vs Yt", family = "AppleGothic")
abline(res_logi_full$fit, col = "red", lwd = 2)

plot(log(Ylag), dlnY, pch = 16, col = "darkorange",
     xlab = "ln Y_(t-1)", ylab = "dlnY_t = S_t / Y_(t-1)",
     main = "Gumbel 검토: dlnYt vs lnYt", family = "AppleGothic")
abline(res_gumb_full$fit, col = "red", lwd = 2)
par(mfrow = c(1, 1))

## R^2 비교로 더 적절한 선형모형(=더 적합한 확산모형) 판단
## R^2가 더 높은 쪽을 아래 c), d) 단계에서 사용할 (m,q)로 채택

## =========================================================================
## c) Q-Q plot 을 이용한 (m, mu, sigma) 추정   ***b)의 OLS와는 독립적인 방법***
##
##   [부록 방법]
##   가) m 값을 "변화시켜가며" 그때마다 t = mu + sigma*G^{-1}(U_t) + e_t 를 적합시켜
##       R^2를 가장 크게(직선에 가장 가깝게) 만드는 m을 선택
##   나) 그 m에서의 회귀직선 절편·기울기로 (mu, sigma)를 추정
##
##   U_t = Y_t / (m+1) ,  t = 1995,...,2009 (문제에서 지정한 구간)
##   G^{-1}_logistic(u) = ln(u/(1-u))
##   G^{-1}_gumbel(u)   = -ln(-ln(u))
##
##   * b)의 OLS로 구한 m은 c)에서 그대로 쓰지 않는다 - c) 자체에서 m을 새로 찾는다 *
## =========================================================================
qq_range <- dat$year >= 1995 & dat$year <= 2009    # 문제에서 지정한 구간
qq_sub   <- dat[qq_range, ]
Y_qq_max <- max(qq_sub$Yt)                         # m은 반드시 이 값보다 커야 함 (U<1)

r2_given_m <- function(m, dist) {
  U <- qq_sub$Yt / (m + 1)
  if (any(U <= 0) || any(U >= 1)) return(-Inf)
  Ginv <- if (dist == "logistic") log(U / (1 - U)) else -log(-log(U))
  fit  <- lm(qq_sub$year ~ Ginv)
  summary(fit)$r.squared
}

## R^2를 극대화하는 m을 탐색 (연속최적화, optimize 사용)
find_best_m <- function(dist, upper) {
  opt <- optimize(function(m) r2_given_m(m, dist),
                   interval = c(Y_qq_max + 1, upper), maximum = TRUE, tol = 1e-4)
  list(m = opt$maximum, r2 = opt$objective)
}

fit_QQ_optimal <- function(dist, upper) {
  best <- find_best_m(dist, upper)
  m_hat <- best$m

  U    <- qq_sub$Yt / (m_hat + 1)
  Ginv <- if (dist == "logistic") log(U / (1 - U)) else -log(-log(U))
  fit  <- lm(qq_sub$year ~ Ginv)
  mu_hat    <- unname(coef(fit)[1])
  sigma_hat <- unname(coef(fit)[2])

  ## 경계값(upper)에서 최적화가 끝났다면 -> 내부 최댓값이 아니라 R^2가 계속 증가만 하는 경우
  hit_boundary <- (m_hat > upper * 0.999)

  list(m = m_hat, r2 = best$r2, fit = fit, Ginv = Ginv, year = qq_sub$year,
       mu = mu_hat, sigma = sigma_hat, q_from_sigma = 1 / sigma_hat,
       hit_boundary = hit_boundary)
}

qq_logi <- fit_QQ_optimal("logistic", upper = 2e5)     # Logistic: 내부 극대점 존재
qq_gumb <- fit_QQ_optimal("gumbel",   upper = 1e9)     # Gumbel  : 경계 도달 여부 확인용으로 매우 크게 탐색

cat(sprintf("\n[Logistic Q-Q] R^2를 극대화하는 m=%.0f (R^2=%.4f), mu=%.2f, sigma=%.3f (q=1/sigma=%.5f)\n",
            qq_logi$m, qq_logi$r2, qq_logi$mu, qq_logi$sigma, qq_logi$q_from_sigma))

if (qq_gumb$hit_boundary) {
  cat(sprintf("[Gumbel   Q-Q] 경고: m을 키울수록 R^2가 계속 개선되어(경계값 m=%.2e 에서 R^2=%.4f) 유한한 최적 m을 특정할 수 없음\n",
              qq_gumb$m, qq_gumb$r2))
  cat("               -> Gumbel 모형은 이 Q-Q 방법으로 (m,mu,sigma)를 안정적으로 추정하기 어려움 (한계점)\n")
} else {
  cat(sprintf("[Gumbel   Q-Q] R^2를 극대화하는 m=%.0f (R^2=%.4f), mu=%.2f, sigma=%.3f (q=1/sigma=%.5f)\n",
              qq_gumb$m, qq_gumb$r2, qq_gumb$mu, qq_gumb$sigma, qq_gumb$q_from_sigma))
}

## ---- R^2 대 m 탐색곡선 (가시화) : 왜 Logistic은 극대점이 있고 Gumbel은 없는지 확인 ----
m_grid_logi <- seq(Y_qq_max + 1, 2e5, length.out = 200)
r2_grid_logi <- sapply(m_grid_logi, r2_given_m, dist = "logistic")
m_grid_gumb <- 10^seq(log10(Y_qq_max + 1), log10(1e9), length.out = 200)
r2_grid_gumb <- sapply(m_grid_gumb, r2_given_m, dist = "gumbel")

par(mfrow = c(1, 2), family = "AppleGothic")
plot(m_grid_logi, r2_grid_logi, type = "l", col = "darkgreen", lwd = 2,
     xlab = "m", ylab = "R^2", main = "Logistic: m별 R^2 (뚜렷한 극대점)", family = "AppleGothic")
abline(v = qq_logi$m, col = "red", lty = 2)

plot(m_grid_gumb, r2_grid_gumb, type = "l", col = "darkorange", lwd = 2, log = "x",
     xlab = "m (log scale)", ylab = "R^2", main = "Gumbel: m별 R^2 (점근적 수렴, 극대점 없음)", family = "AppleGothic")
par(mfrow = c(1, 1))

## ---- Q-Q plot (선택된 m에서의 산점도 + 회귀직선) ---------------------------
par(mfrow = c(1, 2), family = "AppleGothic")
plot(qq_logi$Ginv, qq_logi$year, pch = 16, col = "purple",
     xlab = expression(G^-1*(U[t])), ylab = "t (연도)",
     main = sprintf("Logistic Q-Q (m=%.0f, R^2=%.3f)", qq_logi$m, qq_logi$r2),
     family = "AppleGothic")
abline(qq_logi$fit, col = "red", lwd = 2)

plot(qq_gumb$Ginv, qq_gumb$year, pch = 16, col = "purple",
     xlab = expression(G^-1*(U[t])), ylab = "t (연도)",
     main = sprintf("Gumbel Q-Q (m=%.2e, R^2=%.3f)", qq_gumb$m, qq_gumb$r2),
     family = "AppleGothic")
abline(qq_gumb$fit, col = "red", lwd = 2)
par(mfrow = c(1, 1))

## =========================================================================
## d) c)에서 추정된 (m, mu, sigma)로 미래 S(t) 예측 (1985~2040) 및 실제값과 겹쳐그리기
##    ***주의: b)의 OLS m이 아니라 c)의 Q-Q 최적화로 얻은 (m,mu,sigma)를 그대로 사용***
##
##   S_hat(t) = m * sigma^{-1} * g( (t-mu)/sigma )
##   g1(x) = e^{-x}/(1+e^{-x})^2          (Logistic pdf)
##   g2(x) = e^{-x} * e^{-e^{-x}}         (Gumbel   pdf)
## =========================================================================
g_logistic <- function(x) exp(-x) / (1 + exp(-x))^2
g_gumbel   <- function(x) exp(-x) * exp(-exp(-x))

S_hat <- function(t, m, mu, sigma, dist = c("logistic", "gumbel")) {
  dist <- match.arg(dist)
  x <- (t - mu) / sigma
  g <- if (dist == "logistic") g_logistic(x) else g_gumbel(x)
  m / sigma * g
}

t_grid <- seq(1985, 2040, by = 1)

S_pred_logi <- S_hat(t_grid, qq_logi$m, qq_logi$mu, qq_logi$sigma, "logistic")

par(family = "AppleGothic")
if (qq_gumb$hit_boundary) {
  ## Gumbel은 c)에서 유한한 m을 특정할 수 없었으므로 예측에서 제외하고 경고만 출력
  cat("\n[d) 경고] Gumbel은 c)에서 안정적인 m을 찾지 못해 예측에서 제외함 (Logistic만 표시)\n")
  plot(dat$year, dat$St, pch = 16, col = "black",
       xlim = range(t_grid), ylim = range(c(dat$St, S_pred_logi), na.rm = TRUE),
       xlab = "연도", ylab = "S(t) : 내국인 신규 감염자수",
       main = "국내(내국인) HIV 신규감염자수: 실측치 vs Logistic 예측 (Q-Q 기반)",
       family = "AppleGothic")
  lines(t_grid, S_pred_logi, col = "blue", lwd = 2)
  legend("topleft", legend = c("실제 관측치 S(t)", "Logistic 예측"),
         col = c("black", "blue"), pch = c(16, NA), lty = c(NA, 1),
         lwd = c(NA, 2), bty = "n")
} else {
  S_pred_gumb <- S_hat(t_grid, qq_gumb$m, qq_gumb$mu, qq_gumb$sigma, "gumbel")
  plot(dat$year, dat$St, pch = 16, col = "black",
       xlim = range(t_grid), ylim = range(c(dat$St, S_pred_logi, S_pred_gumb), na.rm = TRUE),
       xlab = "연도", ylab = "S(t) : 내국인 신규 감염자수",
       main = "국내(내국인) HIV 신규감염자수: 실측치 vs Logistic/Gumbel 예측 (Q-Q 기반)",
       family = "AppleGothic")
  lines(t_grid, S_pred_logi, col = "blue", lwd = 2)
  lines(t_grid, S_pred_gumb, col = "red", lwd = 2)
  legend("topleft", legend = c("실제 관측치 S(t)", "Logistic 예측", "Gumbel 예측"),
         col = c("black", "blue", "red"), pch = c(16, NA, NA), lty = c(NA, 1, 1),
         lwd = c(NA, 2, 2), bty = "n")
}
grid()

## ---- 적합도 비교 (관측구간 t<=2024에서 RMSE) ------------------------------
S_fit_logi <- S_hat(dat$year, qq_logi$m, qq_logi$mu, qq_logi$sigma, "logistic")
rmse_logi  <- sqrt(mean((dat$St - S_fit_logi)^2))
cat(sprintf("\n[관측구간 적합도] Logistic(Q-Q 기반) RMSE = %.1f\n", rmse_logi))
cat(sprintf("[Q-Q 기반] 추정 시장포화규모(m) : Logistic = %.0f명\n", qq_logi$m))

if (!qq_gumb$hit_boundary) {
  S_fit_gumb <- S_hat(dat$year, qq_gumb$m, qq_gumb$mu, qq_gumb$sigma, "gumbel")
  rmse_gumb  <- sqrt(mean((dat$St - S_fit_gumb)^2))
  cat(sprintf("[관측구간 적합도] Gumbel(Q-Q 기반)   RMSE = %.1f\n", rmse_gumb))
  cat(sprintf("[Q-Q 기반] 추정 시장포화규모(m) : Gumbel = %.0f명\n", qq_gumb$m))
} else {
  cat("[관측구간 적합도] Gumbel: Q-Q 방법으로 m 추정 불가하여 RMSE 계산 생략\n")
}

## ---- 참고: b)의 OLS 추정치와 c)의 Q-Q 추정치 비교 -------------------------
cat("\n[참고: b) OLS vs c) Q-Q 방법 비교]\n")
cat(sprintf("  Logistic: OLS(전체자료) m=%.0f  vs  Q-Q(1995~2009) m=%.0f\n",
            res_logi_full$m, qq_logi$m))
cat(sprintf("  Gumbel  : OLS(전체자료) m=%.0f  vs  Q-Q(1995~2009) %s\n",
            res_gumb_full$m, ifelse(qq_gumb$hit_boundary, "m 추정 불가", sprintf("m=%.0f", qq_gumb$m))))
