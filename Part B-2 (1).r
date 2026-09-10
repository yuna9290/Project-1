## =========================================================================
## Part B-2 (1) : 미국 분기별 HIV/AIDS 확산예측
##  - 자료 : "DRAM-AIDS-자료.xls"  시트 = "미국AIDS발생자료"
##  - 세 가지 "모형" 비교 : Bass / Logistic / Gumbel  (부록 "3.1 비교 대상 확산모형")
##
##  [회귀식 및 모수 관계]  (부록 그대로)
##
##   Bass      : S_t = a + b*Y_(t-1) + c*Y_(t-1)^2
##               m = (-b - sqrt(b^2-4ac)) / (2c) ,  p = a/m ,  q = -c*m
##
##   Logistic  : S_t = a*Y_(t-1) + b*Y_(t-1)^2          (절편 없음)
##               p = 0 ,  q = a ,  m = -q/b
##
##   Gumbel    : S_t = a*Y_(t-1) + b*Y_(t-1)*ln(Y_(t-1)) (절편 없음, Y_(t-1)>0인 구간만 사용)
##               q = -b ,  m = exp(-a/b)
## =========================================================================

## ---- 0. 준비 ------------------------------------------------------------
# install.packages("readxl")   # 최초 1회만 실행
library(readxl)

path <- "DRAM-AIDS-자료.xls"                 # 본인 작업폴더 경로에 맞게 수정
raw  <- read_excel(path, sheet = "미국AIDS발생자료")

names(raw) <- c("year", "quarter", "t", "St", "blank", "Yt")
dat <- raw[!is.na(raw$St), c("t", "St", "Yt")]   # St가 있는 1~65분기만 사용
dat$t  <- as.numeric(dat$t)
dat$St <- as.numeric(dat$St)
dat$Yt <- as.numeric(dat$Yt)

n_all  <- nrow(dat)                 # 65 (1981.Q1 ~ 1997.Q1)
M_TRUE <- dat$Yt[n_all]              # 669,443  : 자료상 "최근 누적 감염자수"

## ---- 1-a. 시계열도표 -----------------------------------------------------
par(family = "AppleGothic")          # Mac 기준 (Windows는 "맑은 고딕" 등으로 교체)
plot(dat$t, dat$St, type = "o", pch = 16, col = "forestgreen",
     xlab = "t (분기, 1981.Q1=1)", ylab = "St (분기별 신규 HIV/AIDS 감염자수)",
     main = "미국 분기별 HIV/AIDS 감염자수 (1981~1997)", family = "AppleGothic")
grid()

## =========================================================================
## 세 모형의 OLS 추정 함수  (모두 Y_(t-1)를 사용하므로 t=2,...,n 구간만 사용)
## =========================================================================

## ---- (1) Bass 모형 --------------------------------------------------------
fit_Bass <- function(St, Ylag) {
  df  <- data.frame(St = St, Ylag = Ylag)
  fit <- lm(St ~ Ylag + I(Ylag^2), data = df)         # S_t = a + b*Y_(t-1) + c*Y_(t-1)^2
  a <- unname(coef(fit)[1]); b <- unname(coef(fit)[2]); c <- unname(coef(fit)[3])

  disc <- b^2 - 4 * a * c
  m <- (-b - sqrt(disc)) / (2 * c)          # 부록 공식 그대로
  p <- a / m
  q <- -c * m

  fitted <- predict(fit)
  rmse   <- sqrt(mean((St - fitted)^2))
  c(p = p, q = q, m = m, rmse = rmse)
}

## ---- (2) Logistic 모형 (절편 없음) ----------------------------------------
fit_Logistic <- function(St, Ylag) {
  df  <- data.frame(St = St, Ylag = Ylag)
  fit <- lm(St ~ 0 + Ylag + I(Ylag^2), data = df)     # S_t = a*Y_(t-1) + b*Y_(t-1)^2  (절편 없음)
  a <- unname(coef(fit)[1]); b <- unname(coef(fit)[2])

  q <- a
  m <- -q / b
  p <- 0

  fitted <- predict(fit)
  rmse   <- sqrt(mean((St - fitted)^2))
  c(p = p, q = q, m = m, rmse = rmse)
}

## ---- (3) Gumbel 모형 (절편 없음, Y_(t-1)>0 구간만) -------------------------
fit_Gumbel <- function(St, Ylag) {
  lnYlag <- log(Ylag)                                  # Y_(t-1) > 0 이어야 함
  df  <- data.frame(St = St, Ylag = Ylag, YlnY = Ylag * lnYlag)
  fit <- lm(St ~ 0 + Ylag + YlnY, data = df)           # S_t = a*Y_(t-1) + b*Y_(t-1)*ln(Y_(t-1))
  a <- unname(coef(fit)[1]); b <- unname(coef(fit)[2])

  q <- -b
  m <- exp(-a / b)
  p <- NA                                              # Gumbel 모형은 p(혁신계수) 정의 없음

  fitted <- predict(fit)
  rmse   <- sqrt(mean((St - fitted)^2))
  c(p = p, q = q, m = m, rmse = rmse)
}

## =========================================================================
## b) n = 20, 40, 65 에 대해 세 모형(Bass, Logistic, Gumbel) 추정
## =========================================================================
n_list <- c(20, 40, 65)
result <- data.frame()

for (n in n_list) {
  sub  <- dat[dat$t <= n, ]
  Ylag <- c(0, sub$Yt[-nrow(sub)])          # Y_(t-1), Y_0 = 0

  ## Y_(t-1) > 0 인 구간만 사용 (Gumbel의 ln(Y_(t-1)) 때문에 세 모형 모두 동일 구간으로 통일)
  keep <- Ylag > 0
  St_use   <- sub$St[keep]
  Ylag_use <- Ylag[keep]

  bass_est <- fit_Bass(St_use, Ylag_use)
  logi_est <- fit_Logistic(St_use, Ylag_use)
  gumb_est <- fit_Gumbel(St_use, Ylag_use)

  for (model in c("Bass", "Logistic", "Gumbel")) {
    est <- switch(model, Bass = bass_est, Logistic = logi_est, Gumbel = gumb_est)
    rel_err <- 100 * (est["m"] - M_TRUE) / M_TRUE     # 상대오차 = 100*(m_hat-m)/m

    result <- rbind(result, data.frame(
      n = n, model = model,
      p = ifelse(is.na(est["p"]), NA, round(est["p"], 5)),
      q = round(est["q"], 5),
      m_hat = round(est["m"]),
      rel_error_pct = round(rel_err, 2),
      RMSE = round(est["rmse"], 1)
    ))
  }
}

print(result, row.names = FALSE)

## =========================================================================
## 나) 최적 모형 선택 (적합도 RMSE 기준)
## =========================================================================
cat("\n[모형별 평균 RMSE - 값이 작을수록 적합도 우수]\n")
print(aggregate(RMSE ~ model, data = result, FUN = mean))

best <- result[which.min(result$RMSE), ]
cat(sprintf("\n적합도(RMSE) 기준 최적 모형: %s (n=%d, RMSE=%.1f)\n",
            best$model, best$n, best$RMSE))
