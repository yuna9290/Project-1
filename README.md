
# 이론통계학2 – Project #1

3 조  242STG28 황수연 252STG26 이휘민 262STG01 강재서 262STG09 조유나

## Overview

본 프로젝트에서는 Bass, Logistic, Gumbel 등의 확산모형을
이용하여 다양한 실제 자료의 수요 및 확산 패턴을 분석하였다.

분석 대상은 다음과 같다.

- 256K DRAM 및 1M DRAM
- UK COVID-19 일별 사망자
- 미국 HIV/AIDS 감염자
- 국내 HIV/AIDS 감염자
- 영화 관객 수

각 자료에 대해 OLS, Q-Q, NLSE, MLE 등의 추정 방법을
적용하고, 모형별 적합도와 예측 성능을 비교하였다.

## Project Structure

### Part A – DRAM
- Bass / Logistic / Gumbel
- OLS estimation
- Q-Q plot
- Market potential estimation

### Part B-1 – UK COVID-19
- Bass / Logistic / Gumbel
- OLS
- NLSE
- MLE

### Part B-2 – HIV/AIDS
- US HIV/AIDS
- Korean HIV/AIDS
- OLS 및 Q-Q 분석

### Part C – Movie Demand Forecasting
- 왕과 사는 남자
- 오디세이
- Bass / Logistic / Gumbel / Exponential

### Part D – Shiny Application

R Shiny를 이용하여 확산모형을 선택하고
모형 추정 및 영화 관객 수 예측 결과를 확인할 수 있는
웹 애플리케이션을 구현하였다.

## Methods

| Model | Description | Formula |
|---|---|---|
| Bass | 혁신계수와 모방계수를 이용한 확산모형 | $S(t)=m\frac{1-e^{-(p+q)t}}{1+\frac{q}{p}e^{-(p+q)t}}$ |
| Logistic | 로지스틱 성장모형 | $S(t)=\frac{m}{1+e^{-q(t-t_0)}}$ |
| Gumbel | 비대칭 확산 패턴을 고려한 모형 | $S(t)=m\exp(-e^{-q(t-t_0)})$ |
| Exponential | 지수적 성장모형 | $S(t)=ae^{bt}$ |

## Authors

- 이휘민 – \<Part A\>
- 조유나 – \<Part B-1\>
- 강재서 – \<Part B-2\>
- 황서영 – \<Part C, D\>
