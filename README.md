# Project-1
### **[이론통계학2] Project#1** <br>

3조  242STG28 황수연 252STG26 이휘민 262STG01 강재서 262STG09 조유나

<br>

📌 프로젝트 개요 (Project Overview) <br>
이 프로젝트는 Bass, Logistic, Gumbel, Exponential 등 주요 시계열 확산 모형을 데이터에 적용하여, 신제품의 총수요, 영화 누적 관객 수, 그리고 감염병 누적 확진자 및 사망자 추이를 예측하고 모형 간의 성능을 비교 분석합니다.

<br>

📊 주요 분석 방법론 (Key Methodologies) <br>
 - 적용 모형 (Diffusion Models) <br>
   Bass Model: 혁신 효과와 모방 효과를 모두 고려한 신제품 수요 예측 <br>
   Logistic / Gumbel / Exponential Model: 감염병 확산 및 영화 관객 수 등 도메인 특성에 맞춘 모형 적용 <br>

 - 모수 추정 (Parameter Estimation) <br>
   OLS (최소제곱법), NLSE (비선형최소제곱법), MLE (최우추정법) <br>
   Q-Q Plot 기반 추정 방식을 활용한 교차 검증 및 상대 오차 비교 <br>

 - 데이터 보정 기법 (Data Calibration) <br>
   영화 관객 수 분석 시, 주말 및 공휴일의 관객 수 증가(계절성)를 평일 수준으로 스케일링하는 휴일 효과 보정(Holiday Effect Correction) 기법 적용 <br>

   <br>

**\<Part A\> : DRAM 분기별 선적자료 (1982-1995)** <br>
<br>

**\<Part B-1\> : Covid-19 사망자 예측** <br>
<br>

**\<Part B-2\> : 미국 및 한국의 HIV/AIDS 확산 예측** <br>
<br>

**\<Part C\> : 영화 흥행 예측** <br>
<br>

**\<Part D\> : 수요예측 Shiny Application 만들기** <br>

<br>

# 이론통계학 2 – Project #1

확산모형(Diffusion Model)을 이용한 수요예측 및 사례분석

## Overview

본 프로젝트에서는 Bass, Logistic, Gumbel 등의 확산모형을
이용하여 다양한 실제 자료의 수요 및 확산 패턴을 분석하였다.

분석 대상은 다음과 같다.

- 256K DRAM 및 1M DRAM
- UK COVID-19 일별 사망자
- 미국 HIV/AIDS 누적 환자 수
- 국내 HIV/AIDS 신규 감염자
- 영화 관객 수

각 자료에 대해 OLS, Q-Q, NLSE, MLE 등의 추정 방법을
적용하고, 모형별 적합도와 예측 성능을 비교하였다.

## Project Structure

### Part A – DRAM
- Bass Model
- Logistic Model
- Gumbel Model
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

| Model | Description |
|---|---|
| Bass | 혁신계수와 모방계수를 이용한 확산모형 |
| Logistic | 로지스틱 성장모형 |
| Gumbel | 비대칭 확산 패턴을 고려한 모형 |
| Exponential | 지수적 성장모형 |

## Authors

- 이휘민 – Part A
- 조유나 – Part B-1
- 강재서 – Part B-2
- 황서영 – Part C, D

**역할분담**    \<Part A\> : 이휘민, \<Part B-1\> : 조유나, \<Part B-2\> : 강재서, \<Part C, D\> : 황수연
