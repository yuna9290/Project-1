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

역할분담  Part A : 이휘민, Part B-1 : 조유나, Part B-2 : 강재서, Part C, D : 황수연
