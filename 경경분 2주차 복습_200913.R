# 경경분 2주차 복습 200913 
# 제1장 비선형 회귀함수

# 6.1.1 비선형 관계에 있어서 X의 변화에 대한 Y의 효과 
## X 값에 관계 없이 Y의 효과(Y의 변화량, delta Y)가 일정한 선형회귀와는 달리 비선형 회귀는 X 값의 영향을 받는다. 
## 따라서 delta Y를 추정할 때 비선형 회귀는 선형회귀보다 방법이 복잡하다. 우선 delta Y의 표준오차를 구해야한다. 

caschool = read.csv("data/caschool.csv") 
attach(caschool)

avgin2 = (avginc)^2
ols.fit = lm(testscr ~ avginc + avgin2)  # 2차 다항회귀식 
summary(ols.fit)

a1=0; a2=1; a3=21  # X=11일 떄의 회귀식에서 X=10일 떄의 회귀식을 빼기 
a = matrix(c(a1, a2, a3), ncol=1)
delta.y = t(a)%*%matrix(ols.fit$coefficients, ncol=1)  # delta Y 계산, aT * B

ols.cov = vcov(ols.fit)  # 회귀계수의 공분산행렬 
ols.cov

# delta Y의 표준오차 계산: 회귀계수의 공분산행렬 이용 
delta.y.se = sqrt(t(a)%*%ols.cov%*%a) 

# 95% 신뢰구간 계산 
upper.delta.y = delta.y + 1.96*delta.y.se
lower.delta.y = delta.y - 1.96*delta.y.se 


# 6.2 독립변수 1개를 갖는 비선형회귀모형 
## 최초의 다항회귀의 차수 r은 어떻게 잡을까? 
## 경제자료를 포함한 많은 응용분야에서 비선형 함수들은 'sharp jumps'나 'spike'를 가지고 있지 않으므로 일반적으로 r=2(90%), 3(9%), 4(1%)부터 시작한다. 


# 1. 선형성 가설에 대한 F 검정 
caschool = read.csv("data/caschool.csv") 
attach(caschool)

avgin2 = (avginc)^2
avgin3 = (avginc)^3

lm1 = lm(testscr ~ avginc)  # Reduced Model 
lm3 = lm(testscr ~ avginc + avgin2 + avgin3)  # Full Model

anova(lm1, lm3)

# [결과] ANOVA 검정을 한 결과, F 검정통계량은 23.954이고 p-value는 1.424*e^-10으로 0에 가까운 수이며 alpha=0.05보다 작다. 따라서 선형성을 가정한 귀무가설을 기각한다. 즉, 회귀함수는 비선형이다. 


# 2. AIC, BIC를 사용한 다항회귀의 차수(r) 결정 

inc.powers = cbind(avginc, avginc^2, avginc^3, avginc^4, avginc^5, avginc^6)
BIC.lm = c()
n = length(testscr)

# 다항회귀의 찻수를 r = 1, 2, 3, 4, 5, 6로 했을 때의 BIC 계산
for (i in 1:6){
  lm.fit = lm(testscr ~ inc.powers[,1:i])
  BIC.lm[i] = extractAIC(lm.fit, k=log(n))[2]
}

plot(cbind(1,2,3,4,5,6), BIC.lm, type='b')
# [결과] BIC 기준으로 r=2일 때 가장 좋다. 즉, 2차 다항회귀가 가장 좋은 모델이다. 