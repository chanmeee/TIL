# 경경분 3주차 복습 200919
# 제1장 비선형 회귀함수

# 6.2.2 로그함수 

## 비선형 함수 중 가장 많이 사용되는 함수, 특히 경제 변수 다룰 때 많이 사용한다. 
## ln(x)의 변화 -> x 비율의 변화 
## 기울기 = 1/x, x의 작은 값에서 기울기가 더 가파르다.
## 수요 분석에서 종종 가격(x)이 1% 증가할 때 수요(y)가 몇 % 감소하게 된다고 가정한다. 

# 3가지 로그회귀 모형
# 1) Linear-Log model: Y = B0 + B1*lnX1 
# 2) Log-Linear model: ln(Y) = B0 + B1*X1
# 3) Log-Log model: ln(Y) = B0 + B1*ln(X1)
## 세 가지 중에서 좋은 모형을 고르는 방법 ex) R square 기준 가장 좋은 모델 고르기 

caschool = read.csv("data/caschool.csv") 
attach(caschool)

lm.fit = lm(log(testscr) ~ avginc)
summary(lm.fit)

# Y 예측 방법 1 (대략적 예측방법: sigma_u hat이 0에 가까운 경우, 즉 보정항 고려 안해도 되는 경우)
lnY= lm.fit$coefficients[1] + lm.fit$coefficients[2]*40
Y1 = exp(lnY)

# Y 예측 방법 2 (오차항의 분호가 Normal이라는 가정하: sigma_u hat이 0에 가깝지 않은 경우, 즉 보정항 고려해야 하는 경우)
var.u = var(lm.fit$residuals) 
Y2 = exp(lnY)*exp(var.u/2)


# 6.3 교호작용 

# 6.3.1 X1, X2 모두 더미변수인 경우 -> 두 더미변수들 사이의 교호작용 
# ex) 성별, 학력(대졸/고졸이하)
HiStr = ifelse(str>20, 1, 0)
HiEL = ifelse(el_pct>10, 1, 0)
HiStrHiEL = HiStr*HiEL

lm(testscr ~ HiEL + HiStr + HiStrHiEL)

# 6.3.2 연속형 변수와 이항 변수 사이의 교호작용 
# ex) 근무연수, 학력(대졸/고졸이하)
StrHiEL = str * HiEL  # 교호작용 항
cor(HiEL, StrHiEL) # 0.9928631:  
cor(str, HiEL) # 0.1532584: 상관계수 낮음 

full.model = lm(testscr ~ str + HiEL + StrHiEL)
reduced.model = lm(testscr ~ HiEL)
anova(reduced.model, full.model) # F검정 

# 6.3.3 두 연속형 변수들 사이의 교호작용 
# ex) 근무연수, 교육연수 


