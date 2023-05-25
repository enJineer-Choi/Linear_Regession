####201922046 최석진####
###1.'dt.csv'데이터를 이용하여 회귀모형 적합하려고한다.#####
dt=read.csv("dt(1).csv",header=T)
sales=dt$Sales
compprice=dt$CompPrice
income=dt$Income
ad=dt$Advertising
pop=dt$Population
price=dt$Price
age=dt$Age
ed=dt$Education

pairs(sales~compprice+income+ad)
####(1)이 데이터의 산점도 행렬을 그리시오####
plot(sales)
plot(compprice,sales)
plot(income)

####(2)Sales를 예측하기 위한 중회귀분석을 하려고 한다. 이를 위한 모형을 설정하시오.
####(3)최소제곱법에 의한 회귀직선을 적합시키고, 모형 적합 결과를 설명하시오

model=lm(sales~compprice+income+ad+pop+price+age+ed)
#회귀직선을 적합

mod=summary(model)
mod
####(4)회귀직선의 유의성 검정을 위한 가설을 설정하고, 분산분석표를 이용하여 가설검정을 수행
anova(model)
mod$fstatistic
qf(0.95,7,392)
mod

####(5)오차분산에 대한 추정량을 구하시오
MSE=anova(model)[8,3]
sqrt(MSE)

####(6)결정계수와 수정된 결정계수를 구하시오
c(mod$r.squared,mod$adj.r.squared)

####(7)개별 회귀계수의 유의성검정을 수행하시오
mod$coef

####(8) 회귀계수에 대한 90% 신뢰구간을 구하시오
confint(model,level=0.9)

#평균반응과 개별 y값을 위한 dt생성
dt=data.frame(compprice=100,income=70,ad=20,pop=300,age=45,price=80,ed=12)

####(9) CompPrice=100,Income=70,Advertising=20,Population=300,Price=80,Education=12인 
####지역에 위치한 매장의 평균 판매액을 예측하고, 95% 신뢰구간을 구하시오
predict(model,dt,interval=c("confidence"),level=0.95)

####(10)위 매장에 대하여 개별 판매액 예측하고, 95% 신뢰구간을 구하시오.
predict(model,dt,interval=c("prediction"),level=0.95)

#잔차에 대한 분석을 하기 전에 필요한 데이터 생성
y_hat=model$fitted
resid=model$residuals

library(lmtest)
####(11)잔차에 대한 산점도를 그리고, 결과를 설명
plot(y_hat,resid)
abline(h=c(-3,0,3))

####(12)잔차에 대한 등분산선 검정을 수행하여라
bptest(model)

####(13)잔차에 대한 히스토그램, QQ plot을 그리고, 정규성 검정을 수행하여라
par(mfrow=c(1,2))
hist(resid) #히스토그램
qqnorm(resid) ##qqplot
qqline(resid) 
shapiro.test(resid) ### shapiro 검정

par(mfrow=c(1,1))

####(14)잔차에 대한 독립성 검정을 수행하시오.
dwtest(model,alternative = "two.sided")

###2.위 데이터에 대하여 다음 물음에 답하여라
####(1)위에서 적합한 모형에서 개별 회귀계수의 유의성 검정 결과 유의하지 않은 변수는 무엇인가?

####(2)위에서 유의하지 않았던 변수를 제외한 모형을 축소모형(Reduced Model)으로 하는 부분 F검정을 수행하여라.
#### 검정에 필요한 가설을 설정하고, 검정결과를 설명하시오.
model_r=lm(sales~compprice+income+ad+price+age)
anova(model_r,model)

####(3)1번에서 설정한 모형과 축소모형 중 어느 모형이 이 데이터에 대한 설명을 잘 하고 있는지를 비교하시오.
summary(model)
summary(model_r)

### 3. 1번에서 설정한 모형에 대하여 아래의 물음에 답하여라.
####(1) H_0 : CompPrice=Income vs H_1 : not H_0
reduced_model=lm(sales~I(compprice+income)+ad+pop+price+age+ed)
glh=anova(reduced_model,model)
glh

#####(a) 귀무가설하에서 모형에서의 잔차제곱합을 구하시오
SSE_RM=glh[1,2]
SSE_RM

#####(b) 대립가설하에서 모형에서의 잔차제곱합을 구하시오
SSE_FM=glh[2,2]
SSE_FM

#####(c) 일반 선형 가설검정 (General Linear Hypothesis Test)를 수행하기 위한 검정통계량을 구하시오.
glh[2,5]

#####(d) 위의 가설 검정을 위한 기각역을 구하시오
qf(0.95,1,392)

#####(e) 유의확률을 구하고, 결과를 해석하시오
glh[2,6]

library(car)
####(2) 아래의 가설검정을 R에서 linearHypothesis 함수를 사용하여 수행하시오.
#####(a) H_0 : CompPrice=-Price vs H_1:not H_0
linearHypothesis(model,c("compprice+price=0"))
model$coefficients
#####(b) H_0 : CompPrice-Price, Advertising+Price+Age=0 vs H_1: not H_0
linearHypothesis(model,c("compprice+price=0","ad+price+age=0"))
model$coefficients[4]+model$coefficients[6]+model$coefficients[7]
