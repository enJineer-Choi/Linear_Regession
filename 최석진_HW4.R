#####201922046 최석진 HW4
###1."ex4.1.csv" 데이터는 다음의 변수로 이루어져 있다.
install.packages("car")
install.packages("glmnet")
library(MASS)
library(glmnet)
library(car)
dt <- read.csv("ex4.1.csv")
####(1) 변수들 사이의 산점도 행렬을 그리고 설명하라.
pairs(dt)
####(2) 변수들 사이의 상관관수를 구하여라. (소수점 둘째자리까지 반올림) 
round(cor(dt),2)
####(3)선형회귀모형 적합
hipcenter_lm <- lm(hipcenter~.,dt)
####(4)위의 선형회귀모형에서 개별 회귀계수의 
summary(hipcenter_lm)
####(5) car 패키지의 vif 함수를 이용하여 각 vif
vif(hipcenter_lm)
####(6) glmnet 함수를 이용하여 능형회귀모형 적합
#####(a)
X <- model.matrix(hipcenter~.,dt)[,-1]
y <- dt$hipcenter
head(X)
head(y)
ridge.fit <- glmnet(X,y,alpha=0,lambda=seq(0,100,1))
plot(ridge.fit,label=TRUE)
abline(h=0,col="grey",lty=2)

#####(b)
cv.fit <- cv.glmnet(X,y,alpha=0,nfolds=length(y))
cv.fit
plot(cv.fit)
lam <- cv.fit$lambda.min;lam

#####(c)
predict(ridge.fit,type="coefficients",s=lam)

###2.
####(1)
age = c(12, 15, 42, 52, 59, 73, 82, 91, 96, 105, 114, 120, 121, 128, 130, 139, 139, 157, 1, 1, 2, 8, 11, 18, 22, 31, 37, 61, 72, 81, 97, 112, 118, 127, 131, 140, 151, 159, 177, 206)
kyphosis_present = c(rep(1, 18), rep(0, 22))
kyphosis <- data.frame(
  age,kyphosis_present
)

# 로지스틱 회귀모델 적합
glm.fit<- glm(kyphosis_present ~ age, data = kyphosis, family = binomial)
####(2)
summary(glm.fit)
####(3)
plot(kyphosis_present,age)
boxplot(kyphosis$age~kyphosis$kyphosis_present)

####(4)

glm.fit_2 <- glm(kyphosis_present~age+I(age^2),data=kyphosis,family = binomial)
summary(glm.fit_2)
