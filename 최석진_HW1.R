###201922046 최석진###
##HW2##
cars <- read.csv("cars(1).csv")
x <- cars$speed ### car데이터의 속도를 x에 저장
y <- cars$dist ### car데이터의 제동거리를 y에 저장
#######(1)######
plot(y~x,pch=16,col="red",xlab="속도",ylab="제동거리") ## 산점도 그리기
#######(2)######
Sxy <- sum((x-mean(x))*(y-mean(y))) ###Sxy를 직접구하기
Sxx <- sum((x-mean(x))^2) ###Sxx를 직접구하기
beta_1_hat <- Sxy/Sxx ###Sxy/Sxx로 기울기를 직접 구하기
beta_0_hat <- mean(y)-beta_1_hat*mean(x) ### 절편 직접구하기
coef(lm(y~x))

model <- lm(y~x,cars) ### lm함수를 이용해 구한 회귀직선 model이라는 변수에 저장
names(model)
#######(3)######
plot(y~x,pch=16,col="red",xlab="속도",ylab="제동거리") ###산점도 그리기
abline(model,col="blue") ### 그 위에 적합한 회귀직선
#######(4)######
anova(model) ###분산분석표 작성

#######(5)######

summary(model) ###model의 summary에서 결정계수=Multiple R-squared를 구할 수 있다
cor(x,y) ###cor함수를 통해 x와 y의 상관계수를 구할 수 있다
cor(x,y)^2 ### 단순선형회귀에서는 상관계수의 제곱이 결정계수와 같음을 알 수 있다

mod <- summary(model)
mod$r.squared ###결정계수
mod$fstatistic[1] ###F값만 알고 싶을때!
#######(6)######
mod$coef ###개별 회귀계수에 대한 유의성검정
mod$coef[1,] ###절편에 대한 유의성검정
mod$coef[2,] ###기울기에 대한 유의성검정
#######(7)######
confint(model,level = 0.9) ###첫번째 줄이 b_0,두번째 줄이 b_1
#######(8)######
(mod$coef[2,1]-3)/mod$coef[2,2] ###b_1이 3일때의 t값을 구함
mod$coef[2,3]

#######9,10번을 위한 x0값 생성
dt <- data.frame(x=18.5)

#######(9)######
predict(model,newdata = dt,interval = c("confidence"),level=0.95) ###신뢰구간 95%의 평균반응
#######(10)######
predict(model,newdata = dt,interval = c("prediction"),level=0.95) ###신뢰구간 95%의 개별 y값
#######(11)######

##원점을 지나는 회귀직선
model1 <- lm(y~0+x)

mod1 <- summary(model1)
mod1$r.squared ### 원점을 지나는 회귀직선의 결정계수
mod1$fstatistic ### 원점을 지나는 회귀직선의 F값

#######(12)######
confint(model1,level=0.9)  ### 원점을 지나는 직선의 기울기의 90%신뢰구간
#######(13)######
anova(model1)   ### 원점지나는 직선의 분산분석표
#######(14)######
mod1$r.squared ###원점을 지나는 회귀직선의 결정계수
#######(15)######
plot(y~x,pch=16,col="red",xlab="속도",ylab="제동거리") ###산점도 그리기
abline(model,col="blue") ### 원점 포함하지 않은 회귀직선
abline(model1,col="green") ### 원점 포함한 회귀직선
###### 잔차 분석에 필요한 변수들 설정

y_hat <- model$fitted ###X값 대신 가로축에 y_hat을 이용
resid <- model$residuals ### 잔차들 (y축에 이용)

#######(16)######
plot(resid~y_hat,pch=15,col="pink",ylab="Residual") ###잔차에 대한 산점도
abline(h=0,lty=1,col='orange') ### 가로축 h=0 라인 생성


library(lmtest) ## 잔차에 대한 검정을 위한 library
#######(17)######
# H0 : 등분산성 vs H1 : 이분산성
bptest(model) ###잔차에 대한 등분산성 검정 
#######(18)######
### H0 : normal distribution vs H1 : not H0
hist(resid) #히스토그램
qqnorm(resid,pch=16) ##qqplot
qqline(resid,col=2) 
shapiro.test(resid(model)) ### shapiro 검정


#######(19)######
dwtest(model,alternative = "two.sided") #잔차에 대한 독립성 검정

