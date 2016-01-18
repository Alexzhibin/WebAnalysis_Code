#1.
cost = c(1.8,1.2,0.4,0.5,2.5,2.5,1.5,1.2,1.6,1.0,1.5,0.7,1.0,0.8)
sales=c(104,68,39,43,127,134,87,77,102,65,101,46,52,33)
data=data.frame(cost=cost,sales=sales)
plot(data,pch=16,xlab="cost促销让利费用(十万元)",ylab="sales促销销量(十万元)")
sol.lm=lm(sales~cost,data)
abline(sol.lm,col="red")

##2.K and B
#(1)Manully
k=cov(cost,sales)/cov(cost,cost)##k
b=mean(sales)-k*mean(cost)##b
#(2)summary
sol.lm=lm(formula=sales~cost,data)
sol.lm
#(3)
df = sol.lm$df.residual
alpha=0.05
left=summary(sol.lm)$coefficients[,1]-summary(sol.lm)$coefficients[,2]*qt(1-alpha/2,df)
left
right=summary(sol.lm)$coefficient[,1]+summary(sol.lm)$coefficients[,2]*qt(1-alpha/2,df)
right
#the range of b is from 1.667702 to 25.97978; the range of k is from 40.182861 and 57.01138

##3. R Square
r = cor(cost,sales)
r2 = r^2
r2
summary(sol.lm)

##5.T Test of K and B
summary(sol.lm)$coefficients[,4] #Obviously, the p-value of coefficients are less than 0.05, so they belong to T distribution.

##6.F test
summary(sol.lm)
#2.834e-08 is less than 0.05, then it pass F test, which means the variables will make the model different


##7.Residual
shapiro.test(sol.lm$residual) #the residuals belong to t distribution.
summary(sol.lm)

##8.Prediction
predict(sol.lm)
new.data = data.frame(cost=c(0.98,0.88))
sol.pre = predict(sol.lm,new.data,level=0.95,interval="prediction")
sol.pre






















