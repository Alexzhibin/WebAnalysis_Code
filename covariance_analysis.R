price1 = c(79,79,87,79,71,84,82,85,82,88,81,71,76,76,81,75,72,83,76,77)
price2 = c(193,192,191,181,191,192,187,191,196,196,196,196,192,194,
           192,189,196,202,197,206,199,191,194,195,196)
price3 = c(252,252,255,264,254,252,253,260,258,258,259,254,251,250,
           257,250,257,261,246,252,258,259,251)
price4 = c(258,248,263,256,260,261,248,255,254,250,252,258,255,245,253)


uv1 = c(81011,78313,91522,80950,72560,87483,85403,83561,87223,85294,88893,82414,76079,
        80616,78477,85281,77163,74974,85494,79951)
uv2= c(82121,82608,82568,76671,79589,80357,77332,81449,83310,82537,88893,82414,76079,
       80616,78477,85281,77163,74974,85494,79951,81011,78313,91522,80950,72560)
uv3 = c(87483,85403,83561,87223,85294,88893,82121,82608,82568,76671,79589,80357,77332,
        79412,83451,82907,80616,80384,83260,81707,83451,82907,80616)
uv4 = c(82067,80942,84601,82626,83405,84811,79412,83451,82907,80616,80384,83260,81707,
        79674,80317)
price = c(price1,price2,price3,price4)
uv = c(uv1,uv2,uv3,uv4)
group = as.factor(rep(1:4,c(20,25,23,15)))
data = data.frame(age=group,price=price,uv=uv)
##Plot 
coplot(price~uv|age,data=data,pch=16)

#####Assumption of Covariance####
#1. T test #>0.05, pass
shapiro.test(subset(data,age==1)$price)
shapiro.test(subset(data,age==2)$price)
shapiro.test(subset(data,age==3)$price)
shapiro.test(subset(data,age==4)$price)
#2. Barlett.test #>0.05, pass
bartlett.test(price~age,data=data)
#3. Intercation 
#(1)
library(HH)
ancova(price~uv+age,data) #If the lines are paralized, then the interaction of two term is not significant
#(2)
sol = aov(price~uv*age,data=data)
summary(sol) ##the uv:age mean the interaction term of uv and age, the p-value is less than 0.05, so it is significant 
#From the summary, all 3 term affect the price significantly














