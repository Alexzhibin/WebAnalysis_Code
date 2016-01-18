par(family='STXihei')##Show chinese in plot
data1 = c(70,24,80,82) #网龄小于1年的用户，对网站满意度的评分数据
data2 = c(42,48,104,125)#网龄在1～2年的用户，对网站满意度的评分数据
data3 = c(33,28,181,113)#网龄在2～5年的用户，对网站满意度的评分数据
data4 = c(37,18,154,92)#网龄大于5年的用户，对网站满意度的评分数据

data = rbind(data1,data2,data3,data4)
sol=chisq.test(data,correct=FALSE)
sol$p.value #Because p-value<0.05, then 网龄与满意度不相关

library(vcd)
data.tab = as.table(data)
dimnames(data.tab)=list(c("<1 year","1-2 years","2-5 years",">5 years"),c("Not Satisify","So-so","Satisify","Very Satisify"))
mosaic(data.tab,shade=T,legend=T)

