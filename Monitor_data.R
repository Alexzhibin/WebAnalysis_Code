par(family='STXihei')##Show chinese in plot
library(qcc)
par(mfrow=c(1,1))
view = c(10231,12874,11229,9870,11804,11652,13259,11891,12876,14562,12933,13548,
         15230,13815,15766)
targit = c(201,229,231,201,237,224,236,167,213,240,259,241,256,276,248)
sol = qcc(targit,sizes=view,type="p")
summary(sol)
##Show the outfiler 
sol$violations[[1]] #第8个点转化率不正常

##
price = c(103.76,129.12,107.30,97.45,105.1,115.78,105.21,
          98.78,101.74,96.53,97.99,114.20,116.18,80.29,82.76)
sol = qcc(price,type="xbar.one")

##nsigma=1.5
sol = qcc(price,type="xbar.one",nsigma=1.5,plot=FALSE)
sol$violations$beyond.limits

##Combine all nsigma
sol.3 = qcc(price,type="xbar.one",plot=F)
sol.2 = qcc(price,type="xbar.one",plot=F,nsigma=2)
col= c("black","green","red","orange")
x.text = paste("3.",c(1:15),sep="")
plot(price,type="b",xaxt="n",col=col[1],xlim=c(0,17),ylim=c(min(sol.3$limits[1],min(price))-10,
          max(sol.3$limits[2],max(price))+10),
     pch=19,main=list("日客单价:单值－均值质量控制图",cex=1.5),xlab="时间",ylab="日客单价(元)")
warring.3 = sol.3$violations$beyond.limits
warring.2 = sol.2$violations$beyond.limits
axis(1,at=1:15,labels=x.text,tick=FALSE)
abline(h=sol.3$limits[2],col=col[3])
abline(h=sol.3$limits[1],col=col[3])
abline(h=sol.2$limits[2],col=col[4])
abline(h=sol.2$limits[1],col=col[4])
abline(h=sol.3$center,col=col[2])

points(warring.2,price[warring.2],col=col[4],pch=19)
points(warring.3,price[warring.3],col=col[3],pch=19)
text(warring.3,price[warring.3],"异常点")


##单值－移动极差控制
sol = qcc(data.frame(price[-length(price)],c(price[-1])),type="R",nsigma=2)

##t test
#(1)
pv.new = c(10740,13368,11730,10384,12302,12159,13735,13381,15067)
sol = t.test(pv.new,mu=10000)
sol #p-value<0.05, reject the hypothesis, then the mean of not equal to 10000. And the mean is 12540, which is greater than 10000.
#所以推广对网站流量pv有明显的影响 
#(2)
pv.old = c(10231,12874,11229,9870,11804,11652,13259,11891,12876,14562,12933,13548,
           15230,13815,15766)
pv.new = c(10740,13368,11730,10384,12302,12159,13735,12389,13381,15067,13415,14034,
           15729,14327,16267)
#数据成对，是一样的
t.test(pv.new,pv.old,paired=T)$p.value #p-value<0.05,the mean of old and new is different
#数据不同
pv.old = c(10231,12874,11229,9870,11804,11652,13259,11891,12876,14562,12933,13548,
           15230,13815,15766,10500,12509,12737,11216,14117)
pv.new = c(10740,13368,11730,10384,12302,12159,13735,12389,13381,15067,13415,14034,
           15729,14327,16267)
t.test(pv.old,pv.new)$p.value #P-value is much greater than 0.05

###If is possible, we should choose the paired data




























