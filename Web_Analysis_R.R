par(family='STXihei')##Show chinese in plot
###1.barplot 
data = data.frame(pre=c(113,134,123,145,137,196,187),now=c(129,122,134,149,146,215,208))
ylim.max = 550
col = c("azure4","brown4")
barplot(as.matrix(rbind(data$pre,data$now)),beside=TRUE,ylim=c(0,ylim.max),col=col,axes=F)
axis(2)
###2.Matrix Transfer
as.matrix(rbind(data$pre,data$now))
###3.Main and title 
main = list("主标题",font=3,col="red",cex=1.5)
title(main=list("本周pv趋势分析图",cex=1.5,col="red",font=3),sub=paste("范围:2013.4--
                2013.4.28","网站板块:军事科技"),ylab="网站日页面浏览量pv")
##OR
barplot(as.matrix(rbind(data$pre,data$now)),
        beside=T,ylim=c(0,ylim.max),col=col,axes=F,
        main=list("本周pv趋势分析图",cex=1.5,col="red",font=3),
        sub=paste("范围:2013.4--
                2013.4.28","网站板块:军事科技"),ylab="网站日页面浏览量pv")

###4.legend
text.legend=c("上周pv","本周pv","pv同比增长","pv环比增长")
col2 = c("black","blue")
legend("topleft",pch=c(15,15,16,16),legend=text.legend,col=c(col,col2),bty="n",horiz=TRUE)

##
legend(1,500,pch=c(15,15,NA,NA),lty=c(NA,NA,1,1),legend=text.legend,col=c(col,col2,ncol=2,bg="aliceblue"))

##
text.x = c("周一","周二","周三","周四","周五","周六","周日")
axis(side=1,c(2,5,8,11,14,17,20),labels=text.x,tick=FALSE,cex.axis=0.75)


##百分比刻度 pv的同比增长率和环比增长率
axis(4,at=seq(from=250,length.out=7,by=40),labels=c("-60%","-40%","-20%","0","20%","40%","60%"))


##边框
x.text = c("1月","2月","3月","4月","5月","6月","7月","8月","9月","10月","11月","12月")
sales.volume= c(158721,190094,108441,88092,68709,50116,90117,
                 160044,186045,106334,89092,104933)
par(mfrow=c(2,3))
plot(sales.volume,type="b",ylim=c(20000,250000),xaxt="n",yaxt="n",main="bty默认
      取\"o\"",xlab="月份",ylab="销量")
axis(1,at=1:12,labels=x.text,tick=FALSE)
axis(2,tick=FALSE)

























