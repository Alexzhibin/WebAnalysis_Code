par(family='STXihei')##Show chinese in plot
##############
#Convertion Bubbel Plot
##############

##1.Create Data
id = c("手机数据","食品饮料","电脑办公","家具用品","母婴玩具","家用电器",
       "服饰鞋帽","日用百货","虚拟商品","箱包礼品")
conver = c(0.012,0.02,0.015,0.014,0.018,0.013,0.01,0.025,0.045,0.011)
pv=c(23.19,10.89,15.09,12.11,9.6,20.29,40.56,28.66,20.43,13.84)
price = c(3509,59,2501,509,411,3011,476,81,379,610)

##2.Set color of different categories
library(RColorBrewer)
col = brewer.pal(11,"Spectral")[2:11]

##3.Set the size of dots
cex.max = 12
cex.min = 3
a = (cex.max-cex.min)/(max(price)-min(price))
b = cex.min-a*min(price)
cex2= a*price+b

##4.Bubbel Plot
plot(pv,conver,col=col,cex=cex2,pch=16,ylim=c(0,0.6),xlab="页面浏览量(万)",
     ylab="转化率",main=list("各类目转化率－页面浏览量－客单价",cex=2),yaxt="n")
legend("topleft",legend=id,pch=16,col=col,byt="n",cex=0.8,ncol=5)
axis(2,labels=paste(seq(0,5,1),"%",sep=""),at=seq(0,0.05,0.01))
text(x=pv,y=conver,labels=price,cex=0.8)
text(x=40,y=0.055,labels="Z-客单价",cex=1.3)

##############
#Sales Line Plot
##############
x.text = c("1月","2月","3月","4月","5月","6月","7月","8月","9月","10月","11月","12月")
sales.1 = c(49.9,71.5,106.4,129.2,144.0,176.0,135.6,148.5,216.4,194.1,95.6,54.4)
sales.2 = c(83.6,78.8,98.5,93.4,106.0,84.5,105,104.3,91.2,83.5,106.6,92.3)
sales.3 = c(48.9,38.8,39.3,42.4,47.0,48.3,62.0,59.6,52.4,65.2,59.3,53.0)
sales.4 = c(42.4,33.2,34.5,39.7,52.6,70.5,57.4,62.0,47.6,39.1,46.8,51.1)

id = c("帆布鞋","T恤","皮包","冲锋衣") ##Prepare the data
col = c("black","red","orange","purple")
plot(sales.1,type="b",xaxt="n",ylim=c(0,300),col=col[1],main="月销量趋势图",
     xlab="月份",ylab="销量(万元)")
axis(1,at=1:12,labels=x.text,tick=FALSE)
legend("topleft",legend=id,horiz=T,pch=15,col=col,cex=0.8,bty="n")
grid(nx=NA,ny=8,lwd=1,lty=2,col="blue") ##Plot the sale 
lines(sales.2,type="b",col=col[2]) ##add lines
lines(sales.3,type="b",col=col[3]) ##add lines
lines(sales.4,type="b",col=col[4]) ##add lines


##############
#barPlot
##############
par(mfrow=c(2,1))
data = data.frame(pre=c(113,134,123,145,137,196,187),now=c(129,122,134,149,146,215,208))
col=c("azure4","brown4")
barplot(data$now,beside=TRUE,col=col[2],axes=F)
axis(2)
text.x = c("周一","周二","周三","周四","周五","周六","周日")
axis(1,seq(from=0.7,by=1.2,length.out=7),labels=text.x,tick=FALSE,cex.axis=0.75)
title(main=list("以向量形式输入一组数据",cex=1.5,col="red",font=3),ylab="网站日页面浏览量pv")

##
barplot(t(as.matrix(data)),beside=TRUE,col=col,axes=F)
axis(2)
text.x = c("周一","周二","周三","周四","周五","周六","周日")
axis(1,c(2,5,8,11,14,17,20),labels=text.x,tick=FALSE,cex.axis=0.75)
title(main=list("以矩阵形式输入多组数据",cex=1.5,col="red",font=3),ylab="网站日页面浏览量pv")

##
par(mfrow=c(1,1))
barplot(t(as.matrix(data)),beside=TRUE,col=col,axes=F,horiz=T)
axis(1)
text.x = c("周一","周二","周三","周四","周五","周六","周日")
axis(2,c(2,5,8,11,14,17,20),labels=text.x,tick=FALSE,cex.axis=0.75)
title(main=list("horiz=TRUE时绘制条形图",cex=1.5,col="red",font=3),ylab="网站日页面浏览量pv")


##
id = c("订购错误","支付失败","更换其他商品","订单重复","忘选优惠品","等待太久","测试订单",
       "有缺货商品","已在别处购买","价格太贵","其他")
month.3 = c(25746,8595,12832,10910,7043,2978,6934,4770,1137,1164,6926)
month.4 = c(46496,20150,19682,14177,20703,8434,9560,5113,1804,1468,11156)
month.5 = c(53356,26547,23271,16909,14789,12733,11545,7483,2506,1743,11869)
data = matrix(c(month.3,month.4,month.5),ncol=3)

library(RColorBrewer)
col = brewer.pal(11,"Spectral")[1:11]
barplot(data,col=col,xaxt="n",beside=TRUE,ylim=c(0,60000))
title(main=list("订单取消原因",cex=2),
      sub="月份：3-4 品类：帆布鞋",ylab="订单月取消数目")
legend("topleft",legend=id,pch=15,col=col,ncol=2,cex=0.8)
axis(1,labels=c("3月份","4月份","5月份"),at=c(6,18,30),tick=FALSE)

per100 = function(x){
   x = x/sum(x)
   result = paste(round(x*10000)/100,"%",sep="")
   result
}
text(labels=c(per100(month.3),per100(month.4),per100(month.5)),cex=0.7,
     x = c(seq(from=1.5,by=1,length.out=11),seq(from=13.5,by=1,length.out=11),seq(from=25.5,
     by=1,length.out=11)),
     y=c(month.3,month.4,month.5)+1000)

##barplot - horizon
pv = c(29123,279750,89994,15851,61741,119477,65602,75924,37943,11952,34567,
       11894,42780,18511,9450,1011,1533)
id = c("我的个人中心","我的订单","退换货办理","海外订单","我的收藏","礼品卡","优惠券",
       "账户余额","会员积分","收货地址","促销信息退订","个人资料","关联账户","账户安全",
       "商品评论","商品提问","邀请方式","邀请记录查询")
library(RColorBrewer)
col = c(brewer.pal(9,"YlOrRd")[1:9],brewer.pal(9,"Blues")[1:9])
barplot(pv,col=col,horiz=TRUE,xlim=c(-30000,300000))
title(main=list("个人中心各模块用户点击率",cex=2),sub="时间:2012.9.1-2012.9.30",
      ylab="个人中心各模块")
text(y=seq(from=0.7,length.out=18,by=1.2),x=-15000,labels=id)
legend("topright",legend=rev(id),pch=15,col=rev(col),ncol=2,cex=0.7)
text(labels=paste(round(10000*pv/sum(pv))/100,"%",sep=""),cex=0.7,
     y=seq(from=0.7,length.out=18,by=1.2),
     x=pv+10000)

##pieplot 
data = data.frame(id=c("安全性得不到保障","产品质量","售后服务，付款不便","送货耗时，渠道不畅",
                       "价格不够诱人","网上提供消息不可靠","其他"),
                  num=c(0.053,0.46,0.087,0.213,0.078,0.042,0.067))
library(RColorBrewer)
col = brewer.pal(11,"Spectral")[3:11]
pie(data$num,col=col,xaxt="n",labels=paste(data$id,":",round(data$num*10000)/100,"%",sep=""))
title(main=list("网站退货率高的原因",cex=2),sub="月份:3-4 品类:帆布鞋")

##复合图
data = data.frame(id=c("安全性得不到保障","产品质量,售后服务","付款不便",
                       "送货耗时，渠道不畅","价格不够诱人","网上提供消息不可靠",
                       "物流服务人员态度不好","尺码不合适","发现更好的商品"),
                  num=c(0.053,0.46,0.087,0.213,0.078,0.042,0.031,0.026,0.010))
split=6
max.bar2 = 0.4

bar1.data = matrix(rev(c(rep(NA,split+1),data$num[1:split],sum(data$num[-(1:split)]))),
                   ncol=2,nrow=split+1)
bar2.data = matrix(c(rep(NA,split+1),rev(data$num[-(1:split)]),rep(NA,nrow(data)-split+1)),
                   ncol=2,nrow=split+1)
library(RColorBrewer)
col = brewer.pal(11,"Spectral")[3:11]
barplot(bar1.data,col=c("azure3",col[1:split]),axes=FALSE,ylim=c(0,1),xlim=c(0,4.5),border="azure3")
barplot(bar2.data*(max.bar2/sum(data$num[-(1:split)])),col=col[-(1:split)],axes=FALSE,
                   add=TRUE,border="azure3")
polygon(x=c(1.2,1.2,1.4,1.4),y=c(0,sum(data$num[-(1:split)]),max.bar2,0),col="azure3",border="azure3")

labels=paste(round(data$num*10000)/100,"%",sep="")
y1 = 0
for(i in 1:split)(y1[i]=sum(data$num[-(1:i)]))
text(x=1,y=y1+0.02,labels[1:split],cex=0.8)
y2 = 0
for(i in 1:(nrow(data)-split-1)){y2[i]=sum(data$num[(split+i+1):nrow(data)])}
y2 = c(y2,0)
y2 = y2*(max.bar2/sum(data$num[-(1:split)]))
text(x=2,y=y2+0.02,labels[-(1:split)],cex=0.8)
legend("topright",legend=data$id,pch=15,col=c(rev(col[1:split]),rev(col[-(1:split)]),ncol=1,bty="n"))
title(main=list("网站退货率高的原因",cex=2),sub="月份:3-4 品类:帆布鞋")

##Histgram (分布图)
par(mfrow=c(3,1))
hist(pv,main="页面浏览量的直方图:统计尺度间隔为3",xlab="页面浏览量(万)",breaks=seq(200,280,3))
hist(pv,main="页面浏览量的直方图:统计尺度间隔为1",xlab="页面浏览量(万)",breaks=seq(220,280,1))

##Histgram (密度图)
plot(density(pv),main="页面浏览量的密度曲线图",xlab="页面浏览量(万)")
rug(pv)








































































