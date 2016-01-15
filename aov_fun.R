aov.func = function(data,alpha=0.05,plot.logic=T,p.adjust.mod="holm"){
  error = FALSE
  data[,1]=as.factor(data[,1])
  group.levels = as.numeric(levels(data[,1])) #分类变量的水平取值
  #检验不同水平下指标的正态性
  for(i in group.levels){
    if(shapiro.test(data[which(data[,1]==i),2])$p.value<alpha){
      print(paste("ERROR:第",i,"组数据不服从正态分布"))
      error = TRUE
    }
  }
    if(error){
      return()
    }else{
      print("符合正态性前途！")
    }
    #检验不同水平下指标的方差齐次性
    if(bartlett.test(data[,2]~data[,1])$p.value<alpha){
      print("ERROR:符合方差齐次性前提")
      return()
    }else{
      print("符合方差齐次性前提！")
    }
    #绘制线箱图
    if(plot.logic){
      boxplot(data[,2]~data[,1])
    }
    #方差分析
    sol = aov(data[,2]~data[,1])
    p.value=summary(sol)[[1]][,5][1]
    tab = matrix(NA,nrow=3,ncol=5)
    dimnames(tab)=list(c("分类变量A","随机性误差","总和"),c("自由度","差异平方和","均方",
                        "统计量F","p-value"))
    tab[1:2,1] = summary(sol)[[1]][,1];tab[3,1]=sum(tab[1:2,1]) #自由度
    tab[1:2,2] = summary(sol)[[1]][,2];tab[3,2]=sum(tab[1:2,3]) #差异平方和
    tab[1:2,3] = summary(sol)[[1]][,3];#均方
    tab[1:2,4] = summary(sol)[[1]][,4];#统计量F
    tab[1,5] = p.value #p-value
    print("====方差分析=====")
    print(tab)
    #多重T检验
    if(p.value<alpha){
      print(paste(p.value,"<",alpha,":各水平下的指标有明显差别!"))
      sol.t = pairwise.t.test(data[,2],data[,1],p.adjust.method=p.adjust.mod)
      tab=sol.t[[3]]
      tab[which((sol.t[[3]]>alpha)==TRUE)] = "无差别"
      tab[which((sol.t[[3]]>alpha)==FALSE)] = "显著差别"
      print("===多重T检验====")
      print(tab)
    }else{
      print(paste(p.value,">",alpha,":各水平下的指标无明显差别!"))
    }
    return(sol)
}

##Example
price1 = c(79,79,87,79,71,84,82,85,82,88,81,71,76,76,81,75,72,83,76,77)
price2 = c(193,192,191,181,191,192,187,191,196,196,196,196,192,194,
           192,189,196,202,197,206,199,191,194,195,196)
price3 = c(252,252,255,264,254,252,253,260,258,258,259,254,251,250,
           257,250,257,261,246,252,258,259,251)
price4 = c(258,248,263,256,260,261,248,255,254,250,252,258,255,245,253)
price = c(price1,price2,price3,price4)
date =rep(1:4,c(20,25,23,15))
data= data.frame(group=date,x=price)

sol = aov.func(data)

##如果不符合方差分析条件，则直接使用如下语句 
kruskal.test(list(price1,price2,price3))




















