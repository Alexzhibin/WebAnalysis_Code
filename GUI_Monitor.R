par(family='STXihei')##Show chinese in plot
##(1)Packages
options(guiToolkit="RGtk2")
library(gWidgets)
library(RColorBrewer)
##(2)Data #or we can get the data through MySQL
month = c(36313,35500,16965,230116)
week = c(8646,8875,3393,51137)

#########------------------------------
##(5)Buid the updatePlot function
updatePlot = function(h,...){
  x.name = c()
  period.num = which(period.value==svalue(period.droplist))
  if(period.num==1){
    do.num=week
  }else{
    do.num=month
  }
  names(do.num)=c("新发话题数","回复话题数","转发话题数","浏览话题数")
  view.num = which(view.value==iconv(svalue(view.radio),"UTF-8","gbk"))
  if(view.num==1){
    for(i in 1:4){
      x.name[i]=paste(name(do.num)[i],"-",
                      round(10000*do.num[i]/sum(do.num))/100,"%",sep="")
    }
  }else{
    for(i in 1:4){
      x.name[i]=paste(names(do.num)[i],"-",do.num[i],sep="")
    }
  }
  pie(do.num,labels=x.name,col=brewer.pal(4,"Set1"),border="white",family="GB1")
  
}

#########------------------------------

##(3)Build the windows
window = gwindow("xxx网站数据指标分析系统")

##Build the frame
BigGroup = ggroup(cont=window)
group = ggroup(horizontal=FALSE,container=BigGroup) #建立左边区域
period.gf = gframe("选择查看周期",container=group)
view.gf=gframe("显示方式",container=group)
group.plot = ggroup(container=BigGroup,expand=T)#建立左边的设置

##(4)Build buttons
period.value=c("周",'月')
period.droplist = gdroplist(period.value,handler=updatePlot)
size(period.droplist)=c(80,20)

view.value = c("查看百分比","查看数值")
view.radio = gradio(view.value,horizontal=FALSE,handler=updatePlot)

##Apply the buttons
add(period.gf,period.droplist)
add(view.gf,view.radio)
add(group.plot,ggraphics()) #建立右边的显示区域

##
period.droplist = gdroplist(period.value,handler=updatePlot,cont=period.gf)
view.radio = gradio(view.value,horizontal=FALSE,handler=updatePlot,cont=view.gf)



##Combine above all
window = gwindow("xxx网站数据指标分析系统")
BigGroup = ggroup(cont=window)
group=ggroup(horizontal=FALSE,container=BigGroup)#建立左边的设置区域
####添加查看周期####
period.gf = gframe("选择查看周期",container=group)
period.value=c("周","月")
period.droplist=gdroplist(period.value,handler=updatePlot)
size(period.droplist)=c(80,20)
add(period.gf,period.droplist)
####添加显示方式####
view.gf=gframe("显示方式",container=group)
view.value=c("查看百分比","查看数值")
view.radio=gradio(view.value,horizontal=FALSE,handler=updatePlot)
add(view.gf,view.radio)
group.plot=ggroup(container=BigGroup,expand=T)#建立左边的设置区域
add(group.plot,ggraphics())#建立右边的显示区域

##(6)Initialization
updatePlot()



####################################
##Other Buttons
#1.gmessage
gmessage("输入错误,请修改信息",title="gmessage",icon="error")

#2.gconfirm
gconfirm("是否在终端输出ok?",title="gconfirm",handler=function(h,...)print("OK"))

#3.ginput
g.confi = ginput("请输入置信度",text="0.95",title="注意",icon="info")

#4.files
source(gfile())

#5.gbutton
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
gbutton("确定",cont=bg)

#6.text display
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
glabel("文字展示",cont=bg)

#7.input message
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g=gedit("请输入标题信息",cont=bg)
iconv(svalue(g),"UTF-8","gbk") ##Transfer the message to UTF-8

#8.gtext
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g=gtext("请输入标题信息\n第二行\n第三行",cont=bg)
iconv(svalue(g),"UTF-8","gbk")

#9.Multuple choices
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g=gcheckbox("是否使用?",cont=bg)
svalue(g)

#10.Single Choice
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g.value=c("好","中","差")
g=gradio(g.value,horizontal=FALSE,cont=bg)
iconv(svalue(g),"UTF-8","gbk")

#List of Choices
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g.value=c("好","中","差")
g=gdroplist(g.value,horizontal=FALSE,cont=bg)
svalue(g)


#multiple click choice
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g.value=c("好","中","差")
g=gcheckboxgroup(g.value,horizontal=FALSE,cont=bg)
svalue(g)


#gtable
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g=gtable(data,cont=bg)
size(g)=c(600,400)


#slider
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g=gslider(from=0,to=100,by=1,cont=bg)
svalue(g)

#gspinbutton
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
g=gspinbutton(from=0,to=1,by=0.1,cont=bg)


#ggraphics
win=gwindow("demo");bg=ggroup(cont=win,horizontal=FALSE)
add(bg,ggraphics())









####################################









































































