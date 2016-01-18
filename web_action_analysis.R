#setwd("~/Documents/Study/Study/Book/Data Mining/Web_analysis/WebAnalysis_Code")
#(1)
tmmp = read.fwf("anonymous-msweb.data",widths=c(60))
##
train_list = tmmp$V1
#(2)
tmp_page = c(0)
tmp_sequenceid=c(0)
tmp_eventid=c(0)
m=0
sequenceid=0
train_length=length(train_list)
for(i in 302:train_length){
  tmp=unlist(strsplit(as.character(train_list[i]),","))
  if(tmp[1]=="C"){
    sequenceid=sequenceid+1
    eventid=0
  }else if(tmp[1]=="V"){
    m=m+1
    eventid=eventid+1
    tmp_sequenceid[m]=sequenceid#填充sequenceID
    tmp_eventid[m]=eventid#填充eventID
    tmp_page[m]=as.numeric(tmp[2])
    
  }
}

tmp_page=factor(tmp_page)
data=data.frame(page=tmp_page,seqid=tmp_sequenceid,eventid=tmp_eventid)
#(3)Find out the data related to page 1034
user.page=1034
user.sequenceid = unique(data$seqid[which(data$page==user.page)])
i=1
data.user=data[1,]
for(seq.i in user.sequenceid){
  data.user=rbind(data.user,subset(data,seqid==seq.i))
}
data.user=data.user[-1,]


####cspade function transfer###
#(1)data transaction
library(arulesSequences)
tmp_data = data.frame(page=data.user$page)
data.tran=as(tmp_data,"transactions")
transactionInfo(data.tran)$sequenceID=data.user$seqid
transactionInfo(data.tran)$eventID=data.user$eventid
#(2)cspade 
result = cspade(data.tran,parameter=list(support=0,maxlen=2),control=list(verbose=TRUE))
result = sort(result,by="support")
page.2=paste(".*page=",user.page,"[^\\}]*\\}>",sep="")
result.2=result[grep(page.2,as(result,"data.frame")$sequence)]
#take a look
inspect(result.2) #the first one it page 1034 itself, so it can be ingored. 


########Analysis of the key pages########
###Key page filtering
##(1)
result.data.frame = as(result.2[-1],"data.frame")#remove the first row
##(2)calculate the support percentage
persent=result.data.frame$support/sum(result.data.frame$support)
##(3)calculate the accumulated percentage
sum.persent=cumsum(persent)
result.data.frame=cbind(result.data.frame,persent,sum.persent)
##(4)remove the data whose sum.persen is less than 70%
max.persent=0.7
result.data.frame=subset(result.data.frame,sum.persent<=max.persent)

###key page 
##pick up the numbers only
page = 0;i=1;
for(i.seq in result.data.frame$sequence){
  real_seq1=regexpr("<\\{page=",i.seq)+7 #find out the index position of the number, so +7
  real_seq2=regexpr("\\}",i.seq)[1]-1 #same reason as the code above
  page[i]=substr(i.seq,real_seq1,real_seq2) 
  i=i+1
}

###Confidence of key pages
#(1)pick up the 
i=1;uv=0
for(i.page in page){
  uv[i]=length(unique(data$seqid[which(data$page==i.page)]))
  i=i+1
}
#(2)calculate the confidence
conf=result.data.frame$support*result@info$nsequences/uv
#(3)conbind the dat
result.data.frame=cbind(result.data.frame,conf=conf,page=page)

###Plot 
par(family='STXihei')##Show chinese in plot
barplot(as.matrix(result.data.frame$persent,nrow=1),ylim=c(0,1),beside=TRUE,xlab="页面名称",
        main="引导用户进入关键页面1034的重点页面分析")
lines(0.5+c(1:nrow(result.data.frame)),result.data.frame$conf,type="b",col="red")
text(0.5+c(1:nrow(result.data.frame)),result.data.frame$conf,
     labels=paste(round(result.data.frame$conf*100,2),"%",sep=""))
axis(1,at=0.5+c(1:nrow(result.data.frame)),labels=result.data.frame$page,tick=FALSE)




















