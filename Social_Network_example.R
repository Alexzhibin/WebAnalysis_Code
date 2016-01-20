par(family='STXihei')##Show chinese in plot
library(igraph)
##Part 1
#(1)DATA
from = c("a","a","e","b","b","c","d","d","d","f")
to = c("c","e","c","e","c","d","g","g","f","d")
data = data.frame(from=from,to=to)

#(2)get the unique data and use union to make them together
labels = union(unique(data[,1]),unique(data[,2]));labels

#(3)make vector ids and name it with labels 
ids=1:length(labels)
names(ids)=labels
ids

#(4)from the value respone to the ids, and combine them into matrix
from = as.character(data[,1])
to = as.character(data[,2])
edges = matrix(c(ids[from],ids[to]),nc=2)

##Part 2
#(1)make empty network g and set directed=F, so there will be no vector plot
g = graph.empty(directed=F)

#(2)Add length(labels)/7 points to g 
g = add.vertices(g,length(labels))
V(g)$label = labels #name the g with labels

#(3)add edges informations to g network
g = add.edges(g,t(edges))

#(4)count the multiple lines and make them equal to weight
E(g)$weight = count.multiple(g)
E(g)$weight
#combine the lines and remove multiple
g=simplify(g,remove.multiple=TRUE,remove.loops=TRUE,edge.attr.comb='mean')
E(g)$weight

#(5)make function to build social network(combine the codes above)
#when dir = T, means make vector plot; dir=F, means make non-vector plot;
#wehn rem.multi=T, delete the multiple weights; when rem.multi=F,do not delete the multiple weights
init.igraph = function(data,dir=F,rem.multi=T){
  labels=union(unique(data[,1]),unique(data[,2]))
  ids = 1:length(labels);names(ids)=labels
  from = as.character(data[,1]);to=as.character(data[,2])
  edges=matrix(c(ids[from],ids[to]),nc=2)
  g= graph.empty(directed=dir)
  g=add.vertices(g,length(labels))
  V(g)$label=labels
  g=add.edges(g,t(edges))
  if(rem.multi){
    E(g)$weight=count.multiple(g)
    g=simplify(g,remove.multiple=TRUE,remove.loops=TRUE,edge.attr.comb='mean')
  }
  g
}

##Part 3
par(mfcol=c(1,2))
g.undir=init.igraph(data)
plot(g.undir,edge.width=E(g.undir)$weight,main="无向量g.undir",
     edge.label=E(g.undir)$weight)
g.dir=init.igraph(data,dir=T)
plot(g.dir,edge.width=E(g.dir)$weight,main="有向量g.undir",edge.label=E(g.dir)$weight)


#######Check the attribute of i.graph#######
##(1)Vertexs and dots
V(g.undir) #return all the relation of dots in g
length(V(g.undir)) #return the number of relations
V(g.undir)$label #show the labels of all relations

##(2)
member = spinglass.community(g.undir) #group the relations, it can find how many groups here
V(g.undir)$membership = member$membership; V(g.undir)$membership #make the g have membership

mem.col = rainbow(length(unique(V(g.undir)$membership)),alpha=0.3)#take some(2 groups) color,alpha = transperancy
V(g.undir)$color = mem.col[V(g.undir)$membership];V(g.undir)$color #assign the color to each relation

plot(g.undir,edge.width=E(g.undir)$weight,vertex.color=V(g.undir)$color)


##(3)
#a.Pick up some points/dots
V(g.undir)[1:2]
V(g.undir)$label[1:2]

#b.Pich up some groups
V(g.undir)[which(V(g.undir)$membership==1)]
V(g.undir)$label[which(V(g.undir)$membership==1)]
#or
V(g.undir)[membership==1]
V(g.undir)[membership==1]$label

#c.delete some unless point/dot, for example, some indepent dot
g.undir = g.undir-V(g.undir)[degree(g.undir)==0] #delet the indepent point
g.undir = g.undir-V(g.undir)[membership==1] #delet the first group

#d. find the vector close to one dot, for example "d"
V(g.undir)[neighbors(g.undir,v=which(V(g.undir)$label=="d"))]
V(g.undir)$label[neighbors(g.undir,v=which(V(g.undir)$label=="d"))]
#3 different models
V(g.dir)$label[neighbors(g.dir,v=which(V(g.dir)$label=="d"),mode="out")]#start from d point:edges(d,f) and edges(d,g)
V(g.dir)$label[neighbors(g.dir,v=which(V(g.dir)$label=="d"),mode="in")]#end in d point:edges(c,d) and edges(f,d)
V(g.dir)$label[neighbors(g.dir,v=which(V(g.dir)$label=="d"),mode="total")] #start or end in d point

#(4)degree of dot
##in the non-vector plot, the number of neighbors equal to degrees of the dot
length(neighbors(g.undir,v=which(V(g.undir)$label=="d")))
#or use the degree function in igraph
degree(g.undir,v=which(V(g.undir)$label=="d"))
degree(g.undir)[which(V(g.undir)$label=="d")]

degree(g.undir)#return degress of all dots

##in the vector plot,
#set parameter v
degree(g.dir,v=which(V(g.dir)$label=="d"),mode="out")
degree(g.dir,v=which(V(g.dir)$label=="d"),mode="in")
degree(g.dir,v=which(V(g.dir)$label=="d"),mode="total")

#向量方式
degree(g.dir,mode="out")[which(V(g.dir)$label=="d")]
degree(g.dir,mode="in")[which(V(g.dir)$label=="d")]
degree(g.dir,mode="total")[which(V(g.dir)$label=="d")]

#(5)the parameters data of combinition of lines
E(g)$weight = count.multiple(g)

#(6)pick up and deletes lines
E(g.undir)[(weight>1)] #read the lines whose weight >1
g.undir = g.undir-E(g.undir)[(weight>1)] #delete the lines whose weight>1

#(7)Get the shortest distant paths
#make the shortest line between c and g to color red and width =3, also, the dots on the path are green
pa = get.shortest.paths(g.undir,from=which(V(g.undir)$label=="c"),
                        to=which(V(g.undir)$label=="g"))[[1]]
pa = pa[[1]]
E(g.undir)$color="black" 
E(g.undir,path=pa)$color = 'red'
E(g.undir,path=pa)$width=3
V(g.undir)[pa]$color = "green"
plot(g.undir)

#pick up the dots on the path between c and g
pa
E(g.undir,path=pa) #4-5,5-7, so 5 is the point
#so search 5 
V(g.undir)$label[5] #"d"

#(8)filtering in social network
#find out the f,d,g,c in the non-vector plot
tmp.v = c(which(V(g.undir)$label==c("c")),which(V(g.undir)$label==c("d")),
          which(V(g.undir)$label==c("g")),which(V(g.undir)$label==c("f")))
g.undir.1=induced.subgraph(g.undir,tmp.v)
#or 
g.undir.1=induced.subgraph(g.undir,V(g.undir)[tmp.v])

#find out the shortes distance using subgraph.edges
pa = get.shortest.paths(g.undir,from=which(V(g.undir)$label=="c"),
                        to=which(V(g.undir)$label=="g"))[[1]]
pa = pa[[1]]
g.undir.2=subgraph.edges(g.undir,E(g.undir,path=pa))
#find out the line whose weight is greater than 1
g.undir.3 = subgraph.edges(g.undir,which(E(g.undir)$weight>1))
par(mfcol=c(2,2))
plot(g.undir,edge.width=E(g.undir)$weight,main="g.undir的关系网络")
plot(g.undir.1,edge.width=E(g.undir.1)$weight,main="g.undir中筛选出点f,d,g,c")
plot(g.undir.2,edge.width=E(g.undir.2)$weight,main="g.undir中筛选点c和g之间的连线")
plot(g.undir.3,edge.width=E(g.undir.3)$weight,main="g.undir中筛选线权重大于1的连线")



















