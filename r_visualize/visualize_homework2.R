setwd("C:/Users/YHY/Desktop/visualize_homework2")
rawdata = read.table("dailyprice.txt",
                     sep = "\t",
                     header = T,
                     stringsAsFactors = T)
mydata = subset(rawdata,datetime=="2003-1-2",
                select=c("datetime","trade_code","open",
                         "close","volume","mkt_cap","turn"))
mydata[,c(5,6)] = log(mydata[,c(5,6)])
mydata = na.omit(mydata)
mydata = mydata[mydata$mkt_cap!="Inf" & mydata$mkt_cap!="-Inf"&
                  mydata$volume!="-Inf"&mydata$volume!="Inf",]

#histrogram for total mkt_share
library(ggplot2)
binsize = diff(range(mydata$mkt_cap))/40
ggplot(mydata,aes(x=mkt_cap))+
  geom_histogram(aes(y=..density..),
                 colour="white",
                 binwidth = binsize,
                 fill="dodgerblue4",
                 alpha=0.8)+
  geom_line(stat="density",color="grey30",alpha=0.9)+
  labs(title="2003-1-2日沪深上市公司总市值分布的直方图",
       x="log(总市值)",y="频数")


#添加新的列为分类变量com_scale-根据总市值的不同，
#----------------------------------------------
hist(mydata$mkt_cap)
for(i in 1:nrow(mydata)){
  if(mydata$mkt_cap[i]>22){
    mydata$com_scale[i]=3
  }
  else if(mydata$mkt_cap[i]<=21){
    mydata$com_scale[i]=1
  }
  else{
    mydata$com_scale[i]=2
  }
}
#添加新的分类变量price_level，根据收盘价的不同
for(i in 1:nrow(mydata)){
  if(mydata$close[i]>11){
    mydata$price_level[i]="high"
  }
  else if(mydata$close[i]<=9){
    mydata$price_level[i]="low"
  }
  else{
    mydata$price_level[i]="middle"
  }
}
#-------------------------------------------------
##绘制不同规模公司，成交量分布的箱线图--包含异常点
#----------------------------------------------
library(RColorBrewer)
p = ggplot(mydata,aes(x=factor(com_scale),y=volume))
p+geom_boxplot(aes(fill=factor(com_scale)))+
  scale_fill_brewer(palette="PuRd")+
  labs(x="公司市值规模",y="成交量")
##隐去异常点
ggplot(mydata,aes(x=factor(com_scale),y=volume))+
  geom_boxplot(aes(fill=factor(com_scale)),outlier.colour = NA)+
  scale_fill_brewer(palette="PuRd")+
  labs(x="公司市值规模",y="成交量")

#小提琴图+箱线图
ggplot(mydata,aes(x=factor(com_scale),y=volume))+
  geom_violin(scale="count",trim=FALSE,adjust=2)+
  geom_boxplot(aes(fill=factor(com_scale)),width=.1,
               fill="forestgreen",outlier.colour = NA,alpha=0.1)+
  labs(x="公司市值规模",y="")

#
#气泡图
#---------------------------------------
#气泡图
ggplot(mydata[1:20,],aes(x=turn,y=volume,size=mkt_cap))+
  geom_point(color="grey30")+
  labs(x="换手率",y="成交量")

#---------------------------------------
#相关系数图
#---------------------------------------
library(corrplot)
mydata_corplot=na.omit(rawdata[1:10000,-c(1,2,3,19,20)])
corrplot(cor(mydata_corplot),type="lower",
         method="square")

#------------------------------------------------
#散点图矩阵
#------------------------------------------------
#给出了成交量，流动市值和换手率五个因子之间的散点图矩阵
#并且添加了线性和平滑拟合曲线
#同时，在主对角线上给出了核密度曲线和轴须图
library(car)
scatterplotMatrix(~volume+mkt_cap+turn,
                  data=mydata,
                  pch=20,
                  spread=F,
                  smoother.args=list(lty=2),
                  upper.panel=NULL)


#------------------------------------------------
#密度图
#------------------------------------------------
#给出了成交量和流动市值的密度图
library(ggplot2)
ggplot(mydata,aes(x=volume,y=mkt_cap))+
  stat_density2d(aes(fill=..density..),geom="tile",contour=F)+
  labs(x="成交量",y="总市值")

#------------------------------------------------
#热图
#------------------------------------------------
library(ggplot2)
library(RColorBrewer)
ggplot(mydata,aes(x=com_scale,y=price_level,fill=turn))+
  geom_tile()+
  scale_fill_gradient(low="tomato3",high="grey10")


#------------------------------------------------
#平行坐标图
#------------------------------------------------
library(lattice)
parallelplot(~mydata[,-c(1,2,8,9)],mydata,group=com_scale,
             horizontal.axis = FALSE)

#------------------
#雷达图
#------------------
#前5个观测的雷达图
stars(mydata[c(1:5),-c(1,2,8)],full=T,scale=TRUE)

#其中四只股票的雷达图
stars(mydata[c(1:3),-c(1,2,8,9)],locations = c(0,0),col.lines=2:4,radius =F,scale = T,
      key.loc=c(0,0),lwd=2)

#------------------------------------------------
#交互图
#------------------------------------------------
library(plotly)
plot_ly(mydata,x=~turn,y=~volume,type="scatter",color="cyan3",alpha=0.4)
#------------------------------------------------
#指数分布概率密度函数图&累计分布函数图
#------------------------------------------------
sed.seed(1)
x = seq(0,5,length.out = 100)
plot(x,dexp(x,rate=2),xlim=c(0,5),type="l",
     main="指数分布密度函数图",
     ylab="y")

plot(x,pexp(x,rate=2),xlim=c(0,5),type="l",
     main="指数分布函数图",
     ylab="y")

#------------------------------------------------
#F分布概率密度函数图&累计分布函数图
#------------------------------------------------
library(ggplot2)
sed.seed(1)
x = seq(0,5,length.out = 100)
y = df(x,10,10)
y2 = pf(x,10,10)
fdis = data.frame(x,y,y2)
ggplot(fdis,aes(x=x,y=y))+
  geom_line(lwd=1)+
  labs(title="F分布概率密度图")
ggplot(fdis,aes(x=x,y=y2))+
  geom_line(lwd=1)+
  labs(title="F分布函数图")

