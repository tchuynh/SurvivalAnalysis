

aMat<-matrix(c(1,2,3,4), ncol=2)

groupOne<-matrix(c(
  
  0.0,1.8,2.2,2.5,2.6,3.0,3.5,3.8,5.3,5.4,5.7,6.6,8.2,8.7,9.2,9.8,10.0,10.2,10.7,11.0,11.1,11.7,
  25, 25, 24, 23, 22, 21,
  20, 19, 18, 17, 16, 15,
  14, 13, 12, 10, 9, 8,
  7, 6, 5, 4,
  
                   0, 1, 1, 1, 1, 
                   1, 1, 1, 1, 1, 
                   1, 1, 1, 1, 2, 
                   1, 1, 1, 1, 1,
                   1, 1,
                   0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0,
                   0, 3,
                   1.00, .96, .92, .88, .84,
                   .8, .76, .72, .68, .64, .6, .56, .52, .48,
                   .4, .36, .32, .28, .24, .2, .16, .12), ncol=5)

groupOne<-data.frame(groupOne)
groupOne$name<-rep("group1",dim(groupOne)[1])
groupOne


groupTwo<-matrix(c(0.0, 1.4, 1.6, 1.8, 2.4,
                   2.8, 2.9, 3.1, 3.5, 3.6,
                   3.9, 4.1, 4.2, 4.7, 4.9,
                   5.2, 5.8, 5.9, 6.5, 7.8,
                   8.3, 8.4, 8.8, 9.1, 9.9, 11.4,
                   
                   25, 25, 24, 23, 22, 21, 20,
                   19, 18, 17, 16, 15, 14, 13,
                   12, 11, 10,  9,  8,  7,  6, 
                    5,  4,  3,  2,  1,
                   
                   0, 1, 1, 1, 1,
                   1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1,
                   0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0,
                   1, .96, .92, .88, .84,
                   .8, .76, .72, .68, .64,
                   .6, .56, .52, .48, .44,
                   .4, .36, .32, .28, .24,
                   .2, .16, .12, .08, .04,
                   0), ncol=5)


plot(groupTwo[,1],groupTwo[,5], type='ol', col='blue')
lines(groupOne[,1],groupOne[,5], type='ol', col='green')
grid()

groupTwo<-data.frame(groupTwo)
groupTwo$name<-rep("group2",dim(groupTwo)[1])
groupTwo


g3=rbind(group1,group2)

survdiff(Surv(time=g3[,1], event=g3[,3]) ~ name, data = g3)



#survDat<-rbind(group1[c(-1,-22, -15),c(1,3,6)],c(9.2, 1, "group1"),c(9.2, 1, "group1"),c(11.7, 0, "group1"),c(11.7, 0, "group1"),c(11.7, 0, "group1"),c(11.7, 1, "group1"), group2[-1, c(1,3,6)])


survDat<-rbind(group1[c(-1,-22, -15),c(1,3,6)],c(9.2, 1, "group1"),c(9.2, 1, "group1"),c(11.7, 1, "group1"), group2[-1, c(1,3,6)])

names(survDat)<-c("time","status", "group")

survDat$time<-as.numeric(survDat$time)

survDat$status<-as.logical(survDat$status)

survdiff(Surv(time, status) ~ group, data = survDat)

logrank_test(Surv(time,status) ~ group, data=survDat)
Surv(survDat$time, survDat$status)

matrix(rep(c(group1[22,1], group1[22,3], group1[22,6]), group1[22,4]), ncol=3, byrow=T)


survDat[survDat$group=='group2',]


# anderson
anderson <- read.delim(file="andersON.dat", header = FALSE, sep="", as.is=TRUE)

head(anderson)

names(anderson)<-c("Survt", "Relapse", "Sex", "logWBC", "Rx")

anderson$lmh<-NA

anderson$lmh[anderson$logWBC<2.3]<-"low"
anderson$lmh[anderson$logWBC>2.3 & anderson$logWBC<3]<-"medium"
anderson$lmh[anderson$logWBC>3]<-"high"

ggplot(data=anderson, aes(x=Survt, y=logWBC, colour=lmh))+geom_point()+geom_line()

low<-anderson[anderson$lmh=="low",]
low<-low[order(low$Survt),]
low$cumRel<-cumsum(low$Relapse)
low$survRate<-(dim(low)[1] - low$cumRel)/dim(low)[1]

medium<-anderson[anderson$lmh=="medium",]
medium<-medium[order(medium$Survt),]
medium$cumRel<-cumsum(medium$Relapse)
medium$survRate<-(dim(medium)[1] - medium$cumRel)/dim(medium)[1]

high<-anderson[anderson$lmh=="high",]
high<-high[order(high$Survt),]
high$cumRel<-cumsum(high$Relapse)
high$survRate<-(dim(high)[1] - high$cumRel)/dim(high)[1]

lmh<-rbind(low,medium,high)
lmh<-lmh[, c("Survt", "lmh", "survRate")]
lmh<-rbind(lmh, c(0, "low", 1), c(0, "medium", 1), c(0, "high", 1))
lmh$survRate<-as.numeric(lmh$survRate)
lmh$Survt<-as.numeric(lmh$Survt)
ggplot(data=lmh, aes(x=Survt, y=survRate, colour=lmh))+geom_point()+geom_line()

ggplot(data=lmh, aes(x=Survt, y=survRate, colour=lmh))+geom_step()
whatdis(lmh)
whatdis<-function(inputDF){
  
  for(col in 1:dim(inputDF)[2]){
    
    cat("type of: ", names(inputDF)[col], " ", typeof(inputDF[,col]), "\n")
    cat("class of: ", names(inputDF)[col], " ", typeof(inputDF[,col]), "\n")
  }
}
## example
data("glioma", package="coin")
library("survival")
layout(matrix(1:2, ncol=2))
g3<-subset(glioma, histology=="Grade3")

g4<-subset(glioma, histology=="GBM")

logrank_test(Surv(time,event) ~ group, data=g4, distribution = "exact")
survdiff(Surv(time,event) ~ group, data=g4)
