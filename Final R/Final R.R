setwd('C:\\Users\\Jimmy\\Desktop\\Evolution\\Tasks\\Final R')
x<-rnorm(100,mean=0,sd=2)
head(x)
y<-x*5+2+runif(100,min=0,max = 0.1)
head(y)
linearmodel<-lm(y~x)
plot(linearmodel)
slope<-vector("numeric",100)
intercept<-vector("numeric",100)
z<-vector("numeric",100)
for(i in 100){
  x<-rnorm(100,mean=0,sd=2)
  z[i]<-runif(1,min=0.5,max=2)
  y<-x*5+2+runif(100,min=0,max=0.1)
  linearmodel<-lm(y~x)
  intercept<-coef(linearmodel)[1]*z[i]+2
  slope<-coef(linearmodel)[2]*z[i]
  }
plot(z,slope,xlab="Z Estimated Slope")
abline(lm(slope~z),col="darkblue")
n<-10000
prize<-sample(c("1","2","3"),size=n,replace=TRUE)
opened_door<-ifelse(prize=="1",sample(c("2","3"),size=n,replace=TRUE),ifelse(prize=="2","3","2"))
closed_door<-ifelse(opened_door=="2","3","2")
same_door<-sum(prize=="1")/n
diff_door<-sum(prize==closed_door)/n
win_frequency<-c(same_door,diff_door)
barplot(win_frequency,names.arg=c("Same Door","Different Door"),ylab="Frequency of Wins",ylim=c(0,0.8),main="Chance of Winning Game Show",col="yellow")
