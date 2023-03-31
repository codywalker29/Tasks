setwd('C:\\Users\\Jimmy\\Desktop\\Evolution\\Tasks\\Task_08')
library(phytools)
#Questions 1-3
trees<-list()
births<-c()
Fractions<-c()
random<-c()
random2<-c()
treelist<-c()
for(i in 1:100){
  births[i]<-runif(1)
  Fractions[i]<-runif(1)
  trees[[i]]<-pbtree(n=100,b=births[i], d=births[i]*Fractions[i])
  random[[i]]<-births[i]
  random2[[i]]<-(Fractions[i])
  treelist[[i]]<-mean(trees[[i]]$edge.length)
}
#Question 4
# The net diversification increases while the log of the number of tips increases.
div_rate<-sapply(trees,function(x) length(x$tip.label))
log_tips<-log(sapply(trees,function(x)length(x$tip.label)))
library(ggplot2)
ggplot(data=data.frame(div_rate,log_tips), aes(x=log_tips,y=div_rate))+
  geom_point()+
  xlab("Log Number of Tips")+
  ylab("Net Diversification")+
  ggtitle("Net Diversification vs. Log Number of tips")
dev.off()
speciation_rate<-sapply(births,function(x)x)
avg_branchlength<-sapply(trees,function(x) mean(x$edge.length,na.rm=TRUE))
ggplot(data=data.frame(speciation_rate,avg_branchlength), aes(x=speciation_rate,y=avg_branchlength))+
  geom_point()+
  xlab("Speciation Rate")+
  ylab("Average Branch Length")+
  ggtitle("speciation Rate vs. Average Branch Length")
dev.off()
#Question 5 
#This graph shows a decrease towards 0 in branch length.  
cor(speciation_rate,avg_branchlength)
#Question 7
largest_tree<-trees[[which.max(sapply(trees,length))]]
Tree<-largest_tree
plot(Tree)
dev.off()
rates<-c()
traits<-list()
for(i in 1:100){
  rates[i]<-runif(1)
  traits[[i]]<-fastBM(tree=Tree,sig2=rates[i])
}
#Question 8
mean_traits<-sapply(traits,mean)
cor(mean_traits,rates)
#The corelation is -0.02833842 which indicates there is a weak negative correlation. 
plot(rates,mean_traits,xlab="Rates",ylab="Mean Trait")
dev.off()
#Question 9
var_traits<-sapply(traits,var)
cor(var_traits,rates)
#The correlation is 0.6987201 which indicates there is a strong positive correlation. 
plot(rates,var_traits,xlab="Rates",ylab="Variance of Traits")
dev.off()
cor(traits[[1]],traits[[2]])
traitMat<-cbind(traits[[1]],traits[[4]])
plot(traitMat)
dev.off()
#Extra Credit
phylomorphospace(Tree,traitMat,xlab="Trait 1",ylab="Trait 2")
dev.off()
