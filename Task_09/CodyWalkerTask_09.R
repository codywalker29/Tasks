setwd('C:\\Users\\Jimmy\\Desktop\\Evolution\\Tasks\\Task_09')
library(diversitree)
transition_0to1<-0.1
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-50
Pars<-c(speciation_0,speciation_1,extinction_0,extinction_1,transition_0to1,transition_1to0)
simTree<-tree.bisse(Pars,max.taxa=maxN,max.t=maxT)
str(simTree)
?tree.bisse
stateTable<-table(simTree$tip.state)
stateTable/sum(stateTable)
speciation_1range<-seq(0.1,1.0,0.1)
sim_results<-list()
for(i in seq_along(speciation_1range)){
  Pars[2]<-speciation_1range[i]
  simTree<-tree.bisse(Pars,max.taxa=maxN,max.t=maxT)
  sim_results[[i]]<-simTree
}
state_tables<-lapply(sim_results,function(tree) table(tree$tip.state)/sum(tree$tip.state))
state_tables
df<-data.frame(speciation_1=speciation_1range,tip_state_0=rep(0,length(speciation_1range)),tip_state_1=rep(0,length(speciation_1range)))
for(i in seq_along(sim_results)){
  state_table<-state_tables[[i]]
  df$tip_state_0[i]<-state_table[1]
  df$tip_state_1[i]<-state_table[2]
}
nona_df<-na.omit(df)
nona_df
library(ggplot2)
ggplot(nona_df,aes(x=speciation_1))+
  geom_line(aes(y=tip_state_0,color="Tip State 0"))+
  geom_line(aes(y=tip_state_1,color="Tip State 1"))+
  scale_color_manual(values=c("Tip State 0"="blue","Tip State 1"="red"))+
  xlab("Speciation Rate for State 1")+
  ylab("Tip State Frequency")+
  ggtitle("Tip State Frequency vs. Speciation Rate")+
  theme_bw()+
  labs(color="Tips States")
nona_df
#This graph shows that as the frequency of the tips decrease, so does the speciation rate. 