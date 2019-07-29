dt<- read.csv("Wolbachia sc division.csv”)
dtf=data.frame(div=c(dt[,4],dt[,5]),
  sample =c(dt[,6],dt[,7]),
  repl =c(rep(1,3),rep(2,3),rep(1,3),rep(2,3)),
  infec =c(rep(1,6),rep(0,6)),
  indvidual=dt[,1])

#remove the 1st row
# dtf=dtf[-1,]

ggplot(dtf)+
geom_point()+
aes(x=infec,y=div/sample,group=indvidual)+
geom_line(alpha=0.2)+
geom_smooth(method="lm",aes(group=factor(repl),color=factor(repl)),se=F)

# logistic model
modelfit<-glm(cbind(div,sample)~repl+infec+indvidual,family=binomial,data=dtf)
summary(modelfit)

