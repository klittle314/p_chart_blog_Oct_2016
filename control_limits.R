#Kevin Little, Ph.D. 23 October 2016
#calculations used in Blog post on p charts
library(openxlsx)
df2 <- read.xlsx("datafile.xlsx",cols=c(1:15), rows=c(1:16), sheet=2)
df2$date <- as.Date(df2$date, origin="1899-12-30")

df9 <- df2[,c(1,4,5)]
names(df9)[2] <- "denom"
names(df9)[3] <- "num"
df9$per.cent <- 100*df9$num/df9$denom

#Calculate the p-chart control limits
p_hat <- 100*sum(df9$num)/sum(df9$denom)
df9$sigma_hat <- sqrt(p_hat*(100-p_hat)/df9$denom)
df9$UCL <- p_hat+3*df9$sigma_hat
df9$LCL <- p_hat-3*df9$sigma_hat

#Calculate the individuals chart control limits
#note we use the average per cent weighted by the counts of responses;
#if we did not have the counts, we would take the simple average of the per cents
absdiff <- abs(diff(df9$per.cent))
df9$UCLI <- p_hat+2.66*mean(absdiff)
df9$LCLI <- p_hat-2.66*mean(absdiff)

#plot p-chart
p11 <- ggplot(data=df9, aes(x=date,y=per.cent))+
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim(30,90)+
  ylab("Per cent")

p12 <- p11+geom_line(aes(x=date,y=UCL),linetype="dashed",color="red")+
  geom_line(aes(x=date,y=LCL),linetype="dashed",color="red")+
  ggtitle("Per cent patients responding 'Always': \n 'Have you felt listened to by our staff?'")+
  ylab("per cent")+
  xlab("Month")

CL_pchart <- p_hat

p13 <- p12 + geom_hline(yintercept=CL_pchart)
p14 <- p13 + annotate("text",x=as.Date("2015-01-15"),y=72,label="UCL: p-chart")+
  annotate("text",x=as.Date("2015-01-15"),y=42,label="LCL: p-chart")+
  annotate("text",x=as.Date("2015-02-01"),y=60,label=paste0("avg %= ",round(p_hat,1)))
p14

#plot individuals chart
p15 <- p14 + geom_line(aes(x=date,y=UCLI),linetype="longdash",color="blue")+
  geom_line(aes(x=date,y=LCLI),linetype="longdash",color="blue")+
  annotate("text",x=as.Date("2016-01-25"),y=82,label="UCL: Ind. chart")+
  annotate("text",x=as.Date("2016-01-25"),y=35,label="LCL: Ind.chart")
  
p15
