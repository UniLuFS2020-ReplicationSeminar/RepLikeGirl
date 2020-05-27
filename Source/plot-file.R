# Replication file for Figure 1 in Key and Sumner (2019)

setwd("D:/Dropbox/Gendered Research Agendas/")

sig <- read.csv("output-for-figure.csv")

windows(width=12,height=12) # I think this only works on PCs.
par(mar=c(3,16,2,1))
plot(sig$coefficient,c(1:length(sig$coefficient)),yaxt="n",pch=16,xlab="more likely among men                           more likely among women",ylab="",
     main="Gendered Topic Prevalences",xlim=c(-.02,.02),bty="n",xaxt="n",col=col,type="n")
segments(y0=c(1:61),x0=-.03,x1=.02,lty=3,col="gray")
points(sig$coefficient,c(1:length(sig$coefficient)),pch=16)
axis(1,at=c(-0.015,0,0.015),c("more prevalent among men","equal","more prevalent among women"))
segments(x0=sig$coefficient-1.96*sig$stderr,x1=sig$coefficient+1.96*sig$stderr,
         y0=c(1:length(sig$coefficient)))
segments(y0=0,y1=length(sig$coefficient),x0=0,lty=2)
axis(2,at=c(1:length(sig$coefficient)),sig$shorttopic,las=2,cex.axis=.8)

# end of file