setwd("C:/Users/Jane Sumner/Dropbox/Gendered Research Agendas")

library(stringr)
library(stm)
library(SnowballC)

# Read in data
diss <- read.csv("dissertation-data-clean.csv",stringsAsFactors=F)

# Not exactly Figure A1, because the replication data is subset to only the data we used in the analysis,
# but it conveys similar information.
hist(diss$year,main="Dissertations Filed with Proquest\nby Year",col="gray",
     breaks=seq(1999.5,2013.5,1),xlab="year")

# Dichotomous coding of author gender, both loose (Pr(woman)>.5 => "woman") and strict (Pr(woman)>=.7 => "woman",
# Pr(woman)<=.3 => "man").
table(diss$woman.loose,useNA="always")
round(table(diss$woman.loose,useNA="always")/sum(table(diss$woman.loose,
      useNA="always")),3)
table(diss$woman.strict,useNA="always")
round(table(diss$woman.strict,useNA="always")/sum(table(diss$woman.strict,
                                                       useNA="always")),3)


# Figure A2 in Supplementary Online Appendices 
barplot(t(table(diss$year,diss$woman.strict)),legend.text=c("Man","Woman"),
        main="Dissertations Filed With Proquest Per Year\nand Gender",ylim=c(0,250))


# Setting up the STM
tx <- textProcessor(diss$Abstract,meta=diss)
out <- prepDocuments(tx$documents, tx$vocab, tx$meta,lower.thresh=15)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta


# # This part is how we determined the number of topics. I suggest not running it unless you're quite curious,
# # as it takes forever. It'll also yield a slightly different number every time, just by the nature of simulation.
# K.store <- NULL
# for(i in 1:100){
# K <- 0 #0
# fit <- stm(out$documents, out$vocab, K =K,
#                       prevalence =~ woman.strict+as.factor(School)+s(year), max.em.its = 5,
#                       data = meta, init.type = "Spectral")
# K.store <- c(K.store,nrow(labelTopics(fit)$frex))
# 
# }
# par(mar=c(4.1,4.1,4.1,1.1),mgp=c(2.1,.5,.5))
# hist(K.store,main="Discovered Topics",col="gray",xlab="number of topics",breaks=seq(min(K.store)-.5,max(K.store)+.5,1))


# Step 1: Estimate topics and prevalence from abstracts as a function of binary gender, controlling for school and year
K <- 61
fit <- stm(out$documents, out$vocab, K =K,
           prevalence =~ woman.strict+as.factor(School)+s(year), max.em.its = 100,
           data = meta, init.type = "Spectral")

# These are the labels we assigned to the topics on the basis of the frex (Frequent-Exclusive) words from the model.
# The topics themselves are just our shorthand, and people can interpret the topics any way they'd like (within reason).
labeled.topics <- read.csv("frex-8-24.csv",stringsAsFactors=F)
labeled.topics <- labeled.topics$label

# # If you rerun the model, check to make sure the order still lines up -- it should, but if it doesn't, need to reorder
# summary(fit)$frex==labeled.topics[,2:8]

frex <- apply(summary(fit)$frex,1,paste0,collapse=",")


# Runs a series of linear regressions.
prep <- estimateEffect(1:length(subfields) ~ woman.strict+as.factor(School)+s(year), fit,
                       meta = out$meta, uncertainty = "Global")

# Extracts the regression tables.
tabs <- summary(prep)$tables

# Extracts the coefficient for "woman" from each regression table.
sig <- data.frame()
for(i in 1:length(summary(prep)$tables)){
    sig <- rbind(sig,data.frame(topic=frex[i],shorttopic=labeled.topics[i],t(tabs[[i]][2,])))

    }

names(sig) <- c("topic","shorttopic","coefficient","stderr","t","p")
sig <- sig[order(sig$coefficient),]


# Indicates whether the coefficient is positive and significant or negative and significant
# (A relic of when we were basing the color on the significance for ease of viewing.)
sig$positive <- ifelse(sig$coefficient>0 & sig$p<=.05,1,0)
sig$negative <- ifelse(sig$coefficient<0 & sig$p<=.05,1,0)

# Plot
par(mar=c(3,16,2,1))
plot(sig$coefficient,c(1:length(sig$coefficient)),yaxt="n",pch=16,xlab="more likely among men                           more likely among women",ylab="",
     main="Gendered Topic Prevalences",xlim=c(-.02,.02),bty="n",xaxt="n",col=col,type="n")
segments(y0=c(1:K),x0=-.03,x1=.02,lty=3,col="gray")
points(sig$coefficient,c(1:length(sig$coefficient)),pch=16)
axis(1,at=c(-0.015,0,0.015),c("more prevalent among men","equal","more prevalent among women"))
segments(x0=sig$coefficient-1.96*sig$stderr,x1=sig$coefficient+1.96*sig$stderr,
         y0=c(1:length(sig$coefficient)))
segments(y0=0,y1=length(sig$coefficient),x0=0,lty=2)
axis(2,at=c(1:length(sig$coefficient)),sig$shorttopic,las=2,cex.axis=.8)

# end of file