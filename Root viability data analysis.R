setwd("D:/OneDrive - Western Sydney University/Donovin PhD/Thesis/Experiments/Chapter 2/Vital staining")
#read data
vs <- read.csv("vs_Rdata.csv")
head(vs)

library(ggplot2)
library(plyr)
install.packages("extrafont")
library(extrafont)
font_import()
yloadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                 

str(vs)
vs$trt<-as.factor(vs$trt)
vs$trt <- ordered(vs$trt, levels = c("Control", "12 hpi", "24 hpi", "72 hpi"))

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

vs2 <- data_summary(vs, varname= "per",
                    groupnames= "trt")
str(vs2)

head(vs2)

p<- ggplot(vs2, aes(x=trt, y=per))
p +  
  geom_errorbar(aes(ymin=per-sd, ymax=per+sd), width=.2,position=position_dodge(.9))+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  labs(x="\n TIme after inoculation (h)", y = "Cell viability (%) \n")+
  scale_y_continuous(limits=c(0, 100), expand = c(0,0))+
  geom_col(fill = "white", colour = "black")+
  theme_classic()+
  theme(text=element_text(size=18))
  

#stats
library(visreg)
library(car)
library(emmeans)
library(multcompView)
library(multcomp)
install.packages("ggeffects")
library(ggeffects)
#test for normality of data (If p-value >0.05 means data not sig. diff. from normal distr. Therefore assume normality)

shapiro.test(vs$per)
head(vs)


str(vs)
EL1$hpi<-as.factor(EL1$hpi)
EL1$trt<-as.factor(EL1$trt)

levels(EL1$HPI)
levels(EL1$TRT)

#nonparametric test when data not normal
kruskal.test(trt ~ per, data = vs)
kruskal.test(HPI ~ REL, data = EL1)

#create model:
via <- lm(per ~ trt, data=vs)

#look @ marginal effects

mydf <- ggpredict(via, terms = c("trt"))
mydf

plot(mydf)

summary(via)


#Anova:
anova(via)

#residualplot
residualPlot(via)


#qqplot
qqPlot(via)


#Statistical difference in means between treatments
via.emm <- emmeans(via, ~trt, type="response")
via.emm <-pairs(emmeans(via, ~trt, type="response"))
via.emm
cld(via.emm)

