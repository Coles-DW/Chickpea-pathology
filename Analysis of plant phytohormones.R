# load packages
library(dplyr)
library(plyr)
library(plotrix)
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(cowplot)
# devtools::install_github("YuLab-SMU/treeio")
# devtools::install_github("YuLab-SMU/ggtree")
library(tidytree)
library(ggtree)\

# read in dataframe
phyto <- read.csv(file = 'Phythormones_v5.csv')
phytohor <- read.csv(file = 'ratio SAtoJA.csv')
phy <- read.csv(file = 'Ratio SA to JA stats.csv')
head(phyto)
head(phytohor)
head(phy)
Phyt <- pivot_longer(phyto, cols = c("SA", "JA"), names_to = "Phytohormone", values_to = "value")
Phyt$Treatment <- ordered(Phyt$Treatment, levels = c("control", "12hpi", "24hpi", "72hpi"))
head(phyt)

# Determine mean, standard deviation and standard error
#Phyt <- phytohor %>% group_by(Treatment) %>% summarise_at(vars(SA, JA, ABA), funs(mean, sd, std.error))
#head(Phyt)

# plot graph
a <- ggplot(Phyt) +
  geom_boxplot(lwd=1, fatten=1, aes(x=Treatment, y=value, fill = Phytohormone)) +
  xlab(label = "\n ") +
  ylab(label = "Concentration (ppb/mgFW) \n") +
  scale_y_continuous(limits=c(0,0.03)) +
  scale_fill_grey() + theme_classic () +
  theme(legend.position = "top", text = element_text(size = 18))
a

b<- ggplot(phytohor, aes(x=Treatment, y=ratio, group=1)) + 
  geom_line(lwd=1)+
  geom_point()+
  xlab(label = "\nTreatment") +
  ylab(label = "Ratio (SA/JA) \n") +
  theme_classic () +
  theme(text = element_text(size = 18)) +
  geom_errorbar(aes(ymin=ratio-sd, ymax=ratio+sd), width=.2,
                position=position_dodge(0.05))

b

pp <- list(a, b)
plot_grid(plotlist=pp, ncol=1, align='v')


#stats
library(visreg)
library(car)
library(emmeans)
library(multcompView)
library(multcomp)
#test for normality of data (If p-value >0.05 means data not sig. diff. from normal distr. Therefore assume normality)

shapiro.test(phyto$SA)
shapiro.test(phyto$JA)
shapiro.test(phytohor$ABA)
shapiro.test(phy$ratio)

#create model:
sa <- lm(SA ~ Treatment, data=phytohor)
ja <- lm(log(JA+1) ~ Treatment, data=phytohor)
aba <- lm(ABA ~ Treatment, data=phytohor)
saja <- lm(log(ratio) ~ Treatment, data=phy)

summary(sa)
summary(ja)
summary(aba)
summary(saja)

#Anova:
anova(sa)
anova(ja)
anova(aba)
anova(saja)

#residualplot
residualPlot(sa)
residualPlot(ja)
residualPlot(aba)
residualPlot(saja)

#qqplot
qqPlot(sa)
qqPlot(ja)
qqPlot(aba)
qqPlot(saja)

#Statistical difference in means between treatments
sa.emm <- emmeans(sa, ~Treatment)
cld(sa.emm)
ja.emm <- emmeans(ja, ~Treatment)
cld(ja.emm)
aba.emm <- emmeans(aba, ~Treatment)
cld(aba.emm)
saja.emm <- emmeans(saja, ~Treatment)
cld(saja.emm)
