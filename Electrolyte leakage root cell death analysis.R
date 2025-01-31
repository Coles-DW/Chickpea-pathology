#read in data
EL <- read.csv("ELdata_Figure.csv")
head(EL)

#load packages
library(tidyverse)

# Standard deviation of the mean
EL$HPI <- factor(EL$HPI,levels = c("12hpi", "24hpi", "36hpi", "48hpi","60hpi", "72hpi", "120hpi"))
P <- ggplot(EL, aes(x=HPI, y=m, group=Treatment, color=Treatment)) + 
  geom_line(aes(linetype=Treatment)) + geom_point()+
  scale_y_continuous(limits=c(0,1.0)) +
  labs(x="\n Time after inoculation (h)", y = "Electrolyte leakage (%) \n")+
  theme_classic()  
g <- P + theme_classic() + scale_color_manual(values=c('#000000','#000000'))
g + theme(text = element_text(size = 16))
