#read data
DEG <- read.csv("SDEG.csv")
head(DEG)

p <- ggplot(DEG, aes(x=HPI, y=DEG, fill=Regulation)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(x="\n Time after inoculation (h)", y = "Differentially expressed genes \n")+
  scale_y_continuous(limits=c(0,10000))+
  theme_minimal()  
g <- p + scale_fill_grey() 
g + theme(text = element_text(size = 16))

