rm(list=ls())
require(tidyverse)
library("zoo")
library(gtable)
library(grid)


###############################################
## Figure 1 FS by month 
###############################################
mw_hh <- read.csv("data/mw_dataset_hh.csv")

FS.month.mean = mw_hh %>%
  dplyr::mutate(month=as.factor(FS_month)) %>%
  dplyr::mutate(logFCS = log(FCS)) %>% 
  dplyr::select(rCSI,logFCS,HDDS,month) %>%
  gather(-month, value=value,key=measure) %>%
  group_by(month,measure) %>%
  summarise(Food_security=mean(value))


fcs.not.long = FS.month.mean %>% dplyr::filter(measure!="logFCS")
fcs.long = FS.month.mean %>% dplyr::filter(measure=="logFCS")

  
 figure1 = ggplot(fcs.not.long , aes(x=month,color=measure,y=Food_security,shape = measure,group=measure)) +
  theme_bw() + geom_path(size=2.5,show.legend = FALSE)+ geom_point(size=5,show.legend =FALSE )  +
  geom_path(size=2.5,data=fcs.long,aes(y = Food_security*1.1, colour = measure),show.legend = FALSE)  +
  geom_point(data=fcs.long,aes(y = Food_security*1.1, colour = measure),size=5,show.legend = FALSE) +
  scale_y_continuous(sec.axis = sec_axis(~./1.1, name = "Food consumption scores (in log forms)" )) +
  labs(y = "HDDS and RCSI scores",x = "Month",
              colour = "Food Security Measures",
              shape= " ") +
   scale_colour_manual(values = c("#619CFF", "#F8766D","#00BA38"))  +
    theme(legend.position="bottom") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=17),
        legend.text=element_text(size=16),
        axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),
                axis.title=element_text(size=17,face="bold") ) 


ggsave("figure1.png", plot = figure1,device = "png",path = "output/graphs/",
       dpi = 1000, limitsize = TRUE)



###############################################
## Figure 1 FS by month 
###############################################






