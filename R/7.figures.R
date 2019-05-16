rm(list=ls())
require(tidyverse)
library("zoo")
library(gtable)
library(grid)


# make a map that shades the clusters by month 
# change figure s1 to add the GIEWS MARKETS 


################################################################################
#   Estimate (back of the envelope?)
# spatial temporal variation between our good price data and GIEWS prices
################################################################################



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


## Figure 1 Map created using Tableau

#########################################################################################
## Figure 2. The share of variation in out-of-sample cluster-level food security 
# predicted by our models improves with greater spatial granularity and richer data.
#########################################################################################
clust_r2matrix <- read_csv("output/results/prediction/clust/clust_r2matrix.csv")
TA_r2matrix <- read_csv("output/results/prediction/TA/TA_r2matrix.csv")
lhz_r2matrix <- read_csv("output/results/prediction/lhz/lhz_r2matrix.csv")

colnames(clust_r2matrix)[1] = "Model"
colnames(TA_r2matrix)[1] = "Model"
colnames(lhz_r2matrix)[1] = "Model"

# wide to long
require(tidyverse)
lhz_r2matrix.long = lhz_r2matrix  %>% tidyr::gather(-Model,key="Measures",value=RSquared) 
lhz_r2matrix.long[["level"]]="IPC Zone"

TA_r2matrix.long = TA_r2matrix  %>% tidyr::gather(-Model,key="Measures",value=RSquared) 
TA_r2matrix.long[["level"]]="TA"

clust_r2matrix.long = clust_r2matrix  %>% tidyr::gather(-Model,key="Measures",value=RSquared) 
clust_r2matrix.long[["level"]]="Cluster"

# combind levels 
r2.df  = bind_rows(lhz_r2matrix.long,TA_r2matrix.long,clust_r2matrix.long)

# adjust names for figure use
r2.df$Model[r2.df$Model=="model0"]<-"Class 0 (IPC value only)"
r2.df$Model[r2.df$Model=="model1"]<-"Class 1 data"
r2.df$Model[r2.df$Model=="model2"]<-"Class 1 + Class 2 data"
r2.df$Model[r2.df$Model=="model3"]<-"Class 1 + Class 2 + Class 3 data"


 # adjust factor level
ord_measure <- c("HDDS","rCSI","logFCS")
r2.df$Measures <- factor(r2.df$Measures,levels=ord_measure)

ord_level <- c("IPC Zone","TA","Cluster")
r2.df$level <- factor(r2.df$level,levels=ord_level)

ord_model <- c("Class 0 (IPC value only)","Class 1 data","Class 1 + Class 2 data","Class 1 + Class 2 + Class 3 data")
r2.df$Model <- factor(r2.df$Model,levels=ord_model)

write.csv(r2.df,"output/results/prediction/r2df.csv",row.names = FALSE)


# Start the plot
figure2<-ggplot(data = r2.df, aes(x = level, y = RSquared,colour =Measures ,shape = Model )) 
figure2<-figure2 + geom_point(size=7)
figure2<-figure2 + labs( x = "Geo-spatial Level", y = "R Squared")

figure2 <- figure2+ theme_classic()  +    scale_colour_manual(values = c("#619CFF", "#F8766D","#00BA38"))  
figure2 <- figure2 +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                            axis.title=element_text(size=17,face="bold") ) 


figure2

ggsave("figure2.png", plot = figure2,device = "png",path = "output/graphs/",
       dpi = 1000, limitsize = TRUE)


# rplot = rplot + scale_color_grey(start = 0.8, end = 0.2)

# +  theme(plot.margin = unit(c(1, 2, 1, 1), "lines")) 
#  grid.text("Class 1 data contains: past IPC values, precipitation,", x = unit(0.625, "npc"), y = unit(0.35, "npc"),gp=gpar(fontsize=15), check.overlap = TRUE,just="left") 
#  grid.text("market prices, market access measures, and soil quality", x = unit(0.625, "npc"), y = unit(0.32, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left") 
# grid.text("Class 2 data contains: share of households owing cellular", x = unit(0.625, "npc"), y = unit(0.29, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")
# grid.text("phone and share of dwellings with metal versus thatch roof", x = unit(0.625, "npc"), y = unit(0.26, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")
# grid.text("Class 3 data contains: household demographics and assets", x = unit(0.625, "npc"), y = unit(0.23, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")


 



#########################################################################################
## Figure 3. Scatter plots 
#########################################################################################

###############################################################################################
### Figure 3: logFCS  
###############################################################################################

library(grid)
clust_r2matrix <- read_csv("output/results/prediction/clust/clust_r2matrix.csv")
r2list = as.numeric(clust_r2matrix[4,2:4])

logFCS_pair <- read_csv("output/results/prediction/clust/logFCS_clust_model3_pair.csv")

cutoffa <- data.frame( x = c(-Inf, Inf), y =3.332, cutoff = factor(3.332) )
cutoffb <- data.frame( x = c(-Inf, Inf), y = 3.738, cutoff = factor(3.738)  )
cutoffc <- data.frame( y = c(-Inf, Inf), x =3.332, cutoff = factor(3.332)  )
cutoffd <- data.frame( y = c(-Inf, Inf), x = 3.738, cutoff = factor(3.738)  )

ggplot(as.data.frame(logFCS_pair), aes(actual, predict)) +
  
  theme_classic() + geom_abline(intercept = 0, slope = 1)+
  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") ) +
  scale_color_brewer(palette="Dark2") +
  labs(x = "Cluster Average logFCS (Actual)", y = "Cluster Average logFCS (Predicted)") + 
  geom_line(aes( x, y, linetype = cutoff), cutoffa,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoffb,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoffc,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoffd,show.legend = FALSE) +
  geom_ribbon(data=subset(as.data.frame(logFCS_pair), 3.332 <= actual & actual <= 3.738), 
              aes(ymin=3.332,ymax=actual), fill="grey", alpha="0.2") +   # area 3 left 
  geom_ribbon(data=subset(as.data.frame(logFCS_pair), 3.738 <= actual & actual <= 4.6), 
              aes(ymin=3.738,ymax=actual), fill="grey", alpha="0.2") +   # area 3 right 
  geom_ribbon(data=subset(as.data.frame(logFCS_pair), 3.332 <= actual & actual <= 3.738), 
              aes(ymin=actual,ymax=3.738), fill="grey", alpha="0.45") +   # area 2 left
  geom_ribbon(data=subset(as.data.frame(logFCS_pair), 3.738 <= actual & actual <= 4.6), 
              aes(ymin=actual,ymax=4.7), fill="grey", alpha="0.45") +            # area 2 right
  geom_ribbon(data=subset(as.data.frame(logFCS_pair), 3.332<= actual & actual <= 3.738), 
              aes(ymin=3.738,ymax=4.7), fill="grey", alpha="0.8") +        # area 1
  geom_ribbon(data=subset(as.data.frame(logFCS_pair), 3.738 <= actual & actual <= 4.6), 
              aes(ymin=3.332,ymax=3.738), fill="grey", alpha="0.6") +           # area 4
  geom_point(shape = 16, size = 3, show.legend = FALSE,na.rm = TRUE) +
  annotate(geom = "text", size = 7, x = 3.6, y = 3.5, label = "IIIa") +
  annotate(geom = "text", size = 7, x = 4.2, y = 3.9, label = "IIIb") +
  annotate(geom = "text", size = 7, x = 3.55, y = 4.15, label = "I") +
  annotate(geom = "text", size = 7, x = 3.55, y = 3.65, label = "IIa") +
  annotate(geom = "text", size = 7, x = 3.9, y = 4.2, label = "IIb") +
  annotate(geom = "text", size = 7, x = 4, y = 3.5, label = "IV") +
  theme(plot.margin = unit(c(1, 11, 1, 1), "lines")) 

grid.text(paste("R Squared= ",r2list[1],sep = " "), x = unit(0.84, "npc"), y = unit(0.65, "npc"),gp=gpar(fontsize=15),just="left") 

grid.text("Cutoff:", x = unit(0.84, "npc"), y = unit(0.60, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("0 <= logFCS ", x = unit(0.84, "npc"), y = unit(0.56, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("< 3.33 is poor;", x = unit(0.84, "npc"), y = unit(0.52, "npc"),gp=gpar(fontsize=15),just="left") 

grid.text("3.33 <= logFCS ", x = unit(0.84, "npc"), y = unit(0.48, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("< 3.74 is borderline;", x = unit(0.84, "npc"), y = unit(0.44, "npc"),gp=gpar(fontsize=15),just="left") 

grid.text("logFCS >= 3.74 ", x = unit(0.84, "npc"), y = unit(0.40, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("is acceptable.", x = unit(0.84, "npc"), y = unit(0.36, "npc"),gp=gpar(fontsize=15),just="left") 

# grid.text("Area I: Overpredicting; borderline  ", x = unit(0.85, "npc"), y = unit(0.35, "npc"),gp=gpar(fontsize=15),just="left") 
# grid.text("predicted as acceptable", x = unit(0.85, "npc"), y = unit(0.32,"npc"),gp=gpar(fontsize=15),just="left") 
# grid.text("Area IIa: Overpredicting within acceptable", x = unit(0.85, "npc"), y = unit(0.29, "npc"),gp=gpar(fontsize=15),just="left") 
# grid.text("Area IIb: Overpredicting within borderline", x = unit(0.85, "npc"), y = unit(0.26, "npc"),gp=gpar(fontsize=15),just="left") 
# 
# grid.text("Area IIIa: Underpredicting within acceptable", x = unit(0.85, "npc"), y = unit(0.23, "npc"),gp=gpar(fontsize=15),just="left") 
# grid.text("Area IIIb: Underpredicting within borderline", x = unit(0.85, "npc"), y = unit(0.20, "npc"),gp=gpar(fontsize=15),just="left") 
# 
# grid.text("Area IV: Underpredicting; acceptable", x = unit(0.85, "npc"), y = unit(0.17, "npc"),gp=gpar(fontsize=15),just="left") 
# grid.text("predicted as borderline", x = unit(0.85, "npc"), y = unit(0.14, "npc"),gp=gpar(fontsize=15),just="left") 
###############################################################################################
### Figure 3: HDDS 
###############################################################################################
HDDS_pair <- read_csv("output/results/prediction/clust/HDDS_clust_model3_pair.csv")

cutoffw <- data.frame( x = c(-Inf, Inf), y = 3, cutoff = factor(3) )
cutoffx <- data.frame( x = c(-Inf, Inf), y = 6, cutoff = factor(6) )
cutoffy <- data.frame( y = c(-Inf, Inf), x = 3, cutoff = factor(3) )
cutoffz <- data.frame( y = c(-Inf, Inf), x = 6, cutoff = factor(6) )

ggplot(as.data.frame(HDDS_pair), aes(actual, predict)) +
  theme_classic() + geom_abline(intercept = 0, slope = 1)+
  theme(plot.margin = unit(c(1, 10, 1, 1), "lines"))  +
  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") ) +
  scale_color_brewer(palette="Dark2")+
  labs(x = "Cluster Average HDDS (Actual)", y = "Cluster Average HDDS (Predicted)") +
  geom_line(aes( x, y, linetype = cutoff), cutoffw,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoffx,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoffy,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoffz,show.legend = FALSE)+
  geom_rect(data=as.data.frame(HDDS_pair), mapping=aes(xmin=3.01, xmax=4.1, ymin=6.01, ymax=8), fill="grey", alpha=0.03) +
  geom_rect(data=as.data.frame(HDDS_pair), mapping=aes(xmin=3.01, xmax=4.1, ymin=6.01, ymax=8), fill="grey", alpha=0.03) +
  
  geom_ribbon(data=subset(as.data.frame(HDDS_pair), 3 <= actual & actual <= 6), 
              aes(ymin=3,ymax=actual), fill = "grey" , alpha="0.2") +   # area 3 left 
  geom_ribbon(data=subset(as.data.frame(HDDS_pair), 6 <= actual & actual <= 7), 
              aes(ymin=6,ymax=actual), fill="grey", alpha="0.2") +   # area 3 right 
  geom_ribbon(data=subset(as.data.frame(HDDS_pair), 3 <= actual & actual <= 6), 
              aes(ymin=actual,ymax=6), fill="grey", alpha="0.45") +   # area 2 left
  geom_ribbon(data=subset(as.data.frame(HDDS_pair), 6 <= actual & actual <= 7), 
              aes(ymin=actual,ymax=8), fill="grey", alpha="0.45") +            # area 2 right
  geom_ribbon(data=subset(as.data.frame(HDDS_pair), 0<= actual & actual <= 6), 
              aes(ymin=6,ymax=8), fill="grey", alpha="0.8") +        # area 1
  geom_ribbon(data=subset(as.data.frame(HDDS_pair), 6 <= actual & actual <= 7), 
              aes(ymin=3,ymax=6), fill="grey", alpha="0.6") +           # area 4
  geom_point(shape = 16, size = 3, show.legend = FALSE,na.rm = TRUE) +
  
  annotate(geom = "text", size = 7, x = 4.7, y = 3.8, label = "IIIa") +
  annotate(geom = "text", size = 7, x = 6.5, y = 6.2, label = "IIIb") +
  annotate(geom = "text", size = 7, x = 4.7, y = 6.5, label = "I") +
  annotate(geom = "text", size = 7, x = 4.7, y = 5.6, label = "IIa") +
  annotate(geom = "text", size = 7, x = 6.5, y = 6.7, label = "IIb") +
  annotate(geom = "text", size = 7, x = 6.5, y = 3.8, label = "IV") 

grid.text(paste("R Squared= ",r2list[2],sep = " "), x = unit(0.83, "npc"), y = unit(0.65, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("Cutoff:", x = unit(0.83, "npc"), y = unit(0.58, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("0 <= HDDS < 3 ", x = unit(0.83, "npc"), y = unit(0.54, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("is poor diversity;", x = unit(0.83, "npc"), y = unit(0.50, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("3 <= HDDS < 6 ", x = unit(0.83, "npc"), y = unit(0.46, "npc"),gp=gpar(fontsize=15),just="left")
grid.text("is medium diversity;", x = unit(0.83, "npc"), y = unit(0.42, "npc"),gp=gpar(fontsize=15),just="left")
grid.text("HDDS >= 6", x = unit(0.83, "npc"), y = unit(0.38, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("is good diversity.", x = unit(0.83, "npc"), y = unit(0.34, "npc"),gp=gpar(fontsize=15),just="left") 


###############################################################################################
### Figure 3: rCSI 
###############################################################################################
rcsi_pair <- read_csv("output/results/prediction/clust/rCSI_clust_model3_pair.csv")

# Create cutoffs

cutoff1 <- data.frame( x = c(-Inf, Inf), y = 17, cutoff = factor(17) )
cutoff2 <- data.frame( x = c(-Inf, Inf), y = 4, cutoff = factor(4) )
cutoff3 <- data.frame( y = c(-Inf, Inf), x = 17, cutoff = factor(17) )
cutoff4 <- data.frame( y = c(-Inf, Inf), x = 4, cutoff = factor(4) )

# 28 42 3 6 
rcsi_pair = as.data.frame(rcsi_pair)
rcsi_pair$predict[rcsi_pair$predict<0 & is.na(rcsi_pair$predict)==FALSE] =0

ggplot(rcsi_pair, aes(actual, predict )) +
  
  theme_classic() + geom_abline(intercept = 0, slope = 1)+
  scale_color_brewer(palette="Dark2") +
  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") )  +
  labs(x = "Cluster Average rCSI (Actual)", y = "Cluster Average rCSI (Predicted)") +
  theme(plot.margin = unit(c(1, 10, 1, 1), "lines"))  +
  
  geom_line(aes( x, y, linetype = cutoff), cutoff1,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoff2,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoff3,show.legend = FALSE) +
  geom_line(aes( x, y, linetype = cutoff), cutoff4,show.legend = FALSE) +
  geom_ribbon(data=subset(as.data.frame(rcsi_pair), 0 <= actual & actual <= 4), 
              aes(ymin=0,ymax=actual), fill="grey", alpha="0.45")+   # area 2 left 
  geom_ribbon(data=subset(as.data.frame(rcsi_pair), 4 <= actual & actual <= 17), 
              aes(ymin=4,ymax=actual), fill="grey", alpha="0.45")+   # area 2 right 
  geom_ribbon(data=subset(as.data.frame(rcsi_pair), 4 <= actual & actual <= 17), 
              aes(ymin=actual,ymax=17), fill="grey", alpha="0.2")+   # area 3 right
  geom_ribbon(data=subset(as.data.frame(rcsi_pair), 0 <= actual & actual <= 4), 
              aes(ymin=actual,ymax=4), fill="grey", alpha="0.2") +  # area 3 left
  geom_ribbon(data=subset(as.data.frame(rcsi_pair), 0 <= actual & actual <= 4), 
              aes(ymin=4,ymax=17), fill="grey", alpha="0.6") +        # area 4 
  geom_ribbon(data=subset(as.data.frame(rcsi_pair), 4 <= actual & actual <= 17), 
              aes(ymin=0,ymax=4), fill="grey", alpha="0.8") +           # area 1
  geom_point(shape = 16, size = 3, show.legend = FALSE,na.rm = TRUE) +
  annotate(geom = "text", size = 7, x = 10, y = 7, label = "IIb") +
  annotate(geom = "text", size = 7, x = 2.2, y = 0.5, label = "IIa") +
  annotate(geom = "text", size = 7, x = 10, y = 2, label = "I") +
  annotate(geom = "text", size = 7, x = 0.2, y = 3, label = "IIIa") +
  annotate(geom = "text", size = 7, x = 7, y = 10, label = "IIIb") +
  annotate(geom = "text", size = 7, x = 2, y = 10, label = "IV") 

grid.text(paste("R Squared= ",r2list[3],sep = " "), x = unit(0.83, "npc"), y = unit(0.65, "npc"),gp=gpar(fontsize=14),just="left") 

grid.text("Cutoff:", x = unit(0.83, "npc"), y = unit(0.57, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("0 <= rCSI < 4", x = unit(0.83, "npc"), y = unit(0.53, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("is food secure; ", x = unit(0.83, "npc"), y = unit(0.49, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("4 <= rCSI < 17 is ", x = unit(0.83, "npc"), y = unit(0.45, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("midly food insecure;", x = unit(0.83, "npc"), y = unit(0.41, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("rCSI >= 17 is ", x = unit(0.83, "npc"), y = unit(0.37, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("moderately or severely", x = unit(0.83, "npc"), y = unit(0.33, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("food insecure.", x = unit(0.83, "npc"), y = unit(0.29, "npc"),gp=gpar(fontsize=15),just="left") 
# 
# 
#  grid.text("Area I: Overpredicting; mildly food ", x = unit(0.85, "npc"), y = unit(0.41, "npc"),gp=gpar(fontsize=14),just="left") 
#   grid.text("insecure predicted as food secure", x = unit(0.85, "npc"), y = unit(0.38,"npc"),gp=gpar(fontsize=14),just="left") 
#  grid.text("Area IIa: Overpredicting within food secure", x = unit(0.85, "npc"), y = unit(0.35, "npc"),gp=gpar(fontsize=14),just="left") 
#   grid.text("Area IIb: Overpredicting within mildly", x = unit(0.85, "npc"), y = unit(0.32, "npc"),gp=gpar(fontsize=14),just="left") 
#   grid.text("food insecure", x = unit(0.85, "npc"), y = unit(0.29, "npc"),gp=gpar(fontsize=14),just="left") 
# 
# grid.text("Area IIIa: Underpredicting within food secure", x = unit(0.85, "npc"), y = unit(0.26, "npc"),gp=gpar(fontsize=15),just="left") 
# grid.text("Area IIIb: Underpredicting within mildly", x = unit(0.85, "npc"), y = unit(0.23, "npc"),gp=gpar(fontsize=15),just="left") 
# grid.text("food insecure", x = unit(0.85, "npc"), y = unit(0.20, "npc"),gp=gpar(fontsize=15),just="left") 
# 
# grid.text("Area IV: Underpredicting; secure", x = unit(0.85, "npc"), y = unit(0.17, "npc"),gp=gpar(fontsize=15),just="left") 
# grid.text("predicted as mildly insecure", x = unit(0.85, "npc"), y = unit(0.14, "npc"),gp=gpar(fontsize=15),just="left") 




# R1: suggests adding labels on each panel to show what the response variable is (not sure what the issue is?
#                                                                                   
#  I think R1 might be suggesting we label what I, II, etc. in the graphs themselves and remove the roman numerals).
# 
# I think we do need to make the figure bigger, which I tried to do by landscaping the figure.
# In fig 3 I suggest placing labels on each panel to show clearly what the response variable is
# (and not force readers to look at caption or axis label)


##########################################################
# Fig. S2: A series of fixed effect models show that spatial level and temporal frequency
# influence food security status. Bars present the share of variation of each of the 2010 HH Food Security measures
# explained by month and or geographic identifier
###################################################

rm(list=ls())

library("reshape2")
library("ggplot2")
library("zoo")
library(gtable)
library(grid)

malawi <-   read_csv("data/mw_dataset_hh.csv")
malawi["logFCS"] = log(malawi$FCS) 

# start here 
data = malawi %>%
  filter(FS_year!=2013) %>%
  select(rCSI,logFCS,HDDS,FS_month,FNID,TA_names,ea_id)


data$month_clust <-interaction(data$FS_month,data$ea_id)
 
colnames(data)[4] <-"Month" 
colnames(data)[5] <-"IPC" 
colnames(data)[6] <-"TA"
colnames(data)[7] <-"Cluster" 
colnames(data)[8] <-"Month+Cluster"


#	 bar chart of variation in FS security measures (month, IPCzone, TA, cluster, month x cluster)


r2 = matrix(NA,3,5) 
data$rCSI

# month FE 
my_lms <- lapply(1:3, function(x){
  formula = paste(colnames(data)[x] ,"~as.factor(Month)",sep = "")
  lm(formula,data=data)
  
} )

summaries <- lapply(my_lms, summary)
r2[,1]<-sapply(summaries, function(x) c(r_sq = x$r.squared))


# IPC FE 
my_lms <- lapply(1:3, function(x) {
  formula = paste(colnames(data)[x] ,"~as.factor(IPC)",sep = "")
  lm(formula,data=data)
})
  
summaries <- lapply(my_lms, summary) 
r2[,2]<-sapply(summaries, function(x)
  c(r_sq = x$r.squared))

# TA FE 

my_lms <- lapply(1:3, function(x){
  formula = paste(colnames(data)[x] ,"~as.factor(TA)",sep = "")
  lm(formula,data=data)
  } )
summaries <- lapply(my_lms, summary) 
r2[,3]<-sapply(summaries, function(x)
  c(r_sq = x$r.squared))

# Cluster FE 
my_lms <- lapply(1:3, function(x){
  formula = paste(colnames(data)[x] ,"~as.factor(Cluster)",sep = "")
  lm(formula,data=data)
})
summaries <- lapply(my_lms, summary) 
r2[,4]<-sapply(summaries, function(x)
  c(r_sq = x$r.squared))


# Month+Cluster FE 
my_lms <- lapply(1:3, function(x){
  formula = paste(colnames(data)[x] ,"~as.factor(Month+Cluster)",sep = "")
  lm(formula,data=data)
})
  
  
summaries <- lapply(my_lms, summary)
r2[,5]<-sapply(summaries, function(x) c(r_sq = x$r.squared))


# Format results 
r2 <-as.data.frame(r2)
colnames(r2)<-c("Month","IPC","TA","Cluster","Cluster + Month")
rownames(r2)<-c("rCSI","logFCS","HDDS") #r2$measure<-c("RCSI","FCS","HDDS")

long_r2<- melt(t(r2),na.rm=TRUE,measure.vars =c("rCSI","logFCS","HDDS"))
long_r2<-long_r2[long_r2$Var1!="measure",]

colnames(long_r2)<-c("variation","Measures","Rsquared")

ar<-as.character(long_r2$Rsquared)
long_r2$ar<-as.numeric(ar)
# rCSI_r2_fe =  .22320556
# HDDS_r2_fe =  .30987621
# logFCS_r2_fe =  .33045673


## bar_plot figure of r squared
# plot code 
p<-ggplot(long_r2, aes(variation,ar)) +geom_bar(stat = "identity",position = "dodge",aes(fill = Measures))

p <- p + labs(y = "R squares",
              x = " ",
              colour = "Food Security Measures",
              shape= " ")

p <- p+ theme_bw() 
p <- p + theme(text = element_text(size=22)) 
p               

ggsave("figureS2.png", plot = p,device = "png",path = "output/graphs/",
       dpi = 1000, limitsize = TRUE)



##########################################################
# Fig S3: As the level of spatial granularity increases, explanatory power increases and
#the distribution of the predicated values moves towards the actual distribution both in
# the center and the spread of the distributions.  
###################################################

# b.	Density plot (predication using different scales + household)
# i.	Unexplored variation of household level 

# model 3 at different level + cluster actual + model 0 + 

rCSI_clust <- read_csv("output/results/prediction/clust/rCSI_clust_model3_pair.csv")
rCSI_TA <- read_csv("output/results/prediction/TA/rCSI_TA_model3_pair.csv")
rCSI_lhz <- read_csv("output/results/prediction/lhz/rCSI_lhz_model3_pair.csv")
rCSI_model0 <- read_csv("output/results/prediction/clust/rCSI_clust_model0_pair.csv")

logFCS_clust <- read_csv("output/results/prediction/clust/rCSI_clust_model3_pair.csv")
logFCS_TA <- read_csv("output/results/prediction/TA/rCSI_TA_model3_pair.csv")
logFCS_lhz <- read_csv("output/results/prediction/lhz/rCSI_lhz_model3_pair.csv")
logFCS_model0 <- read_csv("output/results/prediction/clust/rCSI_clust_model0_pair.csv")


HDDS_clust <- read_csv("output/results/prediction/clust/rCSI_clust_model3_pair.csv")
HDDS_TA <- read_csv("output/results/prediction/TA/rCSI_TA_model3_pair.csv")
HDDS_lhz <- read_csv("output/results/prediction/lhz/rCSI_lhz_model3_pair.csv")
HDDS_model0 <- read_csv("output/results/prediction/clust/rCSI_clust_model0_pair.csv")

mw_dataset_hh <- read_csv("data/mw_dataset_hh.csv")
mw13.hh = mw_dataset_hh %>% 
  dplyr::filter(FS_year==2013) %>%
  dplyr::mutate(logFCS = log(FCS)) %>%
  dplyr::select(logFCS,HDDS,rCSI)

mw13.hh.logFCS = mw13.hh %>% mutate(level = "Household Actual") %>% dplyr::select(level,logFCS)
mw13.hh.HDDS = mw13.hh %>% mutate(level = "Household Actual") %>% dplyr::select(level,HDDS)
mw13.hh.rCSI = mw13.hh %>% mutate(level = "Household Actual") %>% dplyr::select(level,rCSI)



# combine data at different level 
logFCS_data = cbind(logFCS_lhz$predict,logFCS_TA$predict,logFCS_clust$predict,logFCS_clust$actual,logFCS_model0$predict)
colnames(logFCS_data)<-c("IPC_zone","TA","Cluster","Actual","IPC_value_only")
# Wide to long
long_logFCS = gather( as.data.frame(logFCS_data),key= "level", value =logFCS )

plot_long<- long_logFCS[long_logFCS$level != "IPC_value_only",]
plot_long_ipc_logFCS<- long_logFCS[long_logFCS$level == "IPC_value_only",]
plot_long_logFCS= dplyr::bind_rows(plot_long, mw13.hh.logFCS)


HDDS_data = cbind(HDDS_lhz$predict,HDDS_TA$predict,HDDS_clust$predict,HDDS_clust$actual,HDDS_model0$predict)
colnames(HDDS_data)<-c("IPC_zone","TA","Cluster","Actual","IPC_value_only")
# Wide to long
long_HDDS = gather( as.data.frame(HDDS_data),key= "level", value =HDDS )

plot_long<- long_HDDS[long_HDDS$level != "IPC_value_only",]
plot_long_ipc_HDDS<- long_HDDS[long_HDDS$level == "IPC_value_only",]
plot_long_HDDS= dplyr::bind_rows(plot_long, mw13.hh.HDDS)

# rcsi P1 < 4
#P2 = 4-17
#P3 = 17-42
#P4/5 > 42
rCSI_data = cbind(rCSI_lhz$predict,rCSI_TA$predict,rCSI_clust$predict,rCSI_clust$actual,rCSI_model0$predict)
colnames(rCSI_data)<-c("IPC_zone","TA","Cluster","Actual","IPC_value_only")
# Wide to long
long_rCSI = gather( as.data.frame(rCSI_data),key= "level", value =rCSI )

plot_long<- long_rCSI[long_rCSI$level != "IPC_value_only",]
plot_long_ipc_rCSI<- long_rCSI[long_rCSI$level == "IPC_value_only",]
plot_long_rCSI= dplyr::bind_rows(plot_long, mw13.hh.rCSI)


####################
# Density plot code 
####################
# logFCS density 
ggplot(as.data.frame(plot_long_logFCS),aes(x=logFCS,color=level)) +
  geom_density(alpha=0.1,size=1.5) +xlim(2.8, 4.9) +
  geom_vline(xintercept=log(28),linetype=2) + geom_vline(xintercept=log(42),linetype=2)+
  geom_density(data=plot_long_ipc_logFCS,size=1.5,aes(x=logFCS, y=..scaled..*1,color=level)) + 
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = " density (IPC value only) ")) +  
  theme_classic()+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") ) 


# HDDS density 
ggplot(as.data.frame(plot_long_HDDS),aes(x=HDDS, color=level))+
  geom_density(alpha=0.1,size=1.5)+
  xlim(1.5, 7.6) + 
  geom_vline(xintercept=3,linetype=2) + geom_vline(xintercept=6,linetype=2) +
  geom_density(data=plot_long_ipc_HDDS,aes(x=HDDS,y=..scaled../1,color=level),alpha=0.25,size=1.5) +
  scale_y_continuous(name= "Density",sec.axis = sec_axis(~.*1, name = "Density (IPC value only) ")) + 
  theme_classic()+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=20), legend.text=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") ) 

# rCSI density 
ggplot(as.data.frame(plot_long_rCSI),aes(x=rCSI, color=level))+
  geom_density(data=plot_long_ipc_rCSI,aes(x=rCSI,y=..scaled../1.9,color=level),alpha=0.1,size=1.5) +
  geom_density(alpha=0.1,size=1.5)+xlim(0, 45) + 
  geom_vline(xintercept=4,linetype=2) + geom_vline(xintercept=17,linetype=2)+ geom_vline(xintercept=42,linetype=2)+
  scale_y_continuous(name= "Density",sec.axis = sec_axis(~.*1.9, name = "Density (IPC value only) ")) +
  theme_classic() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=20), legend.text=element_text(size=18),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") )
 
 
################################################# ################################################ 
# Fig S4: We predict 2013 food security using only 2010 data,
# limiting both sample(s) to the most food insecure households and confirm that factors 
# that affect food security do not substantially differ for the subset of the most insecure households. 
# The “tail” is defined by excluding the households that fall in the most food secure category of each food secure measures:
#   rCSI greater than 4, logFCS smaller than 1.623 and HDDS smaller than 6. 
# The cluster average tail food security measures are based on the cluster average of the remaining households at the tail.
# The signs and significance of the coefficients on the variables included in the tails-only models closely match estimates 
# from the full distribution. When estimating just the tails of the distribution, 
# the length of dry spells largely negatively affects food security but other measures are consistent between the full 
# distribution and the tails-only distribution. Results of categorical prediction for models including both full-samples 
# and tail subsamples indicate the full sample’s accuracy is more sensitive to spatial scale and data class than tail subsample.
# The accuracy in categorical predictions is similar between model 1 to model 3 for the same food security measure because
# the predicted values tend to fall into the same category. The same holds for the percent of type I and type II errors.
################################################ ################################################ ################################################ 




################################
# Figure S4 plot code 
################################

tail = read.csv("output/results/figureS4_matrix_tail.csv")
full = read.csv("output/results/figureS4_matrix_full.csv")

# format column names 
tail = tail %>%
separate(model, c("Measures", "Level", "Model","tail"), "_") %>% 
dplyr::select(-tail)  

full = full %>%
  separate(model, c("Measures", "Level", "Model"), "_")  



# formatting  values 
tail$Model = as.character(tail$Model)
tail$Model[tail$Model=="m1"]<-"Class 1 data"
tail$Model[tail$Model=="m2"]<-"Class 1 + Class 2 data"
tail$Model[tail$Model=="m3"]<-"Class 1 + Class 2 + Class 3 data"

full$Model = as.character(full$Model)
full$Model[full$Model=="m1"]<-"Class 1 data"
full$Model[full$Model=="m2"]<-"Class 1 + Class 2 data"
full$Model[full$Model=="m3"]<-"Class 1 + Class 2 + Class 3 data"

full$Level = gsub(full$Level,pattern="clust" ,replacement = "Cluster")
tail$Level = gsub(tail$Level,pattern="clust" ,replacement = "Cluster")
full$Level = gsub(full$Level,pattern="lhz" ,replacement = "IPC Zone")
tail$Level = gsub(tail$Level,pattern="lhz" ,replacement = "IPC Zone")


ord <- c("IPC Zone","TA","Cluster")
full$Level <- factor(full$Level,levels=ord)

ord_measure <- c("rCSI","logFCS","HDDS")
full$Measures <- factor(full$Measures,levels=ord_measure)

# 
ord_m <- c("Class 1 data","Class 1 + Class 2 data","Class 1 + Class 2 + Class 3 data")
full$Model <- factor(full$Model,levels=ord_m)


ord <- c("IPC Zone","TA","Cluster")
tail$Level <- factor(tail$Level,levels=ord)

ord_measure <- c("rCSI","logFCS","HDDS")
tail$Measures <- factor(tail$Measures,levels=ord_measure)

# wide to long on the model measures 

colnames(full)= c("FS.Measure", "Level","Model",  "Accuracy" ,"% Type I",    "% Type II"  )
colnames(tail)= c("FS.Measure", "Level","Model" ,   "Accuracy", "% Type I"   , "% Type II"  )

full.long = full %>% 
  gather(-FS.Measure,-Level,-Model,key ="Model.Measure",value="Value")

tail.long = tail %>% 
  gather(-FS.Measure,-Level,-Model,key ="Model.Measure",value="Value")

ord_s <- c("Accuracy","% Type I","% Type II")
tail.long$Model.Measure <- factor(tail.long$Model.Measure,levels=ord_s)
full.long$Model.Measure <- factor(full.long$Model.Measure,levels=ord_s)



ord_m <- c("Class 1 data","Class 1 + Class 2 data","Class 1 + Class 2 + Class 3 data")
tail.long$Model <- factor(tail.long$Model,levels=ord_m)
full.long$Model <- factor(full.long$Model,levels=ord_m)


# 
# colnames(full)[5] = "Type I Error"
# colnames(full)[6] = "Type II Error"
# 
# 
# colnames(tail)[5] = "Type I Error"
# colnames(tail)[6] = "Type II Error"

###################################
# Figure s4 Plot code 
####################################
# figure S4: plot logFCS

logFCS_full = full.long %>% filter(FS.Measure == "logFCS")

rplot<-ggplot(data = logFCS_full, aes(x =Level, y = Value,shape=Model)) 
rplot<-rplot + geom_point(size=7,color='#00BA38',show.legend=FALSE)   +  facet_grid(. ~  Model.Measure)        

rplot<-rplot + labs( x = "Geo-spatial Level", y = "Accuracy/Type I/Type II")

rplot <- rplot    + theme_classic() + theme(strip.text.x = element_text(size = 25))

rplot <- rplot +  theme(plot.title = element_text(size = 25, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=25),axis.text.x=element_text(size=25),axis.text.y=element_text(size=25),
                        axis.title=element_text(size=35,face="bold") ) 

#rplot = rplot + scale_color_grey(start = 0.8, end = 0.2)

# +  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))  
rplot

ggsave("output/graphs/figureS4/logFCS_full.png", width = 15, height = 15)


 
# figure S4: plot HDDS 
HDDS = full.long %>% filter(FS.Measure == "HDDS")

rplot<-ggplot(data = HDDS, aes(x =Level, y = Value,shape=Model))  
rplot<-rplot + geom_point(size=7,color='#619CFF',show.legend = FALSE)   +  facet_grid(. ~  Model.Measure) 
rplot<-rplot + labs( x = "Geo-spatial Level", y = "Accuracy/Type I/Type II")  

rplot <- rplot    + theme_classic() + theme(strip.text.x = element_text(size = 25))

rplot <- rplot +  theme(plot.title = element_text(size = 25, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=25),axis.text.x=element_text(size=25),axis.text.y=element_text(size=25),
                        axis.title=element_text(size=35,face="bold") ) 


#rplot = rplot + scale_color_grey(start = 0.8, end = 0.2)

# +  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))  
rplot

ggsave("output/graphs/figureS4/HDDS_full.png", width = 15, height = 15)

 
# figure S4: plot rCSI
rCSI = full.long %>% filter(FS.Measure == "rCSI")

rplot<-ggplot(data = rCSI, aes(x =Level, y = Value,shape=Model)) 
rplot<-rplot +  geom_point(size=7,color='#F8766D',show.legend = FALSE)   +  facet_grid(. ~  Model.Measure) 
rplot<-rplot + labs( x = "Geo-spatial Level", y = "Accuracy/Type I/Type II")

rplot <- rplot    + theme_classic() + theme(strip.text.x = element_text(size = 25))

rplot <- rplot +  theme(plot.title = element_text(size = 25, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=25),axis.text.x=element_text(size=25),axis.text.y=element_text(size=25),
                        axis.title=element_text(size=35,face="bold") ) 


#rplot = rplot + scale_color_grey(start = 0.8, end = 0.2)

# +  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))  
rplot

 ggsave("output/graphs/figureS4/rCSI_full.png", width = 15, height = 15)


 # Do the same for the tails 
logFCS_tail  = tail.long %>% filter(FS.Measure == "logFCS")

rplot<-ggplot(data = logFCS_tail, aes(x =Level, y = Value,shape=Model)) 
rplot<-rplot + geom_point(size=7,color='#00BA38',show.legend = FALSE)   +  facet_grid(. ~  Model.Measure)
rplot<-rplot + labs( x = "Geo-spatial Level", y = "Accuracy/Type I/Type II")


rplot <- rplot    + theme_classic() + theme(strip.text.x = element_text(size = 25))

rplot <- rplot +  theme(plot.title = element_text(size = 25, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=25),axis.text.x=element_text(size=25),axis.text.y=element_text(size=25),
                        axis.title=element_text(size=35,face="bold") ) 


#rplot = rplot + scale_color_grey(start = 0.8, end = 0.2)

# +  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))  
rplot

ggsave("output/graphs/figureS4/logFCS_tail.png", width = 15, height = 15)


# HDDS 
HDDS.tail = tail.long %>% filter(FS.Measure == "HDDS")

rplot<-ggplot(data = HDDS.tail, aes(x =Level, y = Value,shape=Model)) 
rplot<-rplot + geom_point(size=7,color='#619CFF',show.legend = FALSE)   +  facet_grid(. ~  Model.Measure) 
rplot<-rplot + labs( x = "Geo-spatial Level", y = "Accuracy/Type I/Type II")

rplot <- rplot    + theme_classic() + theme(strip.text.x = element_text(size = 25))

rplot <- rplot +  theme(plot.title = element_text(size = 25, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=25),axis.text.x=element_text(size=25),axis.text.y=element_text(size=25),
                        axis.title=element_text(size=35,face="bold") ) 

#rplot = rplot + scale_color_grey(start = 0.8, end = 0.2)

# +  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))  
rplot

ggsave("output/graphs/figureS4/HDDS_tail.png", width = 15, height = 15)


# rCSI.tail

rCSI.tail = tail.long %>% filter(FS.Measure == "rCSI")

rplot<-ggplot(data = rCSI, aes(x =Level, y = Value,shape=Model)) 
rplot<-rplot +  geom_point(size=7,color='#F8766D',show.legend = FALSE)   +  facet_grid(. ~  Model.Measure) 
rplot<-rplot + labs( x = "Geo-spatial Level", y = "Accuracy/Type I/Type II")

rplot <- rplot    + theme_classic() + theme(strip.text.x = element_text(size = 25))

rplot <- rplot +  theme(plot.title = element_text(size = 25, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=25),axis.text.x=element_text(size=25),axis.text.y=element_text(size=25),
                        axis.title=element_text(size=35,face="bold") ) 

#rplot = rplot + scale_color_grey(start = 0.8, end = 0.2)

# +  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))  
rplot

ggsave("output/graphs/figureS4/rCSI_tail.png", width = 15, height = 15)
