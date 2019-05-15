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


source("R/functions/R2ComputePair.R")

r2list<- sapply(linear_list,R2ComputePair)
r2list= format(r2list, digits=3, nsmall=2)

FCS_data_model3 =  FCS_data 
FCS_data_model3 = as.data.frame(FCS_data_model3) %>% dplyr::select(-IPC_value_only)

FCS_data_model0<-cbind(logFCS_IPC$clust_logFCS_predict_ipc,logFCS_IPC$clust_logFCS_predict_ipc,logFCS_IPC$clust_logFCS_predict_ipc,predict_df$clust_logFCS)
colnames(FCS_data_model0)<-c("IPC_zone","TA","cluster","Actual")


FCS_data_model2<-cbind(predict_df$clust_logFCS_ipczone_predict_m2,predict_df$clust_logFCS_TA_predict_m2,predict_df$clust_logFCS_clust_predict_m2,predict_df$clust_logFCS)
colnames(FCS_data_model2)<-c("IPC_zone","TA","cluster","Actual")


FCS_data_model1<-cbind(predict_df$clust_logFCS_ipczone_predict_m1,predict_df$clust_logFCS_TA_predict_m1,predict_df$clust_logFCS_clust_predict_m1,predict_df$clust_logFCS)
colnames(FCS_data_model1)<-c("IPC_zone","TA","cluster","Actual")

FCS_List<-list(FCS_data_model0,FCS_data_model1,FCS_data_model2,FCS_data_model3)

HDDS_data_model3 =  HDDS_data 
HDDS_data_model3 = as.data.frame(HDDS_data_model3) %>% dplyr::select(-IPC_value_only)


HDDS_data_model0<-cbind(HDDS_IPC$clust_HDDS_predict_ipc,HDDS_IPC$clust_HDDS_predict_ipc,HDDS_IPC$clust_HDDS_predict_ipc,predict_df$clust_HDDS)
colnames(HDDS_data_model0)<-c("IPC_zone","TA","cluster","Actual")

HDDS_data_model2<-cbind(predict_df$clust_HDDS_ipczone_predict_m2,predict_df$clust_HDDS_TA_predict_m2,predict_df$clust_HDDS_clust_predict_m2,predict_df$clust_HDDS)
colnames(HDDS_data_model2)<-c("IPC_zone","TA","cluster","Actual")



HDDS_data_model1<-cbind(predict_df$clust_HDDS_ipczone_predict_m1,predict_df$clust_HDDS_TA_predict_m1,predict_df$clust_HDDS_clust_predict_m1,predict_df$clust_HDDS)
colnames(HDDS_data_model1)<-c("IPC_zone","TA","cluster","Actual")

HDDS_List<-list(HDDS_data_model0,HDDS_data_model1,HDDS_data_model2,HDDS_data_model3)


RCSI_data_model3 =  RCSI_data 
RCSI_data_model3 = as.data.frame(RCSI_data_model3) %>% dplyr::select(-IPC_value_only)

RCSI_data_model0<-cbind(RCSI_IPC$clust_RCSI_predict_ipc,RCSI_IPC$clust_RCSI_predict_ipc,RCSI_IPC$clust_RCSI_predict_ipc,predict_df$clust_RCSI)
colnames(RCSI_data_model0)<-c("IPC_zone","TA","cluster","Actual")


RCSI_data_model2<-cbind(predict_df$clust_RCSI_ipczone_predict_m2,predict_df$clust_RCSI_TA_predict_m2,predict_df$clust_RCSI_clust_predict_m2,predict_df$clust_RCSI)
colnames(RCSI_data_model2)<-c("IPC_zone","TA","cluster","Actual")



RCSI_data_model1<-cbind(predict_df$clust_RCSI_ipczone_predict_m1,predict_df$clust_RCSI_TA_predict_m1,predict_df$clust_RCSI_clust_predict_m1,predict_df$clust_RCSI)
colnames(RCSI_data_model1)<-c("IPC_zone","TA","cluster","Actual")

RCSI_List<-list(RCSI_data_model0,RCSI_data_model1,RCSI_data_model2,RCSI_data_model3)
source("function/R2Compute.R")
source("function/R2ComputeDF.R")


r2mat_RCSI<- sapply(RCSI_List,R2ComputeDF)
r2mat_HDDS<- sapply(HDDS_List,R2ComputeDF)
r2mat_FCS<- sapply(FCS_List,R2ComputeDF)

colnames(r2mat_RCSI) = c("m0","m1","m2","m3")
colnames(r2mat_HDDS) = c("m0","m1","m2","m3")
colnames(r2mat_FCS) = c("m0","m1","m2","m3")

rownames(r2mat_RCSI) = c("IPC_zone","TA","cluster")
rownames(r2mat_HDDS) = c("IPC_zone","TA","cluster")
rownames(r2mat_FCS) = c("IPC_zone","TA","cluster")


r2matlist<-list(r2mat_FCS,r2mat_HDDS,r2mat_RCSI)

r2matlist<-lapply(r2matlist,as.data.frame)
r2matlist<-lapply(r2matlist,function(x){tibble::rownames_to_column(x,var = "Level")})

r2matlist<-lapply(r2matlist,function(x){tidyr::gather(data= x,key=model,2:5,value = rsquares)})

r2matlist[[1]]["measure"] ="logFCS"
r2matlist[[2]]["measure"] ="HDDS"
r2matlist[[3]]["measure"] ="rCSI"

r2 = bind_rows(r2matlist[[1]],r2matlist[[2]],r2matlist[[3]])
colnames(r2)[2] = "Model"
colnames(r2)[3] = "RSquared"
colnames(r2)[4] = "Measures"


r2$Level[r2$Level=="IPC_zone"]<-"IPC Zone"
r2$Level[r2$Level=="cluster"]<-"Cluster"



r2$Model[r2$Model=="m0"]<-"Class 0 (IPC value only)"
r2$Model[r2$Model=="m1"]<-"Class 1 data"
r2$Model[r2$Model=="m2"]<-"Class 1 + Class 2 data"
r2$Model[r2$Model=="m3"]<-"Class 1 + Class 2 + Class 3 data"


ord <- c("IPC_value_only","IPC Zone","TA","Cluster")
r2$Level <- factor(r2$Level,levels=ord)



ord_m <- c("Class 0 (IPC value only)","Class 1 data","Class 1 + Class 2 data","Class 1 + Class 2 + Class 3 data")
r2$Model <- factor(r2$Model,levels=ord_m)

r2$RSquared <-as.numeric(r2$RSquared)


rplot<-ggplot(data = r2, aes(x =Level, y = RSquared,colour = Model ,shape = Measures)) 
rplot<-rplot + geom_point(size=7)
rplot<-rplot + labs( x = "Geo-spatial Level", y = "R Squared")

rplot <- rplot+ theme_classic() 
rplot <- rplot +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                        axis.title=element_text(size=17,face="bold") ) 

rplot = rplot + scale_color_grey(start = 0.8, end = 0.2)

# +  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))  
rplot
#  grid.text("Class 1 data contains: past IPC values, precipitation,", x = unit(0.625, "npc"), y = unit(0.35, "npc"),gp=gpar(fontsize=15), check.overlap = TRUE,just="left") 
#  grid.text("market prices, market access measures, and soil quality", x = unit(0.625, "npc"), y = unit(0.32, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left") 
# grid.text("Class 2 data contains: share of households owing cellular", x = unit(0.625, "npc"), y = unit(0.29, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")
# grid.text("phone and share of dwellings with metal versus thatch roof", x = unit(0.625, "npc"), y = unit(0.26, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")
# grid.text("Class 3 data contains: household demographics and assets", x = unit(0.625, "npc"), y = unit(0.23, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")



r22 = bind_rows(r2matlist[[1]],r2matlist[[2]],r2matlist[[3]])
colnames(r22)[2] = "Model"
colnames(r22)[3] = "RSquared"
colnames(r22)[4] = "Measures"


r22$Level[r22$Level=="IPC_zone"]<-"IPC Zone"
r22$Level[r22$Level=="cluster"]<-"Cluster"
r22$Model[r22$Model=="m0"]<-"Class 0: IPC value only"
r22$Model[r22$Model=="m1"]<-"Class 1"
r22$Model[r22$Model=="m2"]<-"Class 1 + Class 2  "
r22$Model[r22$Model=="m3"]<-"Class 1 + Class 2 + Class 3"

ord <- c("IPC_value_only","IPC Zone","TA","Cluster")
r22$Level <- factor(r22$Level,levels=ord)

r22$RSquared <-as.numeric(r22$RSquared)

r22 = r22 %>% arrange(Model)

rplot<-ggplot(data = r22, aes(x =Model , y = RSquared,colour =Level  ,shape = Measures)) 
rplot<-rplot + geom_point(size=7)
rplot<-rplot + labs( x = "Model", y = "R Squared")

rplot <- rplot+ theme_classic() 
rplot <- rplot +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                        axis.title=element_text(size=17,face="bold") ) 

rplot = rplot + scale_color_grey(start = 0.8, end = 0.2) +  theme(plot.margin = unit(c(1, 9, 1, 1), "lines"))  
rplot
grid.text("Class 1 data contains:", x = unit(0.77, "npc"), y = unit(0.35, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left") 
grid.text("past IPC values, precipitation,", x = unit(0.77, "npc"), y = unit(0.32, "npc"),gp=gpar(fontsize=15), check.overlap = TRUE,just="left") 
grid.text("market prices, market access", x = unit(0.77, "npc"), y = unit(0.29, "npc"),gp=gpar(fontsize=15), check.overlap = TRUE,just="left") 
grid.text("measures, and soil quality.", x = unit(0.77, "npc"), y = unit(0.26, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left") 
grid.text("Class 2 data contains: ", x = unit(0.77, "npc"), y = unit(0.23, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")
grid.text("share of households owing cellular", x = unit(0.77, "npc"), y = unit(0.20, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")
grid.text("phone and share of dwellings", x = unit(0.77, "npc"), y = unit(0.17, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")
grid.text("with metal versus thatch roof.", x = unit(0.77, "npc"), y = unit(0.14, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")

grid.text("Class 3 data contains:", x = unit(0.77, "npc"), y = unit(0.11, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")
grid.text("household demographics and assets.", x = unit(0.77, "npc"), y = unit(0.08, "npc"),gp=gpar(fontsize=15),check.overlap = TRUE,just="left")




#########################################################################################
## Figure 3. Scatter plots 
#########################################################################################

###############################################################################################
### Figure 3: logFCS  
###############################################################################################

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

grid.text(paste("R Squared= ",r2list[2],sep = " "), x = unit(0.84, "npc"), y = unit(0.65, "npc"),gp=gpar(fontsize=15),just="left") 

grid.text("Cutoff: 0 <= logFCS ", x = unit(0.84, "npc"), y = unit(0.57, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("< 3.33 is poor;", x = unit(0.84, "npc"), y = unit(0.54, "npc"),gp=gpar(fontsize=15),just="left") 

grid.text("3.33 <= logFCS ", x = unit(0.84, "npc"), y = unit(0.51, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("< 3.74 is borderline;", x = unit(0.84, "npc"), y = unit(0.48, "npc"),gp=gpar(fontsize=15),just="left") 

grid.text("logFCS >= 3.74 ", x = unit(0.84, "npc"), y = unit(0.45, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("is acceptable.", x = unit(0.84, "npc"), y = unit(0.42, "npc"),gp=gpar(fontsize=15),just="left") 

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

grid.text(paste("R Squared= ",r2list[3],sep = " "), x = unit(0.85, "npc"), y = unit(0.65, "npc"),gp=gpar(fontsize=15),just="left") 

grid.text("Cutoff: 0 <= HDDS < 3 ", x = unit(0.85, "npc"), y = unit(0.58, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("is poor diversity;", x = unit(0.85, "npc"), y = unit(0.55, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("3 <= HDDS < 6 ", x = unit(0.85, "npc"), y = unit(0.52, "npc"),gp=gpar(fontsize=15),just="left")
grid.text("is medium diversity;", x = unit(0.85, "npc"), y = unit(0.49, "npc"),gp=gpar(fontsize=15),just="left")
grid.text("HDDS >= 6", x = unit(0.85, "npc"), y = unit(0.46, "npc"),gp=gpar(fontsize=15),just="left") 
grid.text("is good diversity.", x = unit(0.85, "npc"), y = unit(0.43, "npc"),gp=gpar(fontsize=15),just="left") 


###############################################################################################
### Figure 3: rCSI 
###############################################################################################

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

grid.text(paste("R Squared= ",r2list[1],sep = " "), x = unit(0.85, "npc"), y = unit(0.65, "npc"),gp=gpar(fontsize=14),just="left") 

grid.text("Cutoff: 0 <= rCSI < 4", x = unit(0.85, "npc"), y = unit(0.57, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("is food secure; ", x = unit(0.85, "npc"), y = unit(0.54, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("4 <= rCSI < 17 is ", x = unit(0.85, "npc"), y = unit(0.51, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("midly food insecure;", x = unit(0.85, "npc"), y = unit(0.48, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("rCSI >= 17 is ", x = unit(0.85, "npc"), y = unit(0.45, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("moderately or severely", x = unit(0.85, "npc"), y = unit(0.42, "npc"),gp=gpar(fontsize=14),just="left") 
grid.text("food insecure.", x = unit(0.85, "npc"), y = unit(0.39, "npc"),gp=gpar(fontsize=15),just="left") 
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






##########################################################
# Fig S3: As the level of spatial granularity increases, explanatory power increases and
#the distribution of the predicated values moves towards the actual distribution both in
# the center and the spread of the distributions.  
###################################################

# b.	Density plot (predication using different scales + household)
# i.	Unexplored variation of household level 


csv_list <- list.files(path="data/all_predict", 
                       pattern = "csv$",
                       full.names=TRUE)
dfnames<-csv_list

# remove irregulars in the file names, 
pattern<-c("data/all_predict/","clust_",".csv")
for (i in 1:length(pattern)) {
  dfnames <- gsub(pattern[i],"", dfnames)
}



list2env(
  lapply(setNames(csv_list, make.names(dfnames)), 
         function(i){read.csv(i)}), envir = .GlobalEnv)

predict_df<-cbind(logFCS_predict_m3[1],RCSI_predict_m3[1],HDDS_predict_m3[1])

for (i in 1:length(dfnames)){
  # exclude unneeded cols  
  temp <- get(dfnames[i])
  predict_df<-cbind(predict_df,temp[2])
}




malawi<- read.csv("data/cluster_fs.csv")

logFCS_IPC <- read.csv("data/all_ipc/logFCS_predict_CLUST_IPC.csv")
HDDS_IPC <- read.csv("data/all_ipc/HDDS_predict_CLUST_IPC.csv")
RCSI_IPC <- read.csv("data/all_ipc/RCSI_predict_CLUST_IPC.csv")



FCS_data<-cbind(predict_df$logFCS_ipczone_predict_m3,predict_df$logFCS_TA_predict_m3,predict_df$logFCS_clust_predict_m3,predict_df$logFCS,logFCS_IPC$logFCS_predict_ipc)
colnames(FCS_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_fcs<- melt(FCS_data,na.rm=TRUE)
colnames(long_fcs)<-c("no","level","logFCS")
plot_long<- long_fcs[long_fcs$level != "IPC_value_only",]
plot_long_ipc_fcs<- long_fcs[long_fcs$level == "IPC_value_only",]


plot_long_FCS= dplyr::bind_rows(plot_long, long_FCS_hh)



HDDS_data<-cbind(predict_df$HDDS_ipczone_predict_m3,predict_df$HDDS_TA_predict_m3,predict_df$HDDS_clust_predict_m3,predict_df$HDDS,HDDS_IPC$HDDS_predict_ipc)
colnames(HDDS_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_HDDS<- melt(HDDS_data,na.rm=TRUE)
colnames(long_HDDS)<-c("no","level","HDDS")
plot_long<- long_HDDS[long_HDDS$level != "IPC_value_only",]
plot_long_ipc_HDDS<- long_HDDS[long_HDDS$level == "IPC_value_only",]

plot_long_HDDS= dplyr::bind_rows(plot_long, long_HDDS_hh)


# rcsi P1 < 4
#P2 = 4-17
#P3 = 17-42
#P4/5 > 42
RCSI_data<-cbind(predict_df$rCSI_ipczone_predict_m3,predict_df$rCSI_TA_predict_m3,predict_df$rCSI_clust_predict_m3,predict_df$rCSI,RCSI_IPC$rCSI_predict_ipc)
colnames(RCSI_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_rcsi<- melt(RCSI_data,na.rm=TRUE)
colnames(long_rcsi)<-c("no","level","RCSI")
plot_long<- long_rcsi[long_rcsi$level != "IPC_value_only",]
plot_long_ipc_RCSI<- long_rcsi[long_rcsi$level == "IPC_value_only",]

plot_long_RCSI= dplyr::bind_rows(plot_long, long_RCSI_hh)

p = ggplot(as.data.frame(plot_long_FCS),aes(x=logFCS,color=level)) + geom_density(alpha=0.1) +xlim(2.8, 4.9) +
  geom_vline(xintercept=log(28),linetype=2) + geom_vline(xintercept=log(42),linetype=2)+
  stat_density(data=plot_long_ipc_fcs,aes(x=logFCS, y=..scaled..*3.5,color=level)) + scale_y_continuous(sec.axis = sec_axis(~.*28, name = " density (IPC value only) ")) +  
  theme_classic()+
  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") ) 

p



p= ggplot(as.data.frame(plot_long_HDDS),aes(x=HDDS, color=level))+ geom_density(alpha=0.25,show.legend = TRUE) +
  xlim(1.5, 7.6) + geom_vline(xintercept=3,linetype=2) + geom_vline(xintercept=6,linetype=2) +
  stat_density(data=plot_long_ipc_HDDS,aes(x=HDDS,y=..scaled..*1.1,color=level)) +
  scale_y_continuous(name= "density",sec.axis = sec_axis(~.*12, name = "density (IPC value only) "))  +  theme_classic()+
  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") ) 

p



p = ggplot(as.data.frame(plot_long_RCSI),aes(x=RCSI, color=level))+ stat_density(data=plot_long_ipc_RCSI,aes(x=RCSI,y=..scaled../2.3,color=level)) + geom_density(alpha=0.25)+xlim(0, 45) + 
  geom_vline(xintercept=4,linetype=2) + geom_vline(xintercept=17,linetype=2)+ geom_vline(xintercept=42,linetype=2)+
  scale_y_continuous(name= "density",sec.axis = sec_axis(~.*3.125, name = "density (IPC value only) ")) +
  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title=element_text(size=17,face="bold") ) +
  theme_classic()
p

 