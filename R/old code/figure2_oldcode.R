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

