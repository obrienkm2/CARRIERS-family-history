
#graphs in color
library(ggplot2)
library(gridExtra)
library(patchwork)
library(grid)

results_noncarrier_noFH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_noncarrier_noFH.csv")
results_noncarrier_FH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_noncarrier_FH.csv")
results_ATM_noFH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_ATM_noFH.csv")
results_ATM_FH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_ATM_FH.csv")
results_BRCA1_noFH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_BRCA1_noFH.csv")
results_BRCA1_FH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_BRCA1_FH.csv")
results_BRCA2_noFH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_BRCA2_noFH.csv")
results_BRCA2_FH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_BRCA2_FH.csv")
results_CHEK2_noFH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_CHEK2_noFH.csv")
results_CHEK2_FH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_CHEK2_FH.csv")
results_PALB2_noFH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_PALB2_noFH.csv")
results_PALB2_FH<-read.csv("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/results_PALB2_FH.csv")


age<-seq(21,80,2)
age8<-cbind(rep(age,12))
est<-c(results_noncarrier_noFH[,3], results_noncarrier_FH[,3], results_ATM_noFH[,3], results_ATM_FH[,3], 
      results_BRCA1_noFH[,3], results_BRCA1_FH[,3],results_BRCA2_noFH[,3], results_BRCA2_FH[,3], results_CHEK2_noFH[,3], results_CHEK2_FH[,3], 
       results_PALB2_noFH[,3], results_PALB2_FH[,3])
lci<-c(results_noncarrier_noFH[,2], results_noncarrier_FH[,2], results_ATM_noFH[,2], results_ATM_FH[,2], 
       results_BRCA1_noFH[,2], results_BRCA1_FH[,2], results_BRCA2_noFH[,2], results_BRCA2_FH[,2], results_CHEK2_noFH[,2], results_CHEK2_FH[,2], 
       results_PALB2_noFH[,2], results_PALB2_FH[,2])
uci<-c(results_noncarrier_noFH[,4], results_noncarrier_FH[,4], results_ATM_noFH[,4], results_ATM_FH[,4], 
       results_BRCA1_noFH[,4], results_BRCA1_FH[,4], results_BRCA2_noFH[,4], results_BRCA2_FH[,4], results_CHEK2_noFH[,4], results_CHEK2_FH[,4], 
       results_PALB2_noFH[,4], results_PALB2_FH[,4])
nc_noFH<-rep("No PV, No Family History",30)
nc_FH<-rep("No PV, Family History",30)
PV_noFH<-rep("PV, No Family History",30)
PV_FH<-rep("PV, Family History",30)

groupname<-c(nc_noFH,nc_FH,PV_noFH,PV_FH,PV_noFH,PV_FH,PV_noFH,PV_FH,PV_noFH,PV_FH,PV_noFH,PV_FH)

data_absrisk<-data.frame(age8,est,lci,uci,groupname)

atm_data<-data.frame(data_absrisk[1:120,])
brca1_data<-data.frame(data_absrisk[c(1:60,121:180),])
brca2_data<-data.frame(data_absrisk[c(1:60,181:240),])
chek2_data<-data.frame(data_absrisk[c(1:60,241:301),])
palb2_data<-data.frame(data_absrisk[c(1:60,301:360),])

atm<- ggplot(data=atm_data) +
  geom_line(aes(x=age8, y=est, group=groupname, colour = factor(groupname))) +
  lims(x=c(20,80), y=c(0,1)) +
  labs(x="Age", y="Cumulative Risk of Breast Cancer",color="Pathogenic Variant (PV) \n and Family History (FH)",title="A) ATM") +
  geom_ribbon(aes(x=age8, ymin=lci, ymax=uci,fill=factor(groupname)),alpha=0.25,show.legend=FALSE) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 14,face='bold',vjust=-1,hjust=0.05), 
        legend.position="none") + 
  scale_color_manual(values = c("green","black","red","blue")) + 
  scale_fill_manual(values = c("green","black","red","blue"))
atm

brca1<- ggplot(data=brca1_data) +
  geom_line(aes(x=age8, y=est, group=groupname, colour = factor(groupname))) +
  lims(x=c(20,80), y=c(0,1)) +
  labs(x="Age", y="Cumulative Risk of Breast Cancer",color="Pathogenic Variant (PV) \n and Family History (FH)",title="B) BRCA1") +
  geom_ribbon(aes(x=age8, ymin=lci, ymax=uci,fill=factor(groupname)),alpha=0.25,show.legend=FALSE) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 14,face='bold',vjust=-1,hjust=0.05), 
        legend.position="none") + 
  scale_color_manual(values = c("green","black","red","blue")) + 
  scale_fill_manual(values = c("green","black","red","blue"))
brca1

brca2<- ggplot(data=brca2_data) +
  geom_line(aes(x=age8, y=est, group=groupname, colour = factor(groupname))) +
  lims(x=c(20,80), y=c(0,1.01)) +
  labs(x="Age", y="Cumulative Risk of Breast Cancer",color="Pathogenic Variant (PV) \n and Family History (FH)",title="C) BRCA2") +
  geom_ribbon(aes(x=age8, ymin=lci, ymax=uci,fill=factor(groupname)),alpha=0.25,show.legend=FALSE) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 14,face='bold',vjust=-1,hjust=0.05),
        legend.position="none") + 
  scale_color_manual(values = c("green","black","red","blue")) + 
  scale_fill_manual(values = c("green","black","red","blue"))
brca2

chek2<- ggplot(data=chek2_data) +
  geom_line(aes(x=age8, y=est, group=groupname, colour = factor(groupname))) +
  lims(x=c(20,80), y=c(0,1)) +
  labs(x="Age", y="Cumulative Risk of Breast Cancer",color="Pathogenic Variant (PV) \n and Family History (FH)",title="D) CHEK2") +
  geom_ribbon(aes(x=age8, ymin=lci, ymax=uci,fill=factor(groupname)),alpha=0.25,show.legend=FALSE) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 14,face='bold',vjust=-1,hjust=0.05), 
        legend.position="none") + 
  scale_color_manual(values = c("green","black","red","blue")) + 
  scale_fill_manual(values = c("green","black","red","blue"))

palb2 <- ggplot(data=palb2_data) +
  geom_line(aes(x=age8, y=est, group=groupname, colour = factor(groupname))) +
  lims(x=c(20,80), y=c(0,1)) +
  labs(x="Age", y="Cumulative Risk of Breast Cancer",color="Pathogenic Variant (PV) \n and Family History (FH)",title="E) PALB2") +
  geom_ribbon(aes(x=age8, ymin=lci, ymax=uci,fill=factor(groupname)),alpha=0.25,show.legend=FALSE) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 14,face='bold',vjust=-1,hjust=0.05), 
        legend.position="none") + 
  scale_color_manual(values = c("green","black","red","blue")) + 
  scale_fill_manual(values = c("green","black","red","blue"))

empty_data<-data.frame(matrix(-0.5,nrow=120,ncol=3))
empty_data<-cbind(age8,empty_data,groupname)

empty <- ggplot(data=empty_data) +
  geom_line(aes(x=empty_data[,1], y=empty_data[,2], group=groupname, colour = factor(groupname))) +
  lims(x=c(20,80), y=c(0,1)) + 
  labs(x=" ", y=" ",color="Pathogenic Variant (PV) and Family History",title=" ") +
  geom_ribbon(aes(x=empty_data[,1], ymin=empty_data[,3], ymax=empty_data[,4],fill=factor(groupname)),alpha=0.25,show.legend=FALSE) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 14,face='bold',vjust=-1,hjust=0.05), 
        legend.title=element_text(size = 14,face='bold'),axis.title=element_blank(),
        axis.text=element_blank(),axis.ticks=element_blank(),axis.line=element_blank(),
        legend.position=c(0.5, 0.55),legend.text=element_text(size=14),legend.key.size = unit(1.2, 'cm'))+
  scale_color_manual(values = c("green","black","red","blue")) + 
  scale_fill_manual(values = c("green","black","red","blue"))
empty

#all_PVs<- grid.arrange(brca1,brca2,nf1,palb2,ncol=2)
#all_PVs<- atm + brca1 + brca2 + chek2 + palb2 + plot_layout(ncol = 2, nrow = 3, guides = "collect")
all_PVs<- atm + brca1 + brca2 + chek2 + palb2 + empty + plot_layout(ncol = 2, nrow = 3)


#ggsave("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/color_cumrisk.png", all_PVs,width=11,height=8.5)
ggsave("/Users/obrienkm2/Documents/OBrien CARRIERS/color_cumrisk_hybrid_new.png", all_PVs,width=11,height=8.5)

