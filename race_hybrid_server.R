library(readxl)
library("iCARE")

Inc <- data.frame(read_excel("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/inc_byage_raceeth_2024_04_17.xlsx",range = "A1:F91"))
Inc_Asian<-Inc[c(1:2)]
names(Inc_Asian)[names(Inc_Asian) == 'Asian'] <- 'Incidence'
Inc_Black<-Inc[c(1,3)]
names(Inc_Black)[names(Inc_Black) == 'Black'] <- 'Incidence'
Inc_Hisp<-Inc[c(1,4)]
names(Inc_Hisp)[names(Inc_Hisp) == 'Hispanic'] <- 'Incidence'
Inc_NHW<-Inc[c(1,5)]
names(Inc_NHW)[names(Inc_NHW) == 'NHW'] <- 'Incidence'
Inc_all<-Inc[c(1,6)]
names(Inc_all)[names(Inc_all) == 'all'] <- 'Incidence'

Inc_all_u50<-Inc_all[1:50,]
Inc_all_50p<-Inc_all[51:90,]

rates_noFH_u50<-Inc_all_u50/1.045
rates_noFH_50p<-Inc_all_50p/1.03
rates_noFH<-rbind(rates_noFH_u50,rates_noFH_50p)
Inc_all_noFH<-cbind(Inc_all[,1],rates_noFH[,2])

rates_FH_u50<-rates_noFH_u50*2.5
rates_FH_50p<-rates_noFH_50p*1.6
rates_FH<-rbind(rates_FH_u50,rates_FH_50p)
Inc_all_FH<-cbind(Inc_all[,1],rates_FH[,2])

Mort <- data.frame(read_excel("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/mort_byage_raceeth_2024_04_17.xlsx",range = "A1:F91"))
Mort_Asian<-Mort[c(1:2)]
names(Mort_Asian)[names(Mort_Asian) == 'Asian'] <- 'Mortality'
Mort_Black<-Mort[c(1,3)]
names(Mort_Black)[names(Mort_Black) == 'Black'] <- 'Mortality'
Mort_Hisp<-Mort[c(1,4)]
names(Mort_Hisp)[names(Mort_Hisp) == 'Hispanic'] <- 'Mortality'
Mort_NHW<-Mort[c(1,5)]
names(Mort_NHW)[names(Mort_NHW) == 'NHW'] <- 'Mortality'
Mort_all<-Mort[c(1,6)]
names(Mort_all)[names(Mort_all) == 'all'] <- 'Mortality'

PV5_u50_noFH_all_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_all_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_FH_all_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_all_1000<-matrix(nrow=1000,ncol=6)

#PV_data <- read_excel("PV_DATA_updated_2024_09_30.xlsx")
PV_data <- read_excel("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/PV_DATA_updated_2024_09_30.xlsx")

PV_data_noFH5<-PV_data[c(1,3:6),1:4]
names(PV_data_noFH5)[1]<-"snp.name"
names(PV_data_noFH5)[4]<-"snp.freq"
PV_data_noFH5<-as.data.frame(PV_data_noFH5)
PV_data_noFH5

PV_data_FH5<-PV_data[c(1,3:6),c(1,5:7)]
names(PV_data_FH5)[1]<-"snp.name"
names(PV_data_FH5)[4]<-"snp.freq"
PV_data_FH5<-as.data.frame(PV_data_FH5)
PV_data_FH5

#PV files
CARRIERS_u50_PVs_noFH5<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_noFH.txt")[,c(1,3:6)]
CARRIERS_u50_PVs_FH5<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_FH.txt")[,c(1,3:6)]
CARRIERS_50p_PVs_noFH5<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_noFH.txt")[,c(1,3:6)]
CARRIERS_50p_PVs_FH5<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_FH.txt")[,c(1,3:6)]

#covar lists

list_famhist <- vector(mode='list', length=2)
names(list_famhist)<-c("name","type")
list_famhist[1]<-"famhist"
list_famhist[2]<-"continuous"
list_height <- vector(mode='list', length=2)
names(list_height)<-c("name","type")
list_height[1]<-"height"
list_height[2]<-"continuous"
list_raceeth <- vector(mode='list', length=3)
names(list_raceeth)<-c("name","type","levels")
list_raceeth[1]<-"raceeth"
list_raceeth[2]<-"factor" 
list_raceeth[3]<-list(c("1","2","3","4","5"))
list_menarche <- vector(mode='list', length=3)
names(list_menarche)<-c("name","type","levels")
list_menarche[1]<-"menarche"
list_menarche[2]<-"factor"
list_menarche[3]<-list(c("1","2","3","4","5"))
list_parity <- vector(mode='list', length=3)
names(list_parity)<-c("name","type","levels")
list_parity[1]<-"parity"
list_parity[2]<-"factor"
list_parity[3]<-list(c("1","2","3","4"))
list_parous <- vector(mode='list', length=2)
names(list_parous)<-c("name","type")
list_parous[1]<-"parous"
list_parous[2]<-"continuous"
list_afb <- vector(mode='list', length=3)
names(list_afb)<-c("name","type","levels")
list_afb[1]<-"afb"
list_afb[2]<-"factor"
list_afb[3]<-list(c("1","2","3","4"))
list_bmi <- vector(mode='list', length=3)
names(list_bmi)<-c("name","type","levels")
list_bmi[1]<-"bmi"
list_bmi[2]<-"factor"
list_bmi[3]<-list(c("1","2","3"))
list_alcohol <- vector(mode='list', length=3)
names(list_alcohol)<-c("name","type","levels")
list_alcohol[1]<-"alcohol"
list_alcohol[2]<-"factor"
list_alcohol[3]<-list(c("1","2","3","4"))
list_alc3 <- vector(mode='list', length=3)
names(list_alc3)<-c("name","type","levels")
list_alc3[1]<-"alc3"
list_alc3[2]<-"factor"
list_alc3[3]<-list(c("1","2","3"))
list_OCuse <- vector(mode='list', length=2)
names(list_OCuse)<-c("name","type")
list_OCuse[1]<-"OCuse"
list_OCuse[2]<-"continuous"
list_HRT <- vector(mode='list', length=3)
names(list_HRT)<-c("name","type","levels")
list_HRT[1]<-"HRT"
list_HRT[2]<-"factor"
list_HRT[3]<-list(c("1","2","3"))
list_agemeno <- vector(mode='list', length=3)
names(list_agemeno)<-c("name","type","levels")
list_agemeno[1]<-"agemeno"
list_agemeno[2]<-"factor"
list_agemeno[3]<-list(c("1","2","3","4"))
list_highalc <- vector(mode='list', length=2)
names(list_highalc)<-c("name","type")
list_highalc[1]<-"highalc"
list_highalc[2]<-"continuous"
list_EPHRT <- vector(mode='list', length=2)
names(list_EPHRT)<-c("name","type")
list_EPHRT[1]<-"EPHRT" 
list_EPHRT[2]<-"continuous"

#hybrid version includes raceeth drops OCuse simplify parity and alcohol and BMI*HRT
hybrid_covar_list_u50<- list(list_height,list_raceeth,list_menarche,list_parous,list_afb,
                             list_bmi,list_highalc)
hybrid_covar_list_u50
hybrid_covar_list_50p<- list(list_height,list_raceeth,list_menarche,list_parous,list_afb,
                             list_bmi,list_alc3,list_EPHRT,list_agemeno)
hybrid_covar_list_50p

#hybrid version
hybrid_formula_u50<-case ~ height + as.factor(raceeth) + as.factor(menarche) + parous + 
  as.factor(afb) + as.factor(bmi) + highalc 
hybrid_formula_50p<-case ~ height + as.factor(raceeth) + as.factor(menarche) + parous + 
  as.factor(afb) + as.factor(bmi) + as.factor(alc3) + EPHRT + as.factor(agemeno)

#real data- covariate file from CARRIERS population
CARRIERS_u50_hybrid_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdata_u50_hybrid_noFH.txt", na.strings =".")
CARRIERS_u50_hybrid_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdata_u50_hybrid_FH.txt", na.strings =".")
CARRIERS_50p_hybrid_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdata_50p_hybrid_noFH.txt", na.strings =".")
CARRIERS_50p_hybrid_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdata_50p_hybrid_FH.txt", na.strings =".")

#model.log.RR = vector with log odds ratios corresponding to the model params; no intercept; 
ORs_hybrid_u50_noFH<-as.data.frame(rep(NA,15))
ORs_hybrid_u50_noFH<-c(log(1.17),
                       log(1.10),log(1.18),log(1.32),log(1.20),
                       log(0.90),log(0.85),log(0.80),log(0.75),
                       log(0.90),
                       log(1.0),log(1.2),log(1.3),
                       log(1.0),log(0.9),
                       log(1.2))
names(ORs_hybrid_u50_noFH)<-c("height",
                              "as.factor(raceeth)2","as.factor(raceeth)3","as.factor(raceeth)4","as.factor(raceeth)5",
                              "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                              "parous",
                              "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                              "as.factor(bmi)2",  "as.factor(bmi)3", 
                              "highalc")

ORs_hybrid_u50_FH<-as.data.frame(rep(NA,15))
ORs_hybrid_u50_FH<-c(log(1.17),
                     log(1.10),log(1.18),log(1.32),log(1.20),
                     log(0.90),log(0.85),log(0.80),log(0.75),
                     log(0.50),
                     log(1.6),log(2.0),log(2.3),
                     log(1.2),log(0.8),
                     log(1.2))
names(ORs_hybrid_u50_FH)<-c("height",
                            "as.factor(raceeth)2","as.factor(raceeth)3","as.factor(raceeth)4","as.factor(raceeth)5",
                            "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                            "parous",
                            "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                            "as.factor(bmi)2",  "as.factor(bmi)3", 
                            "highalc")

ORs_hybrid_50p_noFH <- as.data.frame(rep(NA,20))
ORs_hybrid_50p_noFH<-c(log(1.17),
                       log(1.10),log(1.2),log(1.3),log(1.2),
                       log(0.90),log(0.85),log(0.80),log(0.75),
                       log(0.85),
                       log(1.0),log(1.1),log(1.2),
                       log(1.1),log(1.25),
                       log(1.1),log(1.3),
                       log(1.3),
                       log(1.2),log(1.4),log(1.6))
names(ORs_hybrid_50p_noFH)<-c("height",
                              "as.factor(raceeth)2","as.factor(raceeth)3","as.factor(raceeth)4","as.factor(raceeth)5",
                              "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                              "parous",
                              "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                              "as.factor(bmi)2",  "as.factor(bmi)3", 
                              "as.factor(alc3)2","as.factor(alc3)3", 
                              "EPHRT",
                              "as.factor(agemeno)2","as.factor(agemeno)3","as.factor(agemeno)4")

ORs_hybrid_50p_FH <- as.data.frame(rep(NA,20))
ORs_hybrid_50p_FH<-c(log(1.17),
                     log(1.0),log(0.9),log(1.0),log(1.0),
                     log(1.1),log(1.0),log(0.95),log(0.90),
                     log(0.95),
                     log(1.0),log(1.1),log(1.2),
                     log(1.1),log(1.25),
                     log(1.1),log(1.3),
                     log(1.3),
                     log(1.2),log(1.4),log(1.6))
names(ORs_hybrid_50p_FH)<-c("height",
                            "as.factor(raceeth)2","as.factor(raceeth)3","as.factor(raceeth)4","as.factor(raceeth)5",
                            "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                            "parous",
                            "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                            "as.factor(bmi)2",  "as.factor(bmi)3", 
                            "as.factor(alc3)2","as.factor(alc3)3",
                            "EPHRT",
                            "as.factor(agemeno)2","as.factor(agemeno)3","as.factor(agemeno)4")

#cov profile for reference population = dataframe containing the covariate profiles for which absolute risk will be computed. 
#import sampling weights (same for each version of prediction model)
NHANES_wts_u50_noFH_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_1.txt"))
NHANES_wts_u50_noFH_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_2.txt"))
NHANES_wts_u50_noFH_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_3.txt"))
NHANES_wts_u50_noFH_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_4.txt"))
NHANES_wts_u50_noFH_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_5.txt"))
NHANES_wts_u50_noFH_list<-list(NHANES_wts_u50_noFH_1,NHANES_wts_u50_noFH_2,NHANES_wts_u50_noFH_3,NHANES_wts_u50_noFH_4,NHANES_wts_u50_noFH_5)

NHANES_wts_50p_noFH_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_1.txt"))
NHANES_wts_50p_noFH_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_2.txt"))
NHANES_wts_50p_noFH_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_3.txt"))
NHANES_wts_50p_noFH_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_4.txt"))
NHANES_wts_50p_noFH_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_5.txt"))
NHANES_wts_50p_noFH_list<-list(NHANES_wts_50p_noFH_1,NHANES_wts_50p_noFH_2,NHANES_wts_50p_noFH_3,NHANES_wts_50p_noFH_4,NHANES_wts_50p_noFH_5)

NHANES_wts_u50_FH_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_1.txt"))
NHANES_wts_u50_FH_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_2.txt"))
NHANES_wts_u50_FH_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_3.txt"))
NHANES_wts_u50_FH_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_4.txt"))
NHANES_wts_u50_FH_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_5.txt"))
NHANES_wts_u50_FH_list<-list(NHANES_wts_u50_FH_1,NHANES_wts_u50_FH_2,NHANES_wts_u50_FH_3,NHANES_wts_u50_FH_4,NHANES_wts_u50_FH_5)

NHANES_wts_50p_FH_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_1.txt"))
NHANES_wts_50p_FH_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_2.txt"))
NHANES_wts_50p_FH_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_3.txt"))
NHANES_wts_50p_FH_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_4.txt"))
NHANES_wts_50p_FH_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_5.txt"))
NHANES_wts_50p_FH_list<-list(NHANES_wts_50p_FH_1,NHANES_wts_50p_FH_2,NHANES_wts_50p_FH_3,NHANES_wts_50p_FH_4,NHANES_wts_50p_FH_5)

#import each version of the data set with the correct variable list
NHANES_u50_hybrid_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/outdata_u50_hybrid_noFH.txt")
NHANES_u50_hybrid_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/outdata_u50_hybrid_FH.txt")
NHANES_50p_hybrid_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/outdata_50p_hybrid_noFH.txt")
NHANES_50p_hybrid_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/outdata_50p_hybrid_FH.txt")

for (x in 1:5) {
  assign(paste("NHANES_u50_hybrid_noFH_",x,sep=""),subset(NHANES_u50_hybrid_noFH, NHANES_u50_hybrid_noFH$impute==x)[,1:7] )
  assign(paste("NHANES_u50_hybrid_FH_",x,sep=""),subset(NHANES_u50_hybrid_FH, NHANES_u50_hybrid_FH$impute==x)[,1:7] )
  assign(paste("NHANES_50p_hybrid_noFH_",x,sep=""),subset(NHANES_50p_hybrid_noFH, NHANES_50p_hybrid_noFH$impute==x)[,1:9] )
  assign(paste("NHANES_50p_hybrid_FH_",x,sep=""),subset(NHANES_50p_hybrid_FH, NHANES_50p_hybrid_FH$impute==x)[,1:9] )
}

NHANES_u50_hybrid_noFH_list<-list(NHANES_u50_hybrid_noFH_1,NHANES_u50_hybrid_noFH_2,NHANES_u50_hybrid_noFH_3,NHANES_u50_hybrid_noFH_4,NHANES_u50_hybrid_noFH_5)
NHANES_u50_hybrid_FH_list<-list(NHANES_u50_hybrid_FH_1,NHANES_u50_hybrid_FH_2,NHANES_u50_hybrid_FH_3,NHANES_u50_hybrid_FH_4,NHANES_u50_hybrid_FH_5)
NHANES_50p_hybrid_noFH_list<-list(NHANES_50p_hybrid_noFH_1,NHANES_50p_hybrid_noFH_2,NHANES_50p_hybrid_noFH_3,NHANES_50p_hybrid_noFH_4,NHANES_50p_hybrid_noFH_5)
NHANES_50p_hybrid_FH_list<-list(NHANES_50p_hybrid_FH_1,NHANES_50p_hybrid_FH_2,NHANES_50p_hybrid_FH_3,NHANES_50p_hybrid_FH_4,NHANES_50p_hybrid_FH_5)

set.seed(778899)
PVrisk_hybrid_noFH5_u50_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_noFH5_50p_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_u50_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_50p_1000<-matrix(nrow=1000,ncol=6)

for (j in 1:2){
  cat(paste0("Bootstrap iteration ", j, "\n"))
  
  logOR_PV_noFH<-matrix(nrow=5,ncol=1)
  logOR_PV_FH<-matrix(nrow=5,ncol=1)
  
  for (i in 1:5){
    logOR_PV_noFH[i,]<-rnorm(1,mean=PV_data_noFH5[i,2],sd=PV_data_noFH5[i,3])}
  OR_PV_noFH<-exp(logOR_PV_noFH)
  PV5_noFH<-cbind(PV_data_noFH5[1],OR_PV_noFH,PV_data_noFH5[4])
  names(PV5_noFH)[2]<-"snp.odds.ratio"
  PV5_noFH<-as.data.frame(PV5_noFH)
  PV5_noFH
  
  for (i in 1:5){
    logOR_PV_FH[i,]<-rnorm(1,mean=PV_data_FH5[i,2],sd=PV_data_FH5[i,3])}
  OR_PV_FH<-exp(logOR_PV_FH)
  PV5_FH<-cbind(PV_data_FH5[1],OR_PV_FH,PV_data_FH5[4])
  names(PV5_FH)[2]<-"snp.odds.ratio"
  PV5_FH<-as.data.frame(PV5_FH)
  PV5_FH
  
  for (x in 1:5){
    hybrid5_u50_noFH = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                              model.disease.incidence.rates = Inc_all_noFH,
                                              model.competing.incidence.rates = Mort_all,
                                              apply.age.start = 20,
                                              apply.age.interval.length = 30,
                                              apply.snp.profile=CARRIERS_u50_PVs_noFH5,
                                              model.cov.info = hybrid_covar_list_u50,
                                              model.formula = hybrid_formula_u50,
                                              apply.cov.profile =CARRIERS_u50_hybrid_noFH,
                                              model.log.RR = ORs_hybrid_u50_noFH,
                                              model.ref.dataset= as.data.frame(NHANES_u50_hybrid_noFH_list[[x]]),
                                              model.ref.dataset.weights=NHANES_wts_u50_noFH_list[[x]],
                                              return.refs.risk=TRUE)
    
    noncarriers<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==0
       & hybrid5_u50_noFH$details[,5]==0 & hybrid5_u50_noFH$details[,6]==0 & hybrid5_u50_noFH$details[,7]==0
       & hybrid5_u50_noFH$details[,8]==0))
    ATM<-subset(hybrid5_u50_noFH$details,hybrid5_u50_noFH$details[,4]==1)
    BRCA1<-subset(hybrid5_u50_noFH$details,hybrid5_u50_noFH$details[,5]==1)
    BRCA2<-subset(hybrid5_u50_noFH$details,hybrid5_u50_noFH$details[,6]==1)
    CHEK2<-subset(hybrid5_u50_noFH$details,hybrid5_u50_noFH$details[,7]==1)
    PALB2<-subset(hybrid5_u50_noFH$details,hybrid5_u50_noFH$details[,8]==1)
    
    PVrisk_hybrid_noFH5_u50<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                              mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
    names(PVrisk_hybrid_noFH5_u50)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
    z<-(j-1)*5+x
    PVrisk_hybrid_noFH5_u50_1000[z,]<-PVrisk_hybrid_noFH5_u50
  }
  
  for (x in 1:5){
    hybrid5_50p_noFH = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                           model.disease.incidence.rates = Inc_all_noFH,
                                           model.competing.incidence.rates = Mort_all,
                                           apply.age.start = 50,
                                           apply.age.interval.length = 30,
                                           apply.snp.profile=CARRIERS_50p_PVs_noFH5,
                                           model.cov.info = hybrid_covar_list_50p,
                                           model.formula = hybrid_formula_50p,
                                           apply.cov.profile =CARRIERS_50p_hybrid_noFH,
                                           model.log.RR = ORs_hybrid_50p_noFH,
                                           model.ref.dataset= as.data.frame(NHANES_50p_hybrid_noFH_list[[x]]),
                                           model.ref.dataset.weights=NHANES_wts_50p_noFH_list[[x]],
                                           return.refs.risk=TRUE)
    
    noncarriers<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0
         & hybrid5_50p_noFH$details[,5]==0 & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0
         & hybrid5_50p_noFH$details[,8]==0))
    ATM<-subset(hybrid5_50p_noFH$details,hybrid5_50p_noFH$details[,4]==1)
    BRCA1<-subset(hybrid5_50p_noFH$details,hybrid5_50p_noFH$details[,5]==1)
    BRCA2<-subset(hybrid5_50p_noFH$details,hybrid5_50p_noFH$details[,6]==1)
    CHEK2<-subset(hybrid5_50p_noFH$details,hybrid5_50p_noFH$details[,7]==1)
    PALB2<-subset(hybrid5_50p_noFH$details,hybrid5_50p_noFH$details[,8]==1)
    
    PVrisk_hybrid_noFH5_50p<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                               mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
    names(PVrisk_hybrid_noFH5_50p)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
    z<-(j-1)*5+x
    PVrisk_hybrid_noFH5_50p_1000[z,]<-PVrisk_hybrid_noFH5_50p
  }
  
  for (x in 1:5){
    hybrid5_u50_FH = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                           model.disease.incidence.rates = Inc_all_FH,
                                           model.competing.incidence.rates = Mort_all,
                                           apply.age.start = 20,
                                           apply.age.interval.length = 30,
                                           apply.snp.profile=CARRIERS_u50_PVs_FH5,
                                           model.cov.info = hybrid_covar_list_u50,
                                           model.formula = hybrid_formula_u50,
                                           apply.cov.profile =CARRIERS_u50_hybrid_FH,
                                           model.log.RR = ORs_hybrid_u50_FH,
                                           model.ref.dataset= as.data.frame(NHANES_u50_hybrid_FH_list[[x]]),
                                           model.ref.dataset.weights=NHANES_wts_u50_FH_list[[x]],
                                           return.refs.risk=TRUE)
    
    noncarriers<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==0
        & hybrid5_u50_FH$details[,5]==0 & hybrid5_u50_FH$details[,6]==0 & hybrid5_u50_FH$details[,7]==0
        & hybrid5_u50_FH$details[,8]==0))
    ATM<-subset(hybrid5_u50_FH$details,hybrid5_u50_FH$details[,4]==1)
    BRCA1<-subset(hybrid5_u50_FH$details,hybrid5_u50_FH$details[,5]==1)
    BRCA2<-subset(hybrid5_u50_FH$details,hybrid5_u50_FH$details[,6]==1)
    CHEK2<-subset(hybrid5_u50_FH$details,hybrid5_u50_FH$details[,7]==1)
    PALB2<-subset(hybrid5_u50_FH$details,hybrid5_u50_FH$details[,8]==1)
    
    PVrisk_hybrid_FH5_u50<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                               mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
    names(PVrisk_hybrid_FH5_u50)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
    z<-(j-1)*5+x
    PVrisk_hybrid_FH5_u50_1000[z,]<-PVrisk_hybrid_FH5_u50
  }
  
  for (x in 1:5){
    hybrid5_50p_FH = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                           model.disease.incidence.rates = Inc_all_FH,
                                           model.competing.incidence.rates = Mort_all,
                                           apply.age.start = 50,
                                           apply.age.interval.length = 30,
                                           apply.snp.profile=CARRIERS_50p_PVs_FH5,
                                           model.cov.info = hybrid_covar_list_50p,
                                           model.formula = hybrid_formula_50p,
                                           apply.cov.profile =CARRIERS_50p_hybrid_FH,
                                           model.log.RR = ORs_hybrid_50p_FH,
                                           model.ref.dataset= as.data.frame(NHANES_50p_hybrid_FH_list[[x]]),
                                           model.ref.dataset.weights=NHANES_wts_50p_FH_list[[x]],
                                           return.refs.risk=TRUE)
    
    noncarriers<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0
      & hybrid5_50p_FH$details[,5]==0 & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0
      & hybrid5_50p_FH$details[,8]==0))
    ATM<-subset(hybrid5_50p_FH$details,hybrid5_50p_FH$details[,4]==1)
    BRCA1<-subset(hybrid5_50p_FH$details,hybrid5_50p_FH$details[,5]==1)
    BRCA2<-subset(hybrid5_50p_FH$details,hybrid5_50p_FH$details[,6]==1)
    CHEK2<-subset(hybrid5_50p_FH$details,hybrid5_50p_FH$details[,7]==1)
    PALB2<-subset(hybrid5_50p_FH$details,hybrid5_50p_FH$details[,8]==1)
    
    PVrisk_hybrid_FH5_50p<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                               mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
    names(PVrisk_hybrid_FH5_50p)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
    z<-(j-1)*5+x
    PVrisk_hybrid_FH5_50p_1000[z,]<-PVrisk_hybrid_FH5_50p
  }
}

hybrid5_u50_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_noFH_results[k,]<-quantile(PVrisk_hybrid_noFH5_u50_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_noFH_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_noFH_results
write.csv(hybrid5_u50_noFH_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_noFH_results.csv")

hybrid5_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_noFH_results[k,]<-quantile(PVrisk_hybrid_noFH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_noFH_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_noFH_results
write.csv(hybrid5_50p_noFH_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_noFH_results.csv")

hybrid5_u50_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_FH_results[k,]<-quantile(PVrisk_hybrid_FH5_u50_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_FH_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_FH_results
write.csv(hybrid5_u50_FH_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_FH_results.csv")

hybrid5_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_FH_results[k,]<-quantile(PVrisk_hybrid_FH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_FH_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_FH_results
write.csv(hybrid5_50p_FH_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_FH_results.csv")

### for race specific results, use race specific incidence and mortality
### drop race/ethnicity from all models/ formulas

Inc_NHW_u50<-Inc_NHW[1:50,]
Inc_NHW_50p<-Inc_NHW[51:90,]
Inc_Black_u50<-Inc_Black[1:50,]
Inc_Black_50p<-Inc_Black[51:90,]
Inc_Hisp_u50<-Inc_Hisp[1:50,]
Inc_Hisp_50p<-Inc_Hisp[51:90,]
Inc_Asian_u50<-Inc_Asian[1:50,]
Inc_Asian_50p<-Inc_Asian[51:90,]

rates_noFH_NHW_u50<-Inc_NHW_u50/1.045
rates_noFH_Black_u50<-Inc_Black_u50/1.045
rates_noFH_Hisp_u50<-Inc_Hisp_u50/1.045
rates_noFH_Asian_u50<-Inc_Asian_u50/1.045
rates_noFH_NHW_50p<-Inc_NHW_50p/1.03
rates_noFH_Black_50p<-Inc_Black_50p/1.03
rates_noFH_Hisp_50p<-Inc_Hisp_50p/1.03
rates_noFH_Asian_50p<-Inc_Asian_50p/1.03
rates_noFH_NHW<-rbind(rates_noFH_NHW_u50,rates_noFH_NHW_50p)
rates_noFH_Black<-rbind(rates_noFH_Black_u50,rates_noFH_Black_50p)
rates_noFH_Hisp<-rbind(rates_noFH_Hisp_u50,rates_noFH_Hisp_50p)
rates_noFH_Asian<-rbind(rates_noFH_Asian_u50,rates_noFH_Asian_50p)
Inc_NHW_noFH<-cbind(Inc_NHW[,1],rates_noFH_NHW[,2])
Inc_Black_noFH<-cbind(Inc_Black[,1],rates_noFH_Black[,2])
Inc_Hisp_noFH<-cbind(Inc_Hisp[,1],rates_noFH_Hisp[,2])
Inc_Asian_noFH<-cbind(Inc_Asian[,1],rates_noFH_Asian[,2])

rates_FH_NHW_u50<-rates_noFH_NHW_u50*2.5
rates_FH_Black_u50<-rates_noFH_Black_u50*2.5
rates_FH_Hisp_u50<-rates_noFH_Hisp_u50*2.5
rates_FH_Asian_u50<-rates_noFH_Asian_u50*2.5
rates_FH_NHW_50p<-rates_noFH_NHW_50p*1.6
rates_FH_Black_50p<-rates_noFH_Black_50p*1.6
rates_FH_Hisp_50p<-rates_noFH_Hisp_50p*1.6
rates_FH_Asian_50p<-rates_noFH_Asian_50p*1.6
rates_FH_NHW<-rbind(rates_FH_NHW_u50,rates_FH_NHW_50p)
rates_FH_Black<-rbind(rates_FH_Black_u50,rates_FH_Black_50p)
rates_FH_Hisp<-rbind(rates_FH_Hisp_u50,rates_FH_Hisp_50p)
rates_FH_Asian<-rbind(rates_FH_Asian_u50,rates_FH_Asian_50p)
Inc_NHW_FH<-cbind(Inc_NHW[,1],rates_FH_NHW[,2])
Inc_Black_FH<-cbind(Inc_Black[,1],rates_FH_Black[,2])
Inc_Hisp_FH<-cbind(Inc_Hisp[,1],rates_FH_Hisp[,2])
Inc_Asian_FH<-cbind(Inc_Asian[,1],rates_FH_Asian[,2])

#hybrid covar list without raceeth
hybrid_covar_list_u50_norace<- list(list_height,list_menarche,list_parous,list_afb,
                             list_bmi,list_highalc)
hybrid_covar_list_u50_norace
hybrid_covar_list_50p_norace<- list(list_height,list_menarche,list_parous,list_afb,
                             list_bmi,list_alc3,list_EPHRT,list_agemeno)
hybrid_covar_list_50p_norace

#hybrid formula without raceeth
hybrid_formula_u50_norace<-case ~ height + as.factor(menarche) + parous + as.factor(afb) + as.factor(bmi) + highalc 
hybrid_formula_50p_norace<-case ~ height + as.factor(menarche) + parous + as.factor(afb) + as.factor(bmi) + as.factor(alc3) + EPHRT + as.factor(agemeno)

#import sampling weights for each age/FH/race group
NHANES_wts_u50_noFH_NHW_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_NHW_1.txt"))
NHANES_wts_u50_noFH_NHW_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_NHW_2.txt"))
NHANES_wts_u50_noFH_NHW_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_NHW_3.txt"))
NHANES_wts_u50_noFH_NHW_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_NHW_4.txt"))
NHANES_wts_u50_noFH_NHW_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_NHW_5.txt"))
NHANES_wts_u50_noFH_listNHW<-list(NHANES_wts_u50_noFH_NHW_1,NHANES_wts_u50_noFH_NHW_2,NHANES_wts_u50_noFH_NHW_3,NHANES_wts_u50_noFH_NHW_4,NHANES_wts_u50_noFH_NHW_5)
NHANES_wts_50p_noFH_NHW_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_NHW_1.txt"))
NHANES_wts_50p_noFH_NHW_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_NHW_2.txt"))
NHANES_wts_50p_noFH_NHW_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_NHW_3.txt"))
NHANES_wts_50p_noFH_NHW_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_NHW_4.txt"))
NHANES_wts_50p_noFH_NHW_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_NHW_5.txt"))
NHANES_wts_50p_noFH_listNHW<-list(NHANES_wts_50p_noFH_NHW_1,NHANES_wts_50p_noFH_NHW_2,NHANES_wts_50p_noFH_NHW_3,NHANES_wts_50p_noFH_NHW_4,NHANES_wts_50p_noFH_NHW_5)
NHANES_wts_u50_FH_NHW_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_NHW_1.txt"))
NHANES_wts_u50_FH_NHW_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_NHW_2.txt"))
NHANES_wts_u50_FH_NHW_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_NHW_3.txt"))
NHANES_wts_u50_FH_NHW_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_NHW_4.txt"))
NHANES_wts_u50_FH_NHW_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_NHW_5.txt"))
NHANES_wts_u50_FH_listNHW<-list(NHANES_wts_u50_FH_NHW_1,NHANES_wts_u50_FH_NHW_2,NHANES_wts_u50_FH_NHW_3,NHANES_wts_u50_FH_NHW_4,NHANES_wts_u50_FH_NHW_5)
NHANES_wts_50p_FH_NHW_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_NHW_1.txt"))
NHANES_wts_50p_FH_NHW_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_NHW_2.txt"))
NHANES_wts_50p_FH_NHW_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_NHW_3.txt"))
NHANES_wts_50p_FH_NHW_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_NHW_4.txt"))
NHANES_wts_50p_FH_NHW_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_NHW_5.txt"))
NHANES_wts_50p_FH_listNHW<-list(NHANES_wts_50p_FH_NHW_1,NHANES_wts_50p_FH_NHW_2,NHANES_wts_50p_FH_NHW_3,NHANES_wts_50p_FH_NHW_4,NHANES_wts_50p_FH_NHW_5)

NHANES_wts_u50_noFH_Black_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Black_1.txt"))
NHANES_wts_u50_noFH_Black_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Black_2.txt"))
NHANES_wts_u50_noFH_Black_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Black_3.txt"))
NHANES_wts_u50_noFH_Black_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Black_4.txt"))
NHANES_wts_u50_noFH_Black_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Black_5.txt"))
NHANES_wts_u50_noFH_listBlack_<-list(NHANES_wts_u50_noFH_Black_1,NHANES_wts_u50_noFH_Black_2,NHANES_wts_u50_noFH_Black_3,NHANES_wts_u50_noFH_Black_4,NHANES_wts_u50_noFH_Black_5)
NHANES_wts_50p_noFH_Black_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Black_1.txt"))
NHANES_wts_50p_noFH_Black_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Black_2.txt"))
NHANES_wts_50p_noFH_Black_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Black_3.txt"))
NHANES_wts_50p_noFH_Black_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Black_4.txt"))
NHANES_wts_50p_noFH_Black_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Black_5.txt"))
NHANES_wts_50p_noFH_listBlack<-list(NHANES_wts_50p_noFH_Black_1,NHANES_wts_50p_noFH_Black_2,NHANES_wts_50p_noFH_Black_3,NHANES_wts_50p_noFH_Black_4,NHANES_wts_50p_noFH_Black_5)
NHANES_wts_u50_FH_Black_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Black_1.txt"))
NHANES_wts_u50_FH_Black_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Black_2.txt"))
NHANES_wts_u50_FH_Black_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Black_3.txt"))
NHANES_wts_u50_FH_Black_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Black_4.txt"))
NHANES_wts_u50_FH_Black_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Black_5.txt"))
NHANES_wts_u50_FH_listBlack<-list(NHANES_wts_u50_FH_Black_1,NHANES_wts_u50_FH_Black_2,NHANES_wts_u50_FH_Black_3,NHANES_wts_u50_FH_Black_4,NHANES_wts_u50_FH_Black_5)
NHANES_wts_50p_FH_Black_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Black_1.txt"))
NHANES_wts_50p_FH_Black_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Black_2.txt"))
NHANES_wts_50p_FH_Black_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Black_3.txt"))
NHANES_wts_50p_FH_Black_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Black_4.txt"))
NHANES_wts_50p_FH_Black_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Black_5.txt"))
NHANES_wts_50p_FH_listBlack<-list(NHANES_wts_50p_FH_Black_1,NHANES_wts_50p_FH_Black_2,NHANES_wts_50p_FH_Black_3,NHANES_wts_50p_FH_Black_4,NHANES_wts_50p_FH_Black_5)

NHANES_wts_u50_noFH_Hisp_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Hisp_1.txt"))
NHANES_wts_u50_noFH_Hisp_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Hisp_2.txt"))
NHANES_wts_u50_noFH_Hisp_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Hisp_3.txt"))
NHANES_wts_u50_noFH_Hisp_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Hisp_4.txt"))
NHANES_wts_u50_noFH_Hisp_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Hisp_5.txt"))
NHANES_wts_u50_noFH_listHisp_<-list(NHANES_wts_u50_noFH_Hisp_1,NHANES_wts_u50_noFH_Hisp_2,NHANES_wts_u50_noFH_Hisp_3,NHANES_wts_u50_noFH_Hisp_4,NHANES_wts_u50_noFH_Hisp_5)
NHANES_wts_50p_noFH_Hisp_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Hisp_1.txt"))
NHANES_wts_50p_noFH_Hisp_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Hisp_2.txt"))
NHANES_wts_50p_noFH_Hisp_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Hisp_3.txt"))
NHANES_wts_50p_noFH_Hisp_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Hisp_4.txt"))
NHANES_wts_50p_noFH_Hisp_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Hisp_5.txt"))
NHANES_wts_50p_noFH_listHisp<-list(NHANES_wts_50p_noFH_Hisp_1,NHANES_wts_50p_noFH_Hisp_2,NHANES_wts_50p_noFH_Hisp_3,NHANES_wts_50p_noFH_Hisp_4,NHANES_wts_50p_noFH_Hisp_5)
NHANES_wts_u50_FH_Hisp_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Hisp_1.txt"))
NHANES_wts_u50_FH_Hisp_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Hisp_2.txt"))
NHANES_wts_u50_FH_Hisp_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Hisp_3.txt"))
NHANES_wts_u50_FH_Hisp_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Hisp_4.txt"))
NHANES_wts_u50_FH_Hisp_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Hisp_5.txt"))
NHANES_wts_u50_FH_listHisp<-list(NHANES_wts_u50_FH_Hisp_1,NHANES_wts_u50_FH_Hisp_2,NHANES_wts_u50_FH_Hisp_3,NHANES_wts_u50_FH_Hisp_4,NHANES_wts_u50_FH_Hisp_5)
NHANES_wts_50p_FH_Hisp_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Hisp_1.txt"))
NHANES_wts_50p_FH_Hisp_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Hisp_2.txt"))
NHANES_wts_50p_FH_Hisp_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Hisp_3.txt"))
NHANES_wts_50p_FH_Hisp_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Hisp_4.txt"))
NHANES_wts_50p_FH_Hisp_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Hisp_5.txt"))
NHANES_wts_50p_FH_listHisp<-list(NHANES_wts_50p_FH_Hisp_1,NHANES_wts_50p_FH_Hisp_2,NHANES_wts_50p_FH_Hisp_3,NHANES_wts_50p_FH_Hisp_4,NHANES_wts_50p_FH_Hisp_5)

NHANES_wts_u50_noFH_Asian_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Asian_1.txt"))
NHANES_wts_u50_noFH_Asian_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Asian_2.txt"))
NHANES_wts_u50_noFH_Asian_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Asian_3.txt"))
NHANES_wts_u50_noFH_Asian_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Asian_4.txt"))
NHANES_wts_u50_noFH_Asian_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_noFH_Asian_5.txt"))
NHANES_wts_u50_noFH_listAsian_<-list(NHANES_wts_u50_noFH_Asian_1,NHANES_wts_u50_noFH_Asian_2,NHANES_wts_u50_noFH_Asian_3,NHANES_wts_u50_noFH_Asian_4,NHANES_wts_u50_noFH_Asian_5)
NHANES_wts_50p_noFH_Asian_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Asian_1.txt"))
NHANES_wts_50p_noFH_Asian_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Asian_2.txt"))
NHANES_wts_50p_noFH_Asian_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Asian_3.txt"))
NHANES_wts_50p_noFH_Asian_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Asian_4.txt"))
NHANES_wts_50p_noFH_Asian_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_noFH_Asian_5.txt"))
NHANES_wts_50p_noFH_listAsian<-list(NHANES_wts_50p_noFH_Asian_1,NHANES_wts_50p_noFH_Asian_2,NHANES_wts_50p_noFH_Asian_3,NHANES_wts_50p_noFH_Asian_4,NHANES_wts_50p_noFH_Asian_5)
NHANES_wts_u50_FH_Asian_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Asian_1.txt"))
NHANES_wts_u50_FH_Asian_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Asian_2.txt"))
NHANES_wts_u50_FH_Asian_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Asian_3.txt"))
NHANES_wts_u50_FH_Asian_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Asian_4.txt"))
NHANES_wts_u50_FH_Asian_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_u50_FH_Asian_5.txt"))
NHANES_wts_u50_FH_listAsian<-list(NHANES_wts_u50_FH_Asian_1,NHANES_wts_u50_FH_Asian_2,NHANES_wts_u50_FH_Asian_3,NHANES_wts_u50_FH_Asian_4,NHANES_wts_u50_FH_Asian_5)
NHANES_wts_50p_FH_Asian_1<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Asian_1.txt"))
NHANES_wts_50p_FH_Asian_2<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Asian_2.txt"))
NHANES_wts_50p_FH_Asian_3<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Asian_3.txt"))
NHANES_wts_50p_FH_Asian_4<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Asian_4.txt"))
NHANES_wts_50p_FH_Asian_5<-as.matrix(read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/wts_50p_FH_Asian_5.txt"))
NHANES_wts_50p_FH_listAsian<-list(NHANES_wts_50p_FH_Asian_1,NHANES_wts_50p_FH_Asian_2,NHANES_wts_50p_FH_Asian_3,NHANES_wts_50p_FH_Asian_4,NHANES_wts_50p_FH_Asian_5)

NHANES_u50_hybrid_noFH_NHW<-subset(NHANES_u50_hybrid_noFH,NHANES_u50_hybrid_noFH$raceeth==1)
NHANES_50p_hybrid_noFH_NHW<-subset(NHANES_50p_hybrid_noFH,NHANES_50p_hybrid_noFH$raceeth==1)
NHANES_u50_hybrid_FH_NHW<-subset(NHANES_u50_hybrid_FH,NHANES_u50_hybrid_FH$raceeth==1)
NHANES_50p_hybrid_FH_NHW<-subset(NHANES_50p_hybrid_FH,NHANES_50p_hybrid_FH$raceeth==1)

NHANES_u50_hybrid_noFH_Black<-subset(NHANES_u50_hybrid_noFH,NHANES_u50_hybrid_noFH$raceeth==2)
NHANES_50p_hybrid_noFH_Black<-subset(NHANES_50p_hybrid_noFH,NHANES_50p_hybrid_noFH$raceeth==2)
NHANES_u50_hybrid_FH_Black<-subset(NHANES_u50_hybrid_FH,NHANES_u50_hybrid_FH$raceeth==2)
NHANES_50p_hybrid_FH_Black<-subset(NHANES_50p_hybrid_FH,NHANES_50p_hybrid_FH$raceeth==2)

NHANES_u50_hybrid_noFH_Hisp<-subset(NHANES_u50_hybrid_noFH,NHANES_u50_hybrid_noFH$raceeth==3)
NHANES_50p_hybrid_noFH_Hisp<-subset(NHANES_50p_hybrid_noFH,NHANES_50p_hybrid_noFH$raceeth==3)
NHANES_u50_hybrid_FH_Hisp<-subset(NHANES_u50_hybrid_FH,NHANES_u50_hybrid_FH$raceeth==3)
NHANES_50p_hybrid_FH_Hisp<-subset(NHANES_50p_hybrid_FH,NHANES_50p_hybrid_FH$raceeth==3)

NHANES_u50_hybrid_noFH_Asian<-subset(NHANES_u50_hybrid_noFH,NHANES_u50_hybrid_noFH$raceeth==4)
NHANES_50p_hybrid_noFH_Asian<-subset(NHANES_50p_hybrid_noFH,NHANES_50p_hybrid_noFH$raceeth==4)
NHANES_u50_hybrid_FH_Asian<-subset(NHANES_u50_hybrid_FH,NHANES_u50_hybrid_FH$raceeth==4)
NHANES_50p_hybrid_FH_Asian<-subset(NHANES_50p_hybrid_FH,NHANES_50p_hybrid_FH$raceeth==4)

for (x in 1:5) {
  assign(paste("NHANES_u50_hybrid_noFH_NHW_",x,sep=""),subset(NHANES_u50_hybrid_noFH_NHW, NHANES_u50_hybrid_noFH_NHW$impute==x)[,c(1,3:7)] )
  assign(paste("NHANES_u50_hybrid_FH_NHW_",x,sep=""),subset(NHANES_u50_hybrid_FH_NHW, NHANES_u50_hybrid_FH_NHW$impute==x)[,c(1,3:7)] )
  assign(paste("NHANES_50p_hybrid_noFH_NHW_",x,sep=""),subset(NHANES_50p_hybrid_noFH_NHW, NHANES_50p_hybrid_noFH_NHW$impute==x)[,c(1,3:9)] )
  assign(paste("NHANES_50p_hybrid_FH_NHW_",x,sep=""),subset(NHANES_50p_hybrid_FH_NHW, NHANES_50p_hybrid_FH_NHW$impute==x)[,c(1,3:9)] )
}
NHANES_u50_hybrid_noFH_listNHW<-list(NHANES_u50_hybrid_noFH_NHW_1,NHANES_u50_hybrid_noFH_NHW_2,NHANES_u50_hybrid_noFH_NHW_3,NHANES_u50_hybrid_noFH_NHW_4,NHANES_u50_hybrid_noFH_NHW_5)
NHANES_u50_hybrid_FH_listNHW<-list(NHANES_u50_hybrid_FH_NHW_1,NHANES_u50_hybrid_FH_NHW_2,NHANES_u50_hybrid_FH_NHW_3,NHANES_u50_hybrid_FH_NHW_4,NHANES_u50_hybrid_FH_NHW_5)
NHANES_50p_hybrid_noFH_listNHW<-list(NHANES_50p_hybrid_noFH_NHW_1,NHANES_50p_hybrid_noFH_NHW_2,NHANES_50p_hybrid_noFH_NHW_3,NHANES_50p_hybrid_noFH_NHW_4,NHANES_50p_hybrid_noFH_NHW_5)
NHANES_50p_hybrid_FH_listNHW<-list(NHANES_50p_hybrid_FH_NHW_1,NHANES_50p_hybrid_FH_NHW_2,NHANES_50p_hybrid_FH_NHW_3,NHANES_50p_hybrid_FH_NHW_4,NHANES_50p_hybrid_FH_NHW_5)

for (x in 1:5) {
  assign(paste("NHANES_u50_hybrid_noFH_Black_",x,sep=""),subset(NHANES_u50_hybrid_noFH_Black, NHANES_u50_hybrid_noFH_Black$impute==x)[,c(1,3:7)] )
  assign(paste("NHANES_u50_hybrid_FH_Black_",x,sep=""),subset(NHANES_u50_hybrid_FH_Black, NHANES_u50_hybrid_FH_Black$impute==x)[,c(1,3:7)] )
  assign(paste("NHANES_50p_hybrid_noFH_Black_",x,sep=""),subset(NHANES_50p_hybrid_noFH_Black, NHANES_50p_hybrid_noFH_Black$impute==x)[,c(1,3:9)] )
  assign(paste("NHANES_50p_hybrid_FH_Black_",x,sep=""),subset(NHANES_50p_hybrid_FH_Black, NHANES_50p_hybrid_FH_Black$impute==x)[,c(1,3:9)] )
}
NHANES_u50_hybrid_noFH_listBlack<-list(NHANES_u50_hybrid_noFH_Black_1,NHANES_u50_hybrid_noFH_Black_2,NHANES_u50_hybrid_noFH_Black_3,NHANES_u50_hybrid_noFH_Black_4,NHANES_u50_hybrid_noFH_Black_5)
NHANES_u50_hybrid_FH_listBlack<-list(NHANES_u50_hybrid_FH_Black_1,NHANES_u50_hybrid_FH_Black_2,NHANES_u50_hybrid_FH_Black_3,NHANES_u50_hybrid_FH_Black_4,NHANES_u50_hybrid_FH_Black_5)
NHANES_50p_hybrid_noFH_listBlack<-list(NHANES_50p_hybrid_noFH_Black_1,NHANES_50p_hybrid_noFH_Black_2,NHANES_50p_hybrid_noFH_Black_3,NHANES_50p_hybrid_noFH_Black_4,NHANES_50p_hybrid_noFH_Black_5)
NHANES_50p_hybrid_FH_listBlack<-list(NHANES_50p_hybrid_FH_Black_1,NHANES_50p_hybrid_FH_Black_2,NHANES_50p_hybrid_FH_Black_3,NHANES_50p_hybrid_FH_Black_4,NHANES_50p_hybrid_FH_Black_5)

for (x in 1:5) {
  assign(paste("NHANES_u50_hybrid_noFH_Hisp_",x,sep=""),subset(NHANES_u50_hybrid_noFH_Hisp, NHANES_u50_hybrid_noFH_Hisp$impute==x)[,c(1,3:7)] )
  assign(paste("NHANES_u50_hybrid_FH_Hisp_",x,sep=""),subset(NHANES_u50_hybrid_FH_Hisp, NHANES_u50_hybrid_FH_Hisp$impute==x)[,c(1,3:7)] )
  assign(paste("NHANES_50p_hybrid_noFH_Hisp_",x,sep=""),subset(NHANES_50p_hybrid_noFH_Hisp, NHANES_50p_hybrid_noFH_Hisp$impute==x)[,c(1,3:9)] )
  assign(paste("NHANES_50p_hybrid_FH_Hisp_",x,sep=""),subset(NHANES_50p_hybrid_FH_Hisp, NHANES_50p_hybrid_FH_Hisp$impute==x)[,c(1,3:9)] )
}
NHANES_u50_hybrid_noFH_listHisp<-list(NHANES_u50_hybrid_noFH_Hisp_1,NHANES_u50_hybrid_noFH_Hisp_2,NHANES_u50_hybrid_noFH_Hisp_3,NHANES_u50_hybrid_noFH_Hisp_4,NHANES_u50_hybrid_noFH_Hisp_5)
NHANES_u50_hybrid_FH_listHisp<-list(NHANES_u50_hybrid_FH_Hisp_1,NHANES_u50_hybrid_FH_Hisp_2,NHANES_u50_hybrid_FH_Hisp_3,NHANES_u50_hybrid_FH_Hisp_4,NHANES_u50_hybrid_FH_Hisp_5)
NHANES_50p_hybrid_noFH_listHisp<-list(NHANES_50p_hybrid_noFH_Hisp_1,NHANES_50p_hybrid_noFH_Hisp_2,NHANES_50p_hybrid_noFH_Hisp_3,NHANES_50p_hybrid_noFH_Hisp_4,NHANES_50p_hybrid_noFH_Hisp_5)
NHANES_50p_hybrid_FH_listHisp<-list(NHANES_50p_hybrid_FH_Hisp_1,NHANES_50p_hybrid_FH_Hisp_2,NHANES_50p_hybrid_FH_Hisp_3,NHANES_50p_hybrid_FH_Hisp_4,NHANES_50p_hybrid_FH_Hisp_5)

for (x in 1:5) {
  assign(paste("NHANES_u50_hybrid_noFH_Asian_",x,sep=""),subset(NHANES_u50_hybrid_noFH_Asian, NHANES_u50_hybrid_noFH_Asian$impute==x)[,c(1,3:7)] )
  assign(paste("NHANES_u50_hybrid_FH_Asian_",x,sep=""),subset(NHANES_u50_hybrid_FH_Asian, NHANES_u50_hybrid_FH_Asian$impute==x)[,c(1,3:7)] )
  assign(paste("NHANES_50p_hybrid_noFH_Asian_",x,sep=""),subset(NHANES_50p_hybrid_noFH_Asian, NHANES_50p_hybrid_noFH_Asian$impute==x)[,c(1,3:9)] )
  assign(paste("NHANES_50p_hybrid_FH_Asian_",x,sep=""),subset(NHANES_50p_hybrid_FH_Asian, NHANES_50p_hybrid_FH_Asian$impute==x)[,c(1,3:9)] )
}
NHANES_u50_hybrid_noFH_listAsian<-list(NHANES_u50_hybrid_noFH_Asian_1,NHANES_u50_hybrid_noFH_Asian_2,NHANES_u50_hybrid_noFH_Asian_3,NHANES_u50_hybrid_noFH_Asian_4,NHANES_u50_hybrid_noFH_Asian_5)
NHANES_u50_hybrid_FH_listAsian<-list(NHANES_u50_hybrid_FH_Asian_1,NHANES_u50_hybrid_FH_Asian_2,NHANES_u50_hybrid_FH_Asian_3,NHANES_u50_hybrid_FH_Asian_4,NHANES_u50_hybrid_FH_Asian_5)
NHANES_50p_hybrid_noFH_listAsian<-list(NHANES_50p_hybrid_noFH_Asian_1,NHANES_50p_hybrid_noFH_Asian_2,NHANES_50p_hybrid_noFH_Asian_3,NHANES_50p_hybrid_noFH_Asian_4,NHANES_50p_hybrid_noFH_Asian_5)
NHANES_50p_hybrid_FH_listAsian<-list(NHANES_50p_hybrid_FH_Asian_1,NHANES_50p_hybrid_FH_Asian_2,NHANES_50p_hybrid_FH_Asian_3,NHANES_50p_hybrid_FH_Asian_4,NHANES_50p_hybrid_FH_Asian_5)

CARRIERS_u50_hybrid_noFH_NHW<-subset(CARRIERS_u50_hybrid_noFH,CARRIERS_u50_hybrid_noFH$raceeth==1)[,c(1,3:7)]
CARRIERS_50p_hybrid_noFH_NHW<-subset(CARRIERS_50p_hybrid_noFH,CARRIERS_50p_hybrid_noFH$raceeth==1)[,c(1,3:9)]
CARRIERS_u50_hybrid_FH_NHW<-subset(CARRIERS_u50_hybrid_FH,CARRIERS_u50_hybrid_FH$raceeth==1)[,c(1,3:7)]
CARRIERS_50p_hybrid_FH_NHW<-subset(CARRIERS_50p_hybrid_FH,CARRIERS_50p_hybrid_FH$raceeth==1)[,c(1,3:9)]

CARRIERS_u50_hybrid_noFH_Black<-subset(CARRIERS_u50_hybrid_noFH,CARRIERS_u50_hybrid_noFH$raceeth==2)[,c(1,3:7)]
CARRIERS_50p_hybrid_noFH_Black<-subset(CARRIERS_50p_hybrid_noFH,CARRIERS_50p_hybrid_noFH$raceeth==2)[,c(1,3:9)]
CARRIERS_u50_hybrid_FH_Black<-subset(CARRIERS_u50_hybrid_FH,CARRIERS_u50_hybrid_FH$raceeth==2)[,c(1,3:7)]
CARRIERS_50p_hybrid_FH_Black<-subset(CARRIERS_50p_hybrid_FH,CARRIERS_50p_hybrid_FH$raceeth==2)[,c(1,3:9)]

CARRIERS_u50_hybrid_noFH_Hisp<-subset(CARRIERS_u50_hybrid_noFH,CARRIERS_u50_hybrid_noFH$raceeth==3)[,c(1,3:7)]
CARRIERS_50p_hybrid_noFH_Hisp<-subset(CARRIERS_50p_hybrid_noFH,CARRIERS_50p_hybrid_noFH$raceeth==3)[,c(1,3:9)]
CARRIERS_u50_hybrid_FH_Hisp<-subset(CARRIERS_u50_hybrid_FH,CARRIERS_u50_hybrid_FH$raceeth==3)[,c(1,3:7)]
CARRIERS_50p_hybrid_FH_Hisp<-subset(CARRIERS_50p_hybrid_FH,CARRIERS_50p_hybrid_FH$raceeth==3)[,c(1,3:9)]

CARRIERS_u50_hybrid_noFH_Asian<-subset(CARRIERS_u50_hybrid_noFH,CARRIERS_u50_hybrid_noFH$raceeth==4)[,c(1,3:7)]
CARRIERS_50p_hybrid_noFH_Asian<-subset(CARRIERS_50p_hybrid_noFH,CARRIERS_50p_hybrid_noFH$raceeth==4)[,c(1,3:9)]
CARRIERS_u50_hybrid_FH_Asian<-subset(CARRIERS_u50_hybrid_FH,CARRIERS_u50_hybrid_FH$raceeth==4)[,c(1,3:7)]
CARRIERS_50p_hybrid_FH_Asian<-subset(CARRIERS_50p_hybrid_FH,CARRIERS_50p_hybrid_FH$raceeth==4)[,c(1,3:9)]

ORs_hybrid_u50_noFH_norace<-as.data.frame(rep(NA,11))
ORs_hybrid_u50_noFH_norace<-c(log(1.17),
                       log(0.90),log(0.85),log(0.80),log(0.75),
                       log(0.90),
                       log(1.0),log(1.2),log(1.3),
                       log(1.0),log(0.9),
                       log(1.2))
names(ORs_hybrid_u50_noFH_norace)<-c("height",
                              "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                              "parous",
                              "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                              "as.factor(bmi)2",  "as.factor(bmi)3", 
                              "highalc")

ORs_hybrid_u50_FH_norace<-as.data.frame(rep(NA,11))
ORs_hybrid_u50_FH_norace<-c(log(1.17),
                     log(0.90),log(0.85),log(0.80),log(0.75),
                     log(0.50),
                     log(1.6),log(2.0),log(2.3),
                     log(1.2),log(0.8),
                     log(1.2))
names(ORs_hybrid_u50_FH_norace)<-c("height",
                            "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                            "parous",
                            "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                            "as.factor(bmi)2",  "as.factor(bmi)3", 
                            "highalc")

ORs_hybrid_50p_noFH_norace <- as.data.frame(rep(NA,16))
ORs_hybrid_50p_noFH_norace<-c(log(1.17),
                       log(0.90),log(0.85),log(0.80),log(0.75),
                       log(0.85),
                       log(1.0),log(1.1),log(1.2),
                       log(1.1),log(1.25),
                       log(1.1),log(1.3),
                       log(1.3),
                       log(1.2),log(1.4),log(1.6))
names(ORs_hybrid_50p_noFH_norace)<-c("height",
                              "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                              "parous",
                              "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                              "as.factor(bmi)2",  "as.factor(bmi)3", 
                              "as.factor(alc3)2","as.factor(alc3)3", 
                              "EPHRT",
                              "as.factor(agemeno)2","as.factor(agemeno)3","as.factor(agemeno)4")

ORs_hybrid_50p_FH_norace <- as.data.frame(rep(NA,16))
ORs_hybrid_50p_FH_norace<-c(log(1.17),
                     log(1.1),log(1.0),log(0.95),log(0.90),
                     log(0.95),
                     log(1.0),log(1.1),log(1.2),
                     log(1.1),log(1.25),
                     log(1.1),log(1.3),
                     log(1.3),
                     log(1.2),log(1.4),log(1.6))
names(ORs_hybrid_50p_FH_norace)<-c("height",
                            "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                            "parous",
                            "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                            "as.factor(bmi)2",  "as.factor(bmi)3", 
                            "as.factor(alc3)2","as.factor(alc3)3",
                            "EPHRT",
                            "as.factor(agemeno)2","as.factor(agemeno)3","as.factor(agemeno)4")

##need race specific CARRIERS PV5 data
PVs5_u50_NHW_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_NHW_noFH.txt")[,c(1,3:5,8)]
PVs5_u50_Black_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_Black_noFH.txt")[,c(1,3:5,8)]
PVs5_u50_Hisp_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_Hisp_noFH.txt")[,c(1,3:5,8)]
PVs5_u50_Asian_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_Asian_noFH.txt")[,c(1,3:5,8)]
PVs5_50p_NHW_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_NHW_noFH.txt")[,c(1,3:5,8)]
PVs5_50p_Black_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_Black_noFH.txt")[,c(1,3:5,8)]
PVs5_50p_Hisp_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_Hisp_noFH.txt")[,c(1,3:5,8)]
PVs5_50p_Asian_noFH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_Asian_noFH.txt")[,c(1,3:5,8)]
PVs5_u50_NHW_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_NHW_FH.txt")[,c(1,3:5,8)]
PVs5_u50_Black_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_Black_FH.txt")[,c(1,3:5,8)]
PVs5_u50_Hisp_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_Hisp_FH.txt")[,c(1,3:5,8)]
PVs5_u50_Asian_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_u50_Asian_FH.txt")[,c(1,3:5,8)]
PVs5_50p_NHW_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_NHW_FH.txt")[,c(1,3:5,8)]
PVs5_50p_Black_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_Black_FH.txt")[,c(1,3:5,8)]
PVs5_50p_Hisp_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_Hisp_FH.txt")[,c(1,3:5,8)]
PVs5_50p_Asian_FH<-read.delim("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/realdataPV_50p_Asian_FH.txt")[,c(1,3:5,8)]

PVrisk_hybrid_noFH5_u50_NHW_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_noFH5_50p_NHW_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_u50_NHW_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_50p_NHW_1000<-matrix(nrow=1000,ncol=6)

PVrisk_hybrid_noFH5_u50_Black_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_noFH5_50p_Black_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_u50_Black_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_50p_Black_1000<-matrix(nrow=1000,ncol=6)

PVrisk_hybrid_noFH5_u50_Hisp_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_noFH5_50p_Hisp_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_u50_Hisp_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_50p_Hisp_1000<-matrix(nrow=1000,ncol=6)

PVrisk_hybrid_noFH5_u50_Asian_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_noFH5_50p_Asian_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_u50_Asian_1000<-matrix(nrow=1000,ncol=6)
PVrisk_hybrid_FH5_50p_Asian_1000<-matrix(nrow=1000,ncol=6)

for (j in 1:2){
  cat(paste0("Bootstrap iteration ", j, "\n"))
  
  logOR_PV_noFH<-matrix(nrow=5,ncol=1)
  logOR_PV_FH<-matrix(nrow=5,ncol=1)
  
  for (i in 1:5){
    logOR_PV_noFH[i,]<-rnorm(1,mean=PV_data_noFH5[i,2],sd=PV_data_noFH5[i,3])}
  OR_PV_noFH<-exp(logOR_PV_noFH)
  PV5_noFH<-cbind(PV_data_noFH5[1],OR_PV_noFH,PV_data_noFH5[4])
  names(PV5_noFH)[2]<-"snp.odds.ratio"
  PV5_noFH<-as.data.frame(PV5_noFH)
  PV5_noFH
  
  for (i in 1:5){
    logOR_PV_FH[i,]<-rnorm(1,mean=PV_data_FH5[i,2],sd=PV_data_FH5[i,3])}
  OR_PV_FH<-exp(logOR_PV_FH)
  PV5_FH<-cbind(PV_data_FH5[1],OR_PV_FH,PV_data_FH5[4])
  names(PV5_FH)[2]<-"snp.odds.ratio"
  PV5_FH<-as.data.frame(PV5_FH)
  PV5_FH
  
  for (x in 1:5){
    hybrid5_u50_noFH_NHW = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                           model.disease.incidence.rates = Inc_NHW_noFH,
                                           model.competing.incidence.rates = Mort_NHW,
                                           apply.age.start = 20,
                                           apply.age.interval.length = 30,
                                           apply.snp.profile=PVs5_u50_NHW_noFH,
                                           model.cov.info = hybrid_covar_list_u50_norace,
                                           model.formula = hybrid_formula_u50_norace,
                                           apply.cov.profile =CARRIERS_u50_hybrid_noFH_NHW,
                                           model.log.RR = ORs_hybrid_u50_noFH_norace,
                                           model.ref.dataset= as.data.frame(NHANES_u50_hybrid_noFH_listNHW[[x]]),
                                           model.ref.dataset.weights=NHANES_wts_u50_noFH_listNHW[[x]],
                                           return.refs.risk=TRUE)
    
    noncarriers<-subset(hybrid5_u50_noFH_NHW$details,(hybrid5_u50_noFH_NHW$details[,4]==0
         & hybrid5_u50_noFH_NHW$details[,5]==0 & hybrid5_u50_noFH_NHW$details[,6]==0 & hybrid5_u50_noFH_NHW$details[,7]==0
         & hybrid5_u50_noFH_NHW$details[,8]==0))
    ATM<-subset(hybrid5_u50_noFH_NHW$details,hybrid5_u50_noFH_NHW$details[,4]==1)
    BRCA1<-subset(hybrid5_u50_noFH_NHW$details,hybrid5_u50_noFH_NHW$details[,5]==1)
    BRCA2<-subset(hybrid5_u50_noFH_NHW$details,hybrid5_u50_noFH_NHW$details[,6]==1)
    CHEK2<-subset(hybrid5_u50_noFH_NHW$details,hybrid5_u50_noFH_NHW$details[,7]==1)
    PALB2<-subset(hybrid5_u50_noFH_NHW$details,hybrid5_u50_noFH_NHW$details[,8]==1)
    
    PVrisk_hybrid_noFH5_u50_NHW<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                               mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
    names(PVrisk_hybrid_noFH5_u50_NHW)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
    z<-(j-1)*5+x
    PVrisk_hybrid_noFH5_u50_NHW_1000[z,]<-PVrisk_hybrid_noFH5_u50_NHW
  }

  for (x in 1:5){
    hybrid5_50p_noFH_NHW = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                           model.disease.incidence.rates = Inc_NHW_noFH,
                                           model.competing.incidence.rates = Mort_NHW,
                                           apply.age.start = 50,
                                           apply.age.interval.length = 30,
                                           apply.snp.profile=PVs5_50p_NHW_noFH,
                                           model.cov.info = hybrid_covar_list_50p_norace,
                                           model.formula = hybrid_formula_50p_norace,
                                           apply.cov.profile =CARRIERS_50p_hybrid_noFH_NHW,
                                           model.log.RR = ORs_hybrid_50p_noFH_norace,
                                           model.ref.dataset= as.data.frame(NHANES_50p_hybrid_noFH_listNHW[[x]]),
                                           model.ref.dataset.weights=NHANES_wts_50p_noFH_listNHW[[x]],
                                           return.refs.risk=TRUE)
    
    noncarriers<-subset(hybrid5_50p_noFH_NHW$details,(hybrid5_50p_noFH_NHW$details[,4]==0
       & hybrid5_50p_noFH_NHW$details[,5]==0 & hybrid5_50p_noFH_NHW$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0
       & hybrid5_50p_noFH_NHW$details[,8]==0))
    ATM<-subset(hybrid5_50p_noFH_NHW$details,hybrid5_50p_noFH_NHW$details[,4]==1)
    BRCA1<-subset(hybrid5_50p_noFH_NHW$details,hybrid5_50p_noFH_NHW$details[,5]==1)
    BRCA2<-subset(hybrid5_50p_noFH_NHW$details,hybrid5_50p_noFH_NHW$details[,6]==1)
    CHEK2<-subset(hybrid5_50p_noFH_NHW$details,hybrid5_50p_noFH_NHW$details[,7]==1)
    PALB2<-subset(hybrid5_50p_noFH_NHW$details,hybrid5_50p_noFH_NHW$details[,8]==1)
    
    PVrisk_hybrid_noFH5_50p_NHW<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                               mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
    names(PVrisk_hybrid_noFH5_50p_NHW)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
    z<-(j-1)*5+x
    PVrisk_hybrid_noFH5_50p_NHW_1000[z,]<-PVrisk_hybrid_noFH5_50p_NHW
  }
  
  for (x in 1:5){
    hybrid5_u50_FH_NHW = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                               model.disease.incidence.rates = Inc_NHW_FH,
                                               model.competing.incidence.rates = Mort_NHW,
                                               apply.age.start = 20,
                                               apply.age.interval.length = 30,
                                               apply.snp.profile=PVs5_u50_NHW_FH,
                                               model.cov.info = hybrid_covar_list_u50_norace,
                                               model.formula = hybrid_formula_u50_norace,
                                               apply.cov.profile =CARRIERS_u50_hybrid_FH_NHW,
                                               model.log.RR = ORs_hybrid_u50_FH_norace,
                                               model.ref.dataset= as.data.frame(NHANES_u50_hybrid_FH_listNHW[[x]]),
                                               model.ref.dataset.weights=NHANES_wts_u50_FH_listNHW[[x]],
                                               return.refs.risk=TRUE)
    
    noncarriers<-subset(hybrid5_u50_FH_NHW$details,(hybrid5_u50_FH_NHW$details[,4]==0
       & hybrid5_u50_FH_NHW$details[,5]==0 & hybrid5_u50_FH_NHW$details[,6]==0 & hybrid5_u50_FH_NHW$details[,7]==0
       & hybrid5_u50_FH_NHW$details[,8]==0))
    ATM<-subset(hybrid5_u50_FH_NHW$details,hybrid5_u50_FH_NHW$details[,4]==1)
    BRCA1<-subset(hybrid5_u50_FH_NHW$details,hybrid5_u50_FH_NHW$details[,5]==1)
    BRCA2<-subset(hybrid5_u50_FH_NHW$details,hybrid5_u50_FH_NHW$details[,6]==1)
    CHEK2<-subset(hybrid5_u50_FH_NHW$details,hybrid5_u50_FH_NHW$details[,7]==1)
    PALB2<-subset(hybrid5_u50_FH_NHW$details,hybrid5_u50_FH_NHW$details[,8]==1)
    
    PVrisk_hybrid_FH5_u50_NHW<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                   mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
    names(PVrisk_hybrid_FH5_u50_NHW)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
    z<-(j-1)*5+x
    PVrisk_hybrid_FH5_u50_NHW_1000[z,]<-PVrisk_hybrid_FH5_u50_NHW
  }
  
  for (x in 1:5){
    hybrid5_50p_FH_NHW = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                               model.disease.incidence.rates = Inc_NHW_FH,
                                               model.competing.incidence.rates = Mort_NHW,
                                               apply.age.start = 50,
                                               apply.age.interval.length = 30,
                                               apply.snp.profile=PVs5_50p_NHW_FH,
                                               model.cov.info = hybrid_covar_list_50p_norace,
                                               model.formula = hybrid_formula_50p_norace,
                                               apply.cov.profile =CARRIERS_50p_hybrid_FH_NHW,
                                               model.log.RR = ORs_hybrid_50p_FH_norace,
                                               model.ref.dataset= as.data.frame(NHANES_50p_hybrid_FH_listNHW[[x]]),
                                               model.ref.dataset.weights=NHANES_wts_50p_FH_listNHW[[x]],
                                               return.refs.risk=TRUE)
    
    noncarriers<-subset(hybrid5_50p_FH_NHW$details,(hybrid5_50p_FH_NHW$details[,4]==0
        & hybrid5_50p_FH_NHW$details[,5]==0 & hybrid5_50p_FH_NHW$details[,6]==0 & hybrid5_50p_FH$details[,7]==0
        & hybrid5_50p_FH_NHW$details[,8]==0))
    ATM<-subset(hybrid5_50p_FH_NHW$details,hybrid5_50p_FH_NHW$details[,4]==1)
    BRCA1<-subset(hybrid5_50p_FH_NHW$details,hybrid5_50p_FH_NHW$details[,5]==1)
    BRCA2<-subset(hybrid5_50p_FH_NHW$details,hybrid5_50p_FH_NHW$details[,6]==1)
    CHEK2<-subset(hybrid5_50p_FH_NHW$details,hybrid5_50p_FH_NHW$details[,7]==1)
    PALB2<-subset(hybrid5_50p_FH_NHW$details,hybrid5_50p_FH_NHW$details[,8]==1)
    
    PVrisk_hybrid_FH5_50p_NHW<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                   mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
    names(PVrisk_hybrid_FH5_50p_NHW)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
    z<-(j-1)*5+x
    PVrisk_hybrid_FH5_50p_NHW_1000[z,]<-PVrisk_hybrid_FH5_50p_NHW
   
    for (x in 1:5){
      hybrid5_u50_noFH_Black = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                                 model.disease.incidence.rates = Inc_Black_noFH,
                                                 model.competing.incidence.rates = Mort_Black,
                                                 apply.age.start = 20,
                                                 apply.age.interval.length = 30,
                                                 apply.snp.profile=PVs5_u50_Black_noFH,
                                                 model.cov.info = hybrid_covar_list_u50_norace,
                                                 model.formula = hybrid_formula_u50_norace,
                                                 apply.cov.profile =CARRIERS_u50_hybrid_noFH_Black,
                                                 model.log.RR = ORs_hybrid_u50_noFH_norace,
                                                 model.ref.dataset= as.data.frame(NHANES_u50_hybrid_noFH_listBlack[[x]]),
                                                 model.ref.dataset.weights=NHANES_wts_u50_noFH_listBlack[[x]],
                                                 return.refs.risk=TRUE)
      
      noncarriers<-subset(hybrid5_u50_noFH_Black$details,(hybrid5_u50_noFH_Black$details[,4]==0
                                                        & hybrid5_u50_noFH_Black$details[,5]==0 & hybrid5_u50_noFH_Black$details[,6]==0 & hybrid5_u50_noFH_Black$details[,7]==0
                                                        & hybrid5_u50_noFH_Black$details[,8]==0))
      ATM<-subset(hybrid5_u50_noFH_Black$details,hybrid5_u50_noFH_Black$details[,4]==1)
      BRCA1<-subset(hybrid5_u50_noFH_Black$details,hybrid5_u50_noFH_Black$details[,5]==1)
      BRCA2<-subset(hybrid5_u50_noFH_Black$details,hybrid5_u50_noFH_Black$details[,6]==1)
      CHEK2<-subset(hybrid5_u50_noFH_Black$details,hybrid5_u50_noFH_Black$details[,7]==1)
      PALB2<-subset(hybrid5_u50_noFH_Black$details,hybrid5_u50_noFH_Black$details[,8]==1)
      
      PVrisk_hybrid_noFH5_u50_Black<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                     mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
      names(PVrisk_hybrid_noFH5_u50_Black)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
      z<-(j-1)*5+x
      PVrisk_hybrid_noFH5_u50_Black_1000[z,]<-PVrisk_hybrid_noFH5_u50_Black
    }
    
    
    for (x in 1:5){
      hybrid5_50p_noFH_Black = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                                 model.disease.incidence.rates = Inc_Black_noFH,
                                                 model.competing.incidence.rates = Mort_Black,
                                                 apply.age.start = 50,
                                                 apply.age.interval.length = 30,
                                                 apply.snp.profile=PVs5_50p_Black_noFH,
                                                 model.cov.info = hybrid_covar_list_50p_norace,
                                                 model.formula = hybrid_formula_50p_norace,
                                                 apply.cov.profile =CARRIERS_50p_hybrid_noFH_Black,
                                                 model.log.RR = ORs_hybrid_50p_noFH_norace,
                                                 model.ref.dataset= as.data.frame(NHANES_50p_hybrid_noFH_listBlack[[x]]),
                                                 model.ref.dataset.weights=NHANES_wts_50p_noFH_listBlack[[x]],
                                                 return.refs.risk=TRUE)
      
      noncarriers<-subset(hybrid5_50p_noFH_Black$details,(hybrid5_50p_noFH_Black$details[,4]==0
                                                        & hybrid5_50p_noFH_Black$details[,5]==0 & hybrid5_50p_noFH_Black$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0
                                                        & hybrid5_50p_noFH_Black$details[,8]==0))
      ATM<-subset(hybrid5_50p_noFH_Black$details,hybrid5_50p_noFH_Black$details[,4]==1)
      BRCA1<-subset(hybrid5_50p_noFH_Black$details,hybrid5_50p_noFH_Black$details[,5]==1)
      BRCA2<-subset(hybrid5_50p_noFH_Black$details,hybrid5_50p_noFH_Black$details[,6]==1)
      CHEK2<-subset(hybrid5_50p_noFH_Black$details,hybrid5_50p_noFH_Black$details[,7]==1)
      PALB2<-subset(hybrid5_50p_noFH_Black$details,hybrid5_50p_noFH_Black$details[,8]==1)
      
      PVrisk_hybrid_noFH5_50p_Black<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                     mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
      names(PVrisk_hybrid_noFH5_50p_Black)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
      z<-(j-1)*5+x
      PVrisk_hybrid_noFH5_50p_Black_1000[z,]<-PVrisk_hybrid_noFH5_50p_Black
    }
    
    for (x in 1:5){
      hybrid5_u50_FH_Black = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                               model.disease.incidence.rates = Inc_Black_FH,
                                               model.competing.incidence.rates = Mort_Black,
                                               apply.age.start = 20,
                                               apply.age.interval.length = 30,
                                               apply.snp.profile=PVs5_u50_Black_FH,
                                               model.cov.info = hybrid_covar_list_u50_norace,
                                               model.formula = hybrid_formula_u50_norace,
                                               apply.cov.profile =CARRIERS_u50_hybrid_FH_Black,
                                               model.log.RR = ORs_hybrid_u50_FH_norace,
                                               model.ref.dataset= as.data.frame(NHANES_u50_hybrid_FH_listBlack[[x]]),
                                               model.ref.dataset.weights=NHANES_wts_u50_FH_listBlack[[x]],
                                               return.refs.risk=TRUE)
      
      noncarriers<-subset(hybrid5_u50_FH_Black$details,(hybrid5_u50_FH_Black$details[,4]==0
                                                      & hybrid5_u50_FH_Black$details[,5]==0 & hybrid5_u50_FH_Black$details[,6]==0 & hybrid5_u50_FH_Black$details[,7]==0
                                                      & hybrid5_u50_FH_Black$details[,8]==0))
      ATM<-subset(hybrid5_u50_FH_Black$details,hybrid5_u50_FH_Black$details[,4]==1)
      BRCA1<-subset(hybrid5_u50_FH_Black$details,hybrid5_u50_FH_Black$details[,5]==1)
      BRCA2<-subset(hybrid5_u50_FH_Black$details,hybrid5_u50_FH_Black$details[,6]==1)
      CHEK2<-subset(hybrid5_u50_FH_Black$details,hybrid5_u50_FH_Black$details[,7]==1)
      PALB2<-subset(hybrid5_u50_FH_Black$details,hybrid5_u50_FH_Black$details[,8]==1)
      
      PVrisk_hybrid_FH5_u50_Black<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                   mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
      names(PVrisk_hybrid_FH5_u50_Black)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
      z<-(j-1)*5+x
      PVrisk_hybrid_FH5_u50_Black_1000[z,]<-PVrisk_hybrid_FH5_u50_Black
    }
    
    for (x in 1:5){
      hybrid5_50p_FH_Black = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                               model.disease.incidence.rates = Inc_Black_FH,
                                               model.competing.incidence.rates = Mort_Black,
                                               apply.age.start = 50,
                                               apply.age.interval.length = 30,
                                               apply.snp.profile=PVs5_50p_Black_FH,
                                               model.cov.info = hybrid_covar_list_50p_norace,
                                               model.formula = hybrid_formula_50p_norace,
                                               apply.cov.profile =CARRIERS_50p_hybrid_FH_Black,
                                               model.log.RR = ORs_hybrid_50p_FH_norace,
                                               model.ref.dataset= as.data.frame(NHANES_50p_hybrid_FH_listBlack[[x]]),
                                               model.ref.dataset.weights=NHANES_wts_50p_FH_listBlack[[x]],
                                               return.refs.risk=TRUE)
      
      noncarriers<-subset(hybrid5_50p_FH_Black$details,(hybrid5_50p_FH_Black$details[,4]==0
                                                      & hybrid5_50p_FH_Black$details[,5]==0 & hybrid5_50p_FH_Black$details[,6]==0 & hybrid5_50p_FH$details[,7]==0
                                                      & hybrid5_50p_FH_Black$details[,8]==0))
      ATM<-subset(hybrid5_50p_FH_Black$details,hybrid5_50p_FH_Black$details[,4]==1)
      BRCA1<-subset(hybrid5_50p_FH_Black$details,hybrid5_50p_FH_Black$details[,5]==1)
      BRCA2<-subset(hybrid5_50p_FH_Black$details,hybrid5_50p_FH_Black$details[,6]==1)
      CHEK2<-subset(hybrid5_50p_FH_Black$details,hybrid5_50p_FH_Black$details[,7]==1)
      PALB2<-subset(hybrid5_50p_FH_Black$details,hybrid5_50p_FH_Black$details[,8]==1)
      
      PVrisk_hybrid_FH5_50p_Black<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                   mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
      names(PVrisk_hybrid_FH5_50p_Black)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
      z<-(j-1)*5+x
      PVrisk_hybrid_FH5_50p_Black_1000[z,]<-PVrisk_hybrid_FH5_50p_Black
      
      
      
      for (x in 1:5){
        hybrid5_u50_noFH_Hisp = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                                   model.disease.incidence.rates = Inc_Hisp_noFH,
                                                   model.competing.incidence.rates = Mort_Hisp,
                                                   apply.age.start = 20,
                                                   apply.age.interval.length = 30,
                                                   apply.snp.profile=PVs5_u50_Hisp_noFH,
                                                   model.cov.info = hybrid_covar_list_u50_norace,
                                                   model.formula = hybrid_formula_u50_norace,
                                                   apply.cov.profile =CARRIERS_u50_hybrid_noFH_Hisp,
                                                   model.log.RR = ORs_hybrid_u50_noFH_norace,
                                                   model.ref.dataset= as.data.frame(NHANES_u50_hybrid_noFH_listHisp[[x]]),
                                                   model.ref.dataset.weights=NHANES_wts_u50_noFH_listHisp[[x]],
                                                   return.refs.risk=TRUE)
        
        noncarriers<-subset(hybrid5_u50_noFH_Hisp$details,(hybrid5_u50_noFH_Hisp$details[,4]==0
                                                          & hybrid5_u50_noFH_Hisp$details[,5]==0 & hybrid5_u50_noFH_Hisp$details[,6]==0 & hybrid5_u50_noFH_Hisp$details[,7]==0
                                                          & hybrid5_u50_noFH_Hisp$details[,8]==0))
        ATM<-subset(hybrid5_u50_noFH_Hisp$details,hybrid5_u50_noFH_Hisp$details[,4]==1)
        BRCA1<-subset(hybrid5_u50_noFH_Hisp$details,hybrid5_u50_noFH_Hisp$details[,5]==1)
        BRCA2<-subset(hybrid5_u50_noFH_Hisp$details,hybrid5_u50_noFH_Hisp$details[,6]==1)
        CHEK2<-subset(hybrid5_u50_noFH_Hisp$details,hybrid5_u50_noFH_Hisp$details[,7]==1)
        PALB2<-subset(hybrid5_u50_noFH_Hisp$details,hybrid5_u50_noFH_Hisp$details[,8]==1)
        
        PVrisk_hybrid_noFH5_u50_Hisp<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                       mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
        names(PVrisk_hybrid_noFH5_u50_Hisp)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
        z<-(j-1)*5+x
        PVrisk_hybrid_noFH5_u50_Hisp_1000[z,]<-PVrisk_hybrid_noFH5_u50_Hisp
      }
      
      
      for (x in 1:5){
        hybrid5_50p_noFH_Hisp = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                                   model.disease.incidence.rates = Inc_Hisp_noFH,
                                                   model.competing.incidence.rates = Mort_Hisp,
                                                   apply.age.start = 50,
                                                   apply.age.interval.length = 30,
                                                   apply.snp.profile=PVs5_50p_Hisp_noFH,
                                                   model.cov.info = hybrid_covar_list_50p_norace,
                                                   model.formula = hybrid_formula_50p_norace,
                                                   apply.cov.profile =CARRIERS_50p_hybrid_noFH_Hisp,
                                                   model.log.RR = ORs_hybrid_50p_noFH_norace,
                                                   model.ref.dataset= as.data.frame(NHANES_50p_hybrid_noFH_listHisp[[x]]),
                                                   model.ref.dataset.weights=NHANES_wts_50p_noFH_listHisp[[x]],
                                                   return.refs.risk=TRUE)
        
        noncarriers<-subset(hybrid5_50p_noFH_Hisp$details,(hybrid5_50p_noFH_Hisp$details[,4]==0
                                                          & hybrid5_50p_noFH_Hisp$details[,5]==0 & hybrid5_50p_noFH_Hisp$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0
                                                          & hybrid5_50p_noFH_Hisp$details[,8]==0))
        ATM<-subset(hybrid5_50p_noFH_Hisp$details,hybrid5_50p_noFH_Hisp$details[,4]==1)
        BRCA1<-subset(hybrid5_50p_noFH_Hisp$details,hybrid5_50p_noFH_Hisp$details[,5]==1)
        BRCA2<-subset(hybrid5_50p_noFH_Hisp$details,hybrid5_50p_noFH_Hisp$details[,6]==1)
        CHEK2<-subset(hybrid5_50p_noFH_Hisp$details,hybrid5_50p_noFH_Hisp$details[,7]==1)
        PALB2<-subset(hybrid5_50p_noFH_Hisp$details,hybrid5_50p_noFH_Hisp$details[,8]==1)
        
        PVrisk_hybrid_noFH5_50p_Hisp<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                       mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
        names(PVrisk_hybrid_noFH5_50p_Hisp)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
        z<-(j-1)*5+x
        PVrisk_hybrid_noFH5_50p_Hisp_1000[z,]<-PVrisk_hybrid_noFH5_50p_Hisp
      }
      
      for (x in 1:5){
        hybrid5_u50_FH_Hisp = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                                 model.disease.incidence.rates = Inc_Hisp_FH,
                                                 model.competing.incidence.rates = Mort_Hisp,
                                                 apply.age.start = 20,
                                                 apply.age.interval.length = 30,
                                                 apply.snp.profile=PVs5_u50_Hisp_FH,
                                                 model.cov.info = hybrid_covar_list_u50_norace,
                                                 model.formula = hybrid_formula_u50_norace,
                                                 apply.cov.profile =CARRIERS_u50_hybrid_FH_Hisp,
                                                 model.log.RR = ORs_hybrid_u50_FH_norace,
                                                 model.ref.dataset= as.data.frame(NHANES_u50_hybrid_FH_listHisp[[x]]),
                                                 model.ref.dataset.weights=NHANES_wts_u50_FH_listHisp[[x]],
                                                 return.refs.risk=TRUE)
        
        noncarriers<-subset(hybrid5_u50_FH_Hisp$details,(hybrid5_u50_FH_Hisp$details[,4]==0
                                                        & hybrid5_u50_FH_Hisp$details[,5]==0 & hybrid5_u50_FH_Hisp$details[,6]==0 & hybrid5_u50_FH_Hisp$details[,7]==0
                                                        & hybrid5_u50_FH_Hisp$details[,8]==0))
        ATM<-subset(hybrid5_u50_FH_Hisp$details,hybrid5_u50_FH_Hisp$details[,4]==1)
        BRCA1<-subset(hybrid5_u50_FH_Hisp$details,hybrid5_u50_FH_Hisp$details[,5]==1)
        BRCA2<-subset(hybrid5_u50_FH_Hisp$details,hybrid5_u50_FH_Hisp$details[,6]==1)
        CHEK2<-subset(hybrid5_u50_FH_Hisp$details,hybrid5_u50_FH_Hisp$details[,7]==1)
        PALB2<-subset(hybrid5_u50_FH_Hisp$details,hybrid5_u50_FH_Hisp$details[,8]==1)
        
        PVrisk_hybrid_FH5_u50_Hisp<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                     mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
        names(PVrisk_hybrid_FH5_u50_Hisp)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
        z<-(j-1)*5+x
        PVrisk_hybrid_FH5_u50_Hisp_1000[z,]<-PVrisk_hybrid_FH5_u50_Hisp
      }
      
      for (x in 1:5){
        hybrid5_50p_FH_Hisp = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                                 model.disease.incidence.rates = Inc_Hisp_FH,
                                                 model.competing.incidence.rates = Mort_Hisp,
                                                 apply.age.start = 50,
                                                 apply.age.interval.length = 30,
                                                 apply.snp.profile=PVs5_50p_Hisp_FH,
                                                 model.cov.info = hybrid_covar_list_50p_norace,
                                                 model.formula = hybrid_formula_50p_norace,
                                                 apply.cov.profile =CARRIERS_50p_hybrid_FH_Hisp,
                                                 model.log.RR = ORs_hybrid_50p_FH_norace,
                                                 model.ref.dataset= as.data.frame(NHANES_50p_hybrid_FH_listHisp[[x]]),
                                                 model.ref.dataset.weights=NHANES_wts_50p_FH_listHisp[[x]],
                                                 return.refs.risk=TRUE)
        
        noncarriers<-subset(hybrid5_50p_FH_Hisp$details,(hybrid5_50p_FH_Hisp$details[,4]==0
                                                        & hybrid5_50p_FH_Hisp$details[,5]==0 & hybrid5_50p_FH_Hisp$details[,6]==0 & hybrid5_50p_FH$details[,7]==0
                                                        & hybrid5_50p_FH_Hisp$details[,8]==0))
        ATM<-subset(hybrid5_50p_FH_Hisp$details,hybrid5_50p_FH_Hisp$details[,4]==1)
        BRCA1<-subset(hybrid5_50p_FH_Hisp$details,hybrid5_50p_FH_Hisp$details[,5]==1)
        BRCA2<-subset(hybrid5_50p_FH_Hisp$details,hybrid5_50p_FH_Hisp$details[,6]==1)
        CHEK2<-subset(hybrid5_50p_FH_Hisp$details,hybrid5_50p_FH_Hisp$details[,7]==1)
        PALB2<-subset(hybrid5_50p_FH_Hisp$details,hybrid5_50p_FH_Hisp$details[,8]==1)
        
        PVrisk_hybrid_FH5_50p_Hisp<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                     mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
        names(PVrisk_hybrid_FH5_50p_Hisp)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
        z<-(j-1)*5+x
        PVrisk_hybrid_FH5_50p_Hisp_1000[z,]<-PVrisk_hybrid_FH5_50p_Hisp
      
        
        for (x in 1:5){
          hybrid5_u50_noFH_Asian = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                                     model.disease.incidence.rates = Inc_Asian_noFH,
                                                     model.competing.incidence.rates = Mort_Asian,
                                                     apply.age.start = 20,
                                                     apply.age.interval.length = 30,
                                                     apply.snp.profile=PVs5_u50_Asian_noFH,
                                                     model.cov.info = hybrid_covar_list_u50_norace,
                                                     model.formula = hybrid_formula_u50_norace,
                                                     apply.cov.profile =CARRIERS_u50_hybrid_noFH_Asian,
                                                     model.log.RR = ORs_hybrid_u50_noFH_norace,
                                                     model.ref.dataset= as.data.frame(NHANES_u50_hybrid_noFH_listAsian[[x]]),
                                                     model.ref.dataset.weights=NHANES_wts_u50_noFH_listAsian[[x]],
                                                     return.refs.risk=TRUE)
          
          noncarriers<-subset(hybrid5_u50_noFH_Asian$details,(hybrid5_u50_noFH_Asian$details[,4]==0
                                                            & hybrid5_u50_noFH_Asian$details[,5]==0 & hybrid5_u50_noFH_Asian$details[,6]==0 & hybrid5_u50_noFH_Asian$details[,7]==0
                                                            & hybrid5_u50_noFH_Asian$details[,8]==0))
          ATM<-subset(hybrid5_u50_noFH_Asian$details,hybrid5_u50_noFH_Asian$details[,4]==1)
          BRCA1<-subset(hybrid5_u50_noFH_Asian$details,hybrid5_u50_noFH_Asian$details[,5]==1)
          BRCA2<-subset(hybrid5_u50_noFH_Asian$details,hybrid5_u50_noFH_Asian$details[,6]==1)
          CHEK2<-subset(hybrid5_u50_noFH_Asian$details,hybrid5_u50_noFH_Asian$details[,7]==1)
          PALB2<-subset(hybrid5_u50_noFH_Asian$details,hybrid5_u50_noFH_Asian$details[,8]==1)
          
          PVrisk_hybrid_noFH5_u50_Asian<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                         mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
          names(PVrisk_hybrid_noFH5_u50_Asian)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
          z<-(j-1)*5+x
          PVrisk_hybrid_noFH5_u50_Asian_1000[z,]<-PVrisk_hybrid_noFH5_u50_Asian
        }
        
        
        for (x in 1:5){
          hybrid5_50p_noFH_Asian = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                                     model.disease.incidence.rates = Inc_Asian_noFH,
                                                     model.competing.incidence.rates = Mort_Asian,
                                                     apply.age.start = 50,
                                                     apply.age.interval.length = 30,
                                                     apply.snp.profile=PVs5_50p_Asian_noFH,
                                                     model.cov.info = hybrid_covar_list_50p_norace,
                                                     model.formula = hybrid_formula_50p_norace,
                                                     apply.cov.profile =CARRIERS_50p_hybrid_noFH_Asian,
                                                     model.log.RR = ORs_hybrid_50p_noFH_norace,
                                                     model.ref.dataset= as.data.frame(NHANES_50p_hybrid_noFH_listAsian[[x]]),
                                                     model.ref.dataset.weights=NHANES_wts_50p_noFH_listAsian[[x]],
                                                     return.refs.risk=TRUE)
          
          noncarriers<-subset(hybrid5_50p_noFH_Asian$details,(hybrid5_50p_noFH_Asian$details[,4]==0
                                                            & hybrid5_50p_noFH_Asian$details[,5]==0 & hybrid5_50p_noFH_Asian$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0
                                                            & hybrid5_50p_noFH_Asian$details[,8]==0))
          ATM<-subset(hybrid5_50p_noFH_Asian$details,hybrid5_50p_noFH_Asian$details[,4]==1)
          BRCA1<-subset(hybrid5_50p_noFH_Asian$details,hybrid5_50p_noFH_Asian$details[,5]==1)
          BRCA2<-subset(hybrid5_50p_noFH_Asian$details,hybrid5_50p_noFH_Asian$details[,6]==1)
          CHEK2<-subset(hybrid5_50p_noFH_Asian$details,hybrid5_50p_noFH_Asian$details[,7]==1)
          PALB2<-subset(hybrid5_50p_noFH_Asian$details,hybrid5_50p_noFH_Asian$details[,8]==1)
          
          PVrisk_hybrid_noFH5_50p_Asian<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                         mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
          names(PVrisk_hybrid_noFH5_50p_Asian)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
          z<-(j-1)*5+x
          PVrisk_hybrid_noFH5_50p_Asian_1000[z,]<-PVrisk_hybrid_noFH5_50p_Asian
        }
        
        for (x in 1:5){
          hybrid5_u50_FH_Asian = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                                   model.disease.incidence.rates = Inc_Asian_FH,
                                                   model.competing.incidence.rates = Mort_Asian,
                                                   apply.age.start = 20,
                                                   apply.age.interval.length = 30,
                                                   apply.snp.profile=PVs5_u50_Asian_FH,
                                                   model.cov.info = hybrid_covar_list_u50_norace,
                                                   model.formula = hybrid_formula_u50_norace,
                                                   apply.cov.profile =CARRIERS_u50_hybrid_FH_Asian,
                                                   model.log.RR = ORs_hybrid_u50_FH_norace,
                                                   model.ref.dataset= as.data.frame(NHANES_u50_hybrid_FH_listAsian[[x]]),
                                                   model.ref.dataset.weights=NHANES_wts_u50_FH_listAsian[[x]],
                                                   return.refs.risk=TRUE)
          
          noncarriers<-subset(hybrid5_u50_FH_Asian$details,(hybrid5_u50_FH_Asian$details[,4]==0
                                                          & hybrid5_u50_FH_Asian$details[,5]==0 & hybrid5_u50_FH_Asian$details[,6]==0 & hybrid5_u50_FH_Asian$details[,7]==0
                                                          & hybrid5_u50_FH_Asian$details[,8]==0))
          ATM<-subset(hybrid5_u50_FH_Asian$details,hybrid5_u50_FH_Asian$details[,4]==1)
          BRCA1<-subset(hybrid5_u50_FH_Asian$details,hybrid5_u50_FH_Asian$details[,5]==1)
          BRCA2<-subset(hybrid5_u50_FH_Asian$details,hybrid5_u50_FH_Asian$details[,6]==1)
          CHEK2<-subset(hybrid5_u50_FH_Asian$details,hybrid5_u50_FH_Asian$details[,7]==1)
          PALB2<-subset(hybrid5_u50_FH_Asian$details,hybrid5_u50_FH_Asian$details[,8]==1)
          
          PVrisk_hybrid_FH5_u50_Asian<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                       mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
          names(PVrisk_hybrid_FH5_u50_Asian)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
          z<-(j-1)*5+x
          PVrisk_hybrid_FH5_u50_Asian_1000[z,]<-PVrisk_hybrid_FH5_u50_Asian
        }
        
        for (x in 1:5){
          hybrid5_50p_FH_Asian = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                                   model.disease.incidence.rates = Inc_Asian_FH,
                                                   model.competing.incidence.rates = Mort_Asian,
                                                   apply.age.start = 50,
                                                   apply.age.interval.length = 30,
                                                   apply.snp.profile=PVs5_50p_Asian_FH,
                                                   model.cov.info = hybrid_covar_list_50p_norace,
                                                   model.formula = hybrid_formula_50p_norace,
                                                   apply.cov.profile =CARRIERS_50p_hybrid_FH_Asian,
                                                   model.log.RR = ORs_hybrid_50p_FH_norace,
                                                   model.ref.dataset= as.data.frame(NHANES_50p_hybrid_FH_listAsian[[x]]),
                                                   model.ref.dataset.weights=NHANES_wts_50p_FH_listAsian[[x]],
                                                   return.refs.risk=TRUE)
          
          noncarriers<-subset(hybrid5_50p_FH_Asian$details,(hybrid5_50p_FH_Asian$details[,4]==0
                                                          & hybrid5_50p_FH_Asian$details[,5]==0 & hybrid5_50p_FH_Asian$details[,6]==0 & hybrid5_50p_FH$details[,7]==0
                                                          & hybrid5_50p_FH_Asian$details[,8]==0))
          ATM<-subset(hybrid5_50p_FH_Asian$details,hybrid5_50p_FH_Asian$details[,4]==1)
          BRCA1<-subset(hybrid5_50p_FH_Asian$details,hybrid5_50p_FH_Asian$details[,5]==1)
          BRCA2<-subset(hybrid5_50p_FH_Asian$details,hybrid5_50p_FH_Asian$details[,6]==1)
          CHEK2<-subset(hybrid5_50p_FH_Asian$details,hybrid5_50p_FH_Asian$details[,7]==1)
          PALB2<-subset(hybrid5_50p_FH_Asian$details,hybrid5_50p_FH_Asian$details[,8]==1)
          
          PVrisk_hybrid_FH5_50p_Asian<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BRCA1[,3]),
                                       mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]))
          names(PVrisk_hybrid_FH5_50p_Asian)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
          z<-(j-1)*5+x
          PVrisk_hybrid_FH5_50p_Asian_1000[z,]<-PVrisk_hybrid_FH5_50p_Asian
  }
}

hybrid5_u50_noFH_NHW_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_noFH_NHW_results[k,]<-quantile(PVrisk_hybrid_noFH5_u50_NHW_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_noFH_NHW_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_noFH_NHW_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_noFH_NHW_results
write.csv(hybrid5_u50_noFH_NHW_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_noFH_NHW_results.csv")

hybrid5_50p_noFH_NHW_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_noFH_NHW_results[k,]<-quantile(PVrisk_hybrid_noFH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_noFH_NHW_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_noFH_NHW_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_noFH_NHW_results
write.csv(hybrid5_50p_noFH_NHW_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_noFH_NHW_results.csv")

hybrid5_u50_FH_NHW_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_FH_NHW_results[k,]<-quantile(PVrisk_hybrid_FH5_u50_NHW_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_FH_NHW_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_FH_NHW_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_FH_NHW_results
write.csv(hybrid5_u50_FH_NHW_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_FH_NHW_results.csv")

hybrid5_50p_FH_NHW_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_FH_NHW_results[k,]<-quantile(PVrisk_hybrid_FH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_FH_NHW_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_FH_NHW_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_FH_NHW_results
write.csv(hybrid5_50p_FH_NHW_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_FH_NHW_results.csv")



hybrid5_u50_noFH_Black_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_noFH_Black_results[k,]<-quantile(PVrisk_hybrid_noFH5_u50_Black_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_noFH_Black_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_noFH_Black_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_noFH_Black_results
write.csv(hybrid5_u50_noFH_Black_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_noFH_Black_results.csv")

hybrid5_50p_noFH_Black_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_noFH_Black_results[k,]<-quantile(PVrisk_hybrid_noFH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_noFH_Black_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_noFH_Black_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_noFH_Black_results
write.csv(hybrid5_50p_noFH_Black_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_noFH_Black_results.csv")

hybrid5_u50_FH_Black_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_FH_Black_results[k,]<-quantile(PVrisk_hybrid_FH5_u50_Black_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_FH_Black_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_FH_Black_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_FH_Black_results
write.csv(hybrid5_u50_FH_Black_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_FH_Black_results.csv")

hybrid5_50p_FH_Black_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_FH_Black_results[k,]<-quantile(PVrisk_hybrid_FH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_FH_Black_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_FH_Black_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_FH_Black_results
write.csv(hybrid5_50p_FH_Black_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_FH_Black_results.csv")




hybrid5_u50_noFH_Hisp_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_noFH_Hisp_results[k,]<-quantile(PVrisk_hybrid_noFH5_u50_Hisp_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_noFH_Hisp_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_noFH_Hisp_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_noFH_Hisp_results
write.csv(hybrid5_u50_noFH_Hisp_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_noFH_Hisp_results.csv")

hybrid5_50p_noFH_Hisp_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_noFH_Hisp_results[k,]<-quantile(PVrisk_hybrid_noFH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_noFH_Hisp_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_noFH_Hisp_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_noFH_Hisp_results
write.csv(hybrid5_50p_noFH_Hisp_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_noFH_Hisp_results.csv")

hybrid5_u50_FH_Hisp_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_FH_Hisp_results[k,]<-quantile(PVrisk_hybrid_FH5_u50_Hisp_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_FH_Hisp_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_FH_Hisp_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_FH_Hisp_results
write.csv(hybrid5_u50_FH_Hisp_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_FH_Hisp_results.csv")

hybrid5_50p_FH_Hisp_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_FH_Hisp_results[k,]<-quantile(PVrisk_hybrid_FH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_FH_Hisp_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_FH_Hisp_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_FH_Hisp_results
write.csv(hybrid5_50p_FH_Hisp_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_FH_Hisp_results.csv")



hybrid5_u50_noFH_Asian_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_noFH_Asian_results[k,]<-quantile(PVrisk_hybrid_noFH5_u50_Asian_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_noFH_Asian_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_noFH_Asian_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_noFH_Asian_results
write.csv(hybrid5_u50_noFH_Asian_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_noFH_Asian_results.csv")

hybrid5_50p_noFH_Asian_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_noFH_Asian_results[k,]<-quantile(PVrisk_hybrid_noFH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_noFH_Asian_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_noFH_Asian_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_noFH_Asian_results
write.csv(hybrid5_50p_noFH_Asian_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_noFH_Asian_results.csv")

hybrid5_u50_FH_Asian_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_u50_FH_Asian_results[k,]<-quantile(PVrisk_hybrid_FH5_u50_Asian_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_u50_FH_Asian_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_u50_FH_Asian_results)<-c("median","2.5th %","97.5 %")
hybrid5_u50_FH_Asian_results
write.csv(hybrid5_u50_FH_Asian_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_u50_FH_Asian_results.csv")

hybrid5_50p_FH_Asian_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  hybrid5_50p_FH_Asian_results[k,]<-quantile(PVrisk_hybrid_FH5_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(hybrid5_50p_FH_Asian_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(hybrid5_50p_FH_Asian_results)<-c("median","2.5th %","97.5 %")
hybrid5_50p_FH_Asian_results
write.csv(hybrid5_50p_FH_Asian_results, file = "/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/hybrid5_50p_FH_Asian_results.csv")
