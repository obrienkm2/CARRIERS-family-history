library(readxl)
library("iCARE")

Inc <- data.frame(read_excel("inc_byage_raceeth_2024_04_17.xlsx",range = "A1:F91"))
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

Mort <- data.frame(read_excel("mort_byage_raceeth_2024_04_17.xlsx",range = "A1:F91"))
Mort_all<-Mort[c(1,6)]
names(Mort_all)[names(Mort_all) == 'all'] <- 'Mortality'

PV_data <- read_excel("PV_DATA_updated_2024_09_30.xlsx")

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
CARRIERS_u50_PVs_noFH5<-read.delim("realdataPV_u50_noFH.txt")[,c(1,3:6)]
CARRIERS_u50_PVs_FH5<-read.delim("realdataPV_u50_FH.txt")[,c(1,3:6)]
CARRIERS_50p_PVs_noFH5<-read.delim("realdataPV_50p_noFH.txt")[,c(1,3:6)]
CARRIERS_50p_PVs_FH5<-read.delim("realdataPV_50p_FH.txt")[,c(1,3:6)]

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
CARRIERS_u50_hybrid_noFH<-read.delim("realdata_u50_hybrid_noFH.txt", na.strings =".")
CARRIERS_u50_hybrid_FH<-read.delim("realdata_u50_hybrid_FH.txt", na.strings =".")
CARRIERS_50p_hybrid_noFH<-read.delim("realdata_50p_hybrid_noFH.txt", na.strings =".")
CARRIERS_50p_hybrid_FH<-read.delim("realdata_50p_hybrid_FH.txt", na.strings =".")

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
NHANES_wts_u50_noFH_1<-as.matrix(read.delim("wts_u50_noFH_1.txt"))
NHANES_wts_u50_noFH_2<-as.matrix(read.delim("wts_u50_noFH_2.txt"))
NHANES_wts_u50_noFH_3<-as.matrix(read.delim("wts_u50_noFH_3.txt"))
NHANES_wts_u50_noFH_4<-as.matrix(read.delim("wts_u50_noFH_4.txt"))
NHANES_wts_u50_noFH_5<-as.matrix(read.delim("wts_u50_noFH_5.txt"))
NHANES_wts_u50_noFH_list<-list(NHANES_wts_u50_noFH_1,NHANES_wts_u50_noFH_2,NHANES_wts_u50_noFH_3,NHANES_wts_u50_noFH_4,NHANES_wts_u50_noFH_5)

NHANES_wts_50p_noFH_1<-as.matrix(read.delim("wts_50p_noFH_1.txt"))
NHANES_wts_50p_noFH_2<-as.matrix(read.delim("wts_50p_noFH_2.txt"))
NHANES_wts_50p_noFH_3<-as.matrix(read.delim("wts_50p_noFH_3.txt"))
NHANES_wts_50p_noFH_4<-as.matrix(read.delim("wts_50p_noFH_4.txt"))
NHANES_wts_50p_noFH_5<-as.matrix(read.delim("wts_50p_noFH_5.txt"))
NHANES_wts_50p_noFH_list<-list(NHANES_wts_50p_noFH_1,NHANES_wts_50p_noFH_2,NHANES_wts_50p_noFH_3,NHANES_wts_50p_noFH_4,NHANES_wts_50p_noFH_5)

NHANES_wts_u50_FH_1<-as.matrix(read.delim("wts_u50_FH_1.txt"))
NHANES_wts_u50_FH_2<-as.matrix(read.delim("wts_u50_FH_2.txt"))
NHANES_wts_u50_FH_3<-as.matrix(read.delim("wts_u50_FH_3.txt"))
NHANES_wts_u50_FH_4<-as.matrix(read.delim("wts_u50_FH_4.txt"))
NHANES_wts_u50_FH_5<-as.matrix(read.delim("wts_u50_FH_5.txt"))
NHANES_wts_u50_FH_list<-list(NHANES_wts_u50_FH_1,NHANES_wts_u50_FH_2,NHANES_wts_u50_FH_3,NHANES_wts_u50_FH_4,NHANES_wts_u50_FH_5)

NHANES_wts_50p_FH_1<-as.matrix(read.delim("wts_50p_FH_1.txt"))
NHANES_wts_50p_FH_2<-as.matrix(read.delim("wts_50p_FH_2.txt"))
NHANES_wts_50p_FH_3<-as.matrix(read.delim("wts_50p_FH_3.txt"))
NHANES_wts_50p_FH_4<-as.matrix(read.delim("wts_50p_FH_4.txt"))
NHANES_wts_50p_FH_5<-as.matrix(read.delim("wts_50p_FH_5.txt"))
NHANES_wts_50p_FH_list<-list(NHANES_wts_50p_FH_1,NHANES_wts_50p_FH_2,NHANES_wts_50p_FH_3,NHANES_wts_50p_FH_4,NHANES_wts_50p_FH_5)

#import each version of the data set with the correct variable list
NHANES_u50_hybrid_noFH<-read.delim("outdata_u50_hybrid_noFH.txt")
NHANES_u50_hybrid_FH<-read.delim("outdata_u50_hybrid_FH.txt")
NHANES_50p_hybrid_noFH<-read.delim("outdata_50p_hybrid_noFH.txt")
NHANES_50p_hybrid_FH<-read.delim("outdata_50p_hybrid_FH.txt")

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

set.seed(963258)
noncarrier_u50_noFH<-matrix(nrow=1000,ncol=30)
ATM_u50_noFH<-matrix(nrow=1000,ncol=30)
BRCA1_u50_noFH<-matrix(nrow=1000,ncol=30)
BRCA2_u50_noFH<-matrix(nrow=1000,ncol=30)
CHEK2_u50_noFH<-matrix(nrow=1000,ncol=30)
PALB2_u50_noFH<-matrix(nrow=1000,ncol=30)

mean_risk_noncarrier_u80_noFH<-matrix(nrow=1000,ncol=30)
mean_risk_ATM_u80_noFH<-matrix(nrow=1000,ncol=30)
mean_risk_BRCA1_u80_noFH<-matrix(nrow=1000,ncol=30)
mean_risk_BRCA2_u80_noFH<-matrix(nrow=1000,ncol=30)
mean_risk_CHEK2_u80_noFH<-matrix(nrow=1000,ncol=30)
mean_risk_PALB2_u80_noFH<-matrix(nrow=1000,ncol=30)

noncarrier_u50_FH<-matrix(nrow=1000,ncol=30)
ATM_u50_FH<-matrix(nrow=1000,ncol=30)
BRCA1_u50_FH<-matrix(nrow=1000,ncol=30)
BRCA2_u50_FH<-matrix(nrow=1000,ncol=30)
CHEK2_u50_FH<-matrix(nrow=1000,ncol=30)
PALB2_u50_FH<-matrix(nrow=1000,ncol=30)

mean_risk_noncarrier_u80_FH<-matrix(nrow=1000,ncol=30)
mean_risk_ATM_u80_FH<-matrix(nrow=1000,ncol=30)
mean_risk_BRCA1_u80_FH<-matrix(nrow=1000,ncol=30)
mean_risk_BRCA2_u80_FH<-matrix(nrow=1000,ncol=30)
mean_risk_CHEK2_u80_FH<-matrix(nrow=1000,ncol=30)
mean_risk_PALB2_u80_FH<-matrix(nrow=1000,ncol=30)

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
    for (k in 1:30){
    hybrid5_u50_noFH_byage = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                           model.disease.incidence.rates = Inc_all_noFH,
                                           model.competing.incidence.rates = Mort_all,
                                           apply.age.start = 20,
                                           apply.age.interval.length = k,
                                           apply.snp.profile=CARRIERS_u50_PVs_noFH5,
                                           model.cov.info = hybrid_covar_list_u50,
                                           model.formula = hybrid_formula_u50,
                                           apply.cov.profile =CARRIERS_u50_hybrid_noFH,
                                           model.log.RR = ORs_hybrid_u50_noFH,
                                           model.ref.dataset= as.data.frame(NHANES_u50_hybrid_noFH_list[[x]]),
                                           model.ref.dataset.weights=NHANES_wts_u50_noFH_list[[x]],
                                           return.refs.risk=TRUE)
    
    y<-(j-1)*5+x
    noncarrier_u50_noFH[y,k]<-mean(subset(hybrid5_u50_noFH_byage$details,(hybrid5_u50_noFH_byage$details[,4]==0 & hybrid5_u50_noFH_byage$details[,5]==0 
            & hybrid5_u50_noFH_byage$details[,6]==0 & hybrid5_u50_noFH_byage$details[,7]==0 & hybrid5_u50_noFH_byage$details[,8]==0))[,3])
    ATM_u50_noFH[y,k]<-mean(subset(hybrid5_u50_noFH_byage$details,(hybrid5_u50_noFH_byage$details[,4]==1))[,3])
    BRCA1_u50_noFH[y,k]<-mean(subset(hybrid5_u50_noFH_byage$details,(hybrid5_u50_noFH_byage$details[,5]==1))[,3])
    BRCA2_u50_noFH[y,k]<-mean(subset(hybrid5_u50_noFH_byage$details,(hybrid5_u50_noFH_byage$details[,6]==1))[,3])
    CHEK2_u50_noFH[y,k]<-mean(subset(hybrid5_u50_noFH_byage$details,(hybrid5_u50_noFH_byage$details[,7]==1))[,3])
    PALB2_u50_noFH[y,k]<-mean(subset(hybrid5_u50_noFH_byage$details,(hybrid5_u50_noFH_byage$details[,8]==1))[,3])
    
    hybrid5_u50_FH_byage = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                                 model.disease.incidence.rates = Inc_all_FH,
                                                 model.competing.incidence.rates = Mort_all,
                                                 apply.age.start = 20,
                                                 apply.age.interval.length = k,
                                                 apply.snp.profile=CARRIERS_u50_PVs_FH5,
                                                 model.cov.info = hybrid_covar_list_u50,
                                                 model.formula = hybrid_formula_u50,
                                                 apply.cov.profile =CARRIERS_u50_hybrid_FH,
                                                 model.log.RR = ORs_hybrid_u50_FH,
                                                 model.ref.dataset= as.data.frame(NHANES_u50_hybrid_FH_list[[x]]),
                                                 model.ref.dataset.weights=NHANES_wts_u50_FH_list[[x]],
                                                 return.refs.risk=TRUE)
    
    noncarrier_u50_FH[y,k]<-mean(subset(hybrid5_u50_FH_byage$details,(hybrid5_u50_FH_byage$details[,4]==0 & hybrid5_u50_FH_byage$details[,5]==0 
         & hybrid5_u50_FH_byage$details[,6]==0 & hybrid5_u50_FH_byage$details[,7]==0 & hybrid5_u50_FH_byage$details[,8]==0))[,3])
    ATM_u50_FH[y,k]<-mean(subset(hybrid5_u50_FH_byage$details,(hybrid5_u50_FH_byage$details[,4]==1))[,3])
    BRCA1_u50_FH[y,k]<-mean(subset(hybrid5_u50_FH_byage$details,(hybrid5_u50_FH_byage$details[,5]==1))[,3])
    BRCA2_u50_FH[y,k]<-mean(subset(hybrid5_u50_FH_byage$details,(hybrid5_u50_FH_byage$details[,6]==1))[,3])
    CHEK2_u50_FH[y,k]<-mean(subset(hybrid5_u50_FH_byage$details,(hybrid5_u50_FH_byage$details[,7]==1))[,3])
    PALB2_u50_FH[y,k]<-mean(subset(hybrid5_u50_FH_byage$details,(hybrid5_u50_FH_byage$details[,8]==1))[,3])

    }
    
  #get risk at age 50 estimates for women in 50+ group (use u50 ORs and formula)
 
  hybrid5_u50_noFH_age50 = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
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
  
  noncarrier_u50_noFH_age50<-subset(hybrid5_u50_noFH_age50$details,(hybrid5_u50_noFH_age50$details[,4]==0 & hybrid5_u50_noFH_age50$details[,5]==0
     & hybrid5_u50_noFH_age50$details[,6]==0 & hybrid5_u50_noFH_age50$details[,7]==0 & hybrid5_u50_noFH_age50$details[,8]==0))[,3]
  ATM_u50_noFH_age50<-subset(hybrid5_u50_noFH_age50$details,(hybrid5_u50_noFH_age50$details[,4]==1))[,3]
  BRCA1_u50_noFH_age50<-subset(hybrid5_u50_noFH_age50$details,(hybrid5_u50_noFH_age50$details[,5]==1))[,3]
  BRCA2_u50_noFH_age50<-subset(hybrid5_u50_noFH_age50$details,(hybrid5_u50_noFH_age50$details[,6]==1))[,3]
  CHEK2_u50_noFH_age50<-subset(hybrid5_u50_noFH_age50$details,(hybrid5_u50_noFH_age50$details[,7]==1))[,3]
  PALB2_u50_noFH_age50<-subset(hybrid5_u50_noFH_age50$details,(hybrid5_u50_noFH_age50$details[,8]==1))[,3]
  
  hybrid5_u50_FH_age50 = computeAbsoluteRisk(model.snp.info = PV5_FH, 
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
  
  noncarrier_u50_FH_age50<-subset(hybrid5_u50_FH_age50$details,(hybrid5_u50_FH_age50$details[,4]==0 & hybrid5_u50_FH_age50$details[,5]==0
      & hybrid5_u50_FH_age50$details[,6]==0 & hybrid5_u50_FH_age50$details[,7]==0 & hybrid5_u50_FH_age50$details[,8]==0))[,3]
  ATM_u50_FH_age50<-subset(hybrid5_u50_FH_age50$details,(hybrid5_u50_FH_age50$details[,4]==1))[,3]
  BRCA1_u50_FH_age50<-subset(hybrid5_u50_FH_age50$details,(hybrid5_u50_FH_age50$details[,5]==1))[,3]
  BRCA2_u50_FH_age50<-subset(hybrid5_u50_FH_age50$details,(hybrid5_u50_FH_age50$details[,6]==1))[,3]
  CHEK2_u50_FH_age50<-subset(hybrid5_u50_FH_age50$details,(hybrid5_u50_FH_age50$details[,7]==1))[,3]
  PALB2_u50_FH_age50<-subset(hybrid5_u50_FH_age50$details,(hybrid5_u50_FH_age50$details[,8]==1))[,3]

  
  for (k in 1:30){
    
    hybrid5_50p_noFH_byage = computeAbsoluteRisk(model.snp.info = PV5_noFH, 
                                                 model.disease.incidence.rates = Inc_all_noFH,
                                                 model.competing.incidence.rates = Mort_all,
                                                 apply.age.start = 50,
                                                 apply.age.interval.length = k,
                                                 apply.snp.profile=CARRIERS_50p_PVs_noFH5,
                                                 model.cov.info = hybrid_covar_list_50p,
                                                 model.formula = hybrid_formula_50p,
                                                 apply.cov.profile =CARRIERS_50p_hybrid_noFH,
                                                 model.log.RR = ORs_hybrid_50p_noFH,
                                                 model.ref.dataset= as.data.frame(NHANES_50p_hybrid_noFH_list[[x]]),
                                                 model.ref.dataset.weights=NHANES_wts_50p_noFH_list[[x]],
                                                 return.refs.risk=TRUE)
    risk_noncarrier_50p_noFH<-subset(hybrid5_50p_noFH_byage$details,(hybrid5_50p_noFH_byage$details[,4]==0 & hybrid5_50p_noFH_byage$details[,5]==0 
         & hybrid5_50p_noFH_byage$details[,6]==0 & hybrid5_50p_noFH_byage$details[,7]==0 & hybrid5_50p_noFH_byage$details[,8]==0))[,3]
    risk_noncarrier_u80_noFH<-noncarrier_u50_noFH_age50+(1-noncarrier_u50_noFH_age50)*risk_noncarrier_50p_noFH
    mean_risk_noncarrier_u80_noFH[y,k]<-mean(risk_noncarrier_u80_noFH)
    
    risk_ATM_50p_noFH<-subset(hybrid5_50p_noFH_byage$details,hybrid5_50p_noFH_byage$details[,4]==1)[,3]
    risk_ATM_u80_noFH<-ATM_u50_noFH_age50+(1-ATM_u50_noFH_age50)*risk_ATM_50p_noFH
    mean_risk_ATM_u80_noFH[y,k]<-mean(risk_ATM_u80_noFH)
    
    risk_BRCA1_50p_noFH<-subset(hybrid5_50p_noFH_byage$details,hybrid5_50p_noFH_byage$details[,5]==1)[,3]
    risk_BRCA1_u80_noFH<-BRCA1_u50_noFH_age50+(1-BRCA1_u50_noFH_age50)*risk_BRCA1_50p_noFH
    mean_risk_BRCA1_u80_noFH[y,k]<-mean(risk_BRCA1_u80_noFH)
    
    risk_BRCA2_50p_noFH<-subset(hybrid5_50p_noFH_byage$details,hybrid5_50p_noFH_byage$details[,6]==1)[,3]
    risk_BRCA2_u80_noFH<-BRCA2_u50_noFH_age50+(1-BRCA2_u50_noFH_age50)*risk_BRCA2_50p_noFH
    mean_risk_BRCA2_u80_noFH[y,k]<-mean(risk_BRCA2_u80_noFH)
    
    risk_CHEK2_50p_noFH<-subset(hybrid5_50p_noFH_byage$details,hybrid5_50p_noFH_byage$details[,7]==1)[,3]
    risk_CHEK2_u80_noFH<-CHEK2_u50_noFH_age50+(1-CHEK2_u50_noFH_age50)*risk_CHEK2_50p_noFH
    mean_risk_CHEK2_u80_noFH[y,k]<-mean(risk_CHEK2_u80_noFH)
    
    risk_PALB2_50p_noFH<-subset(hybrid5_50p_noFH_byage$details,hybrid5_50p_noFH_byage$details[,8]==1)[,3]
    risk_PALB2_u80_noFH<-PALB2_u50_noFH_age50+(1-PALB2_u50_noFH_age50)*risk_PALB2_50p_noFH
    mean_risk_PALB2_u80_noFH[y,k]<-mean(risk_PALB2_u80_noFH)
    
    hybrid5_50p_FH_byage = computeAbsoluteRisk(model.snp.info = PV5_FH, 
                                                 model.disease.incidence.rates = Inc_all_FH,
                                                 model.competing.incidence.rates = Mort_all,
                                                 apply.age.start = 50,
                                                 apply.age.interval.length = k,
                                                 apply.snp.profile=CARRIERS_50p_PVs_FH5,
                                                 model.cov.info = hybrid_covar_list_50p,
                                                 model.formula = hybrid_formula_50p,
                                                 apply.cov.profile =CARRIERS_50p_hybrid_FH,
                                                 model.log.RR = ORs_hybrid_50p_FH,
                                                 model.ref.dataset= as.data.frame(NHANES_50p_hybrid_FH_list[[x]]),
                                                 model.ref.dataset.weights=NHANES_wts_50p_FH_list[[x]],
                                                 return.refs.risk=TRUE)
    
    risk_noncarrier_50p_FH<-subset(hybrid5_50p_FH_byage$details,(hybrid5_50p_FH_byage$details[,4]==0 & hybrid5_50p_FH_byage$details[,5]==0 
       & hybrid5_50p_FH_byage$details[,6]==0 & hybrid5_50p_FH_byage$details[,7]==0 & hybrid5_50p_FH_byage$details[,8]==0))[,3]
    risk_noncarrier_u80_FH<-noncarrier_u50_FH_age50+(1-noncarrier_u50_FH_age50)*risk_noncarrier_50p_FH
    mean_risk_noncarrier_u80_FH[y,k]<-mean(risk_noncarrier_u80_FH)
    
    risk_ATM_50p_FH<-subset(hybrid5_50p_FH_byage$details,hybrid5_50p_FH_byage$details[,4]==1)[,3]
    risk_ATM_u80_FH<-ATM_u50_FH_age50+(1-ATM_u50_FH_age50)*risk_ATM_50p_FH
    mean_risk_ATM_u80_FH[y,k]<-mean(risk_ATM_u80_FH)
    
    risk_BRCA1_50p_FH<-subset(hybrid5_50p_FH_byage$details,hybrid5_50p_FH_byage$details[,5]==1)[,3]
    risk_BRCA1_u80_FH<-BRCA1_u50_FH_age50+(1-BRCA1_u50_FH_age50)*risk_BRCA1_50p_FH
    mean_risk_BRCA1_u80_noFH[y,k]<-mean(risk_BRCA1_u80_FH)
    
    risk_BRCA2_50p_FH<-subset(hybrid5_50p_FH_byage$details,hybrid5_50p_FH_byage$details[,6]==1)[,3]
    risk_BRCA2_u80_FH<-BRCA2_u50_FH_age50+(1-BRCA2_u50_noFH_age50)*risk_BRCA2_50p_FH
    mean_risk_BRCA2_u80_FH[y,k]<-mean(risk_BRCA2_u80_FH)
    
    risk_CHEK2_50p_FH<-subset(hybrid5_50p_FH_byage$details,hybrid5_50p_FH_byage$details[,7]==1)[,3]
    risk_CHEK2_u80_FH<-CHEK2_u50_FH_age50+(1-CHEK2_u50_FH_age50)*risk_CHEK2_50p_FH
    mean_risk_CHEK2_u80_FH[y,k]<-mean(risk_CHEK2_u80_FH)
    
    risk_PALB2_50p_FH<-subset(hybrid5_50p_FH_byage$details,hybrid5_50p_FH_byage$details[,8]==1)[,3]
    risk_PALB2_u80_FH<-PALB2_u50_FH_age50+(1-PALB2_u50_FH_age50)*risk_PALB2_50p_FH
    mean_risk_PALB2_u80_FH[y,k]<-mean(risk_PALB2_u80_FH)
  }
}
}

noncarrier_noFH<-cbind(noncarrier_u50_noFH,mean_risk_noncarrier_u80_noFH)
ATM_noFH<-cbind(ATM_u50_noFH,mean_risk_ATM_u80_noFH)
BRCA1_noFH<-cbind(BRCA1_u50_noFH,mean_risk_BRCA1_u80_noFH)
BRCA2_noFH<-cbind(BRCA2_u50_noFH,mean_risk_BRCA2_u80_noFH)
CHEK2_noFH<-cbind(CHEK2_u50_noFH,mean_risk_CHEK2_u80_noFH)
PALB2_noFH<-cbind(PALB2_u50_noFH,mean_risk_PALB2_u80_noFH)

noncarrier_FH<-cbind(noncarrier_u50_FH,mean_risk_noncarrier_u80_FH)
ATM_FH<-cbind(ATM_u50_FH,mean_risk_ATM_u80_FH)
BRCA1_FH<-cbind(BRCA1_u50_FH,mean_risk_BRCA1_u80_FH)
BRCA2_FH<-cbind(BRCA2_u50_FH,mean_risk_BRCA2_u80_FH)
CHEK2_FH<-cbind(CHEK2_u50_FH,mean_risk_CHEK2_u80_FH)
PALB2_FH<-cbind(PALB2_u50_FH,mean_risk_PALB2_u80_FH)

quants <- c(0.025,0.50,0.975)

#### SAVE RESULTS AS NEEDED ####
results_noncarrier_noFH<-as.data.frame(t(apply(noncarrier_noFH,2,quantile,probs = quants,na.rm = TRUE)))
results_noncarrier_FH<-as.data.frame(t(apply(noncarrier_FH,2,quantile,probs = quants,na.rm = TRUE)))
results_ATM_noFH<-as.data.frame(t(apply(ATM_noFH,2,quantile,probs = quants,na.rm = TRUE)))
results_ATM_FH<-as.data.frame(t(apply(ATM_FH,2,quantile,probs = quants,na.rm = TRUE)))
results_BRCA1_noFH<-as.data.frame(t(apply(BRCA1_noFH,2,quantile,probs = quants,na.rm = TRUE)))
results_BRCA1_FH<-as.data.frame(t(apply(BRCA1_FH,2,quantile,probs = quants,na.rm = TRUE)))
results_BRCA2_noFH<-as.data.frame(t(apply(BRCA2_noFH,2,quantile,probs = quants,na.rm = TRUE)))
results_BRCA2_FH<-as.data.frame(t(apply(BRCA2_FH,2,quantile,probs = quants,na.rm = TRUE)))
results_CHEK2_noFH<-as.data.frame(t(apply(CHEK2_noFH,2,quantile,probs = quants,na.rm = TRUE)))
results_CHEK2_FH<-as.data.frame(t(apply(CHEK2_FH,2,quantile,probs = quants,na.rm = TRUE)))
results_PALB2_noFH<-as.data.frame(t(apply(PALB2_noFH,2,quantile,probs = quants,na.rm = TRUE)))
results_PALB2_FH<-as.data.frame(t(apply(PALB2_FH,2,quantile,probs = quants,na.rm = TRUE)))

write.csv(results_noncarrier_noFH, file = "results_noncarrier_noFH.csv")
write.csv(results_noncarrier_FH, file = "results_noncarrier_FH.csv")
write.csv(results_ATM_noFH, file = "results_ATM_noFH.csv")
write.csv(results_ATM_FH, file = "results_ATM_FH.csv")
write.csv(results_BRCA1_noFH, file = "results_BRCA1_noFH.csv")
write.csv(results_BRCA1_FH, file = "results_BRCA1_FH.csv")
write.csv(results_BRCA2_noFH, file = "results_BRCA2_noFH.csv")
write.csv(results_BRCA2_FH, file = "results_BRCA2_FH.csv")
write.csv(results_CHEK2_noFH, file = "results_CHEK2_noFH.csv")
write.csv(results_CHEK2_FH, file = "results_CHEK2_FH.csv")
write.csv(results_PALB2_noFH, file = "results_PALB2_noFH.csv")
write.csv(results_PALB2_FH, file = "results_PALB2_FH.csv")
