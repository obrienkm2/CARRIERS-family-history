
PV5_u50_noFH_nulliparous_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_noFH_parous_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_noFH_nonobese_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_noFH_obese_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_noFH_lowalc_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_noFH_highalc_1000<-matrix(nrow=1000,ncol=6)

PV5_u50_noFH_pardiff_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_noFH_obesediff_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_noFH_alcdiff_1000<-matrix(nrow=1000,ncol=6)

PV5_50p_noFH_nulliparous_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_parous_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_nonobese_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_obese_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_lowalc_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_highalc_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_noEPHRT_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_EPHRT_1000<-matrix(nrow=1000,ncol=6)

PV5_50p_noFH_pardiff_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_obesediff_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_alcdiff_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_noFH_EPHRTdiff_1000<-matrix(nrow=1000,ncol=6)

PV5_u50_FH_nulliparous_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_FH_parous_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_FH_nonobese_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_FH_obese_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_FH_lowalc_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_FH_highalc_1000<-matrix(nrow=1000,ncol=6)

PV5_u50_FH_pardiff_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_FH_obesediff_1000<-matrix(nrow=1000,ncol=6)
PV5_u50_FH_alcdiff_1000<-matrix(nrow=1000,ncol=6)

PV5_50p_FH_nulliparous_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_parous_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_nonobese_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_obese_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_lowalc_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_highalc_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_noEPHRT_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_EPHRT_1000<-matrix(nrow=1000,ncol=6)

PV5_50p_FH_pardiff_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_obesediff_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_alcdiff_1000<-matrix(nrow=1000,ncol=6)
PV5_50p_FH_EPHRTdiff_1000<-matrix(nrow=1000,ncol=6)

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

set.seed(97531)

 for (j in 1:200){
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
  
  # for (x in 1:5){
  # hybrid5_u50_noFH = computeAbsoluteRisk(model.snp.info = PV5_noFH,
  #                                           model.disease.incidence.rates = Inc_all_noFH,
  #                                           model.competing.incidence.rates = Mort_all,
  #                                           apply.age.start = 20,
  #                                           apply.age.interval.length = 30,
  #                                           apply.snp.profile=CARRIERS_u50_PVs_noFH5,
  #                                           model.cov.info = hybrid_covar_list_u50,
  #                                           model.formula = hybrid_formula_u50,
  #                                           apply.cov.profile =CARRIERS_u50_hybrid_noFH,
  #                                           model.log.RR = ORs_hybrid_u50_noFH,
  #                                           model.ref.dataset= as.data.frame(NHANES_u50_hybrid_noFH_list[[x]]),
  #                                           model.ref.dataset.weights=NHANES_wts_u50_noFH_list[[x]],
  #                                           return.refs.risk=TRUE)
  # 
  #       noncarriers_nulliparous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==0 & hybrid5_u50_noFH$details[,5]==0 
  #            & hybrid5_u50_noFH$details[,6]==0 & hybrid5_u50_noFH$details[,7]==0 & hybrid5_u50_noFH$details[,8]==0 & hybrid5_u50_noFH$details[,12]==0))
  #       noncarriers_parous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==0 & hybrid5_u50_noFH$details[,5]==0 
  #            & hybrid5_u50_noFH$details[,6]==0 & hybrid5_u50_noFH$details[,7]==0 & hybrid5_u50_noFH$details[,8]==0 & hybrid5_u50_noFH$details[,12]==1))
  #       noncarriers_nonobese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==0 & hybrid5_u50_noFH$details[,5]==0 
  #            & hybrid5_u50_noFH$details[,6]==0 & hybrid5_u50_noFH$details[,7]==0 & hybrid5_u50_noFH$details[,8]==0 & 
  #             (hybrid5_u50_noFH$details[,14]==1 | hybrid5_u50_noFH$details[,14]==2)))
  #       noncarriers_obese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==0 & hybrid5_u50_noFH$details[,5]==0 
  #           & hybrid5_u50_noFH$details[,6]==0 & hybrid5_u50_noFH$details[,7]==0 & hybrid5_u50_noFH$details[,8]==0 & hybrid5_u50_noFH$details[,14]==3))
  #       noncarriers_lowalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==0 & hybrid5_u50_noFH$details[,5]==0 
  #           & hybrid5_u50_noFH$details[,6]==0 & hybrid5_u50_noFH$details[,7]==0 & hybrid5_u50_noFH$details[,8]==0 & hybrid5_u50_noFH$details[,15]==0))
  #       noncarriers_highalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==0 & hybrid5_u50_noFH$details[,5]==0 
  #           & hybrid5_u50_noFH$details[,6]==0 & hybrid5_u50_noFH$details[,7]==0 & hybrid5_u50_noFH$details[,8]==0 & hybrid5_u50_noFH$details[,15]==1))
  #       
  #       atm_nulliparous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==1 & hybrid5_u50_noFH$details[,12]==0))
  #       atm_parous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==1 & hybrid5_u50_noFH$details[,12]==1))
  #       atm_nonobese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==1 & (hybrid5_u50_noFH$details[,14]==1 | hybrid5_u50_noFH$details[,14]==2)))
  #       atm_obese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==1 & hybrid5_u50_noFH$details[,14]==3))
  #       atm_lowalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==1 & hybrid5_u50_noFH$details[,15]==0))
  #       atm_highalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,4]==1 & hybrid5_u50_noFH$details[,15]==1))
  #       
  #       brca1_nulliparous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,5]==1 & hybrid5_u50_noFH$details[,12]==0))
  #       brca1_parous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,5]==1 & hybrid5_u50_noFH$details[,12]==1))
  #       brca1_nonobese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,5]==1 & (hybrid5_u50_noFH$details[,14]==1 | hybrid5_u50_noFH$details[,14]==2)))
  #       brca1_obese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,5]==1 & hybrid5_u50_noFH$details[,14]==3))
  #       brca1_lowalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,5]==1 & hybrid5_u50_noFH$details[,15]==0))
  #       brca1_highalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,5]==1 & hybrid5_u50_noFH$details[,15]==1))
  #       
  #       brca2_nulliparous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,6]==1 & hybrid5_u50_noFH$details[,12]==0))
  #       brca2_parous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,6]==1 & hybrid5_u50_noFH$details[,12]==1))
  #       brca2_nonobese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,6]==1 & (hybrid5_u50_noFH$details[,14]==1 | hybrid5_u50_noFH$details[,14]==2)))
  #       brca2_obese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,6]==1 & hybrid5_u50_noFH$details[,14]==3))
  #       brca2_lowalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,6]==1 & hybrid5_u50_noFH$details[,15]==0))
  #       brca2_highalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,6]==1 & hybrid5_u50_noFH$details[,15]==1))
  #       
  #       chek2_nulliparous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,7 ]==1 & hybrid5_u50_noFH$details[,12]==0))
  #       chek2_parous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,7]==1 & hybrid5_u50_noFH$details[,12]==1))
  #       chek2_nonobese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,7]==1 & (hybrid5_u50_noFH$details[,14]==1 | hybrid5_u50_noFH$details[,14]==2)))
  #       chek2_obese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,7]==1 & hybrid5_u50_noFH$details[,14]==3))
  #       chek2_lowalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,7]==1 & hybrid5_u50_noFH$details[,15]==0))
  #       chek2_highalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,7]==1 & hybrid5_u50_noFH$details[,15]==1))
  #       
  #       palb2_nulliparous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,8]==1 & hybrid5_u50_noFH$details[,12]==0))
  #       palb2_parous<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,8]==1 & hybrid5_u50_noFH$details[,12]==1))
  #       palb2_nonobese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,8]==1 & (hybrid5_u50_noFH$details[,14]==1 | hybrid5_u50_noFH$details[,14]==2)))
  #       palb2_obese<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,8]==1 & hybrid5_u50_noFH$details[,14]==3))
  #       palb2_lowalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,8]==1 & hybrid5_u50_noFH$details[,15]==0))
  #       palb2_highalc<-subset(hybrid5_u50_noFH$details,(hybrid5_u50_noFH$details[,8]==1 & hybrid5_u50_noFH$details[,15]==1))
  #       
  # PV5risk_u50_noFH_nulliparous<-c(mean(noncarriers_nulliparous[,3]),mean(atm_nulliparous[,3]),mean(brca1_nulliparous[,3]),mean(brca2_nulliparous[,3]),
  #                                 mean(chek2_nulliparous[,3]),mean(palb2_nulliparous[,3]))
  # PV5risk_u50_noFH_parous<-c(mean(noncarriers_parous[,3]),mean(atm_parous[,3]),mean(brca1_parous[,3]),mean(brca2_parous[,3]),
  #                                 mean(chek2_parous[,3]),mean(palb2_parous[,3]))
  # PV5risk_u50_noFH_nonobese<-c(mean(noncarriers_nonobese[,3]),mean(atm_nonobese[,3]),mean(brca1_nonobese[,3]),mean(brca2_nonobese[,3]),
  #                                 mean(chek2_nonobese[,3]),mean(palb2_nonobese[,3]))
  # PV5risk_u50_noFH_obese<-c(mean(noncarriers_obese[,3]),mean(atm_obese[,3]),mean(brca1_obese[,3]),mean(brca2_obese[,3]),
  #                                 mean(chek2_obese[,3]),mean(palb2_obese[,3]))
  # PV5risk_u50_noFH_lowalc<-c(mean(noncarriers_lowalc[,3]),mean(atm_lowalc[,3]),mean(brca1_lowalc[,3]),mean(brca2_lowalc[,3]),
  #                                 mean(chek2_lowalc[,3]),mean(palb2_lowalc[,3]))
  # PV5risk_u50_noFH_highalc<-c(mean(noncarriers_highalc[,3]),mean(atm_highalc[,3]),mean(brca1_highalc[,3]),mean(brca2_highalc[,3]),
  #                                 mean(chek2_highalc[,3]),mean(palb2_highalc[,3]))
  # 
  # PV5risk_u50_noFH_pardiff<-PV5risk_u50_noFH_parous-PV5risk_u50_noFH_nulliparous
  # PV5risk_u50_noFH_obesediff<-PV5risk_u50_noFH_obese-PV5risk_u50_noFH_nonobese
  # PV5risk_u50_noFH_alcdiff<-PV5risk_u50_noFH_highalc-PV5risk_u50_noFH_lowalc 
  # 
  # names(PV5risk_u50_noFH_nulliparous)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # names(PV5risk_u50_noFH_parous)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # names(PV5risk_u50_noFH_nonobese)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # names(PV5risk_u50_noFH_obese)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # names(PV5risk_u50_noFH_lowalc)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # names(PV5risk_u50_noFH_highalc)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # names(PV5risk_u50_noFH_pardiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # names(PV5risk_u50_noFH_obesediff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # names(PV5risk_u50_noFH_alcdiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
  # 
  # z<-(j-1)*5+x
  # 
  # PV5_u50_noFH_nulliparous_1000[z,]<-PV5risk_u50_noFH_nulliparous
  # PV5_u50_noFH_parous_1000[z,]<-PV5risk_u50_noFH_parous
  # PV5_u50_noFH_nonobese_1000[z,]<-PV5risk_u50_noFH_nonobese
  # PV5_u50_noFH_obese_1000[z,]<-PV5risk_u50_noFH_obese
  # PV5_u50_noFH_lowalc_1000[z,]<-PV5risk_u50_noFH_lowalc
  # PV5_u50_noFH_highalc_1000[z,]<-PV5risk_u50_noFH_highalc
  # PV5_u50_noFH_pardiff_1000[z,]<-PV5risk_u50_noFH_pardiff
  # PV5_u50_noFH_obesediff_1000[z,]<-PV5risk_u50_noFH_obesediff
  # PV5_u50_noFH_alcdiff_1000[z,]<-PV5risk_u50_noFH_alcdiff
  # 
  # }
   
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

     noncarriers_nulliparous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0 & hybrid5_50p_noFH$details[,5]==0 
           & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0 & hybrid5_50p_noFH$details[,8]==0 & hybrid5_50p_noFH$details[,12]==0))
     noncarriers_parous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0 & hybrid5_50p_noFH$details[,5]==0 
           & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0 & hybrid5_50p_noFH$details[,8]==0 & hybrid5_50p_noFH$details[,12]==1))
     noncarriers_nonobese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0 & hybrid5_50p_noFH$details[,5]==0 
           & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0 & hybrid5_50p_noFH$details[,8]==0 & 
           (hybrid5_50p_noFH$details[,14]==1 | hybrid5_50p_noFH$details[,14]==2)))
     noncarriers_obese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0 & hybrid5_50p_noFH$details[,5]==0 
          & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0 & hybrid5_50p_noFH$details[,8]==0 & hybrid5_50p_noFH$details[,14]==3))
     noncarriers_lowalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0 & hybrid5_50p_noFH$details[,5]==0 
          & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0 & hybrid5_50p_noFH$details[,8]==0 
          & (hybrid5_50p_noFH$details[,15]==1 | hybrid5_50p_noFH$details[,15]==2)))
     noncarriers_highalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0 & hybrid5_50p_noFH$details[,5]==0 
           & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0 & hybrid5_50p_noFH$details[,8]==0 & hybrid5_50p_noFH$details[,15]==3))
     noncarriers_noEPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0 & hybrid5_50p_noFH$details[,5]==0 
          & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0 & hybrid5_50p_noFH$details[,8]==0 & hybrid5_50p_noFH$details[,16]==0))
     noncarriers_EPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==0 & hybrid5_50p_noFH$details[,5]==0 
          & hybrid5_50p_noFH$details[,6]==0 & hybrid5_50p_noFH$details[,7]==0 & hybrid5_50p_noFH$details[,8]==0 & hybrid5_50p_noFH$details[,16]==1))
     
     
     atm_nulliparous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==1 & hybrid5_50p_noFH$details[,12]==0))
     atm_parous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==1 & hybrid5_50p_noFH$details[,12]==1))
     atm_nonobese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==1 & (hybrid5_50p_noFH$details[,14]==1 | hybrid5_50p_noFH$details[,14]==2)))
     atm_obese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==1 & hybrid5_50p_noFH$details[,14]==3))
     atm_lowalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==1 & (hybrid5_50p_noFH$details[,15]==1 | hybrid5_50p_noFH$details[,15]==2)))
     atm_highalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==1 & hybrid5_50p_noFH$details[,15]==3))
     atm_noEPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==1 & hybrid5_50p_noFH$details[,16]==0))
     atm_EPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,4]==1 & hybrid5_50p_noFH$details[,16]==1))
     
     brca1_nulliparous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,5]==1 & hybrid5_50p_noFH$details[,12]==0))
     brca1_parous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,5]==1 & hybrid5_50p_noFH$details[,12]==1))
     brca1_nonobese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,5]==1 & (hybrid5_50p_noFH$details[,14]==1 | hybrid5_50p_noFH$details[,14]==2)))
     brca1_obese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,5]==1 & hybrid5_50p_noFH$details[,14]==3))
     brca1_lowalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,5]==1 & (hybrid5_50p_noFH$details[,15]==1 | hybrid5_50p_noFH$details[,15]==2)))
     brca1_highalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,5]==1 & hybrid5_50p_noFH$details[,15]==3))
     brca1_noEPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,5]==1 & hybrid5_50p_noFH$details[,16]==0))
     brca1_EPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,5]==1 & hybrid5_50p_noFH$details[,16]==1))
     
     brca2_nulliparous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,6]==1 & hybrid5_50p_noFH$details[,12]==0))
     brca2_parous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,6]==1 & hybrid5_50p_noFH$details[,12]==1))
     brca2_nonobese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,6]==1 & (hybrid5_50p_noFH$details[,14]==1 | hybrid5_50p_noFH$details[,14]==2)))
     brca2_obese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,6]==1 & hybrid5_50p_noFH$details[,14]==3))
     brca2_lowalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,6]==1 & (hybrid5_50p_noFH$details[,15]==1 | hybrid5_50p_noFH$details[,15]==2)))
     brca2_highalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,6]==1 & hybrid5_50p_noFH$details[,15]==3))
     brca2_noEPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,6]==1 & hybrid5_50p_noFH$details[,16]==0))
     brca2_EPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,6]==1 & hybrid5_50p_noFH$details[,16]==1))
     
     chek2_nulliparous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,7 ]==1 & hybrid5_50p_noFH$details[,12]==0))
     chek2_parous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,7]==1 & hybrid5_50p_noFH$details[,12]==1))
     chek2_nonobese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,7]==1 & (hybrid5_50p_noFH$details[,14]==1 | hybrid5_50p_noFH$details[,14]==2)))
     chek2_obese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,7]==1 & hybrid5_50p_noFH$details[,14]==3))
     chek2_lowalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,7]==1 & (hybrid5_50p_noFH$details[,15]==1 | hybrid5_50p_noFH$details[,15]==2)))
     chek2_highalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,7]==1 & hybrid5_50p_noFH$details[,15]==3))
     chek2_noEPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,7]==1 & hybrid5_50p_noFH$details[,16]==0))
     chek2_EPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,7]==1 & hybrid5_50p_noFH$details[,16]==1))
     
     palb2_nulliparous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,8]==1 & hybrid5_50p_noFH$details[,12]==0))
     palb2_parous<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,8]==1 & hybrid5_50p_noFH$details[,12]==1))
     palb2_nonobese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,8]==1 & (hybrid5_50p_noFH$details[,14]==1 | hybrid5_50p_noFH$details[,14]==2)))
     palb2_obese<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,8]==1 & hybrid5_50p_noFH$details[,14]==3))
     palb2_lowalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,8]==1 & (hybrid5_50p_noFH$details[,15]==1 | hybrid5_50p_noFH$details[,15]==2)))
     palb2_highalc<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,8]==1 & hybrid5_50p_noFH$details[,15]==3))
     palb2_noEPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,8]==1 & hybrid5_50p_noFH$details[,16]==0))
     palb2_EPHRT<-subset(hybrid5_50p_noFH$details,(hybrid5_50p_noFH$details[,8]==1 & hybrid5_50p_noFH$details[,16]==1))
     
     PV5risk_50p_noFH_nulliparous<-c(mean(noncarriers_nulliparous[,3]),mean(atm_nulliparous[,3]),mean(brca1_nulliparous[,3]),mean(brca2_nulliparous[,3]),
                                     mean(chek2_nulliparous[,3]),mean(palb2_nulliparous[,3]))
     PV5risk_50p_noFH_parous<-c(mean(noncarriers_parous[,3]),mean(atm_parous[,3]),mean(brca1_parous[,3]),mean(brca2_parous[,3]),
                                mean(chek2_parous[,3]),mean(palb2_parous[,3]))
     PV5risk_50p_noFH_nonobese<-c(mean(noncarriers_nonobese[,3]),mean(atm_nonobese[,3]),mean(brca1_nonobese[,3]),mean(brca2_nonobese[,3]),
                                  mean(chek2_nonobese[,3]),mean(palb2_nonobese[,3]))
     PV5risk_50p_noFH_obese<-c(mean(noncarriers_obese[,3]),mean(atm_obese[,3]),mean(brca1_obese[,3]),mean(brca2_obese[,3]),
                               mean(chek2_obese[,3]),mean(palb2_obese[,3]))
     PV5risk_50p_noFH_lowalc<-c(mean(noncarriers_lowalc[,3]),mean(atm_lowalc[,3]),mean(brca1_lowalc[,3]),mean(brca2_lowalc[,3]),
                                mean(chek2_lowalc[,3]),mean(palb2_lowalc[,3]))
     PV5risk_50p_noFH_highalc<-c(mean(noncarriers_highalc[,3]),mean(atm_highalc[,3]),mean(brca1_highalc[,3]),mean(brca2_highalc[,3]),
                                 mean(chek2_highalc[,3]),mean(palb2_highalc[,3]))
     PV5risk_50p_noFH_noEPHRT<-c(mean(noncarriers_noEPHRT[,3]),mean(atm_noEPHRT[,3]),mean(brca1_noEPHRT[,3]),mean(brca2_noEPHRT[,3]),
                                 mean(chek2_noEPHRT[,3]),mean(palb2_noEPHRT[,3]))
     PV5risk_50p_noFH_EPHRT<-c(mean(noncarriers_EPHRT[,3]),mean(atm_EPHRT[,3]),mean(brca1_EPHRT[,3]),mean(brca2_EPHRT[,3]),
                                 mean(chek2_EPHRT[,3]),mean(palb2_EPHRT[,3]))
     
     PV5risk_50p_noFH_pardiff<-PV5risk_50p_noFH_parous-PV5risk_50p_noFH_nulliparous
     PV5risk_50p_noFH_obesediff<-PV5risk_50p_noFH_obese-PV5risk_50p_noFH_nonobese
     PV5risk_50p_noFH_alcdiff<-PV5risk_50p_noFH_highalc-PV5risk_50p_noFH_lowalc 
     PV5risk_50p_noFH_EPHRTdiff<-PV5risk_50p_noFH_EPHRT-PV5risk_50p_noFH_noEPHRT 
     
     names(PV5risk_50p_noFH_nulliparous)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_parous)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_nonobese)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_obese)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_lowalc)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_highalc)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_pardiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_obesediff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_alcdiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_noFH_EPHRTdiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     
     z<-(j-1)*5+x
     
     PV5_50p_noFH_nulliparous_1000[z,]<-PV5risk_50p_noFH_nulliparous
     PV5_50p_noFH_parous_1000[z,]<-PV5risk_50p_noFH_parous
     PV5_50p_noFH_nonobese_1000[z,]<-PV5risk_50p_noFH_nonobese
     PV5_50p_noFH_obese_1000[z,]<-PV5risk_50p_noFH_obese
     PV5_50p_noFH_lowalc_1000[z,]<-PV5risk_50p_noFH_lowalc
     PV5_50p_noFH_highalc_1000[z,]<-PV5risk_50p_noFH_highalc
     PV5_50p_noFH_noEPHRT_1000[z,]<-PV5risk_50p_noFH_noEPHRT
     PV5_50p_noFH_EPHRT_1000[z,]<-PV5risk_50p_noFH_EPHRT
     PV5_50p_noFH_pardiff_1000[z,]<-PV5risk_50p_noFH_pardiff
     PV5_50p_noFH_obesediff_1000[z,]<-PV5risk_50p_noFH_obesediff
     PV5_50p_noFH_alcdiff_1000[z,]<-PV5risk_50p_noFH_alcdiff
     PV5_50p_noFH_EPHRTdiff_1000[z,]<-PV5risk_50p_noFH_EPHRTdiff
     
   }
   
   # for (x in 1:5){
   #   hybrid5_u50_FH = computeAbsoluteRisk(model.snp.info = PV5_FH,
   #                                          model.disease.incidence.rates = Inc_all_FH,
   #                                          model.competing.incidence.rates = Mort_all,
   #                                          apply.age.start = 20,
   #                                          apply.age.interval.length = 30,
   #                                          apply.snp.profile=CARRIERS_u50_PVs_FH5,
   #                                          model.cov.info = hybrid_covar_list_u50,
   #                                          model.formula = hybrid_formula_u50,
   #                                          apply.cov.profile =CARRIERS_u50_hybrid_FH,
   #                                          model.log.RR = ORs_hybrid_u50_FH,
   #                                          model.ref.dataset= as.data.frame(NHANES_u50_hybrid_FH_list[[x]]),
   #                                          model.ref.dataset.weights=NHANES_wts_u50_FH_list[[x]],
   #                                          return.refs.risk=TRUE)
   #   
   #   noncarriers_nulliparous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==0 & hybrid5_u50_FH$details[,5]==0 
   #          & hybrid5_u50_FH$details[,6]==0 & hybrid5_u50_FH$details[,7]==0 & hybrid5_u50_FH$details[,8]==0 & hybrid5_u50_FH$details[,12]==0))
   #   noncarriers_parous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==0 & hybrid5_u50_FH$details[,5]==0 
   #          & hybrid5_u50_FH$details[,6]==0 & hybrid5_u50_FH$details[,7]==0 & hybrid5_u50_FH$details[,8]==0 & hybrid5_u50_FH$details[,12]==1))
   #   noncarriers_nonobese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==0 & hybrid5_u50_FH$details[,5]==0 
   #          & hybrid5_u50_FH$details[,6]==0 & hybrid5_u50_FH$details[,7]==0 & hybrid5_u50_FH$details[,8]==0 & 
   #          (hybrid5_u50_FH$details[,14]==1 | hybrid5_u50_FH$details[,14]==2)))
   #   noncarriers_obese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==0 & hybrid5_u50_FH$details[,5]==0 
   #           & hybrid5_u50_FH$details[,6]==0 & hybrid5_u50_FH$details[,7]==0 & hybrid5_u50_FH$details[,8]==0 & hybrid5_u50_FH$details[,14]==3))
   #   noncarriers_lowalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==0 & hybrid5_u50_FH$details[,5]==0 
   #           & hybrid5_u50_FH$details[,6]==0 & hybrid5_u50_FH$details[,7]==0 & hybrid5_u50_FH$details[,8]==0 & hybrid5_u50_FH$details[,15]==0))
   #   noncarriers_highalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==0 & hybrid5_u50_FH$details[,5]==0 
   #          & hybrid5_u50_FH$details[,6]==0 & hybrid5_u50_FH$details[,7]==0 & hybrid5_u50_FH$details[,8]==0 & hybrid5_u50_FH$details[,15]==1))
   #   
   #   atm_nulliparous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==1 & hybrid5_u50_FH$details[,12]==0))
   #   atm_parous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==1 & hybrid5_u50_FH$details[,12]==1))
   #   atm_nonobese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==1 & (hybrid5_u50_FH$details[,14]==1 | hybrid5_u50_FH$details[,14]==2)))
   #   atm_obese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==1 & hybrid5_u50_FH$details[,14]==3))
   #   atm_lowalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==1 & hybrid5_u50_FH$details[,15]==0))
   #   atm_highalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,4]==1 & hybrid5_u50_FH$details[,15]==1))
   #   
   #   brca1_nulliparous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,5]==1 & hybrid5_u50_FH$details[,12]==0))
   #   brca1_parous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,5]==1 & hybrid5_u50_FH$details[,12]==1))
   #   brca1_nonobese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,5]==1 & (hybrid5_u50_FH$details[,14]==1 | hybrid5_u50_FH$details[,14]==2)))
   #   brca1_obese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,5]==1 & hybrid5_u50_FH$details[,14]==3))
   #   brca1_lowalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,5]==1 & hybrid5_u50_FH$details[,15]==0))
   #   brca1_highalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,5]==1 & hybrid5_u50_FH$details[,15]==1))
   #   
   #   brca2_nulliparous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,6]==1 & hybrid5_u50_FH$details[,12]==0))
   #   brca2_parous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,6]==1 & hybrid5_u50_FH$details[,12]==1))
   #   brca2_nonobese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,6]==1 & (hybrid5_u50_FH$details[,14]==1 | hybrid5_u50_FH$details[,14]==2)))
   #   brca2_obese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,6]==1 & hybrid5_u50_FH$details[,14]==3))
   #   brca2_lowalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,6]==1 & hybrid5_u50_FH$details[,15]==0))
   #   brca2_highalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,6]==1 & hybrid5_u50_FH$details[,15]==1))
   #   
   #   chek2_nulliparous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,7 ]==1 & hybrid5_u50_FH$details[,12]==0))
   #   chek2_parous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,7]==1 & hybrid5_u50_FH$details[,12]==1))
   #   chek2_nonobese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,7]==1 & (hybrid5_u50_FH$details[,14]==1 | hybrid5_u50_FH$details[,14]==2)))
   #   chek2_obese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,7]==1 & hybrid5_u50_FH$details[,14]==3))
   #   chek2_lowalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,7]==1 & hybrid5_u50_FH$details[,15]==0))
   #   chek2_highalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,7]==1 & hybrid5_u50_FH$details[,15]==1))
   #   
   #   palb2_nulliparous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,8]==1 & hybrid5_u50_FH$details[,12]==0))
   #   palb2_parous<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,8]==1 & hybrid5_u50_FH$details[,12]==1))
   #   palb2_nonobese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,8]==1 & (hybrid5_u50_FH$details[,14]==1 | hybrid5_u50_FH$details[,14]==2)))
   #   palb2_obese<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,8]==1 & hybrid5_u50_FH$details[,14]==3))
   #   palb2_lowalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,8]==1 & hybrid5_u50_FH$details[,15]==0))
   #   palb2_highalc<-subset(hybrid5_u50_FH$details,(hybrid5_u50_FH$details[,8]==1 & hybrid5_u50_FH$details[,15]==1))
   #   
   #   PV5risk_u50_FH_nulliparous<-c(mean(noncarriers_nulliparous[,3]),mean(atm_nulliparous[,3]),mean(brca1_nulliparous[,3]),mean(brca2_nulliparous[,3]),
   #                                   mean(chek2_nulliparous[,3]),mean(palb2_nulliparous[,3]))
   #   PV5risk_u50_FH_parous<-c(mean(noncarriers_parous[,3]),mean(atm_parous[,3]),mean(brca1_parous[,3]),mean(brca2_parous[,3]),
   #                              mean(chek2_parous[,3]),mean(palb2_parous[,3]))
   #   PV5risk_u50_FH_nonobese<-c(mean(noncarriers_nonobese[,3]),mean(atm_nonobese[,3]),mean(brca1_nonobese[,3]),mean(brca2_nonobese[,3]),
   #                                mean(chek2_nonobese[,3]),mean(palb2_nonobese[,3]))
   #   PV5risk_u50_FH_obese<-c(mean(noncarriers_obese[,3]),mean(atm_obese[,3]),mean(brca1_obese[,3]),mean(brca2_obese[,3]),
   #                             mean(chek2_obese[,3]),mean(palb2_obese[,3]))
   #   PV5risk_u50_FH_lowalc<-c(mean(noncarriers_lowalc[,3]),mean(atm_lowalc[,3]),mean(brca1_lowalc[,3]),mean(brca2_lowalc[,3]),
   #                              mean(chek2_lowalc[,3]),mean(palb2_lowalc[,3]))
   #   PV5risk_u50_FH_highalc<-c(mean(noncarriers_highalc[,3]),mean(atm_highalc[,3]),mean(brca1_highalc[,3]),mean(brca2_highalc[,3]),
   #                               mean(chek2_highalc[,3]),mean(palb2_highalc[,3]))
   #   
   #   PV5risk_u50_FH_pardiff<-PV5risk_u50_FH_parous-PV5risk_u50_FH_nulliparous
   #   PV5risk_u50_FH_obesediff<-PV5risk_u50_FH_obese-PV5risk_u50_FH_nonobese
   #   PV5risk_u50_FH_alcdiff<-PV5risk_u50_FH_highalc-PV5risk_u50_FH_lowalc 
   #   
   #   names(PV5risk_u50_FH_nulliparous)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   names(PV5risk_u50_FH_parous)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   names(PV5risk_u50_FH_nonobese)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   names(PV5risk_u50_FH_obese)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   names(PV5risk_u50_FH_lowalc)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   names(PV5risk_u50_FH_highalc)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   names(PV5risk_u50_FH_pardiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   names(PV5risk_u50_FH_obesediff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   names(PV5risk_u50_FH_alcdiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
   #   
   #   z<-(j-1)*5+x
   #   
   #   PV5_u50_FH_nulliparous_1000[z,]<-PV5risk_u50_FH_nulliparous
   #   PV5_u50_FH_parous_1000[z,]<-PV5risk_u50_FH_parous
   #   PV5_u50_FH_nonobese_1000[z,]<-PV5risk_u50_FH_nonobese
   #   PV5_u50_FH_obese_1000[z,]<-PV5risk_u50_FH_obese
   #   PV5_u50_FH_lowalc_1000[z,]<-PV5risk_u50_FH_lowalc
   #   PV5_u50_FH_highalc_1000[z,]<-PV5risk_u50_FH_highalc
   #   PV5_u50_FH_pardiff_1000[z,]<-PV5risk_u50_FH_pardiff
   #   PV5_u50_FH_obesediff_1000[z,]<-PV5risk_u50_FH_obesediff
   #   PV5_u50_FH_alcdiff_1000[z,]<-PV5risk_u50_FH_alcdiff
   #   
   # }
   # 
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
     
     noncarriers_nulliparous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0 & hybrid5_50p_FH$details[,5]==0 
         & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0 & hybrid5_50p_FH$details[,8]==0 & hybrid5_50p_FH$details[,12]==0))
     noncarriers_parous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0 & hybrid5_50p_FH$details[,5]==0 
        & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0 & hybrid5_50p_FH$details[,8]==0 & hybrid5_50p_FH$details[,12]==1))
     noncarriers_nonobese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0 & hybrid5_50p_FH$details[,5]==0 
        & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0 & hybrid5_50p_FH$details[,8]==0 & 
        (hybrid5_50p_FH$details[,14]==1 | hybrid5_50p_FH$details[,14]==2)))
     noncarriers_obese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0 & hybrid5_50p_FH$details[,5]==0 
       & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0 & hybrid5_50p_FH$details[,8]==0 & hybrid5_50p_FH$details[,14]==3))
     noncarriers_lowalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0 & hybrid5_50p_FH$details[,5]==0 
       & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0 & hybrid5_50p_FH$details[,8]==0 
       & (hybrid5_50p_FH$details[,15]==1 | hybrid5_50p_FH$details[,15]==2)))
     noncarriers_highalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0 & hybrid5_50p_FH$details[,5]==0 
       & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0 & hybrid5_50p_FH$details[,8]==0 & hybrid5_50p_FH$details[,15]==3))
     noncarriers_noEPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0 & hybrid5_50p_FH$details[,5]==0 
       & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0 & hybrid5_50p_FH$details[,8]==0 & hybrid5_50p_FH$details[,16]==0))
     noncarriers_EPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==0 & hybrid5_50p_FH$details[,5]==0 
       & hybrid5_50p_FH$details[,6]==0 & hybrid5_50p_FH$details[,7]==0 & hybrid5_50p_FH$details[,8]==0 & hybrid5_50p_FH$details[,16]==1))
     
     
     atm_nulliparous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==1 & hybrid5_50p_FH$details[,12]==0))
     atm_parous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==1 & hybrid5_50p_FH$details[,12]==1))
     atm_nonobese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==1 & (hybrid5_50p_FH$details[,14]==1 | hybrid5_50p_FH$details[,14]==2)))
     atm_obese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==1 & hybrid5_50p_FH$details[,14]==3))
     atm_lowalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==1 & (hybrid5_50p_FH$details[,15]==1 | hybrid5_50p_FH$details[,15]==2)))
     atm_highalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==1 & hybrid5_50p_FH$details[,15]==3))
     atm_noEPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==1 & hybrid5_50p_FH$details[,16]==0))
     atm_EPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,4]==1 & hybrid5_50p_FH$details[,16]==1))
     
     brca1_nulliparous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,5]==1 & hybrid5_50p_FH$details[,12]==0))
     brca1_parous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,5]==1 & hybrid5_50p_FH$details[,12]==1))
     brca1_nonobese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,5]==1 & (hybrid5_50p_FH$details[,14]==1 | hybrid5_50p_FH$details[,14]==2)))
     brca1_obese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,5]==1 & hybrid5_50p_FH$details[,14]==3))
     brca1_lowalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,5]==1 & (hybrid5_50p_FH$details[,15]==1 | hybrid5_50p_FH$details[,15]==2)))
     brca1_highalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,5]==1 & hybrid5_50p_FH$details[,15]==3))
     brca1_noEPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,5]==1 & hybrid5_50p_FH$details[,16]==0))
     brca1_EPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,5]==1 & hybrid5_50p_FH$details[,16]==1))
     
     brca2_nulliparous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,6]==1 & hybrid5_50p_FH$details[,12]==0))
     brca2_parous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,6]==1 & hybrid5_50p_FH$details[,12]==1))
     brca2_nonobese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,6]==1 & (hybrid5_50p_FH$details[,14]==1 | hybrid5_50p_FH$details[,14]==2)))
     brca2_obese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,6]==1 & hybrid5_50p_FH$details[,14]==3))
     brca2_lowalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,6]==1 & (hybrid5_50p_FH$details[,15]==1 | hybrid5_50p_FH$details[,15]==2)))
     brca2_highalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,6]==1 & hybrid5_50p_FH$details[,15]==3))
     brca2_noEPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,6]==1 & hybrid5_50p_FH$details[,16]==0))
     brca2_EPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,6]==1 & hybrid5_50p_FH$details[,16]==1))
     
     chek2_nulliparous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,7 ]==1 & hybrid5_50p_FH$details[,12]==0))
     chek2_parous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,7]==1 & hybrid5_50p_FH$details[,12]==1))
     chek2_nonobese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,7]==1 & (hybrid5_50p_FH$details[,14]==1 | hybrid5_50p_FH$details[,14]==2)))
     chek2_obese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,7]==1 & hybrid5_50p_FH$details[,14]==3))
     chek2_lowalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,7]==1 & (hybrid5_50p_FH$details[,15]==1 | hybrid5_50p_FH$details[,15]==2)))
     chek2_highalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,7]==1 & hybrid5_50p_FH$details[,15]==3))
     chek2_noEPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,7]==1 & hybrid5_50p_FH$details[,16]==0))
     chek2_EPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,7]==1 & hybrid5_50p_FH$details[,16]==1))
     
     palb2_nulliparous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,8]==1 & hybrid5_50p_FH$details[,12]==0))
     palb2_parous<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,8]==1 & hybrid5_50p_FH$details[,12]==1))
     palb2_nonobese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,8]==1 & (hybrid5_50p_FH$details[,14]==1 | hybrid5_50p_FH$details[,14]==2)))
     palb2_obese<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,8]==1 & hybrid5_50p_FH$details[,14]==3))
     palb2_lowalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,8]==1 & (hybrid5_50p_FH$details[,15]==1 | hybrid5_50p_FH$details[,15]==2)))
     palb2_highalc<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,8]==1 & hybrid5_50p_FH$details[,15]==3))
     palb2_noEPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,8]==1 & hybrid5_50p_FH$details[,16]==0))
     palb2_EPHRT<-subset(hybrid5_50p_FH$details,(hybrid5_50p_FH$details[,8]==1 & hybrid5_50p_FH$details[,16]==1))
     
     PV5risk_50p_FH_nulliparous<-c(mean(noncarriers_nulliparous[,3]),mean(atm_nulliparous[,3]),mean(brca1_nulliparous[,3]),mean(brca2_nulliparous[,3]),
                                     mean(chek2_nulliparous[,3]),mean(palb2_nulliparous[,3]))
     PV5risk_50p_FH_parous<-c(mean(noncarriers_parous[,3]),mean(atm_parous[,3]),mean(brca1_parous[,3]),mean(brca2_parous[,3]),
                                mean(chek2_parous[,3]),mean(palb2_parous[,3]))
     PV5risk_50p_FH_nonobese<-c(mean(noncarriers_nonobese[,3]),mean(atm_nonobese[,3]),mean(brca1_nonobese[,3]),mean(brca2_nonobese[,3]),
                                  mean(chek2_nonobese[,3]),mean(palb2_nonobese[,3]))
     PV5risk_50p_FH_obese<-c(mean(noncarriers_obese[,3]),mean(atm_obese[,3]),mean(brca1_obese[,3]),mean(brca2_obese[,3]),
                               mean(chek2_obese[,3]),mean(palb2_obese[,3]))
     PV5risk_50p_FH_lowalc<-c(mean(noncarriers_lowalc[,3]),mean(atm_lowalc[,3]),mean(brca1_lowalc[,3]),mean(brca2_lowalc[,3]),
                                mean(chek2_lowalc[,3]),mean(palb2_lowalc[,3]))
     PV5risk_50p_FH_highalc<-c(mean(noncarriers_highalc[,3]),mean(atm_highalc[,3]),mean(brca1_highalc[,3]),mean(brca2_highalc[,3]),
                                 mean(chek2_highalc[,3]),mean(palb2_highalc[,3]))
     PV5risk_50p_FH_noEPHRT<-c(mean(noncarriers_noEPHRT[,3]),mean(atm_noEPHRT[,3]),mean(brca1_noEPHRT[,3]),mean(brca2_noEPHRT[,3]),
                                 mean(chek2_noEPHRT[,3]),mean(palb2_noEPHRT[,3]))
     PV5risk_50p_FH_EPHRT<-c(mean(noncarriers_EPHRT[,3]),mean(atm_EPHRT[,3]),mean(brca1_EPHRT[,3]),mean(brca2_EPHRT[,3]),
                               mean(chek2_EPHRT[,3]),mean(palb2_EPHRT[,3]))
     
     PV5risk_50p_FH_pardiff<-PV5risk_50p_FH_parous-PV5risk_50p_FH_nulliparous
     PV5risk_50p_FH_obesediff<-PV5risk_50p_FH_obese-PV5risk_50p_FH_nonobese
     PV5risk_50p_FH_alcdiff<-PV5risk_50p_FH_highalc-PV5risk_50p_FH_lowalc 
     PV5risk_50p_FH_EPHRTdiff<-PV5risk_50p_FH_EPHRT-PV5risk_50p_FH_noEPHRT 
     
     names(PV5risk_50p_FH_nulliparous)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_parous)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_nonobese)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_obese)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_lowalc)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_highalc)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_pardiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_obesediff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_alcdiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     names(PV5risk_50p_FH_EPHRTdiff)<-names<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
     
     z<-(j-1)*5+x
     
     PV5_50p_FH_nulliparous_1000[z,]<-PV5risk_50p_FH_nulliparous
     PV5_50p_FH_parous_1000[z,]<-PV5risk_50p_FH_parous
     PV5_50p_FH_nonobese_1000[z,]<-PV5risk_50p_FH_nonobese
     PV5_50p_FH_obese_1000[z,]<-PV5risk_50p_FH_obese
     PV5_50p_FH_lowalc_1000[z,]<-PV5risk_50p_FH_lowalc
     PV5_50p_FH_highalc_1000[z,]<-PV5risk_50p_FH_highalc
     PV5_50p_FH_noEPHRT_1000[z,]<-PV5risk_50p_FH_noEPHRT
     PV5_50p_FH_EPHRT_1000[z,]<-PV5risk_50p_FH_EPHRT
     PV5_50p_FH_pardiff_1000[z,]<-PV5risk_50p_FH_pardiff
     PV5_50p_FH_obesediff_1000[z,]<-PV5risk_50p_FH_obesediff
     PV5_50p_FH_alcdiff_1000[z,]<-PV5risk_50p_FH_alcdiff
     PV5_50p_FH_EPHRTdiff_1000[z,]<-PV5risk_50p_FH_EPHRTdiff
     
   }
   
 }
   
# nulliparous_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   nulliparous_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_nulliparous_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(nulliparous_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(nulliparous_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# nulliparous_u50_noFH_results
# write.csv(nulliparous_u50_noFH_results, file = "nulliparous_u50_noFH_results.csv")
# 
# parous_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   parous_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_parous_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(parous_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(parous_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# parous_u50_noFH_results
# write.csv(parous_u50_noFH_results, file = "parous_u50_noFH_results.csv")
# 
# pardiff_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   pardiff_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_pardiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(pardiff_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(pardiff_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# pardiff_u50_noFH_results
# write.csv(pardiff_u50_noFH_results, file = "pardiff_u50_noFH_results.csv")
# 
# nonobese_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   nonobese_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_nonobese_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(nonobese_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(nonobese_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# nonobese_u50_noFH_results
# write.csv(nonobese_u50_noFH_results, file = "nonobese_u50_noFH_results.csv")
# 
# obese_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   obese_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_obese_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(obese_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(obese_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# obese_u50_noFH_results
# write.csv(obese_u50_noFH_results, file = "obese_u50_noFH_results.csv")
# 
# obesediff_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   obesediff_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_obesediff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(obesediff_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(obesediff_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# obesediff_u50_noFH_results
# write.csv(obesediff_u50_noFH_results, file = "obesediff_u50_noFH_results.csv")
# 
# lowalc_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   lowalc_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_lowalc_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(lowalc_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(lowalc_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# lowalc_u50_noFH_results
# write.csv(lowalc_u50_noFH_results, file = "lowalc_u50_noFH_results.csv")
# 
# highalc_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   highalc_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_highalc_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(highalc_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(highalc_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# highalc_u50_noFH_results
# write.csv(highalc_u50_noFH_results, file = "highalc_u50_noFH_results.csv")
# 
# alcdiff_u50_noFH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   alcdiff_u50_noFH_results[k,]<-quantile(PV5_u50_noFH_alcdiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(alcdiff_u50_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(alcdiff_u50_noFH_results)<-c("median","2.5th %","97.5 %")
# alcdiff_u50_noFH_results
# write.csv(alcdiff_u50_noFH_results, file = "alcdiff_u50_noFH_results.csv")
# 





nulliparous_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  nulliparous_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_nulliparous_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(nulliparous_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(nulliparous_50p_noFH_results)<-c("median","2.5th %","97.5 %")
nulliparous_50p_noFH_results
write.csv(nulliparous_50p_noFH_results, file = "nulliparous_50p_noFH_results.csv")

parous_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  parous_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_parous_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(parous_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(parous_50p_noFH_results)<-c("median","2.5th %","97.5 %")
parous_50p_noFH_results
write.csv(parous_50p_noFH_results, file = "parous_50p_noFH_results.csv")

pardiff_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  pardiff_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_pardiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(pardiff_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(pardiff_50p_noFH_results)<-c("median","2.5th %","97.5 %")
pardiff_50p_noFH_results
write.csv(pardiff_50p_noFH_results, file = "pardiff_50p_noFH_results.csv")

nonobese_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  nonobese_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_nonobese_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(nonobese_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(nonobese_50p_noFH_results)<-c("median","2.5th %","97.5 %")
nonobese_50p_noFH_results
write.csv(nonobese_50p_noFH_results, file = "nonobese_50p_noFH_results.csv")

obese_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  obese_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_obese_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(obese_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(obese_50p_noFH_results)<-c("median","2.5th %","97.5 %")
obese_50p_noFH_results
write.csv(obese_50p_noFH_results, file = "obese_50p_noFH_results.csv")

obesediff_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  obesediff_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_obesediff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(obesediff_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(obesediff_50p_noFH_results)<-c("median","2.5th %","97.5 %")
obesediff_50p_noFH_results
write.csv(obesediff_50p_noFH_results, file = "obesediff_50p_noFH_results.csv")

lowalc_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  lowalc_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_lowalc_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(lowalc_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(lowalc_50p_noFH_results)<-c("median","2.5th %","97.5 %")
lowalc_50p_noFH_results
write.csv(lowalc_50p_noFH_results, file = "lowalc_50p_noFH_results.csv")

highalc_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  highalc_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_highalc_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(highalc_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(highalc_50p_noFH_results)<-c("median","2.5th %","97.5 %")
highalc_50p_noFH_results
write.csv(highalc_50p_noFH_results, file = "highalc_50p_noFH_results.csv")

alcdiff_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  alcdiff_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_alcdiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(alcdiff_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(alcdiff_50p_noFH_results)<-c("median","2.5th %","97.5 %")
alcdiff_50p_noFH_results
write.csv(alcdiff_50p_noFH_results, file = "alcdiff_50p_noFH_results.csv")

noEPHRT_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  noEPHRT_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_noEPHRT_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(noEPHRT_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(noEPHRT_50p_noFH_results)<-c("median","2.5th %","97.5 %")
noEPHRT_50p_noFH_results
write.csv(noEPHRT_50p_noFH_results, file = "noEPHRT_50p_noFH_results.csv")

EPHRT_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  EPHRT_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_EPHRT_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(EPHRT_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(EPHRT_50p_noFH_results)<-c("median","2.5th %","97.5 %")
EPHRT_50p_noFH_results
write.csv(EPHRT_50p_noFH_results, file = "EPHRT_50p_noFH_results.csv")

EPHRTdiff_50p_noFH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
 EPHRTdiff_50p_noFH_results[k,]<-quantile(PV5_50p_noFH_EPHRTdiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(EPHRTdiff_50p_noFH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(EPHRTdiff_50p_noFH_results)<-c("median","2.5th %","97.5 %")
EPHRTdiff_50p_noFH_results
write.csv(EPHRTdiff_50p_noFH_results, file = "EPHRTdiff_50p_noFH_results.csv")
# 
# nulliparous_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   nulliparous_u50_FH_results[k,]<-quantile(PV5_u50_FH_nulliparous_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(nulliparous_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(nulliparous_u50_FH_results)<-c("median","2.5th %","97.5 %")
# nulliparous_u50_FH_results
# write.csv(nulliparous_u50_FH_results, file = "nulliparous_u50_FH_results.csv")
# 
# parous_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   parous_u50_FH_results[k,]<-quantile(PV5_u50_FH_parous_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(parous_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(parous_u50_FH_results)<-c("median","2.5th %","97.5 %")
# parous_u50_FH_results
# write.csv(parous_u50_FH_results, file = "parous_u50_FH_results.csv")
# 
# pardiff_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   pardiff_u50_FH_results[k,]<-quantile(PV5_u50_FH_pardiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(pardiff_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(pardiff_u50_FH_results)<-c("median","2.5th %","97.5 %")
# pardiff_u50_FH_results
# write.csv(pardiff_u50_FH_results, file = "pardiff_u50_FH_results.csv")
# 
# nonobese_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   nonobese_u50_FH_results[k,]<-quantile(PV5_u50_FH_nonobese_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(nonobese_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(nonobese_u50_FH_results)<-c("median","2.5th %","97.5 %")
# nonobese_u50_FH_results
# write.csv(nonobese_u50_FH_results, file = "nonobese_u50_FH_results.csv")
# 
# obese_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   obese_u50_FH_results[k,]<-quantile(PV5_u50_FH_obese_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(obese_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(obese_u50_FH_results)<-c("median","2.5th %","97.5 %")
# obese_u50_FH_results
# write.csv(obese_u50_FH_results, file = "obese_u50_FH_results.csv")
# 
# obesediff_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   obesediff_u50_FH_results[k,]<-quantile(PV5_u50_FH_obesediff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(obesediff_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(obesediff_u50_FH_results)<-c("median","2.5th %","97.5 %")
# obesediff_u50_FH_results
# write.csv(obesediff_u50_FH_results, file = "obesediff_u50_FH_results.csv")
# 
# lowalc_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   lowalc_u50_FH_results[k,]<-quantile(PV5_u50_FH_lowalc_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(lowalc_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(lowalc_u50_FH_results)<-c("median","2.5th %","97.5 %")
# lowalc_u50_FH_results
# write.csv(lowalc_u50_FH_results, file = "lowalc_u50_FH_results.csv")
# 
# highalc_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   highalc_u50_FH_results[k,]<-quantile(PV5_u50_FH_highalc_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(highalc_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(highalc_u50_FH_results)<-c("median","2.5th %","97.5 %")
# highalc_u50_FH_results
# write.csv(highalc_u50_FH_results, file = "highalc_u50_FH_results.csv")
# 
# alcdiff_u50_FH_results<-matrix(nrow=6,ncol=3)
# for (k in 1:6){
#   alcdiff_u50_FH_results[k,]<-quantile(PV5_u50_FH_alcdiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
# rownames(alcdiff_u50_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
# colnames(alcdiff_u50_FH_results)<-c("median","2.5th %","97.5 %")
# alcdiff_u50_FH_results
# write.csv(alcdiff_u50_FH_results, file = "alcdiff_u50_FH_results.csv")
# 
# 
# 



nulliparous_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  nulliparous_50p_FH_results[k,]<-quantile(PV5_50p_FH_nulliparous_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(nulliparous_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(nulliparous_50p_FH_results)<-c("median","2.5th %","97.5 %")
nulliparous_50p_FH_results
write.csv(nulliparous_50p_FH_results, file = "nulliparous_50p_FH_results.csv")

parous_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  parous_50p_FH_results[k,]<-quantile(PV5_50p_FH_parous_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(parous_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(parous_50p_FH_results)<-c("median","2.5th %","97.5 %")
parous_50p_FH_results
write.csv(parous_50p_FH_results, file = "parous_50p_FH_results.csv")

pardiff_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  pardiff_50p_FH_results[k,]<-quantile(PV5_50p_FH_pardiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(pardiff_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(pardiff_50p_FH_results)<-c("median","2.5th %","97.5 %")
pardiff_50p_FH_results
write.csv(pardiff_50p_FH_results, file = "pardiff_50p_FH_results.csv")

nonobese_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  nonobese_50p_FH_results[k,]<-quantile(PV5_50p_FH_nonobese_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(nonobese_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(nonobese_50p_FH_results)<-c("median","2.5th %","97.5 %")
nonobese_50p_FH_results
write.csv(nonobese_50p_FH_results, file = "nonobese_50p_FH_results.csv")

obese_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  obese_50p_FH_results[k,]<-quantile(PV5_50p_FH_obese_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(obese_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(obese_50p_FH_results)<-c("median","2.5th %","97.5 %")
obese_50p_FH_results
write.csv(obese_50p_FH_results, file = "obese_50p_FH_results.csv")

obesediff_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  obesediff_50p_FH_results[k,]<-quantile(PV5_50p_FH_obesediff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(obesediff_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(obesediff_50p_FH_results)<-c("median","2.5th %","97.5 %")
obesediff_50p_FH_results
write.csv(obesediff_50p_FH_results, file = "obesediff_50p_FH_results.csv")

lowalc_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  lowalc_50p_FH_results[k,]<-quantile(PV5_50p_FH_lowalc_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(lowalc_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(lowalc_50p_FH_results)<-c("median","2.5th %","97.5 %")
lowalc_50p_FH_results
write.csv(lowalc_50p_FH_results, file = "lowalc_50p_FH_results.csv")

highalc_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  highalc_50p_FH_results[k,]<-quantile(PV5_50p_FH_highalc_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(highalc_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(highalc_50p_FH_results)<-c("median","2.5th %","97.5 %")
highalc_50p_FH_results
write.csv(highalc_50p_FH_results, file = "highalc_50p_FH_results.csv")

alcdiff_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  alcdiff_50p_FH_results[k,]<-quantile(PV5_50p_FH_alcdiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(alcdiff_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(alcdiff_50p_FH_results)<-c("median","2.5th %","97.5 %")
alcdiff_50p_FH_results
write.csv(alcdiff_50p_FH_results, file = "alcdiff_50p_FH_results.csv")

noEPHRT_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  noEPHRT_50p_FH_results[k,]<-quantile(PV5_50p_FH_noEPHRT_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(noEPHRT_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(noEPHRT_50p_FH_results)<-c("median","2.5th %","97.5 %")
noEPHRT_50p_FH_results
write.csv(noEPHRT_50p_FH_results, file = "noEPHRT_50p_FH_results.csv")

EPHRT_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  EPHRT_50p_FH_results[k,]<-quantile(PV5_50p_FH_EPHRT_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(EPHRT_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(EPHRT_50p_FH_results)<-c("median","2.5th %","97.5 %")
EPHRT_50p_FH_results
write.csv(EPHRT_50p_FH_results, file = "EPHRT_50p_FH_results.csv")

EPHRTdiff_50p_FH_results<-matrix(nrow=6,ncol=3)
for (k in 1:6){
  EPHRTdiff_50p_FH_results[k,]<-quantile(PV5_50p_FH_EPHRTdiff_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)}
rownames(EPHRTdiff_50p_FH_results)<-c("noncarriers","ATM","BRCA1","BRCA2","CHEK2","PALB2")
colnames(EPHRTdiff_50p_FH_results)<-c("median","2.5th %","97.5 %")
EPHRTdiff_50p_FH_results
write.csv(EPHRTdiff_50p_FH_results, file = "EPHRTdiff_50p_FH_results.csv")