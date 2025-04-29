# setwd("/ddn/gs1/home/obrienkm2/CARRIERS")
#repeat with all of covars included 
library(readxl)
Inc <- data.frame(read_excel("inc_byage_raceeth_2024_04_17.xlsx",range = "A1:F91"))
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

Inc_all_FH/Inc_all_noFH

Mort <- data.frame(read_excel("mort_byage_raceeth_2024_04_17.xlsx",range = "A1:F91"))
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

#install.packages("iCARE_1.32.0.tar.gz", lib = "Rlibrary", repos = NULL, type="source")
#library("iCARE",lib = "Rlibrary")
library("iCARE")
library("pROC")

set.seed(123)

#need final version of data set to have 3 columns - PV name, PV odds ratio, PV freq (in non-cases)
#do separately for family history and no family history
#do based on random sampling from distribution
#PV_data <- read_excel("/Volumes/obrienkm2/Documents/CARRIERS/OBrien project/PV_DATA_updated_2024_04_17.xlsx")
PV_data <- read_excel("PV_DATA_updated_2024_09_30.xlsx")

PV_data_all<-PV_data[,c(1,8:10)]
names(PV_data_all)[1]<-"snp.name"
names(PV_data_all)[4]<-"snp.freq"
PV_data_all<-as.data.frame(PV_data_all)

PV_data_noFH<-PV_data[,1:4]
names(PV_data_noFH)[1]<-"snp.name"
names(PV_data_noFH)[4]<-"snp.freq"
PV_data_noFH<-as.data.frame(PV_data_noFH)

PV_data_FH<-PV_data[,c(1,5:7)]
names(PV_data_FH)[1]<-"snp.name"
names(PV_data_FH)[4]<-"snp.freq"
PV_data_FH<-as.data.frame(PV_data_FH)

#non-sampled versions of PV ORs to use for practice runs
logOR_all_nosamp<-matrix(nrow=12,ncol=1)
logOR_all_nosamp<-PV_data[,2]
OR_all_nosamp<-exp(logOR_all_nosamp)
PV_all_nosamp<-cbind(PV_data[1],OR_all_nosamp,PV_data[4])
names(PV_all_nosamp)<-c("snp.name","snp.odds.ratio","snp.freq")
PV_all_nosamp<-as.data.frame(PV_all_nosamp)

logOR_noFH_nosamp<-matrix(nrow=12,ncol=1)
logOR_noFH_nosamp<-PV_data_noFH[,2]
OR_noFH_nosamp<-exp(logOR_noFH_nosamp)
PV_noFH_nosamp<-cbind(PV_data_noFH[1],OR_noFH_nosamp,PV_data_noFH[4])
names(PV_noFH_nosamp)<-c("snp.name","snp.odds.ratio","snp.freq")
PV_noFH_nosamp<-as.data.frame(PV_noFH_nosamp)

logOR_FH_nosamp<-matrix(nrow=12,ncol=1)
logOR_FH_nosamp<-PV_data_FH[,2]
OR_FH_nosamp<-exp(logOR_FH_nosamp)
PV_FH_nosamp<-cbind(PV_data_FH[1],OR_FH_nosamp,PV_data_FH[4])
names(PV_FH_nosamp)<-c("snp.name","snp.odds.ratio","snp.freq")
PV_FH_nosamp<-as.data.frame(PV_FH_nosamp)

PV_all_nosamp
PV_noFH_nosamp
PV_FH_nosamp

#covariate information - list containing information on height, race/ethnicity, 
#age at menarche (years), 
#parity, age at first birth (years), Body Mass Index (kg/sq.m.), 
#alcohol (drinks/week), OC use
#for age 50+ model add in age at menopause and hormone therapy by BMI
#list all and then variable specific lists

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

#model formulas
#hybrid version
hybrid_formula_u50<-case ~ height + as.factor(raceeth) + as.factor(menarche) + parous + 
  as.factor(afb) + as.factor(bmi) + highalc 
hybrid_formula_50p<-case ~ height + as.factor(raceeth) + as.factor(menarche) + parous + 
  as.factor(afb) + as.factor(bmi) + as.factor(alc3) + EPHRT + as.factor(agemeno)

#cov profile for reference population = dataframe containing the covariate profiles for which absolute risk will be computed. 
#Covariates must be in same order with same names as in model.formula.
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
table(NHANES_u50_hybrid_noFH$impute)
NHANES_u50_hybrid_FH<-read.delim("outdata_u50_hybrid_FH.txt")
NHANES_50p_hybrid_noFH<-read.delim("outdata_50p_hybrid_noFH.txt")
dim(NHANES_50p_hybrid_noFH)
NHANES_50p_hybrid_FH<-read.delim("outdata_50p_hybrid_FH.txt")

#extract single impute of each
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

#model.log.RR = vector with log odds ratios corresponding to the model params; no intercept; 
#names must match design matrix arising from model.formula and model.cov.info; 
#check names using function check_design_matrix().
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

#real data- covariate file from CARRIERS population
CARRIERS_u50_hybrid_noFH<-read.delim("realdata_u50_hybrid_noFH.txt", na.strings =".")
CARRIERS_u50_hybrid_FH<-read.delim("realdata_u50_hybrid_FH.txt", na.strings =".")
CARRIERS_50p_hybrid_noFH<-read.delim("realdata_50p_hybrid_noFH.txt", na.strings =".")
CARRIERS_50p_hybrid_FH<-read.delim("realdata_50p_hybrid_FH.txt", na.strings =".")

#PV files
CARRIERS_u50_PVs<-read.delim("realdataPV_u50.txt")
CARRIERS_u50_PVs_noFH<-read.delim("realdataPV_u50_noFH.txt")
CARRIERS_u50_PVs_FH<-read.delim("realdataPV_u50_FH.txt")
CARRIERS_50p_PVs<-read.delim("realdataPV_50p.txt")
CARRIERS_50p_PVs_noFH<-read.delim("realdataPV_50p_noFH.txt")
CARRIERS_50p_PVs_FH<-read.delim("realdataPV_50p_FH.txt")

###updated 08 16 2024 to randomly draw from PV ORs ####
set.seed(654)
PVrisk_hybrid_noFH_u50_1000<-matrix(nrow=1000,ncol=9)
PVrisk_hybrid_noFH_50p_1000<-matrix(nrow=1000,ncol=9)
PVrisk_hybrid_FH_u50_1000<-matrix(nrow=1000,ncol=9)
PVrisk_hybrid_FH_50p_1000<-matrix(nrow=1000,ncol=9)

participant_risk_hybrid_noFH_u50<-matrix(nrow=9123,ncol=1000)
participant_risk_hybrid_noFH_50p<-matrix(nrow=43957,ncol=1000)
participant_risk_hybrid_FH_u50<-matrix(nrow=2655,ncol=1000)
participant_risk_hybrid_FH_50p<-matrix(nrow=11957,ncol=1000)

for (j in 1:200){
  cat(paste0("Bootstrap iteration ", j, "\n"))
  
  logOR_PV_noFH<-matrix(nrow=7,ncol=1)
  logOR_PV_FH<-matrix(nrow=7,ncol=1)
  
  for (i in 1:7){
    logOR_PV_noFH[i,]<-rnorm(1,mean=PV_data_noFH[i,2],sd=PV_data_noFH[i,3])}
  OR_PV_noFH<-exp(logOR_PV_noFH)
  PV7_noFH<-cbind(PV_data_noFH[1],OR_PV_noFH,PV_data_noFH[4])
  names(PV7_noFH)[2]<-"snp.odds.ratio"
  PV7_noFH<-as.data.frame(PV7_noFH)
  
  for (i in 1:7){
    logOR_PV_FH[i,]<-rnorm(1,mean=PV_data_FH[i,2],sd=PV_data_FH[i,3])}
  OR_PV_FH<-exp(logOR_PV_FH)
  PV7_FH<-cbind(PV_data_FH[1],OR_PV_FH,PV_data_FH[4])
  names(PV7_FH)[2]<-"snp.odds.ratio"
  PV7_FH<-as.data.frame(PV7_FH)
  
  for (x in 1:5){
    run_hybrid_noFH_u50 = computeAbsoluteRisk(model.snp.info = PV7_noFH, 
                                        model.disease.incidence.rates = Inc_all_noFH,
                                        model.competing.incidence.rates = Mort_all,
                                        apply.age.start = 20,
                                        apply.age.interval.length = 30,
                                        apply.snp.profile=CARRIERS_u50_PVs_noFH,
                                        model.cov.info = hybrid_covar_list_u50,
                                        model.formula = hybrid_formula_u50,
                                        apply.cov.profile =CARRIERS_u50_hybrid_noFH,
                                        model.log.RR = ORs_hybrid_u50_noFH,
                                        model.ref.dataset= as.data.frame(NHANES_u50_hybrid_noFH_list[[x]]),
                                        model.ref.dataset.weights=NHANES_wts_u50_noFH_list[[x]],
                                        return.refs.risk=TRUE)
    
    noncarriers<-subset(run_hybrid_noFH_u50$details,(run_hybrid_noFH_u50$details[,4]==0
       & run_hybrid_noFH_u50$details[,5]==0 & run_hybrid_noFH_u50$details[,6]==0 & run_hybrid_noFH_u50$details[,7]==0
       & run_hybrid_noFH_u50$details[,8]==0 & run_hybrid_noFH_u50$details[,9]==0 & run_hybrid_noFH_u50$details[,10]==0))
    ATM<-subset(run_hybrid_noFH_u50$details,run_hybrid_noFH_u50$details[,4]==1)
    BARD1<-subset(run_hybrid_noFH_u50$details,run_hybrid_noFH_u50$details[,5]==1)
    BRCA1<-subset(run_hybrid_noFH_u50$details,run_hybrid_noFH_u50$details[,6]==1)
    BRCA2<-subset(run_hybrid_noFH_u50$details,run_hybrid_noFH_u50$details[,7]==1)
    CHEK2<-subset(run_hybrid_noFH_u50$details,run_hybrid_noFH_u50$details[,8]==1)
    PALB2<-subset(run_hybrid_noFH_u50$details,run_hybrid_noFH_u50$details[,9]==1)
    RAD51C<-subset(run_hybrid_noFH_u50$details,run_hybrid_noFH_u50$details[,10]==1)
    anyPV<-subset(run_hybrid_noFH_u50$details,(run_hybrid_noFH_u50$details[,4]==1
         | run_hybrid_noFH_u50$details[,5]==1 | run_hybrid_noFH_u50$details[,6]==1 | run_hybrid_noFH_u50$details[,7]==1
         | run_hybrid_noFH_u50$details[,8]==1 | run_hybrid_noFH_u50$details[,9]==1 | run_hybrid_noFH_u50$details[,10]==1))
    
    PVrisk_hybrid_noFH_u50<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BARD1[,3]),mean(BRCA1[,3]),
                        mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]),mean(RAD51C[,3]),mean(anyPV[,3]))
    names(PVrisk_hybrid_noFH_u50)<-names<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2","CHEK2","PALB2","RAD51C","RAD51D")
    z<-(j-1)*5+x
    PVrisk_hybrid_noFH_u50_1000[z,]<-PVrisk_hybrid_noFH_u50
    participant_risk_hybrid_noFH_u50[,z]<-run_hybrid_noFH_u50$details$Risk_Estimate
    write.table(as.data.frame(t(PVrisk_hybrid_noFH_u50)), file="PVrisk_hybrid_noFH_u50_1000.csv", append = (z>1), col.names = (z==1), sep=",", row.names=FALSE)
  }
  
  for (x in 1:5){
    run_hybrid_noFH_50p = computeAbsoluteRisk(model.snp.info = PV7_noFH,
                                        model.disease.incidence.rates = Inc_all_noFH,
                                        model.competing.incidence.rates = Mort_all,
                                        apply.age.start = 50,
                                        apply.age.interval.length = 30,
                                        apply.snp.profile=CARRIERS_50p_PVs_noFH,
                                        model.cov.info = hybrid_covar_list_50p,
                                        model.formula = hybrid_formula_50p,
                                        apply.cov.profile =CARRIERS_50p_hybrid_noFH,
                                        model.log.RR = ORs_hybrid_50p_noFH,
                                        model.ref.dataset= as.data.frame(NHANES_50p_hybrid_noFH_list[[x]]),
                                        model.ref.dataset.weights=NHANES_wts_50p_noFH_list[[x]],
                                        return.refs.risk=TRUE)
    noncarriers<-subset(run_hybrid_noFH_50p$details,(run_hybrid_noFH_50p$details[,4]==0
      & run_hybrid_noFH_50p$details[,5]==0 & run_hybrid_noFH_50p$details[,6]==0 & run_hybrid_noFH_50p$details[,7]==0
      & run_hybrid_noFH_50p$details[,8]==0 & run_hybrid_noFH_50p$details[,9]==0 & run_hybrid_noFH_50p$details[,10]==0))
    ATM<-subset(run_hybrid_noFH_50p$details,run_hybrid_noFH_50p$details[,4]==1)
    BARD1<-subset(run_hybrid_noFH_50p$details,run_hybrid_noFH_50p$details[,5]==1)
    BRCA1<-subset(run_hybrid_noFH_50p$details,run_hybrid_noFH_50p$details[,6]==1)
    BRCA2<-subset(run_hybrid_noFH_50p$details,run_hybrid_noFH_50p$details[,7]==1)
    CHEK2<-subset(run_hybrid_noFH_50p$details,run_hybrid_noFH_50p$details[,8]==1)
    PALB2<-subset(run_hybrid_noFH_50p$details,run_hybrid_noFH_50p$details[,9]==1)
    RAD51C<-subset(run_hybrid_noFH_50p$details,run_hybrid_noFH_50p$details[,10]==1)
    anyPV<-subset(run_hybrid_noFH_50p$details,(run_hybrid_noFH_50p$details[,4]==1
         | run_hybrid_noFH_50p$details[,5]==1 | run_hybrid_noFH_50p$details[,6]==1 | run_hybrid_noFH_50p$details[,7]==1
        | run_hybrid_noFH_50p$details[,8]==1 | run_hybrid_noFH_50p$details[,9]==1 | run_hybrid_noFH_50p$details[,10]==1))
    
    PVrisk_hybrid_noFH_50p<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BARD1[,3]),mean(BRCA1[,3]),
                        mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]),mean(RAD51C[,3]),mean(anyPV[,3]))
    names(PVrisk_hybrid_noFH_50p)<-names<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2","CHEK2","PALB2","RAD51C","anyPV")
    z<-(j-1)*5+x
    PVrisk_hybrid_noFH_50p_1000[z,]<-PVrisk_hybrid_noFH_50p
    participant_risk_hybrid_noFH_50p[,z]<-run_hybrid_noFH_50p$details$Risk_Estimate
    write.table(as.data.frame(t(PVrisk_hybrid_noFH_50p)), file="PVrisk_hybrid_noFH_50p.csv", append = (z>1), col.names = (z==1), sep=",", row.names=FALSE)
  }
  
  for (x in 1:5){
    run_hybrid_FH_u50 = computeAbsoluteRisk(model.snp.info = PV7_FH,
                                       model.disease.incidence.rates = Inc_all_FH,
                                       model.competing.incidence.rates = Mort_all,
                                       apply.age.start = 20,
                                       apply.age.interval.length = 30,
                                       apply.snp.profile=CARRIERS_u50_PVs_FH,
                                       model.cov.info = hybrid_covar_list_u50,
                                       model.formula = hybrid_formula_u50,
                                       apply.cov.profile =CARRIERS_u50_hybrid_FH,
                                       model.log.RR = ORs_hybrid_u50_FH,
                                       model.ref.dataset= as.data.frame(NHANES_u50_hybrid_FH_list[[x]]),
                                       model.ref.dataset.weights=NHANES_wts_u50_FH_list[[x]],
                                       return.refs.risk=TRUE)
    
    noncarriers<-subset(run_hybrid_FH_u50$details,(run_hybrid_FH_u50$details[,4]==0
          & run_hybrid_FH_u50$details[,5]==0 & run_hybrid_FH_u50$details[,6]==0 & run_hybrid_FH_u50$details[,7]==0
          & run_hybrid_FH_u50$details[,8]==0 & run_hybrid_FH_u50$details[,9]==0 & run_hybrid_FH_u50$details[,10]==0))
    ATM<-subset(run_hybrid_FH_u50$details,run_hybrid_FH_u50$details[,4]==1)
    BARD1<-subset(run_hybrid_FH_u50$details,run_hybrid_FH_u50$details[,5]==1)
    BRCA1<-subset(run_hybrid_FH_u50$details,run_hybrid_FH_u50$details[,6]==1)
    BRCA2<-subset(run_hybrid_FH_u50$details,run_hybrid_FH_u50$details[,7]==1)
    CHEK2<-subset(run_hybrid_FH_u50$details,run_hybrid_FH_u50$details[,8]==1)
    PALB2<-subset(run_hybrid_FH_u50$details,run_hybrid_FH_u50$details[,9]==1)
    RAD51C<-subset(run_hybrid_FH_u50$details,run_hybrid_FH_u50$details[,10]==1)
    anyPV<-subset(run_hybrid_FH_u50$details,(run_hybrid_FH_u50$details[,4]==1
         | run_hybrid_FH_u50$details[,5]==1 | run_hybrid_FH_u50$details[,6]==1 | run_hybrid_FH_u50$details[,7]==1
        | run_hybrid_FH_u50$details[,8]==1 | run_hybrid_FH_u50$details[,9]==1 | run_hybrid_FH_u50$details[,10]==1))
    
    PVrisk_hybrid_FH_u50<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BARD1[,3]),mean(BRCA1[,3]),
                       mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]),mean(RAD51C[,3]),mean(anyPV[,3]))
    names(PVrisk_hybrid_FH_u50)<-names<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2","CHEK2","PALB2","RAD51C","anyPV")
    z<-(j-1)*5+x
    PVrisk_hybrid_FH_u50_1000[z,]<-PVrisk_hybrid_FH_u50
    participant_risk_hybrid_FH_u50[,z]<-run_hybrid_FH_u50$details$Risk_Estimate
    write.table(as.data.frame(t(PVrisk_hybrid_FH_u50)), file="PVrisk_hybrid_FH_u50.csv", append = (z>1), col.names = (z==1), sep=",", row.names=FALSE)
    
  }
  
  for (x in 1:5){
    run_hybrid_FH_50p = computeAbsoluteRisk(model.snp.info = PV7_FH,
                                       model.disease.incidence.rates = Inc_all_FH,
                                       model.competing.incidence.rates = Mort_all,
                                       apply.age.start = 50,
                                       apply.age.interval.length = 30,
                                       apply.snp.profile=CARRIERS_50p_PVs_FH,
                                       model.cov.info = hybrid_covar_list_50p,
                                       model.formula = hybrid_formula_50p,
                                       apply.cov.profile =CARRIERS_50p_hybrid_FH,
                                       model.log.RR = ORs_hybrid_50p_FH,
                                       model.ref.dataset= as.data.frame(NHANES_50p_hybrid_FH_list[[x]]),
                                       model.ref.dataset.weights=NHANES_wts_50p_FH_list[[x]],
                                       return.refs.risk=TRUE)
    
    noncarriers<-subset(run_hybrid_FH_50p$details,(run_hybrid_FH_50p$details[,4]==0
        & run_hybrid_FH_50p$details[,5]==0 & run_hybrid_FH_50p$details[,6]==0 & run_hybrid_FH_50p$details[,7]==0
        & run_hybrid_FH_50p$details[,8]==0 & run_hybrid_FH_50p$details[,9]==0 & run_hybrid_FH_50p$details[,10]==0))
    ATM<-subset(run_hybrid_FH_50p$details,run_hybrid_FH_50p$details[,4]==1)
    BARD1<-subset(run_hybrid_FH_50p$details,run_hybrid_FH_50p$details[,5]==1)
    BRCA1<-subset(run_hybrid_FH_50p$details,run_hybrid_FH_50p$details[,6]==1)
    BRCA2<-subset(run_hybrid_FH_50p$details,run_hybrid_FH_50p$details[,7]==1)
    CHEK2<-subset(run_hybrid_FH_50p$details,run_hybrid_FH_50p$details[,8]==1)
    PALB2<-subset(run_hybrid_FH_50p$details,run_hybrid_FH_50p$details[,9]==1)
    RAD51C<-subset(run_hybrid_FH_50p$details,run_hybrid_FH_50p$details[,10]==1)
    anyPV<-subset(run_hybrid_FH_50p$details,(run_hybrid_FH_50p$details[,4]==1 | run_hybrid_FH_50p$details[,5]==1 | run_hybrid_FH_50p$details[,6]==1 | run_hybrid_FH_50p$details[,7]==1
        | run_hybrid_FH_50p$details[,8]==1 | run_hybrid_FH_50p$details[,9]==1  | run_hybrid_FH_50p$details[,10]==1))
    
    PVrisk_hybrid_FH_50p<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BARD1[,3]),mean(BRCA1[,3]),
                       mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]),mean(RAD51C[,3]),mean(anyPV[,3]))
    names(PVrisk_hybrid_FH_50p)<-names<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2","CHEK2","PALB2","RAD51C","anyPV")
    z<-(j-1)*5+x
    PVrisk_hybrid_FH_50p_1000[z,]<-PVrisk_hybrid_FH_50p
    participant_risk_hybrid_FH_50p[,z]<-run_hybrid_FH_50p$details$Risk_Estimate
    write.table(as.data.frame(t(PVrisk_hybrid_FH_50p)), file="PVrisk_hybrid_FH_50p.csv", append = (z>1), col.names = (z==1), sep=",", row.names=FALSE)
  }
}

hybrid_noFH_u50_results<-matrix(nrow=9,ncol=3)
for (k in 1:9){
  hybrid_noFH_u50_results[k,]<-quantile(PVrisk_hybrid_noFH_u50_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)
}
rownames(hybrid_noFH_u50_results)<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2",
                               "CHEK2","PALB2","RAD51C","anyPV")
colnames(hybrid_noFH_u50_results)<-c("median","2.5th %","97.5 %")
print(hybrid_noFH_u50_results)
write.csv(hybrid_noFH_u50_results, file = "hybrid_noFH_u50_results.csv")

hybrid_noFH_50p_results<-matrix(nrow=9,ncol=3)
for (k in 1:9){
  hybrid_noFH_50p_results[k,]<-quantile(PVrisk_hybrid_noFH_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)
}
rownames(hybrid_noFH_50p_results)<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2",
                               "CHEK2","PALB2","RAD51C","anyPV")
colnames(hybrid_noFH_50p_results)<-c("median","2.5th %","97.5 %")
hybrid_noFH_50p_results
write.csv(hybrid_noFH_50p_results, file = "hybrid_noFH_50p_results.csv")

hybrid_FH_u50_results<-matrix(nrow=9,ncol=3)
for (k in 1:9){
  hybrid_FH_u50_results[k,]<-quantile(PVrisk_hybrid_FH_u50_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)
}
rownames(hybrid_FH_u50_results)<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2",
                              "CHEK2","PALB2","RAD51C","anyPV")
colnames(hybrid_FH_u50_results)<-c("median","2.5th %","97.5 %")
hybrid_FH_u50_results
write.csv(hybrid_FH_u50_results, file = "hybrid_FH_u50_results.csv")

hybrid_FH_50p_results<-matrix(nrow=9,ncol=3)
for (k in 1:9){
  hybrid_FH_50p_results[k,]<-quantile(PVrisk_hybrid_FH_50p_1000[,k],probs=c(0.5,0.025,0.975),na.rm=TRUE)
}
rownames(hybrid_FH_50p_results)<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2",
                              "CHEK2","PALB2","RAD51C","anyPV")
colnames(hybrid_FH_50p_results)<-c("median","2.5th %","97.5 %")
hybrid_FH_50p_results
write.csv(hybrid_FH_50p_results, file = "hybrid_FH_50p_results.csv")

#AUC - bind files with case_control status
CARRIERS_casestatus<-read.delim("carriers_case_status.txt")

CARRIERS_casestatus_u50_noFH<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age<50 & FH1st==0)
CARRIERS_casestatus_50p_noFH<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age>=50 & FH1st==0)
CARRIERS_casestatus_u50_FH<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age<50 & FH1st==1)
CARRIERS_casestatus_50p_FH<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age>=50 & FH1st==1)

CARRIERS_casestatus_u50_wFH<-rbind(CARRIERS_casestatus_u50_noFH,CARRIERS_casestatus_u50_FH)
CARRIERS_casestatus_50p_wFH<-rbind(CARRIERS_casestatus_50p_noFH,CARRIERS_casestatus_50p_FH)
dim(CARRIERS_casestatus_u50_wFH)
dim(CARRIERS_casestatus_50p_wFH)

participant_risk_hybrid_u50<-rbind(participant_risk_hybrid_noFH_u50,participant_risk_hybrid_FH_u50)
participant_risk_hybrid_50p<-rbind(participant_risk_hybrid_noFH_50p,participant_risk_hybrid_FH_50p)
dim(participant_risk_hybrid_u50)
dim(participant_risk_hybrid_50p)

roc_hybrid_u50<-matrix(nrow=1000)
roc_hybrid_50p<-matrix(nrow=1000)



for (x in 1:1000){
  roc_hybrid_u50[x]<-roc(CARRIERS_casestatus_u50_wFH$case50,participant_risk_hybrid_u50[,x])$auc
  roc_hybrid_50p[x]<-roc(CARRIERS_casestatus_50p_wFH$case50_80,participant_risk_hybrid_50p[,x])$auc
}

write.csv(roc_hybrid_u50, file = "roc_hybrid_u50.csv")
write.csv(roc_hybrid_50p, file = "roc_hybrid_50p.csv")

results_roc_hybrid_u50<-quantile(roc_hybrid_u50,probs=c(0.5,0.025,0.975),na.rm=TRUE)
results_roc_hybrid_50p<-quantile(roc_hybrid_50p,probs=c(0.5,0.025,0.975),na.rm=TRUE)

results_roc_hybrid<-cbind(results_roc_hybrid_u50,results_roc_hybrid_50p)

write.csv(results_roc_hybrid, file = "results_roc_hybrid.csv")
