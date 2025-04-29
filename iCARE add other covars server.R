#repeat with all of covars included 

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
  list_EPHRT[1]<-"highalc"
  list_EPHRT[2]<-"continuous"
  
iCARE_covar_list_u50<- list(list_famhist,list_height,list_menarche,list_parity,list_afb,
                      list_bmi,list_alcohol,list_OCuse)
iCARE_covar_list_u50
iCARE_covar_list_50p<- list(list_famhist,list_height,list_menarche,list_parity,list_afb,
                      list_bmi,list_alcohol,list_OCuse,list_HRT,list_agemeno)
iCARE_covar_list_50p

#CARRIERS list also includes race/ethnicity, FH-specific ones don't need famhist term
CARR_covar_list_u50<- list(list_famhist,list_height,list_raceeth,list_menarche,list_parity,list_afb,
                            list_bmi,list_alcohol,list_OCuse)
CARR_covar_list_u50
CARR_covar_list_50p<- list(list_famhist,list_height,list_raceeth,list_menarche,list_parity,list_afb,
                            list_bmi,list_alcohol,list_OCuse,list_HRT,list_agemeno)
CARR_covar_list_50p

#hybrid version includes raceeth drops OCuse simplify parity and alcohol and BMI*HRT
hybrid_covar_list_u50<- list(list_height,list_raceeth,list_menarche,list_parous,list_afb,
                             list_bmi,list_highalc)
hybrid_covar_list_u50
hybrid_covar_list_50p<- list(list_height,list_raceeth,list_menarche,list_parous,list_afb,
                             list_bmi,list_alc3,list_EPHRT,list_agemeno)
hybrid_covar_list_50p

#model formulas
#iCARE versions
iCARE_formula_u50<-case ~ famhist + height + as.factor(menarche) + as.factor(parity) + 
  as.factor(afb) + as.factor(bmi) + as.factor(alcohol) + OCuse
iCARE_formula_50p<-case ~ famhist + height + as.factor(menarche) + as.factor(parity) + 
  as.factor(afb) + as.factor(bmi) + as.factor(alcohol) + OCuse + as.factor(HRT) + as.factor(agemeno) +
  as.factor(bmi)*as.factor(HRT) 
#CARRIERS versions - add raceeth
CARR_formula_u50<-case ~ famhist + height + as.factor(raceeth) + as.factor(menarche) + as.factor(parity) + 
  as.factor(afb) + as.factor(bmi) + as.factor(alcohol) + OCuse
CARR_formula_50p<-case ~ famhist + height + as.factor(raceeth) + as.factor(menarche) + as.factor(parity) + 
  as.factor(afb) + as.factor(bmi) + as.factor(alcohol) + OCuse + as.factor(HRT) + as.factor(agemeno) +
  as.factor(bmi)*as.factor(HRT) 
#hybrid version
hybrid_formula_u50<-case ~ height + as.factor(raceeth) + as.factor(menarche) + parous + 
  as.factor(afb) + as.factor(bmi) + highalc 
hybrid_formula_50p<-case ~ height + as.factor(raceeth) + as.factor(menarche) + parous + 
  as.factor(afb) + as.factor(bmi) + as.factor(alc3) + EPHRT + as.factor(agemeno)

#cov profile for reference population = dataframe containing the covariate profiles for which absolute risk will be computed. 
#Covariates must be in same order with same names as in model.formula.
#import sampling weights (same for each version of prediction model)
NHANES_wts_u50<-as.matrix(read.delim("wts_u50.txt"))
NHANES_wts_50p<-as.matrix(read.delim("wts_50p.txt"))
NHANES_wts_u50_noFH<-as.matrix(read.delim("wts_u50_noFH.txt"))
NHANES_wts_50p_noFH<-as.matrix(read.delim("wts_50p_noFH.txt"))
NHANES_wts_u50_FH<-as.matrix(read.delim("wts_u50_FH.txt"))
NHANES_wts_50p_FH<-as.matrix(read.delim("wts_50p_FH.txt"))

#import each version of the data set with the correct variable list
NHANES_u50_iCARE<-read.delim("outdata_u50_icare.txt")
NHANES_50p_iCARE<-read.delim("outdata_50p_icare.txt")
NHANES_u50_CARR<-read.delim("outdata_u50_CARR.txt")
NHANES_50p_CARR<-read.delim("outdata_50p_CARR.txt")
NHANES_u50_hybrid_noFH<-read.delim("outdata_u50_hybrid_noFH.txt")
NHANES_u50_hybrid_FH<-read.delim("outdata_u50_hybrid_FH.txt")
NHANES_50p_hybrid_noFH<-read.delim("outdata_50p_hybrid_NoFH.txt")
NHANES_50p_hybrid_FH<-read.delim("outdata_50p_hybrid_FH.txt")

#extract single impute of each
for (x in 1:5) {
  assign(paste("NHANES_u50_iCARE_",x,sep=""),subset(NHANES_u50_iCARE, NHANES_u50_iCARE$impute==x)[,1:8] ) 
  assign(paste("NHANES_50p_iCARE_",x,sep=""),subset(NHANES_50p_iCARE, NHANES_50p_iCARE$impute==x)[,1:10] )
  assign(paste("NHANES_u50_CARR_",x,sep=""),subset(NHANES_u50_CARR, NHANES_u50_CARR$impute==x)[,1:9] )
  assign(paste("NHANES_50p_CARR_",x,sep=""),subset(NHANES_50p_CARR, NHANES_50p_CARR$impute==x)[,1:11] )
  assign(paste("NHANES_u50_hybrid_noFH_",x,sep=""),subset(NHANES_u50_hybrid_noFH, NHANES_u50_hybrid_noFH$impute==x)[,1:7] )
  assign(paste("NHANES_u50_hybrid_FH_",x,sep=""),subset(NHANES_u50_hybrid_FH, NHANES_u50_hybrid_FH$impute==x)[,1:7] )
  assign(paste("NHANES_50p_hybrid_noFH_",x,sep=""),subset(NHANES_50p_hybrid_noFH, NHANES_50p_hybrid_noFH$impute==x)[,1:9] )
  assign(paste("NHANES_50p_hybrid_FH_",x,sep=""),subset(NHANES_50p_hybrid_FH, NHANES_50p_hybrid_FH$impute==x)[,1:9] )
  }

NHANES_u50_iCARE_list<-list(NHANES_u50_iCARE_1,NHANES_u50_iCARE_2,NHANES_u50_iCARE_3,NHANES_u50_iCARE_4,NHANES_u50_iCARE_5)
NHANES_50p_iCARE_list<-list(NHANES_50p_iCARE_1,NHANES_50p_iCARE_2,NHANES_50p_iCARE_3,NHANES_50p_iCARE_4,NHANES_50p_iCARE_5)
NHANES_u50_CARR_list<-list(NHANES_u50_CARR_1,NHANES_u50_CARR_2,NHANES_u50_CARR_3,NHANES_u50_CARR_4,NHANES_u50_CARR_5)
NHANES_50p_CARR_list<-list(NHANES_50p_CARR_1,NHANES_50p_CARR_2,NHANES_50p_CARR_3,NHANES_50p_CARR_4,NHANES_50p_CARR_5)
NHANES_u50_hybrid_noFH_list<-list(NHANES_u50_hybrid_noFH_1,NHANES_u50_hybrid_noFH_2,NHANES_u50_hybrid_noFH_3,NHANES_u50_hybrid_noFH_4,NHANES_u50_hybrid_noFH_5)
NHANES_u50_hybrid_FH_list<-list(NHANES_u50_hybrid_FH_1,NHANES_u50_hybrid_FH_2,NHANES_u50_hybrid_FH_3,NHANES_u50_hybrid_FH_4,NHANES_u50_hybrid_FH_5)
NHANES_50p_hybrid_noFH_list<-list(NHANES_50p_hybrid_noFH_1,NHANES_50p_hybrid_noFH_2,NHANES_50p_hybrid_noFH_3,NHANES_50p_hybrid_noFH_4,NHANES_50p_hybrid_noFH_5)
NHANES_50p_hybrid_FH_list<-list(NHANES_50p_hybrid_FH_1,NHANES_50p_hybrid_FH_2,NHANES_50p_hybrid_FH_3,NHANES_50p_hybrid_FH_4,NHANES_50p_hybrid_FH_5)


#model.log.RR = vector with log odds ratios corresponding to the model params; no intercept; 
#names must match design matrix arising from model.formula and model.cov.info; 
#check names using function check_design_matrix().

ORs_iCARE_u50 <- as.data.frame(rep(NA,18))
ORs_iCARE_u50<-c(log(2.5),log(1.17),
                 log(0.91),log(0.84),log(0.80),log(0.69),
                 log(0.87),log(0.81),log(0.71),
                 log(1.01),log(1.11),log(1.24),
                 log(0.92),log(0.74),
                 log(1.01),log(1.03),log(1.2),
                 log(1.14))
names(ORs_iCARE_u50)<-c("famhist","height",
                   "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                   "as.factor(parity)2","as.factor(parity)3","as.factor(parity)4",
                   "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                  "as.factor(bmi)2",  "as.factor(bmi)3", 
                  "as.factor(alcohol)2","as.factor(alcohol)3","as.factor(alcohol)4",
                  "OCuse")

ORs_iCARE_50p <- as.data.frame(rep(NA,27))
ORs_iCARE_50p<-c(log(1.6),
                log(1.17),
                log(0.91),log(0.84),log(0.80),log(0.69),
                log(0.87),log(0.81),log(0.71),
                log(1.01),log(1.11),log(1.24),
                log(1.13),log(1.25),
                log(1.01),log(1.03),log(1.2),
                log(1.14),
                log(1.3),log(1.1),
                log(1.23),log(1.43),log(1.60),
                log(1.4),log(1.5),log(1.2),log(1.3))
names(ORs_iCARE_50p)<-c("famhist",
                             "height",
                             "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                             "as.factor(parity)2","as.factor(parity)3","as.factor(parity)4",
                             "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                             "as.factor(bmi)2",  "as.factor(bmi)3", 
                             "as.factor(alcohol)2","as.factor(alcohol)3","as.factor(alcohol)4",
                             "OCuse",
                             "as.factor(HRT)2","as.factor(HRT)3",
                             "as.factor(agemeno)2","as.factor(agemeno)3","as.factor(agemeno)4",
                             "as.factor(bmi)2:as.factor(HRT)2","as.factor(bmi)3:as.factor(HRT)2","as.factor(bmi)2:as.factor(HRT)3","as.factor(bmi)3:as.factor(HRT)3")

ORs_CARR_u50 <- as.data.frame(rep(NA,21))
ORs_CARR_u50<-c(log(1.59),log(1.04),
                log(1.11),log(1.18),log(1.32),log(1.20),
                log(0.89),log(0.88),log(0.71),log(0.79),
                log(0.81),log(0.86),log(0.80),
                 log(1.09),log(1.39),log(1.46),
                 log(1.00),log(0.86),
                 log(1.01),log(1.06),log(1.13),
                 log(0.97))
names(ORs_CARR_u50)<-c("famhist","height",
                        "as.factor(raceeth)2","as.factor(raceeth)3","as.factor(raceeth)4","as.factor(raceeth)5",
                        "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                        "as.factor(parity)2","as.factor(parity)3","as.factor(parity)4",
                        "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                        "as.factor(bmi)2",  "as.factor(bmi)3", 
                        "as.factor(alcohol)2","as.factor(alcohol)3","as.factor(alcohol)4",
                        "OCuse")

ORs_CARR_50p <- as.data.frame(rep(NA,34))
ORs_CARR_50p<-c(log(1.52),
                 log(1.11),
                 log(1.10),log(1.14),log(1.22),log(1.16),
                 log(0.90),log(0.88),log(0.84),log(0.78),
                 log(0.87),log(0.88),log(0.84),
                 log(0.95),log(1.08),log(1.19),
                 log(1.10),log(1.21),
                 log(1.11),log(1.10),log(1.33),
                 log(1.00),
                 log(1.19),log(0.97),
                 log(0.95),log(1.03),log(1.07),
                 log(1.27),log(1.43),log(0.96),log(1.13))
names(ORs_CARR_50p)<-c("famhist",
                        "height",
                        "as.factor(raceeth)2","as.factor(raceeth)3","as.factor(raceeth)4","as.factor(raceeth)5",
                        "as.factor(menarche)2","as.factor(menarche)3","as.factor(menarche)4","as.factor(menarche)5",
                        "as.factor(parity)2","as.factor(parity)3","as.factor(parity)4",
                        "as.factor(afb)2","as.factor(afb)3","as.factor(afb)4",
                        "as.factor(bmi)2",  "as.factor(bmi)3", 
                        "as.factor(alcohol)2","as.factor(alcohol)3","as.factor(alcohol)4",
                        "OCuse",
                        "as.factor(HRT)2","as.factor(HRT)3",
                        "as.factor(agemeno)2","as.factor(agemeno)3","as.factor(agemeno)4",
                        "as.factor(bmi)2:as.factor(HRT)2","as.factor(bmi)3:as.factor(HRT)2",
                        "as.factor(bmi)2:as.factor(HRT)3","as.factor(bmi)3:as.factor(HRT)3")

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
                        "as.factor(alc3)1","as.factor(alc3)2",
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
CARRIERS_u50_iCARE<-read.delim("realdata_u50_iCARE.txt", na.strings =".")
CARRIERS_50p_iCARE<-read.delim("realdata_50p_iCARE.txt", na.strings =".")
CARRIERS_u50_CARR<-read.delim("realdata_u50_CARR.txt", na.strings =".")
CARRIERS_50p_CARR<-read.delim("realdata_50p_CARR.txt", na.strings =".")
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
PVrisk_iCARE_u50_1000<-matrix(nrow=100,ncol=9)
PVrisk_iCARE_50p_1000<-matrix(nrow=100,ncol=9)

PVrisk_CARR_u50_1000<-matrix(nrow=100,ncol=9)
PVrisk_CARR_50p_1000<-matrix(nrow=100,ncol=9)

for (j in 1:20){
logOR_allPV<-matrix(nrow=7,ncol=1)

for (i in 1:7){
  logOR_allPV[i,]<-rnorm(1,mean=PV_data_all[i,2],sd=PV_data_all[i,3])}
OR_allPV<-exp(logOR_allPV)
PV7<-cbind(PV_data_all[1],OR_allPV,PV_data_all[4])
names(PV7)[2]<-"snp.odds.ratio"
PV7<-as.data.frame(PV7)

for (x in 1:5){
run_iCARE_u50 = computeAbsoluteRisk(model.snp.info = PV7, 
                              model.disease.incidence.rates = Inc_all,
                              model.competing.incidence.rates = Mort_all,
                              apply.age.start = 20,
                              apply.age.interval.length = 30,
                              apply.snp.profile=CARRIERS_u50_PVs,
                              model.cov.info = iCARE_covar_list_u50,
                              model.formula = iCARE_formula_u50,
                              apply.cov.profile =CARRIERS_u50_iCARE,
                              model.log.RR = ORs_iCARE_u50,
                              model.ref.dataset= as.data.frame(NHANES_u50_iCARE_list[x]),
                              model.ref.dataset.weights=NHANES_wts_u50,
                              model.bin.fh.name = "famhist",
                              return.refs.risk=TRUE)

noncarriers<-subset(run_iCARE_u50$details,(run_iCARE_u50$details[,4]==0
      & run_iCARE_u50$details[,5]==0 & run_iCARE_u50$details[,6]==0 & run_iCARE_u50$details[,7]==0
      & run_iCARE_u50$details[,8]==0 & run_iCARE_u50$details[,9]==0 & run_iCARE_u50$details[,10]==0))
ATM<-subset(run_iCARE_u50$details,run_iCARE_u50$details[,4]==1)
BARD1<-subset(run_iCARE_u50$details,run_iCARE_u50$details[,5]==1)
BRCA1<-subset(run_iCARE_u50$details,run_iCARE_u50$details[,6]==1)
BRCA2<-subset(run_iCARE_u50$details,run_iCARE_u50$details[,7]==1)
CHEK2<-subset(run_iCARE_u50$details,run_iCARE_u50$details[,8]==1)
PALB2<-subset(run_iCARE_u50$details,run_iCARE_u50$details[,9]==1)
RAD51C<-subset(run_iCARE_u50$details,run_iCARE_u50$details[,10]==1)
anyPV<-subset(run_iCARE_u50$details,(run_iCARE_u50$details[,4]==1
     | run_iCARE_u50$details[,5]==1 | run_iCARE_u50$details[,6]==1 | run_iCARE_u50$details[,7]==1
     | run_iCARE_u50$details[,8]==1 | run_iCARE_u50$details[,9]==1 | run_iCARE_u50$details[,10]==1))

PVrisk_iCARE_u50<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BARD1[,3]),mean(BRCA1[,3]),
   mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]),mean(RAD51C[,3]),mean(anyPV[,3]))
names(PVrisk_iCARE_u50)<-names<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2","CHEK2","PALB2","RAD51C","RAD51D")
z<-(j-1)*5+x
PVrisk_iCARE_u50_1000[z,]<-PVrisk_iCARE_u50
}

for (x in 1:5){
run_iCARE_50p = computeAbsoluteRisk(model.snp.info = PV7,
                                    model.disease.incidence.rates = Inc_all,
                                    model.competing.incidence.rates = Mort_all,
                                    apply.age.start = 50,
                                    apply.age.interval.length = 30,
                                    apply.snp.profile=CARRIERS_50p_PVs,
                                    model.cov.info = iCARE_covar_list_50p,
                                    model.formula = iCARE_formula_50p,
                                    apply.cov.profile =CARRIERS_50p_iCARE,
                                    model.log.RR = ORs_iCARE_50p,
                                    model.ref.dataset= as.data.frame(NHANES_50p_iCARE_list[x]),
                                    model.ref.dataset.weights=NHANES_wts_50p,
                                    model.bin.fh.name = "famhist",
                                    return.refs.risk=TRUE)
noncarriers<-subset(run_iCARE_50p$details,(run_iCARE_50p$details[,4]==0
   & run_iCARE_50p$details[,5]==0 & run_iCARE_50p$details[,6]==0 & run_iCARE_50p$details[,7]==0
   & run_iCARE_50p$details[,8]==0 & run_iCARE_50p$details[,9]==0 & run_iCARE_50p$details[,10]==0))
ATM<-subset(run_iCARE_50p$details,run_iCARE_50p$details[,4]==1)
BARD1<-subset(run_iCARE_50p$details,run_iCARE_50p$details[,5]==1)
BRCA1<-subset(run_iCARE_50p$details,run_iCARE_50p$details[,6]==1)
BRCA2<-subset(run_iCARE_50p$details,run_iCARE_50p$details[,7]==1)
CHEK2<-subset(run_iCARE_50p$details,run_iCARE_50p$details[,9]==1)
PALB2<-subset(run_iCARE_50p$details,run_iCARE_50p$details[,10]==1)
RAD51C<-subset(run_iCARE_50p$details,run_iCARE_50p$details[,11]==1)
anyPV<-subset(run_iCARE_50p$details,(run_iCARE_50p$details[,4]==1
  | run_iCARE_50p$details[,5]==1 | run_iCARE_50p$details[,6]==1 | run_iCARE_50p$details[,7]==1
  | run_iCARE_50p$details[,8]==1 | run_iCARE_50p$details[,9]==1 | run_iCARE_50p$details[,10]==1))

PVrisk_iCARE_50p<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BARD1[,3]),mean(BRCA1[,3]),
                    mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]),mean(RAD51C[,3]),mean(anyPV[,3]))
names(PVrisk_iCARE_50p)<-names<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2","CHEK2","PALB2","RAD51C","anyPV")
z<-(j-1)*5+x
PVrisk_iCARE_50p_1000[z,]<-PVrisk_iCARE_50p
}

for (x in 1:5){
run_CARR_u50 = computeAbsoluteRisk(model.snp.info = PV7,
                                   model.disease.incidence.rates = Inc_all,
                                   model.competing.incidence.rates = Mort_all,
                                   apply.age.start = 20,
                                   apply.age.interval.length = 30,
                                   apply.snp.profile=CARRIERS_u50_PVs,
                                   model.cov.info = CARR_covar_list_u50,
                                   model.formula = CARR_formula_u50,
                                   apply.cov.profile =CARRIERS_u50_CARR,
                                   model.log.RR = ORs_CARR_u50,
                                   model.ref.dataset= as.data.frame(NHANES_u50_CARR_list[x]),
                                   model.ref.dataset.weights=NHANES_wts_u50,
                                   model.bin.fh.name = "famhist",
                                   return.refs.risk=TRUE)

noncarriers<-subset(run_CARR_u50$details,(run_CARR_u50$details[,4]==0
     & run_CARR_u50$details[,5]==0 & run_CARR_u50$details[,6]==0 & run_CARR_u50$details[,7]==0
     & run_CARR_u50$details[,8]==0 & run_CARR_u50$details[,9]==0 & run_CARR_u50$details[,10]==0))
ATM<-subset(run_CARR_u50$details,run_CARR_u50$details[,4]==1)
BARD1<-subset(run_CARR_u50$details,run_CARR_u50$details[,5]==1)
BRCA1<-subset(run_CARR_u50$details,run_CARR_u50$details[,6]==1)
BRCA2<-subset(run_CARR_u50$details,run_CARR_u50$details[,7]==1)
CHEK2<-subset(run_CARR_u50$details,run_CARR_u50$details[,8]==1)
PALB2<-subset(run_CARR_u50$details,run_CARR_u50$details[,9]==1)
RAD51C<-subset(run_CARR_u50$details,run_CARR_u50$details[,10]==1)
anyPV<-subset(run_CARR_u50$details,(run_CARR_u50$details[,4]==1
    | run_CARR_u50$details[,5]==1 | run_CARR_u50$details[,6]==1 | run_CARR_u50$details[,7]==1
   | run_CARR_u50$details[,8]==1 | run_CARR_u50$details[,9]==1 | run_CARR_u50$details[,10]==1))

PVrisk_CARR_u50<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BARD1[,3]),mean(BRCA1[,3]),
                   mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]),mean(RAD51C[,3]),mean(anyPV[,3]))
names(PVrisk_CARR_u50)<-names<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2","CHEK2","PALB2","RAD51C","anyPV")
z<-(j-1)*5+x
PVrisk_CARR_u50_1000[z,]<-PVrisk_CARR_u50
}

for (x in 1:5){
run_CARR_50p = computeAbsoluteRisk(model.snp.info = PV7,
                                   model.disease.incidence.rates = Inc_all,
                                   model.competing.incidence.rates = Mort_all,
                                   apply.age.start = 50,
                                   apply.age.interval.length = 30,
                                   apply.snp.profile=CARRIERS_50p_PVs,
                                   model.cov.info = CARR_covar_list_50p,
                                   model.formula = CARR_formula_50p,
                                   apply.cov.profile =CARRIERS_50p_CARR,
                                   model.log.RR = ORs_CARR_50p,
                                   model.ref.dataset= as.data.frame(NHANES_50p_CARR_list[x]),
                                   model.ref.dataset.weights=NHANES_wts_50p,
                                   model.bin.fh.name = "famhist",
                                   return.refs.risk=TRUE)

noncarriers<-subset(run_CARR_50p$details,(run_CARR_50p$details[,4]==0
   & run_CARR_50p$details[,5]==0 & run_CARR_50p$details[,6]==0 & run_CARR_50p$details[,7]==0
   & run_CARR_50p$details[,8]==0 & run_CARR_50p$details[,9]==0 & run_CARR_50p$details[,10]==0))
ATM<-subset(run_CARR_50p$details,run_CARR_50p$details[,4]==1)
BARD1<-subset(run_CARR_50p$details,run_CARR_50p$details[,5]==1)
BRCA1<-subset(run_CARR_50p$details,run_CARR_50p$details[,6]==1)
BRCA2<-subset(run_CARR_50p$details,run_CARR_50p$details[,7]==1)
CHEK2<-subset(run_CARR_50p$details,run_CARR_50p$details[,8]==1)
PALB2<-subset(run_CARR_50p$details,run_CARR_50p$details[,9]==1)
RAD51C<-subset(run_CARR_50p$details,run_CARR_50p$details[,10]==1)
anyPV<-subset(run_CARR_50p$details,(run_CARR_50p$details[,4]==1 | run_CARR_50p$details[,5]==1 | run_CARR_50p$details[,6]==1 | run_CARR_50p$details[,7]==1
  | run_CARR_50p$details[,8]==1 | run_CARR_50p$details[,9]==1  | run_CARR_50p$details[,10]==1))

PVrisk_CARR_50p<-c(mean(noncarriers[,3]),mean(ATM[,3]),mean(BARD1[,3]),mean(BRCA1[,3]),
                   mean(BRCA2[,3]),mean(CHEK2[,3]),mean(PALB2[,3]),mean(RAD51C[,3]),mean(anyPV[,3]))
names(PVrisk_CARR_50p)<-names<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2","CHEK2","PALB2","RAD51C","anyPV")
z<-(j-1)*5+x
PVrisk_CARR_50p_1000[z,]<-PVrisk_CARR_50p
}
}

iCARE_u50_results<-matrix(nrow=9,ncol=3)
for (k in 1:9){
  iCARE_u50_results[k,]<-quantile(PVrisk_iCARE_u50_1000[,k],probs=c(0.5,0.025,0.975))
}
rownames(iCARE_u50_results)<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2",
                               "CHEK2","PALB2","RAD51C","anyPV")
colnames(iCARE_u50_results)<-c("median","2.5th %","97.5 %")
iCARE_u50_results
write.csv(iCARE_u50_results, file = "iCARE_u50_results")

iCARE_50p_results<-matrix(nrow=9,ncol=3)
for (k in 1:9){
  iCARE_50p_results[k,]<-quantile(PVrisk_iCARE_50p_1000[,k],probs=c(0.5,0.025,0.975))
}
rownames(iCARE_50p_results)<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2",
                               "CHEK2","PALB2","RAD51C","anyPV")
colnames(iCARE_50p_results)<-c("median","2.5th %","97.5 %")
iCARE_50p_results
write.csv(iCARE_50p_results, file = "iCARE_50p_results")

CARR_u50_results<-matrix(nrow=9,ncol=3)
for (k in 1:9){
  CARR_u50_results[k,]<-quantile(PVrisk_CARR_u50_1000[,k],probs=c(0.5,0.025,0.975))
}
rownames(CARR_u50_results)<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2",
                               "CHEK2","PALB2","RAD51C","anyPV")
colnames(CARR_u50_results)<-c("median","2.5th %","97.5 %")
CARR_u50_results
write.csv(CARR_u50_results, file = "CARR_u50_results")

CARR_50p_results<-matrix(nrow=9,ncol=3)
for (k in 1:9){
  CARR_50p_results[k,]<-quantile(PVrisk_CARR_50p_1000[,k],probs=c(0.5,0.025,0.975))
}
rownames(CARR_50p_results)<-c("noncarriers","ATM","BARD1","BRCA1","BRCA2",
                               "CHEK2","PALB2","RAD51C","anyPV")
colnames(CARR_50p_results)<-c("median","2.5th %","97.5 %")
CARR_50p_results
write.csv(CARR_50p_results, file = "CARR_50p_results")





## now do hybrid models, which are FH specific
#fill in rest from iCARE add other covars oct 2024 document