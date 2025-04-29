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
# to install on server:
# 1) ssh scigate
# 2) ssh triton                # interactive high performance computing (so you can run R in terminal)
# 3) /ddn/gs1/biotools/R/bin/R # opens R
# 4) install.packages("pROC")  # run within R, will install in your user library
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

PV_data_all
PV_data_noFH
PV_data_FH

CARRIERS_u50_PVs<-read.delim("realdataPV_u50.txt")
CARRIERS_u50_PVs_noFH<-read.delim("realdataPV_u50_noFH.txt")
CARRIERS_u50_PVs_FH<-read.delim("realdataPV_u50_FH.txt")
CARRIERS_50p_PVs<-read.delim("realdataPV_50p.txt")
CARRIERS_50p_PVs_noFH<-read.delim("realdataPV_50p_noFH.txt")
CARRIERS_50p_PVs_FH<-read.delim("realdataPV_50p_FH.txt")

all_u50<-matrix(nrow=1000,ncol=9)
all_u50_noFH<-matrix(nrow=1000,ncol=9)
all_u50_FH<-matrix(nrow=1000,ncol=9)
all_50p<-matrix(nrow=1000,ncol=9)
all_50p_noFH<-matrix(nrow=1000,ncol=9)
all_50p_FH<-matrix(nrow=1000,ncol=9)

participant_risk_PVall_u50<-matrix(nrow=11778,ncol=1000)
participant_risk_PVall_50p<-matrix(nrow=55914,ncol=1000)
participant_risk_PV_noFH_u50<-matrix(nrow=9123,ncol=1000)
participant_risk_PV_noFH_50p<-matrix(nrow=43957,ncol=1000)
participant_risk_PV_FH_u50<-matrix(nrow=2655,ncol=1000)
participant_risk_PV_FH_50p<-matrix(nrow=11957,ncol=1000)


#start loop to run whole process through
for (j in 1:1000){

  #do loop to randomly draw from distribution defined by logOR and SE_logOR
  logOR_all<-matrix(nrow=7,ncol=1)
  for (i in 1:7){
    logOR_all[i,]<-rnorm(1,mean=PV_data_all[i,2],sd=PV_data_all[i,3])}
  OR_all<-exp(logOR_all)
  PV_all<-cbind(PV_data_all[1],OR_all,PV_data_all[4])
  names(PV_all)[2]<-"snp.odds.ratio"
  PV_all<-as.data.frame(PV_all)

  logOR_noFH<-matrix(nrow=7,ncol=1)
  for (i in 1:7){
    logOR_noFH[i,]<-rnorm(1,mean=PV_data_noFH[i,2],sd=PV_data_noFH[i,3])}
  OR_noFH<-exp(logOR_noFH)
  PV_noFH<-cbind(PV_data_noFH[1],OR_noFH,PV_data_noFH[4])
  names(PV_noFH)[2]<-"snp.odds.ratio"
  PV_noFH<-as.data.frame(PV_noFH)

  logOR_FH<-matrix(nrow=7,ncol=1)
  for (i in 1:7){
    logOR_FH[i,]<-rnorm(1,mean=PV_data_FH[i,2],sd=PV_data_FH[i,3])}
  OR_FH<-exp(logOR_FH)
  PV_FH<-(cbind(PV_data_FH[1],OR_FH,PV_data_FH[4]))
  names(PV_FH)[2]<-"snp.odds.ratio"
  PV_FH<-as.data.frame(PV_FH)

  #overall, risk by age 50
  run_all_u50 = computeAbsoluteRisk(model.snp.info = PV_all,
                                    model.disease.incidence.rates = Inc_all,
                                    model.competing.incidence.rates = Mort_all,
                                    apply.age.start = 20,
                                    apply.age.interval.length = 30,
                                    apply.snp.profile=CARRIERS_u50_PVs,
                                    return.refs.risk=TRUE)
  #save the details in a large data frame
  noncarriers_all_u50<-mean(subset(run_all_u50$details,(run_all_u50$details[,4]==0
      & run_all_u50$details[,5]==0 & run_all_u50$details[,6]==0 & run_all_u50$details[,7]==0
      & run_all_u50$details[,8]==0 & run_all_u50$details[,9]==0 & run_all_u50$details[,10]==0))[,3])
  ATM_all_u50<-mean(subset(run_all_u50$details,run_all_u50$details[,4]==1)[,3])
  BARD1_all_u50<-mean(subset(run_all_u50$details,run_all_u50$details[,5]==1)[,3])
  BRCA1_all_u50<-mean(subset(run_all_u50$details,run_all_u50$details[,6]==1)[,3])
  BRCA2_all_u50<-mean(subset(run_all_u50$details,run_all_u50$details[,7]==1)[,3])
  CHEK2_all_u50<-mean(subset(run_all_u50$details,run_all_u50$details[,8]==1)[,3])
  PALB2_all_u50<-mean(subset(run_all_u50$details,run_all_u50$details[,9]==1)[,3])
  RAD51C_all_u50<-mean(subset(run_all_u50$details,run_all_u50$details[,10]==1)[,3])
  anyPV_all_u50<-mean(subset(run_all_u50$details,(run_all_u50$details[,4]==1
     | run_all_u50$details[,5]==1 | run_all_u50$details[,6]==1 | run_all_u50$details[,7]==1
     | run_all_u50$details[,8]==1 | run_all_u50$details[,9]==1 | run_all_u50$details[,10]==1))[,3])
  all_u50[j,]<-c(noncarriers_all_u50,ATM_all_u50,BARD1_all_u50,BRCA1_all_u50,BRCA2_all_u50,CHEK2_all_u50,PALB2_all_u50,
                 RAD51C_all_u50,anyPV_all_u50)
  participant_risk_PVall_u50[,j]<-run_all_u50$details$Risk_Estimate

  #no FH, risk by age 50
  run_noFH_u50 = computeAbsoluteRisk(model.snp.info = PV_noFH,
                                     model.disease.incidence.rates = Inc_all_noFH,
                                     model.competing.incidence.rates = Mort_all,
                                     apply.age.start = 20,
                                     apply.age.interval.length = 30,
                                     apply.snp.profile=CARRIERS_u50_PVs_noFH,
                                     return.refs.risk=TRUE)
  #save the details in a large data frame
  noncarriers_all_u50_noFH<-mean(subset(run_noFH_u50$details,(run_noFH_u50$details[,4]==0
        & run_noFH_u50$details[,5]==0 & run_noFH_u50$details[,6]==0 & run_noFH_u50$details[,7]==0
         & run_noFH_u50$details[,8]==0 & run_noFH_u50$details[,9]==0 & run_noFH_u50$details[,10]==0))[,3])
  ATM_all_u50_noFH<-mean(subset(run_noFH_u50$details,run_noFH_u50$details[,4]==1)[,3])
  BARD1_all_u50_noFH<-mean(subset(run_noFH_u50$details,run_noFH_u50$details[,5]==1)[,3])
  BRCA1_all_u50_noFH<-mean(subset(run_noFH_u50$details,run_noFH_u50$details[,6]==1)[,3])
  BRCA2_all_u50_noFH<-mean(subset(run_noFH_u50$details,run_noFH_u50$details[,7]==1)[,3])
  CHEK2_all_u50_noFH<-mean(subset(run_noFH_u50$details,run_noFH_u50$details[,8]==1)[,3])
  PALB2_all_u50_noFH<-mean(subset(run_noFH_u50$details,run_noFH_u50$details[,9]==1)[,3])
  RAD51C_all_u50_noFH<-mean(subset(run_noFH_u50$details,run_noFH_u50$details[,10]==1)[,3])
  anyPV_all_u50_noFH<-mean(subset(run_noFH_u50$details,(run_noFH_u50$details[,4]==1
       | run_noFH_u50$details[,5]==1 | run_noFH_u50$details[,6]==1 | run_noFH_u50$details[,7]==1
         | run_noFH_u50$details[,8]==1 | run_noFH_u50$details[,9]==1 | run_noFH_u50$details[,10]==1))[,3])
  all_u50_noFH[j,]<-c(noncarriers_all_u50_noFH,ATM_all_u50_noFH,BARD1_all_u50_noFH,BRCA1_all_u50_noFH,BRCA2_all_u50_noFH,
                      CHEK2_all_u50_noFH,PALB2_all_u50_noFH,RAD51C_all_u50_noFH,anyPV_all_u50_noFH)
  participant_risk_PV_noFH_u50[,j]<-run_noFH_u50$details$Risk_Estimate

  #FH, risk by age 50
  run_FH_u50 = computeAbsoluteRisk(model.snp.info = PV_FH,
                                   model.disease.incidence.rates = Inc_all_FH,
                                   model.competing.incidence.rates = Mort_all,
                                   apply.age.start = 20,
                                   apply.age.interval.length = 30,
                                   apply.snp.profile=CARRIERS_u50_PVs_FH,
                                   return.refs.risk=TRUE)
  #save the details in a large data frame
  noncarriers_all_u50_FH<-mean(subset(run_FH_u50$details,(run_FH_u50$details[,4]==0
                                                          & run_FH_u50$details[,5]==0 & run_FH_u50$details[,6]==0 & run_FH_u50$details[,7]==0
                                                          & run_FH_u50$details[,8]==0 & run_FH_u50$details[,9]==0 & run_FH_u50$details[,10]==0))[,3])
  ATM_all_u50_FH<-mean(subset(run_FH_u50$details,run_FH_u50$details[,4]==1)[,3])
  BARD1_all_u50_FH<-mean(subset(run_FH_u50$details,run_FH_u50$details[,5]==1)[,3])
  BRCA1_all_u50_FH<-mean(subset(run_FH_u50$details,run_FH_u50$details[,6]==1)[,3])
  BRCA2_all_u50_FH<-mean(subset(run_FH_u50$details,run_FH_u50$details[,7]==1)[,3])
  CHEK2_all_u50_FH<-mean(subset(run_FH_u50$details,run_FH_u50$details[,8]==1)[,3])
  PALB2_all_u50_FH<-mean(subset(run_FH_u50$details,run_FH_u50$details[,9]==1)[,3])
  RAD51C_all_u50_FH<-mean(subset(run_FH_u50$details,run_FH_u50$details[,10]==1)[,3])
  anyPV_all_u50_FH<-mean(subset(run_FH_u50$details,(run_FH_u50$details[,4]==1
    | run_FH_u50$details[,5]==1 | run_FH_u50$details[,6]==1 | run_FH_u50$details[,7]==1
    | run_FH_u50$details[,8]==1 | run_FH_u50$details[,9]==1 | run_FH_u50$details[,10]==1))[,3])
  all_u50_FH[j,]<-c(noncarriers_all_u50_FH,ATM_all_u50_FH,BARD1_all_u50_FH,BRCA1_all_u50_FH,BRCA2_all_u50_FH,
                    CHEK2_all_u50_FH,PALB2_all_u50_FH,RAD51C_all_u50_noFH,anyPV_all_u50_FH)
  participant_risk_PV_FH_u50[,j]<-run_FH_u50$details$Risk_Estimate

  #overall 50-80
  run_all_50p = computeAbsoluteRisk(model.snp.info = PV_all,
                                    model.disease.incidence.rates = Inc_all,
                                    model.competing.incidence.rates = Mort_all,
                                    apply.age.start = 50,
                                    apply.age.interval.length = 30,
                                    apply.snp.profile=CARRIERS_50p_PVs,
                                    return.refs.risk=TRUE)
  #save the details in a large data frame
  noncarriers_all_50p<-mean(subset(run_all_50p$details,(run_all_50p$details[,4]==0
                                                        & run_all_50p$details[,5]==0 & run_all_50p$details[,6]==0 & run_all_50p$details[,7]==0
                                                        & run_all_50p$details[,8]==0 & run_all_50p$details[,9]==0 & run_all_50p$details[,10]==0))[,3])
  ATM_all_50p<-mean(subset(run_all_50p$details,run_all_50p$details[,4]==1)[,3])
  BARD1_all_50p<-mean(subset(run_all_50p$details,run_all_50p$details[,5]==1)[,3])
  BRCA1_all_50p<-mean(subset(run_all_50p$details,run_all_50p$details[,6]==1)[,3])
  BRCA2_all_50p<-mean(subset(run_all_50p$details,run_all_50p$details[,7]==1)[,3])
  CHEK2_all_50p<-mean(subset(run_all_50p$details,run_all_50p$details[,8]==1)[,3])
  PALB2_all_50p<-mean(subset(run_all_50p$details,run_all_50p$details[,9]==1)[,3])
  RAD51C_all_50p<-mean(subset(run_all_50p$details,run_all_50p$details[,10]==1)[,3])
  anyPV_all_50p<-mean(subset(run_all_50p$details,(run_all_50p$details[,4]==1
      | run_all_50p$details[,5]==1 | run_all_50p$details[,6]==1 | run_all_50p$details[,7]==1
      | run_all_50p$details[,8]==1 | run_all_50p$details[,9]==1 | run_all_50p$details[,10]==1))[,3])
  all_50p[j,]<-c(noncarriers_all_50p,ATM_all_50p,BARD1_all_50p,BRCA1_all_50p,BRCA2_all_50p,CHEK2_all_50p,PALB2_all_50p,
                 RAD51C_all_50p,anyPV_all_50p)
  participant_risk_PVall_50p[,j]<-run_all_50p$details$Risk_Estimate

  run_noFH_50p = computeAbsoluteRisk(model.snp.info = PV_noFH,
                                     model.disease.incidence.rates = Inc_all_noFH,
                                     model.competing.incidence.rates = Mort_all,
                                     apply.age.start = 50,
                                     apply.age.interval.length = 30,
                                     apply.snp.profile=CARRIERS_50p_PVs_noFH,
                                     return.refs.risk=TRUE)
  #save the details in a large data frame
  noncarriers_all_50p_noFH<-mean(subset(run_noFH_50p$details,(run_noFH_50p$details[,4]==0
                                                              & run_noFH_50p$details[,5]==0 & run_noFH_50p$details[,6]==0 & run_noFH_50p$details[,7]==0
                                                              & run_noFH_50p$details[,8]==0 & run_noFH_50p$details[,9]==0 & run_noFH_50p$details[,10]==0))[,3])
  ATM_all_50p_noFH<-mean(subset(run_noFH_50p$details,run_noFH_50p$details[,4]==1)[,3])
  BARD1_all_50p_noFH<-mean(subset(run_noFH_50p$details,run_noFH_50p$details[,5]==1)[,3])
  BRCA1_all_50p_noFH<-mean(subset(run_noFH_50p$details,run_noFH_50p$details[,6]==1)[,3])
  BRCA2_all_50p_noFH<-mean(subset(run_noFH_50p$details,run_noFH_50p$details[,7]==1)[,3])
  CHEK2_all_50p_noFH<-mean(subset(run_noFH_50p$details,run_noFH_50p$details[,8]==1)[,3])
  PALB2_all_50p_noFH<-mean(subset(run_noFH_50p$details,run_noFH_50p$details[,9]==1)[,3])
  RAD51C_all_50p_noFH<-mean(subset(run_noFH_50p$details,run_noFH_50p$details[,10]==1)[,3])
  anyPV_all_50p_noFH<-mean(subset(run_noFH_50p$details,(run_noFH_50p$details[,4]==1
                                                        | run_noFH_50p$details[,5]==1 | run_noFH_50p$details[,6]==1 | run_noFH_50p$details[,7]==1
                                                        | run_noFH_50p$details[,8]==1 | run_noFH_50p$details[,9]==1 | run_noFH_50p$details[,10]==1))[,3])
  all_50p_noFH[j,]<-c(noncarriers_all_50p_noFH,ATM_all_50p_noFH,BARD1_all_50p_noFH,BRCA1_all_50p_noFH,BRCA2_all_50p_noFH,
                      CHEK2_all_50p_noFH,PALB2_all_50p_noFH,RAD51C_all_50p_noFH,anyPV_all_50p_noFH)
  participant_risk_PV_noFH_50p[,j]<-run_noFH_50p$details$Risk_Estimate

  run_FH_50p = computeAbsoluteRisk(model.snp.info = PV_FH,
                                   model.disease.incidence.rates = Inc_all_FH,
                                   model.competing.incidence.rates = Mort_all,
                                   apply.age.start = 50,
                                   apply.age.interval.length = 30,
                                   apply.snp.profile=CARRIERS_50p_PVs_FH,
                                   return.refs.risk=TRUE)
  #save the details in a large data frame
  noncarriers_all_50p_FH<-mean(subset(run_FH_50p$details,(run_FH_50p$details[,4]==0
      & run_FH_50p$details[,5]==0 & run_FH_50p$details[,6]==0 & run_FH_50p$details[,7]==0
      & run_FH_50p$details[,8]==0 & run_FH_50p$details[,9]==0 & run_FH_50p$details[,10]==0))[,3])
  ATM_all_50p_FH<-mean(subset(run_FH_50p$details,run_FH_50p$details[,4]==1)[,3])
  BARD1_all_50p_FH<-mean(subset(run_FH_50p$details,run_FH_50p$details[,5]==1)[,3])
  BRCA1_all_50p_FH<-mean(subset(run_FH_50p$details,run_FH_50p$details[,6]==1)[,3])
  BRCA2_all_50p_FH<-mean(subset(run_FH_50p$details,run_FH_50p$details[,7]==1)[,3])
  CHEK2_all_50p_FH<-mean(subset(run_FH_50p$details,run_FH_50p$details[,8]==1)[,3])
  PALB2_all_50p_FH<-mean(subset(run_FH_50p$details,run_FH_50p$details[,9]==1)[,3])
  RAD51C_all_50p_FH<-mean(subset(run_FH_50p$details,run_FH_50p$details[,10]==1)[,3])
  anyPV_all_50p_FH<-mean(subset(run_FH_50p$details,(run_FH_50p$details[,4]==1
     | run_FH_50p$details[,5]==1 | run_FH_50p$details[,6]==1 | run_FH_50p$details[,7]==1
     | run_FH_50p$details[,8]==1 | run_FH_50p$details[,9]==1 | run_FH_50p$details[,10]==1))[,3])
  all_50p_FH[j,]<-c(noncarriers_all_50p_FH,ATM_all_50p_FH,BARD1_all_50p_FH,BRCA1_all_50p_FH,BRCA2_all_50p_FH,
                    CHEK2_all_50p_FH,PALB2_all_50p_FH,RAD51C_all_50p_noFH,anyPV_all_50p_FH)
  participant_risk_PV_FH_50p[,j]<-run_FH_50p$details$Risk_Estimate
  }

write.table(as.data.frame(t(all_u50)), file="all_u50_1000.csv", sep=",")
write.table(as.data.frame(t(all_u50_noFH)), file="all_u50_noFH_1000.csv", sep=",")
write.table(as.data.frame(t(all_u50_FH)), file="all_u50_FH_1000.csv", sep=",")
write.table(as.data.frame(t(all_50p)), file="all_50p_1000.csv", sep=",")
write.table(as.data.frame(t(all_50p_noFH)), file="all_50p_noFH_1000.csv", sep=",")
write.table(as.data.frame(t(all_50p_FH)), file="all_50p_FH_1000.csv", sep=",")


quants <- c(0.025,0.50,0.975)
results_all_u50<-apply(all_u50,2,quantile,probs = quants,na.rm = TRUE)
results_noFH_u50<-apply(all_u50_noFH,2,quantile,probs = quants,na.rm = TRUE)
results_FH_u50<-apply(all_u50_FH,2,quantile,probs = quants,na.rm = TRUE)
results_all_50p<-apply(all_50p,2,quantile,probs = quants,na.rm = TRUE)
results_noFH_50p<-apply(all_50p_noFH,2,quantile,probs = quants,na.rm = TRUE)
results_FH_50p<-apply(all_50p_FH,2,quantile,probs = quants,na.rm = TRUE)

results_all_u50
results_noFH_u50
results_FH_u50
results_all_50p
results_noFH_50p
results_FH_50p

#save these files
write.csv(results_all_u50, file = "results_all_u50.csv")
write.csv(results_noFH_u50, file = "results_noFH_u50.csv")
write.csv(results_FH_u50, file = "results_FH_u50.csv")
write.csv(results_all_50p, file = "results_all_50p.csv")
write.csv(results_noFH_50p, file = "results_noFH_50p.csv")
write.csv(results_FH_50p, file = "results_FH_50p.csv")

#AUC - bind files with case_control status
CARRIERS_casestatus<-read.delim("carriers_case_status.txt")

CARRIERS_casestatus_u50<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age<50)
CARRIERS_casestatus_50p<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age>=50)

CARRIERS_casestatus_u50_noFH<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age<50 & FH1st==0)
CARRIERS_casestatus_50p_noFH<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age>=50 & FH1st==0)
CARRIERS_casestatus_u50_FH<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age<50 & FH1st==1)
CARRIERS_casestatus_50p_FH<-subset(CARRIERS_casestatus,CARRIERS_casestatus$age>=50 & FH1st==1)

CARRIERS_casestatus_u50_wFH<-rbind(CARRIERS_casestatus_u50_noFH,CARRIERS_casestatus_u50_FH)
CARRIERS_casestatus_50p_wFH<-rbind(CARRIERS_casestatus_50p_noFH,CARRIERS_casestatus_50p_FH)
dim(CARRIERS_casestatus_u50_wFH)
dim(CARRIERS_casestatus_50p_wFH)

participant_risk_PV_u50<-rbind(participant_risk_PV_noFH_u50,participant_risk_PV_FH_u50)
participant_risk_PV_50p<-rbind(participant_risk_PV_noFH_50p,participant_risk_PV_FH_50p)
dim(participant_risk_PV_u50)
dim(participant_risk_PV_50p)

roc_PVall_u50<-matrix(nrow=1000)
roc_PVall_50p<-matrix(nrow=1000)
roc_PV_u50<-matrix(nrow=1000)
roc_PV_50p<-matrix(nrow=1000)

CARRIERS_casestatus_u50$case50[1:20]
participant_risk_PVall_u50[1:100,1:10]

for (x in 1:1000){
roc_PVall_u50[x]<-roc(CARRIERS_casestatus_u50$case50,participant_risk_PVall_u50[,x])$auc
roc_PVall_50p[x]<-roc(CARRIERS_casestatus_50p$case50_80,participant_risk_PVall_50p[,x])$auc
roc_PV_u50[x]<-roc(CARRIERS_casestatus_u50_wFH$case50,participant_risk_PV_u50[,x])$auc
roc_PV_50p[x]<-roc(CARRIERS_casestatus_50p_wFH$case50_80,participant_risk_PV_50p[,x])$auc
}

write.csv(roc_PVall_u50, file = "roc_PVall_u50.csv")
write.csv(roc_PVall_50p, file = "roc_PVall_50p.csv")
write.csv(roc_PV_u50, file = "roc_PV_u50.csv")
write.csv(roc_PV_50p, file = "roc_PV_50p.csv")

results_roc_all_u50<-quantile(roc_PVall_u50,probs=c(0.5,0.025,0.975),na.rm=TRUE)
results_roc_all_50p<-quantile(roc_PVall_50p,probs=c(0.5,0.025,0.975),na.rm=TRUE)
results_roc_u50<-quantile(roc_PV_u50,probs=c(0.5,0.025,0.975),na.rm=TRUE)
results_roc_50p<-quantile(roc_PV_50p,probs=c(0.5,0.025,0.975),na.rm=TRUE)

results_roc_nocovar<-cbind(results_roc_all_u50,results_roc_all_50p,results_roc_u50,results_roc_50p)

write.csv(results_roc_nocovar, file = "results_roc_nocovar.csv")
