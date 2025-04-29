
*code covariates to match iCARE-Lit values;
DATA CARRIERS.FHdata2;
	SET CARRIERS.FHdata;

	*age at menarche;
	IF 0<AgeMenarche<11 THEN menarche_cat=0;
		ELSE IF 11<=AgeMenarche<13 THEN menarche_cat=1;
		ELSE IF 13<=AgeMenarche<14 THEN menarche_cat=2;
		ELSE IF 14<=AgeMenarche<16 THEN menarche_cat=3;
		ELSE IF 16<=AgeMenarche<100 THEN menarche_cat=4;
		ELSE IF AgeMenarche=888 THEN menarche_cat=.;

	*parity;
	parity2=parity;
	IF parity="NA" THEN parity2=" ";
	parity_num=input(parity2,12.);

	IF parity_num=0 THEN parity_cat=0;
		ELSE IF parity_num=1 THEN parity_cat=1;
		ELSE IF parity_num=2 THEN parity_cat=2;
		ELSE IF 3<=parity_num<35 THEN parity_cat=3;
		ELSE IF parity_num=888 THEN parity_cat=.;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;

	*age at first pregnancy;
	AgeFirstFTPreg2=AgeFirstFTPreg;
	IF AgeFirstFTPreg="NA" THEN AgeFirstFTPreg2=" ";
	AgeFirstFTPreg_num=input(AgeFirstFTPreg2,12.);

	IF 10<=AgeFirstFTPreg_num<20 THEN agepreg_cat=0;
		ELSE IF 20<=AgeFirstFTPreg_num<25 THEN agepreg_cat=1;
		ELSE IF 25<=AgeFirstFTPreg_num<30 THEN agepreg_cat=2;
		ELSE IF 30<=AgeFirstFTPreg_num<100 THEN agepreg_cat=3;
		ELSE agepreg_cat=.;
	*filler for non-parous;
	IF parous=0 THEN agepreg_cat=0;
	*age at menopause;
	Menopause_age2=Menopause_age;
	IF Menopause_age="NA" THEN Menopause_age2=" ";
	Menopause_age_num=input(Menopause_age2,12.);


	IF MenoStat="2" AND 10<=Menopause_age_num<45 THEN agemeno_cat=0;
		ELSE IF MenoStat="2" AND 45<=Menopause_age_num<50 THEN agemeno_cat=1;
		ELSE IF MenoStat="2" AND 50<=Menopause_age_num<55 THEN agemeno_cat=2;
		ELSE IF MenoStat="2" AND 55<=Menopause_age_num<100 THEN agemeno_cat=3;
		ELSE IF MenoStat="2" THEN agemeno_cat=.;
		ELSE IF MenoStat="1" THEN agemeno_cat=0; *filler for premenopausal women;
		ELSE IF MenoStat="888" THEN agemeno_cat=.M;

	*keep continuous age at menopause variable as well, imputed as 51 if unknown/premeno;
	IF 10<=Menopause_age_num<=100 THEN menoage=Menopause_age_num;

	*fix a few that had age at menopause but not menostatus;
	*first code menopause status;
	IF MenoStat="2" THEN postmeno=1;
		ELSE IF MenoStat="1" THEN postmeno=0;
		ELSE IF Menostat="888" AND 0<menoage<Age THEN postmeno=1;
		ELSE IF Menostat="888" AND menoage>=Age AND 0<age<100 THEN postmeno=0;
	*then correct menopause age;

	IF MenoStat="888" AND postmeno=1 AND 0<menoage<45 THEN agemeno_cat=0;
		ELSE IF MenoStat="888" AND postmeno=1 AND 45<=menoage<50 THEN agemeno_cat=1;
		ELSE IF MenoStat="888" AND postmeno=1 AND 50<=menoage<55 THEN agemeno_cat=2;
		ELSE IF MenoStat="888" AND postmeno=1 AND 55<=menoage<100 THEN agemeno_cat=3;
		ELSE IF MenoStat="888" AND postmeno=1 AND menoage>100 THEN agemeno_cat=.;
	*use placeholder if still premenopausal;
	IF postmeno=0 THEN agemeno_cat=0;

	*height;
	height2=height;
	IF height="NA" THEN height2=" ";
	height_num=input(height2,12.);

	IF height_num<800 THEN height_dec=height_num/10;

	*BMI;
	BMI2=BMI;
	IF BMI="NA" THEN BMI2=" ";
	BMI_num=input(BMI2,12.);

	IF 0<bmi_num<25 THEN bmi_cat=0;
		ELSE IF 25<=bmi_num<30 THEN bmi_cat=1;
		ELSE IF 30<=bmi_num<100 THEN bmi_cat=2;
		ELSE IF bmi_num>100 THEN bmi_cat=.;

	IF HRTever="0" THEN HRTever_num=0;
		ELSE IF HRTever="1" THEN HRTever_num=1;
		ELSE IF HRTever="888" THEN HRTever_num=.;
	IF HRTever_num=0 THEN HRTtype=0;
		ELSE IF HRTever_num=1 AND HRTuse_eplusp in ("1","2","3") THEN HRTtype=1;
		ELSE IF HRTever_num=1 AND HRTuse_eonly in ("1","2","3") THEN HRTtype=2;
	IF postmeno=0 THEN HRTtype=0;
	IF postmeno=0 THEN HRTever_num=0;

	*alcohol;
	alcohol=alcoholcum;
	IF alcoholcum="NA" THEN alcohol=" ";
	alcohol_num=input(alcohol,12.);

	IF alcohol_num=0 THEN alcoholgday_cat=0;
		ELSE IF 0<alcohol_num<5 THEN alcoholgday_cat=1;
		ELSE IF 5<=alcohol_num<15 THEN alcoholgday_cat=2;
		ELSE IF 15<=alcohol_num<100 THEN alcoholgday_cat=3;

	*WCHS has age start of alcohol, create never regular, before 21 regular and after 21 regular;
	IF  AlcRegStartAge=777 THEN everregAlcohol=0;
		ELSE IF 5<AlcRegStartAge<21 THEN everregAlcohol=1;
		ELSE IF 21<=AlcRegStartAge<100 THEN everregAlcohol=2;
	IF alcoholgday_cat=0 AND AlcRegStartAge<0 THEN everregAlcohol=0;

	*breastfeeding;
	IF breastfeed="1" THEN breastfedyn=1;
		ELSE IF breastfeed="2" THEN breastfedyn=0;
		ELSE IF breastfeed="888" THEN breastfedyn=.;
	IF breastfedyn=. AND parous=0 THEN breastfedyn=0;

	*oral contraceptive use;
	IF OCuse="0" THEN OCuse_cat=0; *never;
		ELSE IF OCuse="2" THEN OCuse_cat=1; *former;
		ELSE IF OCuse="1" THEN OCuse_cat=2; *current;
		*if missing timing, make assumptions based on age and menopause status;
		ELSE IF OCuse="3" AND postmeno=1 THEN OCuse_cat=1;
		ELSE IF OCuse="3" AND postmeno=0 AND Age>=50 THEN OCuse_cat=1;
		ELSE IF OCuse="3" AND postmeno=0 AND Age<50 THEN OCuse_cat=2; *805 current, 4693 former;

	*agegroup variable;
	IF 0<age<50 THEN age_50p=0;
		ELSE IF age>=50 THEN age_50p=1;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	TABLES raceethnicity menarche_cat postmeno postmeno*agemeno_cat parity_cat parous agepreg_cat
	bmi_cat HRTever_num HRTtype alcoholgday_cat everregAlcohol breastfedyn OCuse_cat age_50p/ MISSING;
RUN;

PROC MEANS data=CARRIERS.FHdata2 n nmiss;
	VAR age height_dec;
RUN;
*%missing each variable;
	/*age at first pregnancy - 28%
	*OCuse - 24%
	*age at menopause = 29%
	*alcohol - 36%
	*breastfeeding - 50%
	*BMI - 5%
	*height - 3%
	*parity - 3%
	*age at menarche = 2.7%
	*HRT type - 20%
	*race-ethnicity = 0%
	*postmeno = 0% (imputed at 51) */
	
*are there  studies missing all or most of covars?;
DATA CARRIERS.FHdata2;
	SET CARRIERS.FHdata2;
	IF agepreg_cat<0 THEN agepreg_miss=1;
	IF OCuse_cat<0 THEN OCuse_miss=1;
	IF agemeno_cat<0 THEN agemeno_miss=1;
	IF alcoholgday_cat<0 THEN alcohol_miss=1;
	IF breastfedyn<0 THEN breastfed_miss=1;
	IF bmi_cat<0 THEN Bmi_miss=1;
	IF height_dec<0 THEN height_miss=1;
	IF parity_cat<0 THEN parity_miss=1;
	IF menarche_cat<0 THEN menarche_miss=1;
	IF HRTtype<0 THEN HRT_miss=1;
	SUM_miss=SUM(agepreg_miss,OCuse_miss,agemeno_miss,alcohol_miss,breastfed_miss,
		Bmi_miss,height_miss,parity_miss,menarche_miss,HRT_miss);
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	TABLES study*SUM_miss / nocum nocol norow nopercent;
RUN;

*DECISION: drop breastfeeding as covariate;

*next biggest offender is alcohol with 36% missing;
PROC FREQ data=CARRIERS.FHdata2;
	TABLES study*alcoholgday_cat / MISSING NOCUM NOCOL NOPERCENT;
RUN;
*missing all alcohol: CTS, MMHS, WCHS, WHI; 

*28% missing age at first pregnancy;
PROC FREQ data=CARRIERS.FHdata2;
	TABLES study*agepreg_cat / MISSING NOCUM NOCOL NOPERCENT;
RUN;
*missing all ages: BWHS, MEC, WCHS missing 59%;

 *24% missing OCuse;
PROC FREQ data=CARRIERS.FHdata2;
	TABLES study*OCuse_cat / MISSING NOCUM NOCOL NOPERCENT;
RUN;
*missing all: BWHS, MEC, WCHS missing 70%;

PROC FREQ data=CARRIERS.FHdata2;
	WHERE study in ("BWHS", "MEC","WCHS");
	TABLES OCuse;
RUN;

 *24% missing age at menopause;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE postmeno=1;
	TABLES study*agemeno_cat / MISSING NOCUM NOCOL NOPERCENT;
RUN;
*NOTE: if only show up as 0's and 2's this is based on imputed data;
*missing all: BWHS, MEC, WCHS missing 74%;

PROC FREQ data=CARRIERS.FHdata2;
	TABLES study*postmeno / MISSING NOCUM NOCOL NOPERCENT;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	WHERE study in ("BWHS", "MEC");
	TABLES MenoStat Menopause_age postmeno menoage;
RUN;

*20% missing HRT type;
PROC FREQ data=CARRIERS.FHdata2;
	TABLES study*HRTever_num study*HRTtype/ MISSING NOCUM NOCOL NOPERCENT;
RUN;
*missing type of HRT: BWHS, CTS, MEC;
*use ever/never form of variable to aid with imputation;

PROC FREQ data=CARRIERS.FHdata2;
	WHERE study in ("BWHS","CTS","MEC");
	TABLES HRTEver HRTCurrent HRTuse HRTuse_eplusp HRTuse_eonly HRTtype;
RUN;

*Decision point: could drop BWHS, MEC, and WCHS, but these are the ones 
	with larger proportions of Black or non-White women: KEEP IN;

*redo height to nearest 0.5;
DATA CARRIERS.FHdata2;
	SET CARRIERS.FHdata2;
	height_dec_r=round(height_dec,0.5);	

	IF OCuse_cat=1 | OCuse_cat=2 THEN OCuse_ever=1;
		ELSE IF OCuse_cat=0 THEN OCuse_ever=0;
		ELSE IF OCuse_cat<0 THEN OCuse_ever=.;

	IF raceethnicity="Non_Hispanic_White" THEN raceeth=0;
		ELSE IF raceethnicity="Black_or_AA" THEN raceeth=1;
		ELSE IF raceethnicity="Hispanic" THEN raceeth=2;
		ELSE IF raceethnicity="Asian" THEN raceeth=3;
		ELSE IF raceethnicity="Other" THEN raceeth=4;

	IF agemeno_cat>0 AND postmeno=0 THEN postmeno=1;

	IF FH1st=0 THEN FH1st_age50=0;
		ELSE IF FH1st=1 AND age<50 THEN FH1st_age50=1;
		ELSE IF FH1st=1 AND age>=50 THEN FH1st_age50=2;

RUN;

PROC FREQ data=CARRIERS.FHdata2;
	TABLES postmeno*agemeno_cat;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	TABLES FH1st FH1st*case;
RUN;

PROC MEANS data=CARRIERS.FHdata2 median p25 p75 n nmiss;
	CLASS FH1st case;
	VAR age height_dec_r;
RUN;
PROC MEANS data=CARRIERS.FHdata2 n nmiss;
	VAR age height_dec_r;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	TABLES case*FH1st_age50;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	TABLES raceeth menarche_cat postmeno parity_cat parous alcoholgday_cat OCuse_ever bmi_cat / missing;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE postmeno=1;
	TABLES agemeno_cat HRTtype / missing;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE parous=1;
	TABLES agepreg_cat / missing;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=0 AND case=1;
	TABLES raceeth menarche_cat postmeno parity_cat alcoholgday_cat OCuse_ever;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=0 AND case=1 AND postmeno=1;
	TABLES agemeno_cat HRTtype*bmi_cat;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=0 AND case=1 AND parous=1;
	TABLES agepreg_cat;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=0 AND case=1 AND postmeno=0;
	TABLES bmi_cat;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=0 AND case=0;
	TABLES raceeth menarche_cat postmeno parity_cat parous alcoholgday_cat OCuse_ever ;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=0 AND case=0 AND postmeno=1;
	TABLES agemeno_cat HRTtype*bmi_cat;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=0 AND case=0 AND parous=1;
	TABLES agepreg_cat ;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=0 AND case=0 AND postmeno=0;
	TABLES bmi_cat;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=1 AND case=1;
	TABLES raceeth menarche_cat postmeno parity_cat parous alcoholgday_cat OCuse_ever ;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=1 AND case=1 AND postmeno=1;
	TABLES agemeno_cat HRTtype*bmi_cat;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=1 AND case=1 AND parous=1;
	TABLES agepreg_cat ;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=1 AND case=1 AND postmeno=0;
	TABLES bmi_cat;
RUN;

PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=1 AND case=0;
	TABLES raceeth menarche_cat postmeno parity_cat parous alcoholgday_cat OCuse_ever ;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=1 AND case=0 AND postmeno=1;
	TABLES agemeno_cat HRTtype*bmi_cat;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=1 AND case=0 AND parous=1;
	TABLES agepreg_cat ;
RUN;
PROC FREQ data=CARRIERS.FHdata2;
	WHERE FH1st=1 AND case=0 AND postmeno=0;
	TABLES bmi_cat;
RUN;

*impute to get ORs;
*run imputation model. use 10 imputes so math is easy;
PROC MI data=CARRIERS.FHdata2 out=carriers_imp nimpute=10 seed=42424;
	CLASS case raceethnicity HRTever_num parous menarche_cat agemeno_cat parity_cat 
	agepreg_cat bmi_cat alcoholgday_cat OCuse_ever study HRTtype height_dec_r postmeno;
	FCS discrim (HRTever_num / classeffects=include);
	FCS discrim (parous / classeffects=include);
	FCS discrim (height_dec_r / classeffects=include);
	FCS discrim (parity_cat / classeffects=include);
	FCS discrim (menarche_cat / classeffects=include);
	FCS discrim (bmi_cat / classeffects=include);
	FCS discrim (HRTtype / classeffects=include);
	FCS discrim (postmeno / classeffects=include);
	FCS discrim (agemeno_cat / classeffects=include);
	FCS discrim (OCuse_ever / classeffects=include);
	FCS discrim (agepreg_cat / classeffects=include);
	FCS discrim (alcoholgday_cat / classeffects=include);
	VAR raceethnicity FH1st age study case postmeno HRTever_num parous
	height_dec_r parity_cat menarche_cat bmi_cat HRTtype agemeno_cat
	OCuse_ever agepreg_cat alcoholgday_cat;
RUN;

DATA CARRIERS.carriers_imp;
	SET carriers_imp;
RUN;


PROC MEANS data=carriers_imp median p25 p75 n nmiss;
	CLASS FH1st case;
	VAR age height_dec_r;
RUN;

PROC FREQ data=carriers_imp;
	TABLES raceeth menarche_cat postmeno parity_cat parous alcoholgday_cat OCuse_ever bmi_cat / missing;
RUN;
PROC FREQ data=carriers_imp;
	WHERE postmeno=1;
	TABLES agemeno_cat HRTtype / missing;
RUN;
PROC FREQ data=carriers_imp;
	WHERE parous=1;
	TABLES agepreg_cat / missing;
RUN;




*check certain variables;
PROC FREQ data=CARRIERS.carriers_imp;
	TABLES HRTever_num*HRTtype parous*parity_cat parous*agepreg_cat
	postmeno*agemeno_cat;
RUN;

*not many discrepancies, but re-set to clean up for consistency;
DATA CARRIERS.carriers_imp;
	SET CARRIERS.carriers_imp;
	IF HRTever_num=0 THEN HRTtype=0;
	IF parous=0 THEN parity_cat=0;
	IF parous=0 THEN agepreg_cat=0;

	*assign those age 50+ an age at menopause even if premenopausal (will use in 50+ analysis);
	IF agemeno_cat=3 AND age>=55 THEN postmeno=1;
		ELSE IF agemeno_cat=3 AND age<55 THEN postmeno=0;
	IF agemeno_cat=2 AND age>=50 THEN postmeno=1;
		ELSE IF agemeno_cat=2 AND age<50 THEN postmeno=0;
	IF agemeno_cat=1 AND age>=45 THEN postmeno=1;
		ELSE IF agemeno_cat=1 AND age<45 THEN postmeno=0;
	IF postmeno=0 THEN agemeno_cat=0;

	IF postmeno=0 THEN HRTtype=0;

RUN;

*check again;
PROC FREQ data=CARRIERS.carriers_imp;
	TABLES HRTever_num*HRTtype parous*parity_cat parous*agepreg_cat
	postmeno*agemeno_cat;
RUN;

*create indicator variables;
DATA CARRIERS.carriers_imp;
	SET CARRIERS.carriers_imp;
	IF raceethnicity="Black_or_AA" THEN Black=1;
		ELSE Black=0;
	IF raceethnicity="Hispanic" THEN Hispanic=1;
		ELSE Hispanic=0;
	IF raceethnicity="Asian" THEN Asian=1;
		ELSE Asian=0;
	IF raceethnicity="Other" THEN other=1;
		ELSE other=0;
	IF menarche_cat=1 THEN menarche_cat1=1;
		ELSE menarche_cat1=0;
	IF menarche_cat=2 THEN menarche_cat2=1;
		ELSE menarche_cat2=0;
	IF menarche_cat=3 THEN menarche_cat3=1;
		ELSE menarche_cat3=0;
	IF menarche_cat=4 THEN menarche_cat4=1;
		ELSE menarche_cat4=0;
	IF agemeno_cat=1 AND postmeno=1 THEN agemeno_cat1=1;
		ELSE agemeno_cat1=0;
	IF agemeno_cat=2 AND postmeno=1 THEN agemeno_cat2=1;
		ELSE agemeno_cat2=0;
	IF agemeno_cat=3 AND postmeno=1 THEN agemeno_cat3=1;
		ELSE agemeno_cat3=0;
	IF parity_cat=1 THEN parity_cat1=1;
		ELSE parity_cat1=0;
	IF parity_cat=2 THEN parity_cat2=1;
		ELSE parity_cat2=0;
	IF parity_cat=3 THEN parity_cat3=1;
		ELSE parity_cat3=0;
	IF agepreg_cat=1 AND parous=1 THEN agepreg_cat1=1;
		ELSE agepreg_cat1=0;
	IF agepreg_cat=2 AND parous=1 THEN agepreg_cat2=1;
		ELSE agepreg_cat2=0;
	IF agepreg_cat=3 AND parous=1 THEN agepreg_cat3=1;
		ELSE agepreg_cat3=0;
	IF bmi_cat=1 AND postmeno=0 THEN bmi_cat1=1;
		ELSE bmi_cat1=0;
	IF bmi_cat=2 AND postmeno=0 THEN bmi_cat2=1;
		ELSE bmi_cat2=0;
	IF HRTtype=0 AND bmi_cat=1 AND postmeno=1 THEN noHRTow=1;
		ELSE noHRTow=0;
	IF HRTtype=0 AND bmi_cat=2 AND postmeno=1 THEN noHRTob=1;
		ELSE noHRTob=0;
	IF HRTtype=1 AND bmi_cat=0 AND postmeno=1 THEN EPnorm=1;
		ELSE EPnorm=0;
	IF HRTtype=1 AND bmi_cat=1 AND postmeno=1 THEN EPow=1;
		ELSE EPow=0;
	IF HRTtype=1 AND bmi_cat=2 AND postmeno=1 THEN EPob=1;
		ELSE EPob=0;
	IF HRTtype=2 AND bmi_cat=0 AND postmeno=1 THEN Enorm=1;
		ELSE Enorm=0;
	IF HRTtype=2 AND bmi_cat=1 AND postmeno=1 THEN Eow=1;
		ELSE Eow=0;
	IF HRTtype=2 AND bmi_cat=2 AND postmeno=1 THEN Eob=1;
		ELSE Eob=0;
	IF alcoholgday_cat=1 THEN alcohol_cat1=1;
		ELSE alcohol_cat1=0;
	IF alcoholgday_cat=2 THEN alcohol_cat2=1;
		ELSE alcohol_cat2=0;
	IF alcoholgday_cat=3 THEN alcohol_cat3=1;
		ELSE alcohol_cat3=0;
	IF OCuse_cat=1 THEN OCuse_cat1=1;
		ELSE OCuse_cat1=0;
	IF OCuse_cat=2 THEN OCuse_cat2=1;
		ELSE OCuse_cat2=0;
RUN;

*do everything stratified by age 50;
PROC FREQ data=CARRIERS.carriers_imp;
	WHERE age_50p=0;
	TABLES FH1st*case;
RUN;

*overall ORs;
PROC LOGISTIC data=CARRIERS.carriers_imp DESC;
	CLASS study;
	WHERE age_50p=0;
	MODEL case = age FH1st study height_dec_r Black Hispanic Asian other menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	parity_cat1 parity_cat2 parity_cat3 agepreg_cat1 agepreg_cat2 agepreg_cat3 bmi_cat1 bmi_cat2
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=ORs;
RUN;
PROC MIANALYZE PARMS(classvar=classval)=ORs;
	MODELEFFECTS age FH1st height_dec_r Black Hispanic Asian other menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	parity_cat1 parity_cat2 parity_cat3 agepreg_cat1 agepreg_cat2 agepreg_cat3 bmi_cat1 bmi_cat2
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	ODS OUTPUT parameterestimates=ORs_all_u50;
RUN;
*merge all together and create LCL and UCL;
*output covariance matrix;
DATA ORs_all_u50;
	LENGTH Parm $ 25;
	SET ORs_all_u50;
	OR=exp(Estimate);
	LCL=exp(LCLMean);
	UCL=exp(UCLMean);
	KEEP Parm OR LCL UCL;
RUN;
proc print data=ORs_all_u50;
run;

PROC LOGISTIC data=CARRIERS.carriers_imp DESC;
	WHERE FH1st=0 AND age_50p=0;
	CLASS study;
	MODEL case = age study height_dec_r Black Hispanic Asian other menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	parity_cat1 parity_cat2 parity_cat3 
	agepreg_cat1 agepreg_cat2 agepreg_cat3 bmi_cat1 bmi_cat2
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=ORs;
RUN;

PROC MIANALYZE PARMS(classvar=classval)=ORs;
	MODELEFFECTS age height_dec_r Black Hispanic Asian other
	menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	parity_cat1 parity_cat2 parity_cat3
	agepreg_cat1 agepreg_cat2 agepreg_cat3 bmi_cat1 bmi_cat2
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	ODS OUTPUT parameterestimates=ORs_noFH_u50;
RUN;
DATA ORs_noFH_u50;
	LENGTH Parm $ 25;
	SET ORs_noFH_u50;
	OR=exp(Estimate);
	LCL=exp(LCLMean);
	UCL=exp(UCLMean);
	KEEP Parm OR LCL UCL;
RUN;
proc print data=ORs_noFH_u50;
run;

PROC LOGISTIC data=CARRIERS.carriers_imp DESC;
	WHERE FH1st=1 AND age_50p=0;
	CLASS study;
	MODEL case = study age height_dec_r Black Hispanic Asian other menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	parity_cat1 parity_cat2 parity_cat3 
	agepreg_cat1 agepreg_cat2 agepreg_cat3 bmi_cat1 bmi_cat2
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=ORsFH;
RUN;

PROC MIANALYZE PARMS(classvar=classval)=ORsFH;
	MODELEFFECTS age height_dec_r Black Hispanic Asian other 
	menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	parity_cat1 parity_cat2 parity_cat3
	agepreg_cat1 agepreg_cat2 agepreg_cat3 bmi_cat1 bmi_cat2 
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	ODS OUTPUT parameterestimates=ORs_FH_u50;
RUN;
DATA ORs_FH_u50;
	LENGTH Parm $ 25;
	SET ORs_FH_u50;
	OR=exp(Estimate);
	LCL=exp(LCLMean);
	UCL=exp(UCLMean);
	KEEP Parm OR LCL UCL;
RUN;
proc print data=ORs_FH_u50;
run;

/*
PROC LOGISTIC data=CARRIERS.carriers_imp DESC;
	WHERE age_50p=0;
	CLASS study raceeth menarche_cat parity_cat agepreg_cat bmi_cat alcoholgday_cat;
	MODEL case = study age height_dec_r FH1st raceeth menarche_cat parity_cat bmi_cat
	alcoholgday_cat OCuse_ever FH1st*study FH1st*age FH1st*height_dec_r FH1st*raceeth 
	FH1st*menarche_cat FH1st*parity_cat FH1st*agepreg_cat FH1st*bmi_cat
	FH1st*alcoholgday_cat FH1st*OCuse_ever;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=ORs_int_u50;
RUN;

PROC MIANALYZE PARMS(classvar=classval)=ORs_int_u50;
	MODELEFFECTS FH1st*age FH1st*height_dec_r FH1st*raceeth FH1st*menarche_cat  
	FH1st*parity_cat FH1st*agepreg_cat FH1st*bmi_cat
	FH1st*alcoholgday_ca FH1st*OCuse_ever;
	ODS OUTPUT parameterestimates=ORs_int2_u50;
RUN;

DATA ORs_int2_u50;
	LENGTH Parm $ 25;
	SET ORs_int2_u50;
	KEEP Parm Probt;
RUN;
proc print data=ORs_int2_u50;
RUN;*/

*get joint Wald from one imputation;
DATA carriers_inter;
	SET CARRIERS.carriers_imp;
	FH_age=FH1st*age;
	FH_height=FH1st*height_dec_r;
	FH_Black=FH1st*Black;
	FH_Hispanic=FH1st*Hispanic;
	FH_ASian=FH1st*Asian;
	FH_Other=FH1st*other;
	FH_menarche1=FH1st*menarche_cat1;
	FH_menarche2=FH1st*menarche_cat2;
	FH_menarche3=FH1st*menarche_cat3;
	FH_menarche4=FH1st*menarche_cat4;
	FH_parity1=FH1st*parity_cat1;
	FH_parity2=FH1st*parity_cat2;
	FH_parity3=FH1st*parity_cat3;
	FH_agepreg1=FH1st*agepreg_cat1;
	FH_agepreg2=FH1st*agepreg_cat2;
	FH_agepreg3=FH1st*agepreg_cat3;
	FH_bmi1=FH1st*bmi_cat1;
	FH_bmi2=FH1st*bmi_cat2;
	FH_alcohol1=FH1st*alcohol_cat1;
	FH_alcohol2=FH1st*alcohol_cat2;
	FH_alcohol3=FH1st*alcohol_cat3;
	FH_OCever=FH1st*OCuse_Ever;
RUN;
PROC LOGISTIC data=carriers_inter DESC;
	WHERE age_50p=0 AND _Imputation_=7;
	CLASS study;
	MODEL case = study age height_dec_r Black Hispanic Asian other menarche_cat1 menarche_cat2 menarche_cat3 
	menarche_cat4 parity_cat1 parity_cat2 parity_cat3 
	agepreg_cat1 agepreg_cat2 agepreg_cat3 bmi_cat1 bmi_cat2
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever 
	FH1st*study FH_age FH_height FH_Black FH_Hispanic FH_Asian FH_other
	FH_menarche1 FH_menarche2 FH_menarche3 FH_menarche4 FH_parity1 FH_parity2 FH_parity3
	FH_agepreg1 FH_agepreg2 FH_agepreg3 FH_bmi1 FH_bmi2
	FH_alcohol1 FH_alcohol2 FH_alcohol3 FH_OCever;
	TEST1: test FH_age;
	TEST2: test FH_height;
	TEST3: test FH_Black,FH_Hispanic,FH_Asian,FH_other;
	TEST4: test FH_menarche1,FH_menarche2,FH_menarche3,FH_menarche4;
	TEST5: test FH_parity1, FH_parity2, FH_parity3;
	TEST6: test FH_agepreg1,FH_agepreg2,FH_agepreg3;
	TEST7: test FH_bmi1,FH_bmi2;
	TEST8: test FH_alcohol1,FH_alcohol2,FH_alcohol3;
	TEST9: test FH_OCever;
RUN;

*look at distributions by parity, age at preg;
PROC FREQ data=CARRIERS.carriers_imp;
	WHERE age_50p=0;
	TABLES FH1st*parity_cat FH1st*agepreg_cat;
RUN;

*now do those over 50 - this is a proxy for menopausal status so don't need that variable;
PROC FREQ data=CARRIERS.carriers_imp;
	WHERE age_50p=1;
	TABLES FH1st*case;
RUN;

*overall ORs;
PROC LOGISTIC data=CARRIERS.carriers_imp DESC;
	WHERE age_50p=1;
	CLASS study;
	MODEL case = age study FH1st height_dec_r Black Hispanic Asian other menarche_cat1 
	menarche_cat2 menarche_cat3 menarche_cat4
	agemeno_cat1 agemeno_cat2 agemeno_cat3 parity_cat1 parity_cat2 parity_cat3 
	agepreg_cat1 agepreg_cat2 agepreg_cat3
	noHRTow noHRTob EPnorm EPow EPob Enorm Eow Eob
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=ORs;
RUN;
PROC MIANALYZE PARMS(classvar=classval)=ORs;
	MODELEFFECTS age FH1st height_dec_r Black Hispanic Asian other
	menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
 	agemeno_cat1 agemeno_cat2 agemeno_cat3 parity_cat1 parity_cat2 parity_cat3
	agepreg_cat1 agepreg_cat2 agepreg_cat3 
	noHRTow noHRTob EPnorm EPow EPob Enorm Eow Eob
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	ODS OUTPUT parameterestimates=ORs_50p;
RUN;
*merge all together and create LCL and UCL;
DATA ORs_50p;
	LENGTH Parm $ 25;
	SET ORs_50p;
	OR=exp(Estimate);
	LCL=exp(LCLMean);
	UCL=exp(UCLMean);
	KEEP Parm OR LCL UCL;
RUN;
proc print data=ORs_50p;
run;

PROC LOGISTIC data=CARRIERS.carriers_imp DESC;
	WHERE FH1st=0 AND age_50p=1;
	CLASS study;
	MODEL case = age study height_dec_r Black Hispanic Asian other 
	menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	agemeno_cat1 agemeno_cat2 agemeno_cat3 parity_cat1 parity_cat2 parity_cat3 
	agepreg_cat1 agepreg_cat2 agepreg_cat3 
	noHRTow noHRTob EPnorm EPow EPob Enorm Eow Eob
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=ORs;
RUN;

PROC MIANALYZE PARMS(classvar=classval)=ORs;
	MODELEFFECTS age height_dec_r Black Hispanic Asian other
	menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	agemeno_cat1 agemeno_cat2 agemeno_cat3 parity_cat1 parity_cat2 parity_cat3
	agepreg_cat1 agepreg_cat2 agepreg_cat3
	noHRTow noHRTob EPnorm EPow EPob Enorm Eow Eob
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	ODS OUTPUT parameterestimates=ORs_noFH_age50p;
RUN;
*merge all together and create LCL and UCL;
DATA ORs_noFH_age50p;
	LENGTH Parm $ 25;
	SET ORs_noFH_age50p;
	OR=exp(Estimate);
	LCL=exp(LCLMean);
	UCL=exp(UCLMean);
	KEEP Parm OR LCL UCL;
RUN;
proc print data=ORs_noFH_age50p;
run;

PROC LOGISTIC data=CARRIERS.carriers_imp DESC;
	WHERE FH1st=1 AND age_50p=1;
	CLASS study;
	MODEL case = study age height_dec_r Black Hispanic Asian other 
	menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	agemeno_cat1 agemeno_cat2 agemeno_cat3 parity_cat1 parity_cat2 parity_cat3 
	agepreg_cat1 agepreg_cat2 agepreg_cat3 
	noHRTow noHRTob EPnorm EPow EPob Enorm Eow Eob
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=FH_age50p;
RUN;

PROC MIANALYZE PARMS(classvar=classval)=FH_age50p;
	MODELEFFECTS age height_dec_r Black Hispanic Asian other 
	menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	agemeno_cat1 agemeno_cat2 agemeno_cat3 parity_cat1 parity_cat2 parity_cat3
	agepreg_cat1 agepreg_cat2 agepreg_cat3 
	noHRTow noHRTob EPnorm EPow EPob Enorm Eow Eob 
	alcohol_cat1 alcohol_cat2 alcohol_cat3 OCuse_ever;
	ODS OUTPUT parameterestimates=ORs_FH_age50p;
RUN;
*merge all together and create LCL and UCL;
DATA ORs_FH_age50p;
	LENGTH Parm $ 25;
	SET ORs_FH_age50p;
	OR=exp(Estimate);
	LCL=exp(LCLMean);
	UCL=exp(UCLMean);
	KEEP Parm OR LCL UCL;
RUN;

proc print data=ORs_FH_age50p;
run;

*recode version of BMIs and HRT so have 9 terms to interaction;
DATA BMIHRT_interact;
	SET CARRIERS.carriers_imp;
	IF BMI_cat=0 THEN BMI_cat_inter=1;
		ELSE IF BMI_cat=1 THEN BMI_cat_inter=2;
		ELSE IF BMI_cat=2 THEN BMI_cat_inter=3;
	IF HRTtype=0 THEN HRTtype_inter=7;
		ELSE IF HRTtype=1 THEN HRTtype_inter=8;
		ELSE IF HRTtype=2 THEN HRTtype_inter=9;
	IF FH1st=0 THEN FH1st_inter=1;
		ELSE IF FH1st=1 THEN FH1st_inter=2;
RUN;

proc freq data=BMIHRT_Interact;
	TABLE BMI_cat_inter*HRTtype_inter;
run;

/* PROC LOGISTIC data=BMIHRT_interact DESC;
	WHERE age_50p=1;
	CLASS study raceeth menarche_cat agemeno_cat parity_cat agepreg_cat HRTtype_inter alcoholgday_cat bmi_cat_inter / DESC;
	MODEL case = study age height_dec_r FH1st_inter raceeth menarche_cat agemeno_cat parity_cat
	bmi_cat_inter*HRTtype_inter alcoholgday_cat OCuse_ever FH1st_inter*study FH1st_inter*age FH1st_inter*height_dec_r FH1st_inter*raceeth 
	FH1st_inter*menarche_cat FH1st_inter*agemeno_cat FH1st_inter*parity_cat FH1st_inter*agepreg_cat
	FH1st_inter*bmi_cat_inter*HRTtype_inter FH1st_inter*alcoholgday_cat FH1st_inter*OCuse_ever;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=ORs_int_age50p;
RUN;
PROC MIANALYZE PARMS(classvar=classval)=ORs_int_age50p;
	MODELEFFECTS age*FH1st_inter height_de*FH1st_inte FH1st_inter*raceeth FH1st_int*menarche_c
	FH1st_int*agemeno_ca FH1st_int*parity_cat FH1st_int*agepreg_ca FH1st_*bmi_ca*HRTtyp
	FH1st_int*alcoholgda FH1st_int*OCuse_ever;
	ODS OUTPUT parameterestimates=ORs_int2_age50p;
RUN;

DATA ORs_int2_age50p;
	LENGTH Parm $ 25;
	SET ORs_int2_age50p;
	KEEP Parm Probt;
RUN;
proc print data=ORs_int2_age50p;
RUN;*/


*get joint Wald from one imputation;
DATA carriers_inter;
	SET carriers_inter;
	FH_agemeno1=FH1st*agemeno_cat1;
	FH_agemeno2=FH1st*agemeno_cat2;
	FH_agemeno3=FH1st*agemeno_cat3;
	FH_noHRTow=FH1st*noHRTow;
	FH_noHRTob=FH1st*noHRTob;
	FH_EPnorm=FH1st*EPnorm;
	FH_EPow=FH1st*EPow;
	FH_EPob=FH1st*EPob;
	FH_Enorm=FH1st*Enorm;
	FH_Eow=FH1st*Eow;
	FH_Eob=FH1st*Eob;
RUN;


PROC LOGISTIC data=carriers_inter DESC;
	WHERE age_50p=1 and _imputation_=7;
	CLASS study;
	MODEL case = age study FH1st height_dec_r Black Hispanic Asian other 
	menarche_cat1 menarche_cat2 menarche_cat3 menarche_cat4
	agemeno_cat1 agemeno_cat2 agemeno_cat3 
	parity_cat1 parity_cat2 parity_cat3 
	agepreg_cat1 agepreg_cat2 agepreg_cat3
	noHRTow noHRTob EPnorm EPow EPob Enorm Eow Eob
	alcohol_cat1 alcohol_cat2 alcohol_cat3 
	OCuse_ever
	FH1st*study FH_age FH_height FH_Black FH_Hispanic FH_Asian FH_other
	FH_menarche1 FH_menarche2 FH_menarche3 FH_menarche4 
	FH_agemeno1 FH_agemeno2 FH_agemeno3
	FH_parity1 FH_parity2 FH_parity3
	FH_agepreg1 FH_agepreg2 FH_agepreg3 
	FH_noHRTow FH_noHRTob FH_EPnorm FH_EPow FH_EPob FH_Enorm FH_Eow FH_Eob
	FH_alcohol1 FH_alcohol2 FH_alcohol3 FH_OCever;
	TEST1: test FH_age;
	TEST2: test FH_height;
	TEST3: test FH_Black,FH_Hispanic,FH_Asian,FH_other;
	TEST4: test FH_menarche1,FH_menarche2,FH_menarche3,FH_menarche4;
	TEST5: test FH_agemeno1,FH_agemeno2,FH_agemeno3;
	TEST6: test FH_parity1, FH_parity2, FH_parity3;
	TEST7: test FH_agepreg1,FH_agepreg2,FH_agepreg3;
	TEST8: test FH_noHRTow,FH_noHRTob,FH_EPnorm,FH_EPow,FH_EPob,FH_Enorm,FH_Eow,FH_Eob;
	TEST9: test FH_alcohol1,FH_alcohol2,FH_alcohol3;
	TEST10: test FH_OCever;
RUN;

*PV by environment interactions - new supplemental table 7;
DATA carriers_imp2;
	SET CARRIERS.carriers_imp;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	IF BMI_cat=2 THEN obese=1;
		ELSE IF 0<=BMI_cat<=1 THEN obese=0;
	IF alcoholgday_cat=3 THEN highalc=1;
		ELSE IF 0<=alcoholgday_cat<=2 THEN highalc=0;
	IF HRTtype=1 THEN EPHRT=1;
		ELSE IF HRTtype=0 | HRTtype=2 THEN EPHRT=0;
RUN;

PROC FREQ data=carriers_imp2;
	TABLES parous obese highalc EPHRT;
RUN;

PROC FREQ data=carriers_imp2;
	WHERE _Imputation_=1;
	TABLES age_50p*FH1st*anyPV age_50p*FH1st*ATM_mutation age_50p*FH1st*BRCA1_mutation
	age_50p*FH1st*BRCA2_mutation age_50p*FH1st*CHEK2_mutation age_50p*FH1st*PALB2_mutation
	/ norow nocol nocum nopercent;
RUN;


%MACRO u50_inter (FHvalue= ,PV= ,PVvalue= );
PROC LOGISTIC data=carriers_imp2 DESC;
	WHERE age_50p=0 AND FH1st=&FHvalue AND &PV=&PVvalue;
	MODEL case = age parous obese highalc;
	BY _Imputation_;
	ODS OUTPUT parameterestimates=ORs;
RUN;
PROC MIANALYZE PARMS(classvar=classval)=ORs;
	MODELEFFECTS parous obese highalc;
	ODS OUTPUT parameterestimates=ORs_u50;
RUN;
DATA ORs_u50_&PV._&FHvalue;
	LENGTH Parm $ 25;
	SET ORs_u50;
	OR=exp(Estimate);
	LCL=exp(LCLMean);
	UCL=exp(UCLMean);
	PV="&PV";
	FH=&FHvalue;
	KEEP Parm PV FH OR LCL UCL;
RUN;
%MEND;

%u50_inter(FHvalue=0,PV=anyPV,PVvalue=0);
%u50_inter(FHvalue=0,PV=ATM_mutation,PVvalue=1);
%u50_inter(FHvalue=0,PV=BRCA1_mutation,PVvalue=1);
%u50_inter(FHvalue=0,PV=BRCA2_mutation,PVvalue=1);
%u50_inter(FHvalue=0,PV=CHEK2_mutation,PVvalue=1);
%u50_inter(FHvalue=0,PV=PALB2_mutation,PVvalue=1);
%u50_inter(FHvalue=1,PV=anyPV,PVvalue=0);
%u50_inter(FHvalue=1,PV=ATM_mutation,PVvalue=1);
%u50_inter(FHvalue=1,PV=BRCA1_mutation,PVvalue=1);
%u50_inter(FHvalue=1,PV=BRCA2_mutation,PVvalue=1);
%u50_inter(FHvalue=1,PV=CHEK2_mutation,PVvalue=1);
%u50_inter(FHvalue=1,PV=PALB2_mutation,PVvalue=1);


DATA ORs_u50_all;
	SET ORs_u50_anyPV_0 ORs_u50_ATM_mutation_0 ORs_u50_BRCA1_mutation_0 ORs_u50_BRCA2_mutation_0
	ORs_u50_CHEK2_mutation_0 ORs_u50_PALB2_mutation_0 ORs_u50_anyPV_1 ORs_u50_ATM_mutation_1 
	ORs_u50_BRCA1_mutation_1 ORs_u50_BRCA2_mutation_1 ORs_u50_CHEK2_mutation_1 ORs_u50_PALB2_mutation_1;
RUN;
PROC PRINT data=ORs_u50_all;
RUN;

*generate models to bring into iCARE. use original non-imputed data set;
*iCARE models;
DATA forexport_u50_iCARE;
	RETAIN famhist height menarche parity afb bmi alcohol OCuse;
	SET CARRIERS.FHdata2;
	WHERE age<50;
	famhist=FH1st;
	height=height_dec_r;
	menarche=menarche_cat+1;
	parity=parity_cat+1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	alcohol=alcoholgday_cat+1;
	OCuse=OCuse_ever;
	INFORMAT height 8.0;
	KEEP famhist height menarche parity afb bmi alcohol OCuse;
RUN;
PROC EXPORT DATA= WORK.forexport_u50_iCARE
            OUTFILE= "realdata_u50_iCARE.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

PROC CONTENTS data=forexport_u50_iCARE;
run;

DATA forexport_50p_iCARE;
	RETAIN famhist height menarche parity afb bmi alcohol OCuse HRT agemeno;
	SET CARRIERS.FHdata2;
	WHERE age>=50;
	famhist=FH1st;
	height=height_dec_r;
	menarche=menarche_cat+1;
	parity=parity_cat+1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	alcohol=alcoholgday_cat+1;
	OCuse=OCuse_ever;
	HRT=HRTtype+1;
	agemeno=agemeno_cat+1;
	KEEP famhist height menarche parity afb bmi alcohol OCuse HRT agemeno;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_iCARE
            OUTFILE= "realdata_50p_iCARE.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

*CARRIERS models;
DATA forexport_u50_CARR;
	RETAIN famhist height raceeth menarche parity afb bmi alcohol OCuse;
	SET CARRIERS.FHdata2;
	WHERE age<50;
	famhist=FH1st;
	height=height_dec_r;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	parity=parity_cat+1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	alcohol=alcoholgday_cat+1;
	OCuse=OCuse_ever;
	KEEP famhist height raceeth menarche parity afb bmi alcohol OCuse;
RUN;
PROC EXPORT DATA= WORK.forexport_u50_CARR
            OUTFILE= "realdata_u50_CARR.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA forexport_50p_CARR;
	RETAIN famhist height raceeth menarche parity afb bmi alcohol OCuse HRT agemeno;
	SET CARRIERS.FHdata2;
	WHERE age>=50;
	famhist=FH1st;
	height=height_dec_r;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	parity=parity_cat+1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	alcohol=alcoholgday_cat+1;
	OCuse=OCuse_ever;
	HRT=HRTtype+1;
	agemeno=agemeno_cat+1;
	KEEP famhist height raceeth menarche parity afb bmi alcohol OCuse HRT agemeno;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_CARR
            OUTFILE= "realdata_50p_CARR.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

*hybrid models;
DATA forexport_u50_hybrid;
	RETAIN height raceeth menarche parous afb bmi highalc;
	SET CARRIERS.FHdata2;
	WHERE age<50;
	height=height_dec_r;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	IF alcoholgday_cat=3 THEN highalc=1;
		ELSE highalc=0;
	KEEP height raceeth menarche parous afb bmi highalc;
RUN;
PROC EXPORT DATA= WORK.forexport_u50_hybrid
            OUTFILE= "realdata_u50_hybrid.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_u50_hybrid_noFH;
	RETAIN height raceeth menarche parous afb bmi highalc;
	SET CARRIERS.FHdata2;
	WHERE age<50 AND FH1st=0;
	height=height_dec_r;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	IF alcoholgday_cat=3 THEN highalc=1;
		ELSE highalc=0;
	KEEP height raceeth menarche parous afb bmi highalc;
RUN;
PROC EXPORT DATA= WORK.forexport_u50_hybrid_noFH
            OUTFILE= "realdata_u50_hybrid_noFH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_u50_hybrid_FH;
	RETAIN height raceeth menarche parous afb bmi highalc;
	SET CARRIERS.FHdata2;
	WHERE age<50 AND FH1st=1;
	height=height_dec_r;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	IF alcoholgday_cat=3 THEN highalc=1;
		ELSE highalc=0;
	KEEP height raceeth menarche parous afb bmi highalc;
RUN;
PROC EXPORT DATA= WORK.forexport_u50_hybrid_FH
            OUTFILE= "realdata_u50_hybrid_FH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA forexport_50p_hybrid;
	RETAIN height raceeth menarche parous afb bmi alcohol EPHRT agemeno;
	SET CARRIERS.FHdata2;
	WHERE age>=50;
	height=height_dec_r;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	IF alcoholgday_cat=0 THEN alcohol=0;
		ELSE IF alcoholgday_cat in (1,2) THEN alcohol=1;
		ELSE IF alcoholgday_cat=3 THEN alcohol=2;
	IF HRTtype=2 THEN EPHRT=1;
		ELSE EPHRT=0;
	agemeno=agemeno_cat+1;
	KEEP height raceeth menarche parous afb bmi alcohol EPHRT agemeno;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_hybrid
            OUTFILE= "realdata_50p_hybrid.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_50p_hybrid_noFH;
	RETAIN height raceeth menarche parous afb bmi alc3 EPHRT agemeno;
	SET CARRIERS.FHdata2;
	WHERE age>=50 AND FH1st=0;
	height=height_dec_r;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	IF alcoholgday_cat=0 THEN alc3=1;
		ELSE IF alcoholgday_cat in (1,2) THEN alc3=2;
		ELSE IF alcoholgday_cat=3 THEN alc3=3;
	IF HRTtype=2 THEN EPHRT=1;
		ELSE EPHRT=0;
	agemeno=agemeno_cat+1;
	KEEP height raceeth menarche parous afb bmi alc3 EPHRT agemeno;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_hybrid_noFH
            OUTFILE= "realdata_50p_hybrid_noFH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_50p_hybrid_FH;
	RETAIN height raceeth menarche parous afb bmi alc3 EPHRT agemeno;
	SET CARRIERS.FHdata2;
	WHERE age>=50 AND FH1st=1;
	height=height_dec_r;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=bmi_cat+1;
	IF alcoholgday_cat=0 THEN alc3=1;
		ELSE IF alcoholgday_cat in (1,2) THEN alc3=2;
		ELSE IF alcoholgday_cat=3 THEN alc3=3;
	IF HRTtype=2 THEN EPHRT=1;
		ELSE EPHRT=0;
	agemeno=agemeno_cat+1;
	KEEP height raceeth menarche parous afb bmi alc3 EPHRT agemeno;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_hybrid_FH
            OUTFILE= "realdata_50p_hybrid_FH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

*also need a PV files;
DATA forexport_u50;
	RETAIN ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
	SET CARRIERS.FHdata2;
	WHERE age<50;
	KEEP ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
RUN;
PROC EXPORT DATA= WORK.forexport_u50
            OUTFILE= "realdataPV_u50.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_u50_noFH;
	RETAIN ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
	SET CARRIERS.FHdata2;
	WHERE age<50 AND FH1st=0;
	KEEP ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
RUN;
PROC EXPORT DATA= WORK.forexport_u50_noFH
            OUTFILE= "realdataPV_u50_noFH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_u50_FH;
	RETAIN ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
	SET CARRIERS.FHdata2;
	WHERE age<50 AND FH1st=1;
	KEEP ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
RUN;
PROC EXPORT DATA= WORK.forexport_u50_FH
            OUTFILE= "realdataPV_u50_FH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA forexport_50p;
	RETAIN ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
	SET CARRIERS.FHdata2;
	WHERE age>=50;
	KEEP ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
RUN;
PROC EXPORT DATA= WORK.forexport_50p
            OUTFILE= "realdataPV_50p.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_50p_noFH;
	RETAIN ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
	SET CARRIERS.FHdata2;
	WHERE age>=50 AND FH1st=0;
	KEEP ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_noFH
            OUTFILE= "realdataPV_50p_noFH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_50p_FH;
	RETAIN ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
	SET CARRIERS.FHdata2;
	WHERE age>=50 AND FH1st=1;
	KEEP ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation PALB2_mutation RAD51C_mutation;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_FH
            OUTFILE= "realdataPV_50p_FH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;


*export a file with just case status (ordered by CARRIERS_ID) to see how well model did;
*make sure correctly sorted by ID;
DATA casestatus;
	SET CARRIERS.FHdata2;
	case=case;
	IF case=1 AND age<50 THEN case50=1;
		ELSE case50=0;
	IF case=1 AND 50<=age<80 THEN case50_80=1;
		ELSE case50_80=0;
	FH1st=FH1st;
	KEEP FH1st case case50 case50_80 age_imp;
RUN;

PROC FREQ data=casestatus;
	TABLES FH1st case case50 case50_80;
RUN;

PROC EXPORT DATA= WORK.casestatus
            OUTFILE= "carriers_case_status.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

*race/eth specific PV files;
%MACRO raceethPV (raceeth= ,racename= ,FHname= ,FH1st= );
DATA forexport_u50_&racename._&FHname;
	RETAIN ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation CHEK2_1100 NF1_mutation PALB2_mutation RAD51C_mutation;
	SET CARRIERS.FHdata2;
	WHERE age<50 AND FH1st=&FH1st AND raceethnicity=&raceeth;
	KEEP ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation CHEK2_1100 NF1_mutation PALB2_mutation RAD51C_mutation;
RUN;
PROC EXPORT DATA= WORK.forexport_u50_&racename._&FHname
            OUTFILE= "realdataPV_u50_&racename._&FHname..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA forexport_50p_&racename._&FHname;
	RETAIN ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation CHEK2_1100 NF1_mutation PALB2_mutation RAD51C_mutation;
	SET CARRIERS.FHdata2;
	WHERE age>=50 AND FH1st=&FH1st AND raceethnicity=&raceeth;
	KEEP ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation
 	CHEK2_mutation CHEK2_1100 NF1_mutation PALB2_mutation RAD51C_mutation;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_&racename._&FHname
            OUTFILE= "realdataPV_50p_&racename._&FHname..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
%MEND;

%raceethPV(raceeth="Non_Hispanic_White",racename=NHW,FHname=noFH,FH1st=0)
%raceethPV(raceeth="Black_or_AA",racename=Black,FHname=noFH,FH1st=0)
%raceethPV(raceeth="Hispanic",racename=Hisp,FHname=noFH,FH1st=0)
%raceethPV(raceeth="Asian",racename=Asian1,FHname=noFH,FH1st=0)
%raceethPV(raceeth="Other",racename=other1,FHname=noFH,FH1st=0)
%raceethPV(raceeth="Non_Hispanic_White",racename=NHW,FHname=FH,FH1st=1)
%raceethPV(raceeth="Black_or_AA",racename=Black,FHname=FH,FH1st=1)
%raceethPV(raceeth="Hispanic",racename=Hisp,FHname=FH,FH1st=1)
%raceethPV(raceeth="Asian",racename=Asian1,FHname=FH,FH1st=1)
%raceethPV(raceeth="Other",racename=other1,FHname=FH,FH1st=1)


PROC EXPORT DATA= WORK.forexport_u50_Asian1_noFH
            OUTFILE= "realdataPV_u50_Asian_noFH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_Asian1_noFH
            OUTFILE= "realdataPV_50p_Asian_noFH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= WORK.forexport_u50_Asian1_FH
            OUTFILE= "realdataPV_u50_Asian_FH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= WORK.forexport_50p_Asian1_FH
            OUTFILE= "realdataPV_50p_Asian_FH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

