libname nhanes "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\NHANES data";

**ALL files taken from NHANES website;

options nofmterr;

%macro import (file= );
libname xptfile xport "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\NHANES data\&file..xpt";
proc copy in=xptfile out=nhanes memtype=data;
run;
%MEND;


%import(file=DEMO);
%import(file=DEMO_B);
%import(file=DEMO_C);
%import(file=DEMO_D);
%import(file=DEMO_E);
%import(file=DEMO_F);
%import(file=DEMO_G);
%import(file=DEMO_H);
%import(file=DEMO_I);
%import(file=DEMO_J);

DATA DEMO_A; SET nhanes.DEMO; year="99-00"; RUN;
DATA DEMO_B; SET nhanes.DEMO_B; year="01-02"; RUN;
DATA DEMO_C; SET nhanes.DEMO_C; year="03-04"; RUN;
DATA DEMO_D; SET nhanes.DEMO_D; year="05-06"; RUN;
DATA DEMO_E; SET nhanes.DEMO_E; year="07-08"; RUN;
DATA DEMO_F; SET nhanes.DEMO_F; year="09-10"; RUN;
DATA DEMO_G; SET nhanes.DEMO_G; year="11-12"; RUN;
DATA DEMO_H; SET nhanes.DEMO_H; year="13-14"; RUN;
DATA DEMO_I; SET nhanes.DEMO_I; year="15-16"; RUN;
DATA DEMO_J; SET nhanes.DEMO_J; year="17-18"; RUN;

PROC FORMAT;
VALUE male 1="male" 2="female";
VALUE RIDRETH1 1="Mexican American" 2="Other Hispanic" 3="Non-Hispanic White"
	4="Non-Hispanic Black" 5="Other";
VALUE RACEETH 0="Non-Hispanic White" 1="Non-Hispanic Black" 2="Hispanic" 3="Asian" 4="Other";
VALUE educ 0="< High School" 1="High school / GED" 2="some college" 
		3="college graduate or higher";
VALUE married 0="never married" 1="divorced, separated, widowed" 2="married/living together";
RUN;

DATA nhanes.demo_all;
	SET demo_a demo_b demo_c demo_d demo_e demo_f demo_g demo_h demo_i demo_j;
	*all studies have RIDRETH1 so use thar version (modified);
	IF RIDRETH3=3 THEN RACEETH=0;
		ELSE IF RIDRETH3=4 THEN RACEETH=1;
		ELSE IF RIDRETH3=1 | RIDRETH3=2 THEN RACEETH=2;
		ELSE IF RIDRETH3=6 THEN RACEETH=3; *Asian;
		ELSE IF RIDRETH3=7 THEN RACEETH=4; *other;

		ELSE IF RIDRETH3<0 AND RIDRETH1=3 THEN RACEETH=0;
		ELSE IF RIDRETH3<0 AND RIDRETH1=4 THEN RACEETH=1;
		ELSE IF RIDRETH3<0 AND (RIDRETH1=1 | RIDRETH1=2) THEN RACEETH=2;
		ELSE IF RIDRETH3<0 AND RIDRETH1=5 THEN RACEETH=3; *Asian;
	*education;
	IF 1<=DMDEDUC2<=2 THEN educ=0; *less than HS;
		ELSE IF DMDEDUC2=3 THEN educ=1; *HS/equiv;
		ELSE IF DMDEDUC2=4 THEN educ=2; *some college;
		ELSE IF DMDEDUC2=5 THEN educ=3; *college grad + ;
	*marital status;
	IF DMDMARTL=5 THEN marital=0; *never married;
		ELSE IF DMDMARTL in (2,3,4) THEN marital=1; *divorced, separated, widowed;
		ELSE IF DMDMARTL in (1,6) THEN marital=2; *married/living together;
	IF DMDMARTZ=3 THEN marital=0; *never married;
		ELSE IF DMDMARTZ=2 THEN marital=1; *divorced, separated, widowed;
		ELSE IF DMDMARTZ=1 THEN marital=2; *married/living together;
	*family income;
	IF 0<=INDFMPIR<1 THEN IPR=0;
		ELSE IF 1<=INDFMPIR<2 THEN IPR=1;
		ELSE IF 2<=INDFMPIR<5 THEN IPR=2;
		ELSE IF 5<=INDFMPIR<300 THEN IPR=3;

	*survey weights;
	IF year in ("99-00","01-02") then wt = (2/10) * WTINT4YR; /* for 1999-2002 */
		ELSE if year in ("03-04","05-06","07-08","09-10","11-12","13-14","15-16","17-18")
				THEN wt = (1/10) * WTINT2YR; /* for 2003-2018 */

	KEEP year RIAGENDR RIDAGEYR RACEETH educ SEQN wt marital IPR sdmvstra SDMVPSU;
RUN;
PROC FREQ data=nhanes.demo_all;
	TABLES RIAGENDR RACEETH year*educ year*marital year*IPR/ missing;
	FORMAT RIAGENDR male. RACEETH RACEETH. educ educ.;
RUN;
PROC MEANS data=nhanes.demo_all;
	CLASS year;
	VAR RIDAGEYR wt;
RUN;

%import(file=BMX);
%import(file=BMX_B);
%import(file=BMX_C);
%import(file=BMX_D);
%import(file=BMX_E);
%import(file=BMX_F);
%import(file=BMX_G);
%import(file=BMX_H);
%import(file=BMX_I);
%import(file=BMX_J);

DATA BMX_A; SET nhanes.BMX; year="99-00"; RUN;
DATA BMX_B; SET nhanes.BMX_B; year="01-02"; RUN;
DATA BMX_C; SET nhanes.BMX_C; year="03-04"; RUN;
DATA BMX_D; SET nhanes.BMX_D; year="05-06"; RUN;
DATA BMX_E; SET nhanes.BMX_E; year="07-08"; RUN;
DATA BMX_F; SET nhanes.BMX_F; year="09-10"; RUN;
DATA BMX_G; SET nhanes.BMX_G; year="11-12"; RUN;
DATA BMX_H; SET nhanes.BMX_H; year="13-14"; RUN;
DATA BMX_I; SET nhanes.BMX_I; year="15-16"; RUN;
DATA BMX_J; SET nhanes.BMX_J; year="17-18"; RUN;

DATA nhanes.bmx_all;
	SET bmx_a bmx_b bmx_c bmx_d bmx_e bmx_f bmx_g bmx_h bmx_i bmx_j;
	KEEP year BMXHT BMXBMI SEQN;
RUN;
PROC MEANS data=nhanes.bmx_all;
	VAR  BMXHT BMXBMI;
RUN;

%import(file=RHQ);
%import(file=RHQ_B);
%import(file=RHQ_C);
%import(file=RHQ_D);
%import(file=RHQ_E);
%import(file=RHQ_F);
%import(file=RHQ_G);
%import(file=RHQ_H);
%import(file=RHQ_I);
%import(file=RHQ_J);


DATA RHQ_A; SET nhanes.RHQ; year="99-00"; RUN;
DATA RHQ_B; SET nhanes.RHQ_B; year="01-02"; RUN;
DATA RHQ_C; SET nhanes.RHQ_C; year="03-04"; RUN;
DATA RHQ_D; SET nhanes.RHQ_D; year="05-06"; RUN;
DATA RHQ_E; SET nhanes.RHQ_E; year="07-08"; RUN;
DATA RHQ_F; SET nhanes.RHQ_F; year="09-10"; RUN;
DATA RHQ_G; SET nhanes.RHQ_G; year="11-12"; RUN;
DATA RHQ_H; SET nhanes.RHQ_H; year="13-14"; RUN;
DATA RHQ_I; SET nhanes.RHQ_I; year="15-16"; RUN;
DATA RHQ_J; SET nhanes.RHQ_J; year="17-18"; RUN;

DATA nhanes.rhq_all;
	SET rhq_a rhq_b rhq_c rhq_d rhq_e rhq_f rhq_g rhq_h rhq_i rhq_j;
	*age at menarche;
	IF 0<RHQ010<30 THEN menarcheage=RHQ010;
	IF RHQ010=999 AND RHQ020=1 THEN menarcheage=9;
		ELSE IF RHQ010=999 AND RHQ020=2 THEN menarcheage=11;
		ELSE IF RHQ010=999 AND RHQ020=3 THEN menarcheage=14;
		ELSE IF RHQ010=999 AND RHQ020=4 THEN menarcheage=16;
	*parity;
	IF year in ("99-00", "01-02") AND RHD130=2 THEN parity=0;
	IF year in ("99-00", "01-02") AND RHD130=1 AND RHD170=0 THEN parity=0;
	IF year in ("99-00", "01-02") AND RHD130=1 AND 0<RHD170<77 AND 0<=RHD270<77 THEN parity=RHD170-RHD270;
	IF year in ("99-00", "01-02") AND RHD130=1 AND 0<RHD170<77 AND RHD270<0 THEN parity=RHD170;
	*currently pregnant w/ first pregnancy;
	IF year in ("99-00") AND RHD130=1 AND RHQ140=1 AND RHQ160=1 THEN parity=0;
	IF year in ("01-02") AND RHD130=1 AND RHQ141=1 AND RHQ160=1 THEN parity=0;
	*some variable names change in 2003;
	IF year in ("03-04", "05-06","07-08","09-10","11-12","13-14","15-16","17-18","19-20")
		AND RHQ131=2 THEN parity=0;
	IF year in ("03-04") AND RHQ131=1 AND RHD170=0 THEN parity=0;
	IF year in ("03-04") AND RHQ131=1 AND 0<RHD170<77 AND 0<=RHD270<77 THEN parity=RHD170-RHD270;
	IF year in ("03-04") AND RHQ131=1 AND 0<RHD170<77 AND RHD270<0 THEN parity=RHD170;
	IF year in ("03-04","05-06","07-08","09-10","11-12","13-14","15-16","17-18","19-20") 
			AND RHQ131=1 AND RHD143=1 AND RHQ160=1 THEN parity=0;
	*more changes in 2005;
	IF year in ("05-06") AND RHQ131=1 AND RHQ171=0 THEN parity=0;
	IF year in ("05-06") AND RHQ131=1 AND 0<RHQ171<77 AND 0<=RHD270<77 THEN parity=RHQ171-RHD270;
	IF year in ("05-06") AND RHQ131=1 AND 0<RHQ171<77 AND RHD270<0 THEN parity=RHQ171;
	*no preterm count starting 2007;
	IF year in ("07-08","09-10","11-12","13-14","15-16","17-18","19-20") 
		AND RHQ131=1 AND RHQ171=0 THEN parity=0;
	IF year in ("07-08","09-10","11-12","13-14","15-16","17-18","19-20") 
		AND RHQ131=1 AND 0<RHQ171<77 THEN parity=RHQ171;
	*missing y/n pregnancy but answered parity;
	IF year in ("07-08","09-10","11-12","13-14","15-16","17-18","19-20") 
		AND RHQ131<0 AND 0<RHQ171<77 THEN parity=RHQ171;
	*note, max for 19-20 was 5 births;

	*age at first birth;
	IF parity>0 AND 0<RHQ180<70 THEN agefirstbirth=RHQ180;
		ELSE IF parity=0 THEN agefirstbirth=.N;
	IF agefirstbirth<0 AND 0<RHD180<70 THEN agefirstbirth=RHD180;
	IF agefirstbirth<0 AND parity=1 THEN agefirstbirth=RHD190; *age at last birth (use if parity=1);

	*breastfeeding;
	IF parity=0 THEN breastfedyn=0;
		ELSE IF parity>0 AND RHQ210=1 THEN breastfedyn=1;
		ELSE IF parity>0 AND RHQ210=2 THEN breastfedyn=0;
	IF breastfedyn<0 AND RHQ205=1 THEN breastfedyn=1;
	IF breastfedyn<0 AND RHQ205=2 THEN breastfedyn=0;
	*not captured 2013 forward;

	*ooph, hyst and menopausal status do each questionnaire separately;
	IF year in ("99-00","01-02") THEN DO;
		*hyst;
		IF RHD280=1 THEN hyst=1;
			ELSE IF RHD280=2 THEN hyst=0;
		*ooph;
		IF RHQ300=1 AND RHQ310=1 THEN bilat_ooph=1;
			ELSE IF RHQ300=1 AND RHQ310 in (2,7,9) THEN bilat_ooph=0;
			ELSE IF RHQ300=2 THEN bilat_ooph=0;
		*menopause;
		IF RHQ030=1 THEN menopause=0;
			ELSE IF RHQ030=2 AND RHQ040 in (1,2,3,4) THEN menopause=0;
			ELSE IF RHQ030=2 AND RHQ040 in (5,6,77,99,.) AND RHQ050=6 THEN menopause=0;
		IF RHQ040=5 THEN menopause=1;
		IF bilat_ooph=1 THEN menopause=1;
		*use looser definitions only if missing all prior data;
		IF menopause<0 AND RHQ050=6 THEN menopause=1;
			ELSE IF menopause<0 AND RHQ050 in (1,2,3,4,5) THEN menopause=0;
		IF menopause<0 AND hyst=1 AND 51<=RHQ290<100 THEN menopause=1;
		IF menopause<0 AND 0<RHD080<365 THEN menopause=0;
		*if pregnant/bf now;
		IF RHQ140=1 THEN menopause=0; *99-00;
		IF RHQ141=1 THEN menopause=0; *01-02;
		IF RHQ200=1 THEN menopause=0;
		*age at menopause - minimum of age at last period, age at oophorectomy;
		IF menopause=1 AND 0<RHQ060<100 THEN menoage=RHQ060;
		IF menopause=1 AND bilat_ooph=1 AND 0<RHQ340<RHQ060 THEN menoage=RHQ340;
	END;

	IF year in ("03-04", "05-06") THEN DO;
		*hyst;
		IF RHD280=1 THEN hyst=1;
			ELSE IF RHD280=2 THEN hyst=0;
		*ooph;
		IF RHQ300=1 AND RHQ310=1 THEN bilat_ooph=1;
			ELSE IF RHQ300=1 AND RHQ310 in (2,7,9) THEN bilat_ooph=0;
			ELSE IF RHQ300=2 THEN bilat_ooph=0;
		*menopause;
		IF RHQ031=1 THEN menopause=0;
			ELSE IF RHQ031=2 AND RHD042 in (1,2) THEN menopause=0;
		IF RHQ031=1 IF RHD042=7 THEN menopause=1;
		IF bilat_ooph=1 THEN menopause=1;
		*use looser definitions only if missing all prior data;
		IF menopause<0 AND RHQ051 in (1,2,3,4,5) THEN menopause=0;
		IF menopause<0 AND hyst=1 AND 51<=RHQ291<100 THEN menopause=1;
		IF menopause<0 AND 0<RHD080<365 THEN menopause=0;
		*if pregnant/bf now;
		IF RHD143=1 THEN menopause=0; 
		IF RHQ200=1 THEN menopause=0;
		*age at menopause - minimum of age at last period, age at oophorectomy;
			*if still missing and had hysterectomy then impute 51;
		IF menopause=1 AND 0<RHQ060<100 THEN menoage=RHQ060;
		IF menopause=1 AND bilat_ooph=1 AND 0<RHQ340<RHQ060 THEN menoage=RHQ340;
	END;

	IF year in ("07-08","09-10","11-12") THEN DO;
		*hyst;
		IF RHD280=1 THEN hyst=1;
			ELSE IF RHD280=2 THEN hyst=0;
		*ooph;
		IF RHQ305=1 THEN bilat_ooph=1;
			ELSE IF RHQ305=2 THEN bilat_ooph=0;
		*menopause;
		IF RHQ031=1 THEN menopause=0;
			ELSE IF RHQ031=2 AND RHD042 in (1,2) THEN menopause=0;
			ELSE IF RHQ031=2 AND RHD042=7 THEN menopause=1;
		IF RHQ031=1 AND RHD042=7 THEN menopause=1;
		IF bilat_ooph=1 THEN menopause=1;
		IF menopause<0 AND hyst=1 AND 51<=RHQ291<100 THEN menopause=1;
		*if pregnant/bf now;
		IF RHD143=1 THEN menopause=0; 
		IF RHQ200=1 THEN menopause=0;
		*age at menopause - minimum of age at last period, age at oophorectomy;
			*if still missing and had hysterectomy then impute 51;
		IF menopause=1 AND 0<RHQ060<100 THEN menoage=RHQ060;
		IF menopause=1 AND bilat_ooph=1 AND 0<RHQ332<RHQ060 THEN menoage=RHQ332;
	END;

	IF year in ("13-14","15-16","17-18") THEN DO;
		*hyst;
		IF RHD280=1 THEN hyst=1;
			ELSE IF RHD280=2 THEN hyst=0;
		IF RHQ282=1 THEN hyst=1;
			ELSE IF RHQ282=2 THEN hyst=0; *2017;
		*ooph;
		IF RHQ305=1 THEN bilat_ooph=1;
			ELSE IF RHQ305=2 THEN bilat_ooph=0;
		*menopause;
		IF RHQ031=1 THEN menopause=0;
			ELSE IF RHQ031=2 AND RHD043 in (1,2) THEN menopause=0;
			ELSE IF RHQ031=2 AND RHD043=7 THEN menopause=1;
		IF RHQ031=1 AND RHQ043=7 THEN menopause=1;
		IF bilat_ooph=1 THEN menopause=1;
		IF menopause<=0 AND hyst=1 AND 51<=RHQ291<100 THEN menopause=1;
		*if pregnant/bf now;
		IF RHD143=1 THEN menopause=0; 
		IF RHQ143=1 THEN menopause=0;
		IF RHQ200=1 THEN menopause=0;
		*age at menopause - minimum of age at last period, age at oophorectomy;
			*if still missing and had hysterectomy then impute 51;
		IF menopause=1 AND 0<RHQ060<100 THEN menoage=RHQ060;
		IF menopause=1 AND bilat_ooph=1 AND 0<RHQ332<RHQ060 THEN menoage=RHQ332;
	END;

	*hormone therapy;
	IF RHQ540=2 then HRTuse=0;
		ELSE IF RHQ540=1 AND (RHQ570=1 | RHQ596=1) THEN HRTuse=1; *E + P;
		ELSE IF RHQ540=1 AND (RHQ554=1 | RHQ580=1) THEN HRTuse=2; *E only;
		ELSE IF RHQ540=1 AND RHQ562=1 THEN HRTuse=0;

	*overall ever never birth control;
	IF RHQ420=2 THEN OCuse=0;
		ELSE IF RHQ420=1 THEN OCuse=1;

	*ever pregnant;
	IF RHQ131=1 THEN everpreg=1;
		ELSE IF RHQ131=2 THEN everpreg=0;
	IF RHD130=1 THEN everpreg=1;
		ELSE IF RHD130=2 THEN everpreg=0;

	KEEP SEQN year menarcheage parity agefirstbirth breastfedyn menopause menoage
		bilat_ooph hyst HRTuse OCuse everpreg RHQ131 RHD143 RHQ160 RHQ171 RHQ180 RHD180 RHD190
		RHQ300 RHQ310 RHQ030 RHQ040 RHQ050 RHQ290 RHQ140 RHQ200 RHQ060 RHQ340
		RHQ031 RHD042 RHQ051 RHQ291 RHD080 RHD143 RHQ200 RHQ060 RHQ340 RHQ305 
		RHQ332 RHQ031 RHD043 RHD143 RHQ200;
RUN;

PROC MEANS data=nhanes.rhq_all n nmiss mean std min max;
	CLASS year;
	VAR menarcheage parity agefirstbirth;
RUN;
PROC FREQ data=nhanes.rhq_all;
	TABLES year*breastfedyn year*menopause year*bilat_ooph year*hyst
	year*HRTuse year*OCuse year*everpreg/ norow nocol nocum nopercent missing;
RUN;

%import(file=ALQ);
%import(file=ALQ_B);
%import(file=ALQ_C);
%import(file=ALQ_D);
%import(file=ALQ_E);
%import(file=ALQ_F);
%import(file=ALQ_G);
%import(file=ALQ_H);
%import(file=ALQ_I);
%import(file=ALQ_J);

DATA ALQ_A; SET nhanes.ALQ; year="99-00"; RUN;
DATA ALQ_B; SET nhanes.ALQ_B; year="01-02"; RUN;
DATA ALQ_C; SET nhanes.ALQ_C; year="03-04"; RUN;
DATA ALQ_D; SET nhanes.ALQ_D; year="05-06"; RUN;
DATA ALQ_E; SET nhanes.ALQ_E; year="07-08"; RUN;
DATA ALQ_F; SET nhanes.ALQ_F; year="09-10"; RUN;
DATA ALQ_G; SET nhanes.ALQ_G; year="11-12"; RUN;
DATA ALQ_H; SET nhanes.ALQ_H; year="13-14"; RUN;
DATA ALQ_I; SET nhanes.ALQ_I; year="15-16"; RUN;
DATA ALQ_J; SET nhanes.ALQ_J; year="17-18"; RUN;

DATA nhanes.alq_all;
	SET alq_a alq_b alq_c alq_d alq_e alq_f alq_g alq_h alq_i alq_j;
	IF ALQ100=2 THEN days_drunk=0;
		ELSE IF ALQ100=1 AND ALQ120U=1 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q*52;
		ELSE IF ALQ100=1 AND ALQ120U=2 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q*12;
		ELSE IF ALQ100=1 AND ALQ120U=3 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q;
	IF ALD100=2 THEN days_drunk=0;
		ELSE IF ALD100=1 AND ALQ120U=1 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q*52;
		ELSE IF ALD100=1 AND ALQ120U=2 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q*12;
		ELSE IF ALD100=1 AND ALQ120U=3 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q;
	IF ALQ101=2 THEN days_drunk=0;
		ELSE IF ALQ101=1 AND ALQ120U=1 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q*52;
		ELSE IF ALQ101=1 AND ALQ120U=2 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q*12;
		ELSE IF ALQ101=1 AND ALQ120U=3 AND 0<=ALQ120Q<400 THEN days_drunk=ALQ120Q;

	IF ALQ121=0 THEN days_drunk=0;
		ELSE IF ALQ121=1 THEN days_drunk=365;
		ELSE IF ALQ121=2 THEN days_drunk=286; *5.5 days per week;
		ELSE IF ALQ121=3 THEN days_drunk=182; *3.5 days per week;
		ELSE IF ALQ121=4 THEN days_drunk=104; *2 days per week;
		ELSE IF ALQ121=5 THEN days_drunk=52; *1 day per week;
		ELSE IF ALQ121=6 THEN days_drunk=30; *2.5 times per month;
		ELSE IF ALQ121=7 THEN days_drunk=12; *1 time per month;
		ELSE IF ALQ121=8 THEN days_drunk=9; *7-11 times per year;
		ELSE IF ALQ121=9 THEN days_drunk=4.5; *3-6 times per year;
		ELSE IF ALQ121=10 THEN days_drunk=1.5; *1-2 times per year;

	IF 0<=ALQ130<30 THEN drinks_yr=days_drunk*ALQ130;
		IF ALQ100=2 THEN drinks_yr=0;
		IF ALD100=2 THEN drinks_yr=0;
		IF ALQ101=2 THEN drinks_yr=0;
		IF ALQ130<0 AND days_drunk=0 THEN drinks_yr=0;
	grams_day=(drinks_yr*14)/365;
		IF ALQ100=2 THEN grams_day=0;
		IF ALD100=2 THEN grams_day=0;
		IF ALQ101=2 THEN grams_day=0;

	*lifetime ever/never alcohol use variable for imputation;
	IF ALQ110=1 THEN everdranklife=1;
		ELSE IF ALQ110=2 THEN everdranklife=0;
	IF ALQ111=1 THEN everdranklife=1;
		ELSE IF ALQ111=2 THEN everdranklife=0;

	IF grams_day<0 AND everdranklife=0 THEN grams_day=0;

	*also binge drink indicator;
	IF ALQ140Q=0 THEN bingedrink=0;
		ELSE IF 0<ALQ140Q<=5 THEN bingedrink=1;
		ELSE IF 5<ALQ140Q<=365 THEN bingedrink=2;
	IF ALQ141Q=0 THEN bingedrink=0;
		ELSE IF 0<ALQ141Q<=5 THEN bingedrink=1;
		ELSE IF 5<ALQ141Q<=365 THEN bingedrink=2;
	IF ALQ142=0 THEN bingedrink=0;
		ELSE IF ALQ142 in (9,10) THEN bingedrink=1;
		ELSE IF ALQ142 in (1,2,3,4,5,6,7,8) THEN bingedrink=2;

	KEEP SEQN year days_drunk drinks_yR grams_day everdranklife bingedrink ALQ121 ALQ130 ALQ111 ALQ110 ALQ100 ALQ101;
RUN;

PROC MEANS data=nhanes.alq_all n nmiss mean min max;
	CLASS year;
	VAR days_drunk drinks_yR grams_day;
RUN;

PROC FREQ data=nhanes.alq_all;
	TABLES year*everdranklife year*bingedrink / nocol norow nocum nopercent missing;
RUN;

PROC SORT data=nhanes.demo_all;
	BY SEQN;
RUN;
PROC SORT data=nhanes.bmx_all;
	BY SEQN;
RUN;
PROC SORT data=nhanes.rhq_all;
	BY SEQN;
RUN;
PROC SORT data=nhanes.alq_all;
	BY SEQN;
RUN;

DATA nhanes.data;
	MERGE nhanes.demo_all nhanes.bmx_all nhanes.rhq_all nhanes.alq_all;
	BY SEQN;
RUN;

*get rid of children and men;
DATA nhanes.data;
	SET nhanes.data;
	WHERE RIAGENDR=2;
	IF RIDAGEYR<20 THEN DELETE;
RUN;

*now format into same versions of varaibles as in CARRIERS;
DATA nhanes.data;
	SET nhanes.data;

	height10=round(BMXHT/10,0.5);

	IF 0<menarcheage<11 THEN menarche_cat=0;
		ELSE IF 11<=menarcheage<13 THEN menarche_cat=1;
		ELSE IF 13<=menarcheage<14 THEN menarche_cat=2;
		ELSE IF 14<=menarcheage<16 THEN menarche_cat=3;
		ELSE IF 16<=menarcheage<100 THEN menarche_cat=4;

	*if a woman has a hysterectomy and is older than 51, consider her postmenopausal;
	IF menopause<=0 AND hyst=1 AND 51<=RIDAGEYR<100 THEN menopause=1;
	IF menopause=1 AND hyst=1 AND menoage<0 THEN menoage=51;

	*if menopausal but had period in last year than age at menopause is same as survey year;
	IF menopause=1 AND menoage<0 AND RHQ030=1 THEN menoage=RIDAGEYR;
	IF menopause=1 AND menoage<0 AND RHQ050 in (2,3) THEN menoage=RIDAGEYR;
	IF menopause=1 AND menoage<0 AND RHQ050 in (4,5,6) THEN menoage=RIDAGEYR-1;
	IF menopause=1 AND menoage<0 AND RHQ040=5 THEN menoage=RIDAGEYR;
	IF menopause=1 AND menoage<0 AND RHD042=7 THEN menoage=RIDAGEYR;
	IF Menopause=1 AND menoage<0 AND RHQ043=7 THEN menoage=RIDAGEYR;

	*age at menopause;
	IF menopause=1 AND 0<menoage<45 THEN agemeno_cat=0;
		ELSE IF menopause=1 AND 45<=menoage<50 THEN agemeno_cat=1;
		ELSE IF menopause=1 AND 50<=menoage<55 THEN agemeno_cat=2;
		ELSE IF menopause=1 AND 55<=menoage<100 THEN agemeno_cat=3;
		ELSE IF menopause=0 THEN agemeno_cat=0; *filler for premenopausal women;
	
	*parity;
	parity_cat=parity;
	IF 3<=parity<40 THEN parity_cat=3;

	IF parity=0 THEN parous=0;
		ELSE IF 0<parity<40 THEN parous=1;

	IF 5<=agefirstbirth<20 THEN agepreg_cat=0;
		ELSE IF 20<=agefirstbirth<25 THEN agepreg_cat=1;
		ELSE IF 25<=agefirstbirth<30 THEN agepreg_cat=2;
		ELSE IF 30<=agefirstbirth<100 THEN agepreg_cat=3;
		ELSE IF agefirstbirth>100 THEN agepreg_cat=.;
	*filler for non-parous;
	IF parous=0 THEN agepreg_cat=0;

	*BMI;
	IF 0<BMXBMI<25 THEN BMI_cat=0;
		ELSE IF 25<=BMXBMI<30 THEN BMI_cat=1;
		ELSE IF BMXBMI>=30 THEN BMI_cat=2;

	*alcohol;
	IF grams_day=0 THEN alcoholgday_cat=0;
		ELSE IF 0<grams_day<5 THEN alcoholgday_cat=1;
		ELSE IF 5<=grams_day<15 THEN alcoholgday_cat=2;
		ELSE IF grams_day>=15 THEN alcoholgday_cat=3;
	*if otherwise missing, but binge drinkers in top category;
	IF grams_day<0 AND bingedrink>0 THEN alcoholgday_cat=3;

	*recode HRT to 0 if premenopausal;
	IF menopause=0 AND HRTuse<0 THEN HRTuse=0;

	IF menopause=0 THEN agemeno_cat=0;
RUN;

PROC MEANS data=nhanes.data median p25 p75 n nmiss;
	VAR RIDAGEYR height10;
	WEIGHT WT;
RUN;
PROC FREQ data=nhanes.data;
	TABLES RACEETH menarche_cat menopause menopause*agemeno_cat parity_cat
	parous*agepreg_cat BMI_cat HRTuse*bmi_cat alcoholgday_cat OCuse;
RUN;
PROC FREQ data=nhanes.data;
	TABLES RACEETH menarche_cat menopause menopause*agemeno_cat parity_cat
	parous*agepreg_cat BMI_cat HRTuse*bmi_cat alcoholgday_cat OCuse / missing;
RUN;
PROC FREQ data=nhanes.data;
	TABLES RACEETH menarche_cat menopause menopause*agemeno_cat parity_cat
	parous*agepreg_cat BMI_cat HRTuse*bmi_cat alcoholgday_cat OCuse;
	WEIGHT WT;
RUN;

*missing by year to look for errors;
PROC FREQ data=nhanes.data;
	TABLES year*BMI_cat year*HRTuse year*OCuse year*RACEETH year*agemeno_cat year*agepreg_cat 
	year*alcoholgday_cat year*height10 year*menarche_cat year*menopause 
	year*parity_cat / nocol nocum nopercent missing;
RUN;

*imputation model;
*run imputation model for fibroid age category status and all covars - will impute those as well;
PROC MI data=nhanes.data out=nhanes.dataimpute nimpute=5 seed=0927;
	CLASS BMI_cat HRTuse IPR RACEETH agepreg_cat alcoholgday_cat OCuse height10
	educ marital menarche_cat parity_cat year menopause breastfedyn agemeno_cat;
	FCS discrim (height10 / classeffects=include);
	FCS discrim (IPR / classeffects=include);
	FCS discrim (educ / classeffects=include);
	FCS discrim (marital / classeffects=include);
	FCS discrim (BMI_cat / classeffects=include);
	FCS discrim (menopause / classeffects=include);
	FCS discrim (menarche_cat / classeffects=include);
	FCS discrim (parity_cat / classeffects=include);
	FCS discrim (HRTuse / classeffects=include);
	FCS discrim (alcoholgday_cat / classeffects=include);
	FCS discrim (agepreg_cat / classeffects=include);
	FCS discrim (OCuse / classeffects=include);
	FCS discrim (agemeno_cat / classeffects=include);

	VAR year RACEETH RIDAGEYR IPR bilat_ooph bingedrink
	days_drunk drinks_yr educ everdranklife everpreg grams_day hyst marital 
	height10 BMI_cat menopause menarche_cat 
	parity_cat HRTuse alcoholgday_cat agepreg_cat agemeno_cat breastfedyn OCuse;
RUN;

*make related variables consistent with one another;
DATA nhanes.dataimpute;
	SET nhanes.dataimpute;
	IF agemeno_cat=3 THEN menopause=1;
		ELSE IF agemeno_cat=2 THEN menopause=1;
		ELSE IF agemeno_cat=1 THEN menopause=1;
	IF agepreg_cat>0 AND parity_cat=0 THEN parity_cat=1;
	IF menopause=0 THEN HRTuse=0;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;

RUN;

PROC MEANS data=nhanes.dataimpute median p25 p75 min max mean;
	VAR RIDAGEYR height10;
	WEIGHT wt;
RUN;

*counts;
PROC FREQ data=nhanes.dataimpute;
	TABLES RACEETH menarche_cat menopause menopause*agemeno_cat parity_cat
	parous*agepreg_cat menopause*BMI_cat menopause*HRTuse*bmi_cat alcoholgday_cat parous*breastfedyn OCuse;
RUN;
*proportions only;
PROC FREQ data=nhanes.dataimpute;
	TABLES RACEETH menarche_cat menopause menopause*agemeno_cat parity_cat
	parous*agepreg_cat menopause*BMI_cat menopause*HRTuse*bmi_cat alcoholgday_cat OCuse;
	WEIGHT wt;
RUN;

*add in family history information by age group and race/ethnicity (from NHIS);
DATA nhanes.dataimputeFH;
	CALL streaminit(92724);
	SET nhanes.dataimpute;
	IF 18<=RIDAGEYR<25 THEN agegroup=0;
		ELSE IF 25<=RIDAGEYR<30 THEN agegroup=1;
		ELSE IF 30<=RIDAGEYR<35 THEN agegroup=2;
		ELSE IF 35<=RIDAGEYR<40 THEN agegroup=3;
		ELSE IF 40<=RIDAGEYR<45 THEN agegroup=4;
		ELSE IF 45<=RIDAGEYR<50 THEN agegroup=5;
		ELSE IF 50<=RIDAGEYR<55 THEN agegroup=6;
		ELSE IF 55<=RIDAGEYR<60 THEN agegroup=7;
		ELSE IF 60<=RIDAGEYR<65 THEN agegroup=8;
		ELSE IF 65<=RIDAGEYR<70 THEN agegroup=9;
		ELSE IF 70<=RIDAGEYR<75 THEN agegroup=10;
		ELSE IF 75<=RIDAGEYR<80 THEN agegroup=11;
		ELSE IF 80<=RIDAGEYR THEN agegroup=12;

	IF RIDAGEYR<50 THEN agegroup50=0;
		ELSE IF RIDAGEYR>=50 THEN agegroup50=1;

FHprob=rand("Uniform");

*Hispanic women;
IF RACEETH=2 AND agegroup=0 AND FHprob<=0.0093 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=0 THEN famhist=0;
IF RACEETH=2 AND agegroup=1 AND FHprob<=0.0040 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=1 THEN famhist=0;
IF RACEETH=2 AND agegroup=2 AND FHprob<=0.0361 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=2 THEN famhist=0;
IF RACEETH=2 AND agegroup=3 AND FHprob<=0.0151 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=3 THEN famhist=0;
IF RACEETH=2 AND agegroup=4 AND FHprob<=0.0330 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=4 THEN famhist=0;
IF RACEETH=2 AND agegroup=5 AND FHprob<=0.0749 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=5 THEN famhist=0;
IF RACEETH=2 AND agegroup=6 AND FHprob<=0.0375 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=6 THEN famhist=0;
IF RACEETH=2 AND agegroup=7 AND FHprob<=0.0881 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=7 THEN famhist=0;
IF RACEETH=2 AND agegroup=8 AND FHprob<=0.0665 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=8 THEN famhist=0;
IF RACEETH=2 AND agegroup=9 AND FHprob<=0.1067 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=9 THEN famhist=0;
IF RACEETH=2 AND agegroup=10 AND FHprob<=0.0825 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=10 THEN famhist=0;
IF RACEETH=2 AND agegroup=11 AND FHprob<=0.1569 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=11 THEN famhist=0;
IF RACEETH=2 AND agegroup=12 AND FHprob<=0.0128 THEN famhist=1;
	ELSE IF RACEETH=2 AND agegroup=12 THEN famhist=0;
*NHB women;
IF RACEETH=1 AND agegroup=0 AND FHprob<=0.0232 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=0 THEN famhist=0;
IF RACEETH=1 AND agegroup=1 AND FHprob<=0.0151 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=1 THEN famhist=0;
IF RACEETH=1 AND agegroup=2 AND FHprob<=0.0244 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=2 THEN famhist=0;
IF RACEETH=1 AND agegroup=3 AND FHprob<=0.1175 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=3 THEN famhist=0;
IF RACEETH=1 AND agegroup=4 AND FHprob<=0.0552 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=4 THEN famhist=0;
IF RACEETH=1 AND agegroup=5 AND FHprob<=0.0710 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=5 THEN famhist=0;
IF RACEETH=1 AND agegroup=6 AND FHprob<=0.1240 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=6 THEN famhist=0;
IF RACEETH=1 AND agegroup=7 AND FHprob<=0.1076 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=7 THEN famhist=0;
IF RACEETH=1 AND agegroup=8 AND FHprob<=0.1124 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=8 THEN famhist=0;
IF RACEETH=1 AND agegroup=9 AND FHprob<=0.0880 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=9 THEN famhist=0;
IF RACEETH=1 AND agegroup=10 AND FHprob<=0.0913 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=10 THEN famhist=0;
IF RACEETH=1 AND agegroup=11 AND FHprob<=0.1086 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=11 THEN famhist=0;
IF RACEETH=1 AND agegroup=12 AND FHprob<=0.0516 THEN famhist=1;
	ELSE IF RACEETH=1 AND agegroup=12 THEN famhist=0;
*NHW women;
IF RACEETH=0 AND agegroup=0 AND FHprob<=0.0319 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=0 THEN famhist=0;
IF RACEETH=0 AND agegroup=1 AND FHprob<=0.0326 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=1 THEN famhist=0;
IF RACEETH=0 AND agegroup=2 AND FHprob<=0.0507 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=2 THEN famhist=0;
IF RACEETH=0 AND agegroup=3 AND FHprob<=0.0593 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=3 THEN famhist=0;
IF RACEETH=0 AND agegroup=4 AND FHprob<=0.0767 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=4 THEN famhist=0;
IF RACEETH=0 AND agegroup=5 AND FHprob<=0.0944 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=5 THEN famhist=0;
IF RACEETH=0 AND agegroup=6 AND FHprob<=0.1113 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=6 THEN famhist=0;
IF RACEETH=0 AND agegroup=7 AND FHprob<=0.1071 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=7 THEN famhist=0;
IF RACEETH=0 AND agegroup=8 AND FHprob<=0.1210 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=8 THEN famhist=0;
IF RACEETH=0 AND agegroup=9 AND FHprob<=0.1315 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=9 THEN famhist=0;
IF RACEETH=0 AND agegroup=10 AND FHprob<=0.1553 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=10 THEN famhist=0;
IF RACEETH=0 AND agegroup=11 AND FHprob<=0.1390 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=11 THEN famhist=0;
IF RACEETH=0 AND agegroup=12 AND FHprob<=0.1383 THEN famhist=1;
	ELSE IF RACEETH=0 AND agegroup=12 THEN famhist=0;

*Asian women;
IF RACEETH=3 AND agegroup=0 AND FHprob<=0.00 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=0 THEN famhist=0;
IF RACEETH=3 AND agegroup=1 AND FHprob<=0.00 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=1 THEN famhist=0;
IF RACEETH=3 AND agegroup=2 AND FHprob<=0.00 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=2 THEN famhist=0;
IF RACEETH=3 AND agegroup=3 AND FHprob<=0.0137 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=3 THEN famhist=0;
IF RACEETH=3 AND agegroup=4 AND FHprob<=0.0460 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=4 THEN famhist=0;
IF RACEETH=3 AND agegroup=5 AND FHprob<=0.1297 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=5 THEN famhist=0;
IF RACEETH=3 AND agegroup=6 AND FHprob<=0.0578 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=6 THEN famhist=0;
IF RACEETH=3 AND agegroup=7 AND FHprob<=0.0448 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=7 THEN famhist=0;
IF RACEETH=3 AND agegroup=8 AND FHprob<=0.000 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=8 THEN famhist=0;
IF RACEETH=3 AND agegroup=9 AND FHprob<=0.000 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=9 THEN famhist=0;
IF RACEETH=3 AND agegroup=10 AND FHprob<=0.00 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=10 THEN famhist=0;
IF RACEETH=3 AND agegroup=11 AND FHprob<=0.0771 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=11 THEN famhist=0;
IF RACEETH=3 AND agegroup=12 AND FHprob<=0.1040 THEN famhist=1;
	ELSE IF RACEETH=3 AND agegroup=12 THEN famhist=0;

*other race/eth women;
IF RACEETH=4 AND agegroup=0 AND FHprob<=0.00 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=0 THEN famhist=0;
IF RACEETH=4 AND agegroup=1 AND FHprob<=0.00 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=1 THEN famhist=0;
IF RACEETH=4 AND agegroup=2 AND FHprob<=0.00 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=2 THEN famhist=0;
IF RACEETH=4 AND agegroup=3 AND FHprob<=0.000 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=3 THEN famhist=0;
IF RACEETH=4 AND agegroup=4 AND FHprob<=0.000 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=4 THEN famhist=0;
IF RACEETH=4 AND agegroup=5 AND FHprob<=0.0857 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=5 THEN famhist=0;
IF RACEETH=4 AND agegroup=6 AND FHprob<=0.000 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=6 THEN famhist=0;
IF RACEETH=4 AND agegroup=7 AND FHprob<=0.1037 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=7 THEN famhist=0;
IF RACEETH=4 AND agegroup=8 AND FHprob<=0.2288 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=8 THEN famhist=0;
IF RACEETH=4 AND agegroup=9 AND FHprob<=0.000 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=9 THEN famhist=0;
IF RACEETH=4 AND agegroup=10 AND FHprob<=0.00 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=10 THEN famhist=0;
IF RACEETH=4 AND agegroup=11 AND FHprob<=0.000 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=11 THEN famhist=0;
IF RACEETH=4 AND agegroup=12 AND FHprob<=0.000 THEN famhist=1;
	ELSE IF RACEETH=4 AND agegroup=12 THEN famhist=0;
RUN;

PROC FREQ Data=nhanes.dataimputeFH;
	TABLES RACEETH*agegroup*famhist famhist*agegroup50;
RUN;

*now re-structure to match the format of the variable listed needed for R package;
DATA nhanes.outdata_u50_icare;
	RETAIN famhist height menarche parity afb bmi alcohol OCuse;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50;
	height=height10;
	menarche=menarche_cat+1;
	parity=parity_cat+1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
	alcohol=alcoholgday_cat+1;
	OCuse=OCuse;
	KEEP famhist height menarche parity afb bmi alcohol OCuse ;
RUN;
PROC EXPORT DATA= nhanes.outdata_u50_icare 
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_u50_icare.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA nhanes.outdata_50p_icare;
	RETAIN famhist height menarche parity afb bmi alcohol OCuse HRT agemeno _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50;
	height=height10;
	menarche=menarche_cat+1;
	parity=parity_cat+1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
	alcohol=alcoholgday_cat+1;
	OCuse=OCuse;
	HRT=HRTuse+1;
	agemeno=agemeno_cat+1;
	impute=_imputation_;
	KEEP famhist height menarche parity afb bmi alcohol OCuse HRT agemeno impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_50p_icare 
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_50p_icare.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA nhanes.outdata_u50_carr;
	RETAIN famhist height raceeth menarche parity afb bmi alcohol OCuse _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR_imp<50;
	height=height10_imp;
	raceeth=raceeth_imp+1;
	menarche=menarche_cat_imp+1;
	parity=parity_cat_imp+1;
	afb=agepreg_cat_imp+1;
	bmi=BMI_cat_imp+1;
	alcohol=alcoholgday_cat_imp+1;
	OCuse=OCuse_imp;
	impute=_imputation_;
	KEEP famhist height raceeth menarche parity afb bmi alcohol OCuse impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_u50_carr 
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_u50_carr.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA nhanes.outdata_50p_carr;
	RETAIN famhist height raceeth menarche parity afb bmi alcohol OCuse HRT agemeno _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50;
	height=height10;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	parity=parity_cat+1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
	alcohol=alcoholgday_cat+1;
	OCuse=OCuse;
	HRT=HRTuse+1;
	agemeno=agemeno_cat+1;
	impute=_imputation_;
	KEEP famhist height raceeth menarche parity afb bmi alcohol OCuse HRT agemeno impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_50p_carr
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_50p_carr.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA nhanes.outdata_u50_hybrid;
	RETAIN height raceeth menarche parous afb bmi highalc _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50;
	height=height10;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
	IF 0<=alcoholgday_cat<=2 THEN highalc=0;
		ELSE IF alcoholgday_cat=3 THEN highalc=1;
	impute=_imputation_;
	KEEP height raceeth menarche parous afb bmi highalc impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_u50_hybrid
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_u50_hybrid.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.outdata_u50_hybrid_noFH;
	RETAIN height raceeth menarche parous afb bmi highalc _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50 AND famhist=0;
	height=height10;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
	IF 0<=alcoholgday_cat<=2 THEN highalc=0;
		ELSE IF alcoholgday_cat=3 THEN highalc=1;
	impute=_imputation_;
	KEEP height raceeth menarche parous afb bmi highalc impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_u50_hybrid_noFH
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_u50_hybrid_noFH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.outdata_u50_hybrid_FH;
	RETAIN height raceeth menarche parous afb bmi highalc _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50 AND famhist=1;
	height=height10;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
	IF 0<=alcoholgday_cat<=2 THEN highalc=0;
		ELSE IF alcoholgday_cat=3 THEN highalc=1;
	impute=_imputation_;
	KEEP height raceeth menarche parous afb bmi highalc impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_u50_hybrid_FH
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_u50_hybrid_FH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA nhanes.outdata_50p_hybrid;
	RETAIN height raceeth menarche parous afb bmi alcohol EPHRT agemeno _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50;
	height=height10;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
	IF alcoholgday_cat=0 THEN alcohol=0;
		ELSE IF alcoholgday_cat in (1,2) THEN alcohol=1;
		ELSE IF alcoholgday_cat=3 THEN alcohol=2;
	IF HRTtype=2 THEN EPHRT=1;
		ELSE EPHRT=0;
	agemeno=agemeno_cat+1;
	impute=_imputation_;
	KEEP height raceeth menarche parous afb bmi alcohol EPHRT agemeno impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_50p_hybrid
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_50p_hybrid.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.outdata_50p_hybrid_noFH;
	RETAIN height raceeth menarche parous afb bmi alcohol EPHRT agemeno _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50 AND famhist=0;
	height=height10;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
	IF alcoholgday_cat=0 THEN alcohol=0;
		ELSE IF alcoholgday_cat in (1,2) THEN alcohol=1;
		ELSE IF alcoholgday_cat=3 THEN alcohol=2;
	IF HRTtype=2 THEN EPHRT=1;
		ELSE EPHRT=0;
	agemeno=agemeno_cat+1;
	impute=_imputation_;
	KEEP height raceeth menarche parous afb bmi alcohol EPHRT agemeno impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_50p_hybrid_noFH
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_50p_hybrid_noFH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.outdata_50p_hybrid_FH;
	RETAIN height raceeth menarche parous afb bmi alcohol EPHRT agemeno _imputation_;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50 AND famhist=1;
	height=height10;
	raceeth=raceeth+1;
	menarche=menarche_cat+1;
	IF parity_cat=0 THEN parous=0;
		ELSE IF parity_cat>0 THEN parous=1;
	afb=agepreg_cat+1;
	bmi=BMI_cat+1;
IF alcoholgday_cat=0 THEN alcohol=0;
		ELSE IF alcoholgday_cat in (1,2) THEN alcohol=1;
		ELSE IF alcoholgday_cat=3 THEN alcohol=2;
	IF HRTtype=2 THEN EPHRT=1;
		ELSE EPHRT=0;
	agemeno=agemeno_cat+1;
	impute=_imputation_;
	KEEP height raceeth menarche parous afb bmi alcohol EPHRT agemeno impute;
RUN;
PROC EXPORT DATA= nhanes.outdata_50p_hybrid_FH
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\outdata_50p_hybrid_FH.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

*file2 with just weights;
DATA nhanes.weights_u50;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50 AND _Imputation_=1;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_u50 
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_u50.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.weights_50p;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50 AND _Imputation_=1;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_50p
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_50p.txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

*create 5 versions of data set for each age/FH combo;
%MACRO byimpute;
%DO p=1 % TO 5;
DATA nhanes.weights_u50_noFH;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50 AND famhist=0 AND _Imputation_=&p;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_u50_noFH 
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_u50_noFH_&p..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.weights_u50_FH;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50 AND famhist=1 AND _Imputation_=&p;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_u50_FH
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_u50_FH_&p..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA nhanes.weights_50p_noFH;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50 AND famhist=0  AND _Imputation_=&p;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_50p_noFH
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_50p_noFH_&p..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.weights_50p_FH;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50 AND famhist=1  AND _Imputation_=&p;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_50p_FH
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_50p_FH_&p..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
%END;
%MEND;

%byimpute;


*2nd version to also do by race/ethnicity;
%MACRO byimpute_race (race= ,racename= );
%DO p=1 % TO 5;
DATA nhanes.weights_u50_noFH_&racename;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50 AND famhist=0 AND raceeth=&race AND _Imputation_=&p;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_u50_noFH_&racename 
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_u50_noFH_&racename._&p..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.weights_u50_FH_&racename;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR<50 AND famhist=1 AND raceeth=&race AND _Imputation_=&p;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_u50_FH_&racename
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_u50_FH_&racename._&p..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;

DATA nhanes.weights_50p_noFH_&racename;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50 AND famhist=0 AND raceeth=&race AND _Imputation_=&p;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_50p_noFH_&racename
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_50p_noFH_&racename._&p..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
DATA nhanes.weights_50p_FH_&racename;
	SET nhanes.dataimputeFH;
	WHERE RIDAGEYR>=50 AND famhist=1 AND raceeth=&race AND _Imputation_=&p;
	KEEP wt;
RUN;
PROC EXPORT DATA= nhanes.weights_50p_FH_&racename
            OUTFILE= "\\userdata\obrienkm2\Documents\CARRIERS\OBrien project\wts_50p_FH_&racename._&p..txt" 
            DBMS=TAB REPLACE;
     PUTNAMES=YES;
RUN;
%END;
%MEND;

%byimpute_race(race=0,racename=NHW);
%byimpute_race(race=1,racename=Black);
%byimpute_race(race=2,racename=Hisp);
%byimpute_race(race=3,racename=Asian);
