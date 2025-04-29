
DATA CARRIERS.data;
	SET original.data;
RUN;

PROC FREQ data=CARRIERS.data;
	TABLES Study;
RUN;

*drop NC-BCFR = family registry study;
*original NEJM paper only included NC-BCFR, BEST, UCIBCS, Sister and Two Sister in supplement;
*also drop UCI bc missing age data;

DATA CARRIERS.data;
	SET CARRIERS.data;
	IF Study="NC-BCFR" | Study="MOFFITT" | Study="TWOSISTER" THEN DELETE;
	IF Study="UCI" THEN DELETE;
	*delete sister participants that should not have been included;
	IF Study="SISTER" AND AgeEnroll="NA" THEN DELETE;
RUN;
*lose 7831 left with 68952;

PROC FREQ data=CARRIERS.data;
	TABLES FamHist brCancerSis brCancerMom brCancerDad FHisFstBC FHisFstBCNr FHisSecBCNr
	FHisThirdBCNr FHisFstOC FHisFstOCNr FHisSecOCNr
	FHisFstPanCNr FHisSecPanCNr FHisFstColCNr FHisSecColCNr
	FHisFstEndoCNr FHisSecEndoCNr;
RUN;

PROC CONTENTS data=CARRIERS.data;
RUN;

*drop those with no family history data;
DATA CARRIERS.FHdata;
	SET CARRIERS.data;
	WHERE FamHist in ("0","1") | brCancerSis in ("0","1","2","3","4","5") | brCancerMom in ("0","1") | brCancerDad in ("0","1")
	| FHisFstBC in ("0","1") | FHisFstBCNr in ("0","1","2","3","4","5") | FHisFstOCNr in ("0","1","2","3","4") 
	| FHisFstPanCNr in ("0","1","2") | FHisFstColCNr in ("0","1","2","3","4","5") | FHisFstEndoCNr in ("0","1","2","3");
RUN;
*drop 815 left with 68137 women;

*drop those that are of unknown case status;
DATA CARRIERS.FHdata;
	SET CARRIERS.FHdata;
	IF status in (1,2,3) THEN case=1;
		ELSE IF status=0 THEN case=0;
	IF status=9 THEN DELETE;
RUN;
*no further exclusions;

PROC FREQ data=CARRIERS.FHdata;
	TABLES case;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES case*study / nocum nopercent nocol;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES FHisFstBCNr FHisFstBCNr*FHisFstBC;
RUN;

*code other covars;
DATA CARRIERS.FHdata;
	SET CARRIERS.FHdata;
	*since some studies collected limited data on family history, use crude categories of:

	*Mom or Sister with breast cancer;
	IF FHisFstBC="1" THEN FH1st=1;
		ELSE IF FHisFstBC="0" THEN FH1st=0;
		ELSE IF FHisFstBC="888" THEN FH1st=.;
	*if missing FHisFstBC but FamHist=0 THEN 0 / FamHist=1 THEN 1 (bc BC in men rare);
	IF FH1st<0 AND FamHist="0" THEN FH1st=0;
	IF FH1st<0 AND FamHist="1" THEN FH1st=1;
	*recode some women in CPS3 who are missing key variables, but have "0" for sister;
	IF study="CPS3" AND FH1st<0 AND brCancerSis="0" THEN FH1st=0;
	*recode some women who have data for individual first degree relatives;
	IF (brCancerSis="0" AND brCancerMom="0" AND brCancerDad="0") THEN FH1st=0;
		ELSE IF (brCancerSis="888" AND brCancerMom="0" AND brCancerDad="0") THEN FH1st=0;
		ELSE IF (brCancerSis="0" AND brCancerMom="888" AND brCancerDad="0") THEN FH1st=0;
		ELSE IF (brCancerSis="0" AND brCancerMom="0" AND brCancerDad="888") THEN FH1st=0;
		ELSE IF (brCancerSis="888" AND brCancerMom="888" AND brCancerDad="0") THEN FH1st=0;
		ELSE IF (brCancerSis="888" AND brCancerMom="0" AND brCancerDad="888") THEN FH1st=0;
		ELSE IF (brCancerSis="0" AND brCancerMom="888" AND brCancerDad="888") THEN FH1st=0;
	IF study="SISTER" THEN FH1st=1;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES FH1st;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES case*FH1st;
RUN;

*other cancers family history- looks like only ovarian captured;
DATA CARRIERS.FHdata;
	SET CARRIERS.FHdata;
	IF FHisFstOC="1" THEN OC_FH1st=1;
		ELSE IF FHisFstOC="0" THEN OC_FH1st=0;
		ELSE IF FHisFstOC="888" THEN OC_FH1st=.;

	IF OC_FH1st<0 AND (OvCancerDau="0" | OvCancerSis="0" | OvCancerMom="0") THEN OC_FH1st=0;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES OC_FH1st OC_FH1st*case;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES OC_FH1st*case / missing;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	WHERE OC_FH1st<0;
	TABLES study;
RUN;
*WCHS missing all ovarian data;

PROC FREQ data=CARRIERS.FHdata;
	TABLES race ethnicity raceethnicity;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES raceethnicity*case;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES raceethnicity*case / missing;
RUN;

DATA CARRIERS.FHdata;
	SET CARRIERS.FHdata;
	
	IF agediag<200 THEN age=agediag;
		ELSE IF agediag>200 THEN age=.;

	*code age differently for case-cohort (SIS, CPS3);
	IF ageenroll="NA" THEN ageenroll_num=.;
		ELSE IF ageenroll="21" THEN ageenroll_num=21;
		ELSE IF ageenroll="22" THEN ageenroll_num=22;
		ELSE IF ageenroll="23" THEN ageenroll_num=23;
		ELSE IF ageenroll="24" THEN ageenroll_num=24;
		ELSE IF ageenroll="25" THEN ageenroll_num=25;
		ELSE IF ageenroll="26" THEN ageenroll_num=26;
		ELSE IF ageenroll="27" THEN ageenroll_num=27;
		ELSE IF ageenroll="28" THEN ageenroll_num=28;
		ELSE IF ageenroll="29" THEN ageenroll_num=29;
		ELSE IF ageenroll="30" THEN ageenroll_num=30;
		ELSE IF ageenroll="31" THEN ageenroll_num=31;
		ELSE IF ageenroll="32" THEN ageenroll_num=32;
		ELSE IF ageenroll="33" THEN ageenroll_num=33;
		ELSE IF ageenroll="34" THEN ageenroll_num=34;
		ELSE IF ageenroll="35" THEN ageenroll_num=35;
		ELSE IF ageenroll="36" THEN ageenroll_num=36;
		ELSE IF ageenroll="37" THEN ageenroll_num=37;
		ELSE IF ageenroll="38" THEN ageenroll_num=38;
		ELSE IF ageenroll="39" THEN ageenroll_num=39;
		ELSE IF ageenroll="40" THEN ageenroll_num=40;
		ELSE IF ageenroll="41" THEN ageenroll_num=41;
		ELSE IF ageenroll="42" THEN ageenroll_num=42;
		ELSE IF ageenroll="43" THEN ageenroll_num=43;
		ELSE IF ageenroll="44" THEN ageenroll_num=44;
		ELSE IF ageenroll="45" THEN ageenroll_num=45;
		ELSE IF ageenroll="46" THEN ageenroll_num=46;
		ELSE IF ageenroll="47" THEN ageenroll_num=47;
		ELSE IF ageenroll="48" THEN ageenroll_num=48;
		ELSE IF ageenroll="49" THEN ageenroll_num=49;
		ELSE IF ageenroll="50" THEN ageenroll_num=50;
		ELSE IF ageenroll="51" THEN ageenroll_num=51;
		ELSE IF ageenroll="52" THEN ageenroll_num=52;
		ELSE IF ageenroll="53" THEN ageenroll_num=53;
		ELSE IF ageenroll="54" THEN ageenroll_num=54;
		ELSE IF ageenroll="55" THEN ageenroll_num=55;
		ELSE IF ageenroll="56" THEN ageenroll_num=56;
		ELSE IF ageenroll="57" THEN ageenroll_num=57;
		ELSE IF ageenroll="58" THEN ageenroll_num=58;
		ELSE IF ageenroll="59" THEN ageenroll_num=59;
		ELSE IF ageenroll="60" THEN ageenroll_num=60;
		ELSE IF ageenroll="61" THEN ageenroll_num=61;
		ELSE IF ageenroll="62" THEN ageenroll_num=62;
		ELSE IF ageenroll="63" THEN ageenroll_num=63;
		ELSE IF ageenroll="64" THEN ageenroll_num=64;
		ELSE IF ageenroll="65" THEN ageenroll_num=65;
		ELSE IF ageenroll="66" THEN ageenroll_num=66;
		ELSE IF ageenroll="67" THEN ageenroll_num=67;
		ELSE IF ageenroll="68" THEN ageenroll_num=68;
		ELSE IF ageenroll="69" THEN ageenroll_num=69;
		ELSE IF ageenroll="70" THEN ageenroll_num=70;
		ELSE IF ageenroll="71" THEN ageenroll_num=71;
		ELSE IF ageenroll="72" THEN ageenroll_num=72;
		ELSE IF ageenroll="73" THEN ageenroll_num=73;
		ELSE IF ageenroll="74" THEN ageenroll_num=74;
		ELSE IF ageenroll="75" THEN ageenroll_num=75;
		ELSE IF ageenroll="76" THEN ageenroll_num=76;
		ELSE IF ageenroll="77" THEN ageenroll_num=77;
		ELSE IF ageenroll="78" THEN ageenroll_num=78;
		ELSE IF ageenroll="79" THEN ageenroll_num=79;
		ELSE IF ageenroll="80" THEN ageenroll_num=80;
		ELSE IF ageenroll="81" THEN ageenroll_num=81;
		ELSE IF ageenroll="82" THEN ageenroll_num=82;
		ELSE IF ageenroll="83" THEN ageenroll_num=83;
		ELSE IF ageenroll="84" THEN ageenroll_num=84;
		ELSE IF ageenroll="85" THEN ageenroll_num=85;
		ELSE IF ageenroll="86" THEN ageenroll_num=86;
		ELSE IF ageenroll="87" THEN ageenroll_num=87;
		ELSE IF ageenroll="88" THEN ageenroll_num=88;
		ELSE IF ageenroll="89" THEN ageenroll_num=89;
		ELSE IF ageenroll="90" THEN ageenroll_num=90;
		ELSE IF ageenroll="91" THEN ageenroll_num=91;
		ELSE IF ageenroll="92" THEN ageenroll_num=92;
		ELSE IF ageenroll="93" THEN ageenroll_num=93;
		ELSE IF ageenroll="94" THEN ageenroll_num=94;
		
	IF Study in ("MMHS","CPS3","SISTER") AND AgeEnroll_num<200 THEN age=AgeEnroll_num;

	*assume that those in MMHS and CPS3 missing age at enrollment / DNA collection (both cases and non) 
		have that as age at enrollment;
	IF (study="MMHS" | study="CPS3") AND age<0 AND agediag<200 THEN age=agediag;

	IF 0<age<30 THEN agecat=0;
		ELSE IF 30<=age<40 THEN agecat=1;
		ELSE IF 40<=age<50 THEN agecat=2;
		ELSE IF 50<=age<60 THEN agecat=3;
		ELSE IF 60<=age<70 THEN agecat=4;
		ELSE IF 70<=age<80 THEN agecat=5;
		ELSE IF 80<=age<800 THEN agecat=6;

	IF agediag<200 THEN agediag2=agediag;
	IF AgeEnroll_num<200 THEN ageenroll2=ageenroll_num;

RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES agecat*case;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	TABLES FH1st*case agecat*case raceethnicity*case/ missing;
RUN;

*drop those missing FH info;
DATA CARRIERS.FHdata;
	SET CARRIERS.FHdata;
	IF FH1st<0 THEN DELETE;
RUN;
*lose 226, leaving 67911;

*Table 1: distributions by family history of breast cancer;
PROC FREQ data=CARRIERS.FHdata;
	TABLES FH1st*case FH1st*study FH1st*case*study FH1st*agecat FH1st*case*agecat FH1st*raceethnicity
	FH1st*case*raceethnicity;
RUN;	

DATA CARRIERS.FHdata;
	SET CARRIERS.FHdata;
	sumPV=SUM(ATM_mutation,BARD1_mutation,BRCA1_mutation,BRCA2_mutation,
	CDH1_mutation,CHEK2_mutation,NF1_mutation,PALB2_mutation,PTEN_mutation,
	RAD51C_mutation,RAD51D_mutation,TP53_mutation);
	IF sumPV=0 THEN anyPV=0;
		ELSE IF sumPV>0 THEN anyPV=1;
	sumPV2=SUM(ATM_mutation,BARD1_mutation,BRCA1_mutation,BRCA2_mutation,CHEK2_mutation,PALB2_mutation,RAD51C_mutation);
	IF sumPV2=0 THEN anyPV2=0;
		ELSE IF sumPV2>0 THEN anyPV2=1;
	*complete case;
	IF agecat<0 | raceethnicity=" " THEN DELETE;
	raceethnicity2=raceethnicity;
	IF raceethnicity="Asian" THEN raceethnicity2="Other";
	raceethnicity3=raceethnicity;
	IF raceethnicity="Other" THEN raceethnicity3=" ";
RUN;
*drop 219, left with 67692;

PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="BARD1" | GENE_2="BARD1" | GENE_3="BARD1";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;

PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="CDH1" | GENE_2="CDH1" | GENE_3="CDH1";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="CHEK2" | GENE_2="CHEK2" | GENE_3="CHEK2";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="NF1" | GENE_2="NF1" | GENE_3="NF1";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="PTEN" | GENE_2="PTEN" | GENE_3="PTEN";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="RAD51C" | GENE_2="RAD51C" | GENE_3="RAD51C";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="RAD51D" | GENE_2="RAD51D" | GENE_3="RAD51D";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	WHERE GENE_1="TP53" | GENE_2="TP53" | GENE_3="TP53";
	TABLES VARIANT_1 VARIANT_2 VARIANT_3;
RUN;

DATA CARRIERS.FHdata;
	SET CARRIERS.FHdata;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.1564_1565delGA" | VARIANT_2="c.1564_1565delGA" | VARIANT_3="c.1564_1565delGA") then atm_1564=1;
		else ATM_1564=0;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.2250G>A_p." | VARIANT_2="c.2250G>A_p." | VARIANT_3="c.2250G>A_p.") then atm_2250=1;
		else ATM_2250=0;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.3247delC" | VARIANT_2="c.3247delC" | VARIANT_3="c.3247delC") then atm_3247=1;
		else ATM_3247=0;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.3802delG" | VARIANT_2="c.3802delG" | VARIANT_3="c.3802delG") then atm_3802=1;
		else ATM_3802=0;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.5932G>T_p.Glu1978X" | VARIANT_2="c.5932G>T_p.Glu1978X" | VARIANT_3="c.5932G>T_p.Glu1978X") then atm_5932=1;
		else ATM_5932=0;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.7271T>G_p.Val2424Gly" | VARIANT_2="c.7271T>G_p.Val2424Gly" | VARIANT_3="c.7271T>G_p.Val2424Gly") then atm_7271=1;
		else ATM_7271=0;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.748C>T_p.Arg250X" | VARIANT_2="c.748C>T_p.Arg250X" | VARIANT_3="c.748C>T_p.Arg250X") then atm_748=1;
		else ATM_748=0;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.7638_7646del9_p.Arg2547_Ser2549del" | VARIANT_2="c.7638_7646del9_p.Arg2547_Ser2549del" | VARIANT_3="c.7638_7646del9_p.Arg2547_Ser2549del") then atm_7638=1;
		else ATM_7638=0;
	IF (GENE_1="ATM" | GENE_2="ATM" | GENE_3="ATM") 
		AND (VARIANT_1="c.8147T>C_p.Val2716Ala" | VARIANT_2="c.8147T>C_p.Val2716Ala" | VARIANT_3="c.8147T>C_p.Val2716Ala") then atm_8147=1;
		else ATM_8147=0;
	IF (GENE_1="BARD1" | GENE_2="BARD1" | GENE_3="BARD1") 
		AND (VARIANT_1="c.1690C>T_p.Gln564X" | VARIANT_2="c.1690C>T_p.Gln564X" | VARIANT_3="c.1690C>T_p.Gln564X") then bard_1690=1;
		else bard_1690=0;
	IF (GENE_1="BARD1" | GENE_2="BARD1" | GENE_3="BARD1") 
		AND (VARIANT_1="c.1935_1954dup20" | VARIANT_2="c.1935_1954dup20" | VARIANT_3="c.1935_1954dup20") then bard_1935=1;
		else bard_1935=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.181T>G_p.Cys61Gly" | VARIANT_2="c.181T>G_p.Cys61Gly" | VARIANT_3="c.181T>G_p.Cys61Gly") then brca1_181=1;
		else brca1_181=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.3748G>T_p.Glu1250X" | VARIANT_2="c.3748G>T_p.Glu1250X" | VARIANT_3="c.3748G>T_p.Glu1250X") then brca1_3748=1;
		else brca1_3748=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.3756_3759delGTCT" | VARIANT_2="c.3756_3759delGTCT" | VARIANT_3="c.3756_3759delGTCT") then brca1_3756=1;
		else brca1_3756=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.4065_4068delTCAA" | VARIANT_2="c.4065_4068delTCAA" | VARIANT_3="c.4065_4068delTCAA") then brca1_4065=1;
		else brca1_4065=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.4357+1G>A" | VARIANT_2="c.4357+1G>A" | VARIANT_3="c.4357+1G>A") then brca1_4357=1;
		else brca1_4357=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.5177_5180delGAAA" | VARIANT_2="c.5177_5180delGAAA" | VARIANT_3="c.5177_5180delGAAA") then brca1_5177=1;
		else brca1_5177=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.5266dupC" | VARIANT_2="c.5266dupC" | VARIANT_3="c.5266dupC") then brca1_5266=1;
		else brca1_5266=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.5324T>G_p.Met1775Arg" | VARIANT_2="c.5324T>G_p.Met1775Arg" | VARIANT_3="c.5324T>G_p.Met1775Arg") then brca1_5324=1;
		else brca1_5324=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.68_69delAG" | VARIANT_2="c.68_69delAG" | VARIANT_3="c.68_69delAG") then brca1_68=1;
		else brca1_68=0;
	IF (GENE_1="BRCA1" | GENE_2="BRCA1" | GENE_3="BRCA1") 
		AND (VARIANT_1="c.815_824dup10" | VARIANT_2="c.815_824dup10" | VARIANT_3="c.815_824dup10") then brca1_815=1;
		else brca1_815=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.1813dupA" | VARIANT_2="c.1813dupA" | VARIANT_3="c.1813dupA") then brca2_1813=1;
		else brca2_1813=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.1929delG" | VARIANT_2="c.1929delG" | VARIANT_3="c.1929delG") then brca2_1929=1;
		else brca2_1929=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.2808_2811delACAA" | VARIANT_2="c.2808_2811delACAA" | VARIANT_3="c.2808_2811delACAA") then brca2_2808=1;
		else brca2_2808=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.3847_3848delGT" | VARIANT_2="c.3847_3848delGT" | VARIANT_3="c.3847_3848delGT") then brca2_3847=1;
		else brca2_3847=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.4477_4478delGA" | VARIANT_2="c.4477_4478delGA" | VARIANT_3="c.4477_4478delGA") then brca2_4477=1;
		else brca2_4477=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.4936_4939delGAAA" | VARIANT_2="c.4936_4939delGAAA" | VARIANT_3="c.4936_4939delGAAA") then brca2_4936=1;
		else brca2_4936=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.4965C>G_p.Tyr1655X" | VARIANT_2="c.4965C>G_p.Tyr1655X" | VARIANT_3="c.4965C>G_p.Tyr1655X") then brca2_4965=1;
		else brca2_4965=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.5073dupA" | VARIANT_2="c.5073dupA" | VARIANT_3="c.5073dupA") then brca2_5073=1;
		else brca2_5073=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.5350_5351delAA" | VARIANT_2="c.5350_5351delAA" | VARIANT_3="c.5350_5351delAA") then brca2_5350=1;
		else brca2_5350=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.5946delT" | VARIANT_2="c.5946delT" | VARIANT_3="c.5946delT") then brca2_5946=1;
		else brca2_5946=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.6275_6276delTT" | VARIANT_2="c.6275_6276delTT" | VARIANT_3="c.6275_6276delTT") then brca2_6275=1;
		else brca2_6275=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.658_659delGT" | VARIANT_2="c.658_659delGT" | VARIANT_3="c.658_659delGT") then brca2_658=1;
		else brca2_658=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.7558C>T_p.Arg2520X" | VARIANT_2="c.7558C>T_p.Arg2520X" | VARIANT_3="c.7558C>T_p.Arg2520X") then brca2_7558=1;
		else brca2_7558=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.8537_8538delAG" | VARIANT_2="c.8537_8538delAG" | VARIANT_3="c.8537_8538delAG") then brca2_8537=1;
		else brca2_8537=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.9253dupA" | VARIANT_2="c.9253dupA" | VARIANT_3="c.9253dupA") then brca2_9253=1;
		else brca2_9253=0;
	IF (GENE_1="BRCA2" | GENE_2="BRCA2" | GENE_3="BRCA2") 
		AND (VARIANT_1="c.9382C>T_p.Arg3128X" | VARIANT_2="c.9382C>T_p.Arg3128X" | VARIANT_3="c.9382C>T_p.Arg3128X") then brca2_9382=1;
		else brca2_9382=0;
	IF (GENE_1="CHEK2" | GENE_2="CHEK2" | GENE_3="CHEK2") 
		AND (VARIANT_1="c.1100delC" | VARIANT_2="c.1100delC" | VARIANT_3="c.1100delC") then chek2_1100=1;
		else chek2_1100=0;
	IF (GENE_1="CHEK2" | GENE_2="CHEK2" | GENE_3="CHEK2") 
		AND (VARIANT_1="c.1263delT" | VARIANT_2="c.1263delT" | VARIANT_3="c.1263delT") then chek2_1263=1;
		else chek2_1263=0;
	IF (GENE_1="CHEK2" | GENE_2="CHEK2" | GENE_3="CHEK2") 
		AND (VARIANT_1="c.444+1G>A" | VARIANT_2="c.444+1G>A" | VARIANT_3="c.444+1G>A") then chek2_444=1;
		else chek2_444=0;
	IF (GENE_1="CHEK2" | GENE_2="CHEK2" | GENE_3="CHEK2") 
		AND (VARIANT_1="c.846+4_846+7delAGTA" | VARIANT_2="c.846+4_846+7delAGTA" | VARIANT_3="c.846+4_846+7delAGTA") then chek2_846=1;
		else chek2_846=0;
	IF (GENE_1="CHEK2" | GENE_2="CHEK2" | GENE_3="CHEK2") 
		AND (VARIANT_1="del exon 9-10" | VARIANT_2="del exon 9-10" | VARIANT_3="del exon 9-10") then chek2_exon9=1;
		else chek2_exon9=0;
	IF (GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2") 
		AND (VARIANT_1="c.172_175delTTGT" | VARIANT_2="c.172_175delTTGT" | VARIANT_3="c.172_175delTTGT") then palb2_172=1;
		else PALB2_172=0;
	IF (GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2") 
		AND (VARIANT_1="c.2167_2168delAT" | VARIANT_2="c.2167_2168delAT" | VARIANT_3="c.2167_2168delAT") then palb2_2167=1;
		else PALB2_2167=0;
	IF (GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2") 
		AND (VARIANT_1="c.2257C>T_p.Arg753X" | VARIANT_2="c.2257C>T_p.Arg753X" | VARIANT_3="c.2257C>T_p.Arg753X") then palb2_2257=1;
		else PALB2_2257=0;
	IF (GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2") 
		AND (VARIANT_1="c.3113G>A_p.Trp1038X" | VARIANT_2="c.3113G>A_p.Trp1038X" | VARIANT_3="c.3113G>A_p.Trp1038X") then palb2_3113=1;
		else PALB2_3113=0;
	IF (GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2") 
		AND (VARIANT_1="c.3116delA" | VARIANT_2="c.3116delA" | VARIANT_3="c.3116delA") then palb2_3116=1;
		else PALB2_3116=0;
	IF (GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2") 
		AND (VARIANT_1="c.3323delA" | VARIANT_2="c.3323delA" | VARIANT_3="c.3323delA") then palb2_3323=1;
		else PALB2_3323=0;
	IF (GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2") 
		AND (VARIANT_1="c.509_510delGA" | VARIANT_2="c.509_510delGA" | VARIANT_3="c.509_510delGA") then palb2_509=1;
		else PALB2_509=0;
	IF (GENE_1="PALB2" | GENE_2="PALB2" | GENE_3="PALB2") 
		AND (VARIANT_1="c.758dupT" | VARIANT_2="c.758dupT" | VARIANT_3="c.758dupT") then palb2_758=1;
		else PALB2_758=0;
	IF (GENE_1="RAD51C" | GENE_2="RAD51C" | GENE_3="RAD51C") 
		AND (VARIANT_1="c.1026+5_1026+7delGTA" | VARIANT_2="c.1026+5_1026+7delGTA" | VARIANT_3="c.1026+5_1026+7delGTA") then rad51c_1026=1;
		else RAD51C_1026=0;
	IF (GENE_1="RAD51D" | GENE_2="RAD51D" | GENE_3="RAD51D") 
		AND (VARIANT_1="c.620C>T_p.Ser207Leu" | VARIANT_2="c.620C>T_p.Ser207Leu" | VARIANT_3="c.620C>T_p.Ser207Leu") then rad51d_620=1;
		else RAD51D_620=0;

	IF GENE_1~=" " AND GENE_2~=" " THEN multiPV=1;
		ELSE multiPV=0;
	IF GENE_1 in ("BRIP1","CDKN2A","FANCC","FANCM","MLH1","MSH6","NBN") THEN multiPV=0;
	IF GENE_2 in ("BRIP1","CDKN2A","FANCC","FANCM","MLH1","MSH6","NBN") THEN multiPV=0;
	IF GENE_1=GENE_2 THEN multiPV=0;

RUN;

PROC FREQ Data=CARRIERS.FHdata;
	TABLES ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation CDH1_mutation CHEK2_mutation NF1_mutation 
	PALB2_mutation PTEN_mutation RAD51C_mutation RAD51D_mutation TP53_mutation;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES ATM_1564 ATM_2250 ATM_3247 ATM_3802 ATM_5932 ATM_7271 ATM_748 ATM_7638 ATM_8147;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES BARD_1690 BARD_1935;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES BRCA1_181 BRCA1_3748 BRCA1_3756 BRCA1_4065 BRCA1_4357 BRCA1_5177 BRCA1_5266 BRCA1_5324 BRCA1_68 BRCA1_815;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES BRCA2_1813 BRCA2_1929 BRCA2_2808 BRCA2_3847 BRCA2_4477 BRCA2_4936 BRCA2_4965 BRCA2_5073 BRCA2_5350 BRCA2_5946
		BRCA2_6275 BRCA2_658 BRCA2_7558 BRCA2_8537 BRCA2_9253 BRCA2_9382;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES CHEK2_1100 CHEK2_1263 CHEK2_444 CHEK2_846 CHEK2_exon9;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES PALB2_172 PALB2_2167 PALB2_2257 PALB2_3113 PALB2_3116 PALB2_3323 PALB2_509 PALB2_758;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES RAD51C_1026;
RUN;
PROC FREQ data=CARRIERS.FHdata;
	TABLES RAD51D_620;
RUN;
PROC PRINT data=CARRIERS.FHdata;
	WHERE multiPV=1;
	VAR GENE_1 GENE_2 GENE_3;
RUN;


PROC FREQ data=CARRIERS.FHdata;
	WHERE FH1st=0 AND case=1;
	TABLES ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation CDH1_mutation CHEK2_mutation NF1_mutation 
	PALB2_mutation PTEN_mutation RAD51C_mutation RAD51D_mutation TP53_mutation
	ATM_1564 ATM_2250 ATM_3247 ATM_3802 ATM_7271 ATM_7638 BARD_1690 BARD_1935
	BRCA1_5266 BRCA1_68 BRCA2_2808 BRCA2_3847 BRCA2_5350 BRCA2_5946 BRCA2_658 BRCA2_7558 
	CHEK2_1100 CHEK2_1263 CHEK2_444 CHEK2_846 CHEK2_exon9 
	PALB2_172 PALB2_3113 PALB2_3116 PALB2_509 RAD51C_1026 RAD51D_620 AnyPV2;
RUN;	
PROC FREQ data=CARRIERS.FHdata;
	WHERE FH1st=0 AND case=0;
	TABLES ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation CDH1_mutation CHEK2_mutation NF1_mutation 
	PALB2_mutation PTEN_mutation RAD51C_mutation RAD51D_mutation TP53_mutation
	ATM_1564 ATM_2250 ATM_3247 ATM_3802 ATM_7271 ATM_7638 BARD_1690 BARD_1935
	BRCA1_5266 BRCA1_68 BRCA2_2808 BRCA2_3847 BRCA2_5350 BRCA2_5946 BRCA2_658 BRCA2_7558 
	CHEK2_1100 CHEK2_1263 CHEK2_444 CHEK2_846 CHEK2_exon9 
	PALB2_172 PALB2_3113 PALB2_3116 PALB2_509 RAD51C_1026 RAD51D_620 anyPV2;
RUN;	
PROC FREQ data=CARRIERS.FHdata;
	WHERE FH1st=1 AND case=1;
	TABLES ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation CDH1_mutation CHEK2_mutation NF1_mutation 
	PALB2_mutation PTEN_mutation RAD51C_mutation RAD51D_mutation TP53_mutation
	ATM_1564 ATM_2250 ATM_3247 ATM_3802 ATM_7271 ATM_7638 BARD_1690 BARD_1935
	BRCA1_5266 BRCA1_68 BRCA2_2808 BRCA2_3847 BRCA2_5350 BRCA2_5946 BRCA2_658 BRCA2_7558 
	CHEK2_1100 CHEK2_1263 CHEK2_444 CHEK2_846 CHEK2_exon9 
	PALB2_172 PALB2_3113 PALB2_3116 PALB2_509 RAD51C_1026 RAD51D_620 anyPV2;
RUN;	
PROC FREQ data=CARRIERS.FHdata;
	WHERE FH1st=1 AND case=0;
	TABLES ATM_mutation BARD1_mutation BRCA1_mutation BRCA2_mutation CDH1_mutation CHEK2_mutation NF1_mutation 
	PALB2_mutation PTEN_mutation RAD51C_mutation RAD51D_mutation TP53_mutation
	ATM_1564 ATM_2250 ATM_3247 ATM_3802 ATM_7271 ATM_7638 BARD_1690 BARD_1935
	BRCA1_5266 BRCA1_68 BRCA2_2808 BRCA2_3847 BRCA2_5350 BRCA2_5946 BRCA2_658 BRCA2_7558 
	CHEK2_1100 CHEK2_1263 CHEK2_444 CHEK2_846 CHEK2_exon9 
	PALB2_172 PALB2_3113 PALB2_3116 PALB2_509 RAD51C_1026 RAD51D_620 anyPV2;
RUN;	

*logistic regression models;
*adjust for age, race/ethnicity, study;
*within strata of family history;
%MACRO OR (gene= );
PROC LOGISTIC data=CARRIERS.FHdata DESC;
	WHERE FH1st=0;
	CLASS raceethnicity study;
	MODEL CASE = &gene age raceethnicity study;
	ODDSRATIO &gene;
RUN;
PROC LOGISTIC data=CARRIERS.FHdata DESC;
	WHERE FH1st=1;
	CLASS raceethnicity study;
	MODEL CASE = &gene age raceethnicity study;
	ODDSRATIO &gene;
RUN;
PROC LOGISTIC data=CARRIERS.FHdata DESC;
	CLASS raceethnicity study;
	MODEL CASE = &gene age raceethnicity study FH1st FH1st*age FH1st*raceethnicity FH1st*study FH1st*&gene;
RUN;
%MEND;

%OR(gene=ATM_mutation);
%OR(gene=BARD1_mutation);
%OR(gene=BRCA1_mutation);
%OR(gene=BRCA2_mutation);
%OR(gene=CHEK2_mutation);
%OR(gene=CHEK2_1100);
%OR(gene=NF1_mutation);
%OR(gene=PALB2_mutation);
%OR(gene=RAD51C_mutation);
%OR(gene=anyPV2);

%MACRO ORall (gene= );
PROC LOGISTIC data=CARRIERS.FHdata DESC;
	CLASS raceethnicity study;
	MODEL CASE = &gene age raceethnicity study;
	ODDSRATIO &gene;
RUN;
%MEND;

%ORall(gene=ATM_mutation);
%ORall(gene=BARD1_mutation);
%ORall(gene=BRCA1_mutation);
%ORall(gene=BRCA2_mutation);
%ORall(gene=CHEK2_mutation);
%ORall(gene=CHEK2_1100);
%ORall(gene=NF1_mutation);
%ORall(gene=PALB2_mutation);
%ORall(gene=RAD51C_mutation);
%ORall(gene=anyPV2);
