/*  SORT and RANK */
DATA mylib.cholest;
	INPUT gender $ age super @@;
CARDS;
M 23 40  M 64 88   M 66 110  M 31 86   M 55 137
M 48 78  M 58 111  M 31 88   M 27 80   M 25 86
M 20 80  M 32 47   M 63 106  M 23 65   M 62 74
M 43 66  M 43 79   M 36 58   M 67 123  M 27 87
M 29 88  M 48 90   M 63 56   M 27 73   M 19 112
M 59 110 M 65 118  M 26 52   M 53 106  M 42 67
M 60 57  F 30 66   F 25 69   F 40 65   F 38 52
F 57 84  F 33 86   F 23 35   F 42 116  F 49 76
F 35 55  F 49 73   F 44 89   F 50 127  F 60 87
F 63 142 F 47 77   F 23 76   F 27 58   F 36 91
F 48 107 F 23 98   F 74 128  F 44 84   F 56 146
F 53 75  F 37 120  F 41 80   F 41 82   F 57 123
;
RUN;

PROC SORT DATA=mylib.cholest;
	BY gender;
RUN;

PROC PRINT DATA=mylib.cholest;
	BY gender;
RUN;

PROC SORT DATA=mylib.cholest;
	BY descending gender age super; /*sorting 순서이지 칼럼 순서는 그대로 출력*/
RUN;

PROC PRINT DATA=mylib.cholest;
RUN;

/*MEANS*/
PROC MEANS DATA=mylib.htwt2;
RUN;

PROC MEANS DATA=mylib.cholest;
RUN;

/*RANK*/

proc print data=mylib.cholest;
run;

proc sort data=mylib.cholest;
	by gender;
run;

PROC RANK DATA=mylib.cholest OUT=mylib.rankout TIES=dense; /*새 변수 r_age까지 포함된 rankout이 저장됨*/
	BY gender;
	VAR age;
	RANKS r_age;
RUN;

proc print data=mylib.rankout;
	by gender;
	id r_age;
run;

PROC SORT DATA=mylib.rankout;
	BY r_age;
RUN;

PROC PRINT DATA=mylib.rankout;
	ID r_age;
	VAR age super; /*ID에 설정된 변수는 VAR에 한번 더 입력할 필요 없다. 한번 더 입력하면 두번 출력됨.*/
RUN;

PROC RANK DATA=mylib.cholest OUT=mylib.rankout1 TIES= low DESCENDING; /*rank_ties에서는 descending을 뒤에 입력*/
	WHERE gender='F';
	VAR age;
	RANKS r_age;
RUN;

PROC SORT DATA=mylib.rankout1;
	BY r_age descending super;
RUN;

PROC PRINT DATA=mylib.rankout1;
	ID r_age;
RUN;

/*TIES=high: 동률일 때 큰 숫자 rank 사용*/
PROC RANK DATA=mylib.cholest OUT=mylib.rankout2 TIES=high;
	WHERE gender='F';
	VAR age;
	RANKS r_age;
RUN;

PROC SORT DATA=mylib.rankout2;
	BY r_age;
RUN;

PROC PRINT DATA=mylib.rankout2;
	ID r_age;
	VAR age;
RUN;

/*TIES=dense: 동률을 같은 순위로 매기고 바로 다음 rank를 동률 data 수와 관계 없이 바로 다음 숫자로 매김*/
PROC RANK DATA=mylib.cholest OUT=mylib.rankout3 TIES=dense;
	BY gender;
	VAR age;
	RANKS r_age;
RUN;

PROC SORT DATA=mylib.rankout3;
	BY gender r_age;
RUN;

PROC PRINT DATA=mylib.rankout3;
	BY gender;
	ID r_age;
	VAR age;
RUN;

/* UNIVARIATE*/
PROC SORT DATA=mylib.cholest; /*univariate_by 사용 위해 sorting 선행*/
	BY descending gender;
RUN;

PROC UNIVARIATE DATA=mylib.cholest;
	BY descending gender; /*gender구분 후 super age 별 출력*/
	VAR super age;
	LABEL super='콜레스테롤 과포화율' age='나이';
RUN;
/*sort와 univariate by에 동시에 descending 넣어주면 역순으로 분석 출력*/

PROC UNIVARIATE DATA=mylib.cholest;
	CLASS gender; /*class는 sorting 없이 바로 사용 가능, super age 구분 후 gender 구분*/
	VAR super age;
	LABEL super='콜레스테롤 과포화율' age='나이';
RUN;

/* Histogram */
/* histogram 의 변수가 VAR에 반드시 들어가야 한다. */
PROC UNIVARIATE DATA=mylib.cholest ;
	CLASS gender; /*BY 사용 시 sorting 먼저*/
	VAR age super;
	HISTOGRAM age super /
		OUTHISTOGRAM=mylib.outhisto /*HISTOGRAM 그릴 때 집계된 DATA가 저장됨*/
		VAXIS=0 to 35 by 5
		MIDPOINTS=10 to 80 by 10
		NROW=1 NCOL=2;
RUN;

PROC PRINT DATA=mylib.outhisto;
RUN;

/*
PROC UNIVARIATE DATA=mylib.cholest ;
	CLASS gender; 
	VAR age super;
	HISTOGRAM age super /
		OUTHISTOGRAM=mylib.outhisto
		VAXIS=(0 to 35 by 5, 0 to 30 by 3)
		MIDPOINTS=(10 to 80 by 10, 40 to 150 by 10)
		NROW=1 NCOL=2;
RUN; */

/* PLOT */
ods graphics on;
PROC UNIVARIATE DATA=mylib.cholest plot;
	BY gender;
	VAR age ;
RUN;

/* BOX PLOT */
PROC BOXPLOT DATA=mylib.cholest ;
	PLOT age*gender / boxstyle=skeletal;
	PLOT age*gender / boxstyle=schematic;
RUN;

/* MEANS, SUMMARY, FREQ */
/* DATA */
DATA mylib.score;
	INPUT dept $ gender $ age score @@;
	DATALINES;
	Stat M 10 94 Stat F 10 96 Stat M 15 91 Stat M 15 86
	Stat F 10 76 Stat M 20 88 Math M 20 71 Math F 20 66
	Math M 15 81 Math F 10 77 Math F 15 55 Math F 20 78
	;
RUN;

DATA mylib.drink;
	INPUT age drink $ count @@;
	CARDS;
	18 A 10 19 A 13 20 A 12 18 B 14 19 B 7 20 B 4
	18 C 2 19 C 10 20 C 6 18 D 12 19 D 8 20 D 10
	;
RUN;

proc print data=mylib.score;
run;

/* MEANS */
PROC MEANS DATA=mylib.cholest  MAXDEC=2  MEAN  STD  CV  RANGE;
	BY gender;
	VAR age super ;
	TITLE '>>>>> 성별 기초통계량 <<<<<<';
RUN;

PROC MEANS DATA=mylib.score;
	CLASS dept gender;
	VAR age score;
RUN;	

PROC SORT DATA=mylib.score;
	BY dept gender;
RUN;
proc print data=mylib.score;
run;
PROC MEANS DATA=score  MAXDEC=2  MEAN  STD  CV  RANGE;
	BY dept gender;
	VAR age score;
RUN;


PROC MEANS DATA=score MAXDEC=2 MAX MIN MEAN;
	CLASS dept gender;
	VAR age score;
	OUTPUT OUT=scoreout MEAN (age score)=m_age m_score
			      STD (age score)=s_age s_score;
RUN;
PROC PRINT DATA=scoreout;
RUN;

/* PROC MEANS vs UNIVARIATE */
PROC MEANS DATA=mylib.cholest  MAXDEC=2  MEAN  STD  CV  RANGE;
	BY gender;
	VAR age super;
RUN;

PROC UNIVARIATE DATA=mylib.cholest;
	BY gender;
	VAR super age;
	LABEL super='콜레스테롤 과포화율' age='나이';
	OUTPUT OUT=mylib.univ_out
              MEAN=s_mean a_mean STD=s_std a_std 
              PCTLPTS=33.3 66.6 PCTLPRE=s_p a_p;
RUN;

PROC PRINT DATA=mylib.univ_out;
RUN;


/* PROC SUMMARY */
PROC SUMMARY DATA=mylib.score;
	CLASS dept gender;
	VAR age score;
	OUTPUT OUT=mylib.n_score MEAN(age score)= ; /* = 반드시 써줘야함 */
RUN;
PROC PRINT DATA=mylib.n_score;
RUN;

/* PROC FREQ */
PROC FREQ DATA=mylib.score;
	TABLES dept gender dept*gender;
	TABLES dept*gender / NOROW NOPERCENT;
RUN;

PROC FREQ DATA=mylib.drink;
	WEIGHT COUNT;
	TABLES age age*drink / NOCOL NOPERCENT;
RUN;

/* EXAMPLE */
DATA mylib.weather; 
INPUT region $ maxtemp mintemp rain; 
CARDS;
SEATTLE 46 36 0.000 
LA 11 -11 0.080 
HAWAII 44 29 0.040 
SEATTLE 43 25 0.080 
LA 12 -6 1.360 
HAWAII 43 33 0.570 
SEATTLE 47 36 1.010 
LA 11 -11 0.080 
HAWAII 20 12 0.410 
SEATTLE 47 33 0.040 
LA 51 44 0.290 
HAWAII 37 20 0.350 
SEATTLE 53 46 0.230 
SEATTLE 49 40 0.080 
HAWAII 42 36 0.000 
HAWAII 53 46 0.230 
SEATTLE 49 40 0.080 
LA 42 36 0.000 
; 
RUN;

PROC SORT DATA=mylib.weather;
	BY descending region;
RUN;

PROC MEANS DATA=mylib.weather maxdec=2 n mean var std;
	VAR maxtemp mintemp rain;
	BY descending region;
RUN;


