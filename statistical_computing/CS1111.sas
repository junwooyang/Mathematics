/*************
* Chapter 5  *
*************/
LIBNAME MYLIB '/folders/myfolders/mylib';

DATA MYLIB.edu;
    INPUT group score @@;
    CARDS;
     1 65 1 70 1 76 1 63 1 72 1 71 1 68 1 68
     2 75 2 80 2 72 2 77 2 69 2 81 2 71 2 78 
;
RUN;

PROC TTEST DATA=MYLIB.edu COCHRAN; /*COCHRAN method 출력*/
	CLASS group;
	VAR score;
RUN;

PROC TTEST DATA=MYLIB.edu H0=-6.25 COCHRAN; /*H0=-6.25 하에서 검정 출력*/
	CLASS group;
	VAR score;
RUN;

DATA MYLIB.paired;
    INPUT id pretest posttest @@;
    CARDS;
     01 80 82 02 73 71 03 70 95 04 60 69 05 88 100
     06 84 71 07 65 75 08 37 60 09 91 95 10 98 99
     11 52 65 12 78 83 13 40 60 14 79 86 15 59 62 
;
RUN;

PROC TTEST DATA=MYLIB.paired;
	PAIRED pretest*posttest;
RUN;

DATA MYLIB.support;
    INPUT gender $ yesno $ count @@;
CARDS;
남자  YES  110   남자  NO   140
여자  YES  104   여자  NO    96
;
RUN;

PROC FREQ DATA=MYLIB.support ORDER=DATA; /*FREQ DATA INTERNAL*/
	WEIGHT count;
	TABLES gender*yesno  /  CHISQ FISHER EXPECTED DEVIATION NOPERCENT NOCOL;
	/*CHISQ FISHER 하나만 넣어도 같은 결과, 둘 다 빼면 출력X*/
	/*DEVIATION: 실제빈도-기대빈도 출력*/
RUN;


DATA MYLIB.mcpaired;
    INPUT pre $ post $ count @@;
CARDS;
YESA  YESB  63   YESA  NOB    4
NOA   YESB  21   NOA   NOB   12
;
RUN;

PROC FREQ DATA=MYLIB.mcpaired ORDER=DATA;
	WEIGHT count;
	EXACT MCNEM;
	TABLES pre*post  /  NOCOL NOPERCENT;
RUN;


/* example */
DATA MYLIB.two;
    INPUT group $ density @@;
CARDS;
A 0.95 A 0.82 A 0.78 A 0.96 A 0.71 A 0.86 A 0.99
B 0.89 B 0.91 B 0.94 B 0.91 B 0.90 B 0.89
;
RUN;
PROC TTEST DATA=MYLIB.two;
	CLASS group;
	VAR density;
RUN;

/* example */
DATA MYLIB.paired;
    INPUT id before after @@;
CARDS;
01 1.45 0.19 02 2.37 1.03 03 0.79 0.15 04 0.77 0.35 05 0.93 1.19
06 0.77 0.27 07 1.35 0.67 08 0.33 0.58 09 0.80 0.39 10 1.42 0.68
11 2.50 1.58 12 0.51 0.82 13 0.41 1.58 14 2.83 1.28 15 1.56 0.91
16 1.01 2.02 17 1.93 0.28 18 0.81 0.24 19 1.35 0.12 20 3.48 0.36
;
RUN;

PROC TTEST DATA=MYLIB.paired;
	PAIRED before*after;
RUN;


/* example */
DATA MYLIB.abortion;
    INPUT  year $ yesno $ count @@;
CARDS;
1980년  YES  120  1980년  NO   280
1990년  YES  120  1990년  NO   180
;
RUN;

PROC FREQ DATA=MYLIB.abortion order=data;
	weight count;
	table year*yesno / chisq fisher expected nopercent nocol;
run;
