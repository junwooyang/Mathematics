/*************
* Chapter 8  *
*************/
LIBNAME MYLIB '/folders/myfolders/mylib';

/* 단순선형회귀 */
DATA MYLIB.adsales;
    INPUT company adver sales @@;
CARDS;
01  11  23  02  19  32  03  23  36  04  26  46  05  56  93
06  62  99  07  29  49  08  30  50  09  38  65  10  39  70
11  46  71  12  49  89
;
RUN;

proc reg data=mylib.adsales;
model sales = adver;
run;

PROC REG DATA=MYLIB.adsales GRAPHICS;
	MODEL sales=adver / P CLM CLI;
RUN;


PROC REG DATA=MYLIB.adsales GRAPHICS;
	MODEL sales=adver;
	PLOT sales*adver;
RUN;

PROC REG DATA=MYLIB.adsales GRAPHICS;
	MODEL sales=adver / P CLM CLI;
	PLOT sales*adver;
RUN;

/*잔차도표 출력*/
PROC REG DATA=MYLIB.adsales GRAPHICS;
	MODEL sales=adver / R;
	OUTPUT OUT=MYLIB.regout STUDENT=std_r;
	PLOT STUDENT.*adver;
RUN;

proc print data=mylib.regout;
run;

/*잔차 정규성 검토*/
PROC UNIVARIATE DATA=MYLIB.regout;
	HISTOGRAM std_r / NORMAL (MU=EST SIGMA=EST)
	CFILL=grey
	MIDPOINTS= -3 TO 3 BY 1;
	INSET NORMAL (CVM CVMPVAL AD ADPVAL); /*그래프 색인*/
RUN;

PROC UNIVARIATE DATA=MYLIB.regout;
	PROBPLOT std_r / NORMAL (MU=EST SIGMA=EST);
	INSET MEAN STD;
RUN;

proc univariate data=mylib.regout;
qqplot std_r / normal(mu=est sigma=est);

run;

/*독립성 검토*/
PROC REG DATA=MYLIB.adsales;
	MODEL sales=adver / DW;
RUN;

/* 다중선형회귀 */
DATA MYLIB.satisfac;
    INFILE '/folders/myfolders/satisfaction.txt';
	INPUT ID 3. x1 1. x2 1. x3 1. x4 1. y 1.;
	LABEL x1='디자인' x2='편리성' x3='성능' x4='견고성' y='구입의향';
RUN;

PROC PRINT DATA=MYLIB.satisfac;
RUN;

PROC CORR DATA=MYLIB.satisfac PEARSON SPEARMAN nosimple;
	VAR y x1 x2 x3 x4;
RUN;

proc corr data=mylib.satisfac pearson spearman nosimple;
	var y x4;
	partial x1 x2 x3;
run;

PROC REG DATA=MYLIB.satisfac;
	MODEL y=x1 x2 x3 x4 / STB;
RUN;

PROC CORR DATA=MYLIB.satisfac;
	VAR y x4;
	PARTIAL x1 x2 x3;
RUN;

/* 변수선택 */

DATA MYLIB.fitness;
    INPUT oxygen age weight RUNtime rstpulse RUNpulse maxpulse @@;
CARDS;
44.609 44 89.47 11.37 62 178 182  45.313 40 75.07 10.07 62 185 185
54.297 44 85.84  8.65 45 156 168  59.571 42 68.15  8.17 40 166 172
49.874 38 89.02  9.22 55 178 180  44.811 47 77.45 11.63 58 176 176
45.681 40 75.98 11.95 70 176 180  49.091 43 81.19 10.85 64 162 170
39.442 44 81.42 13.08 63 174 176  60.055 38 81.87  8.63 48 170 186
50.541 44 73.03 10.13 45 168 168  37.388 45 87.66 14.03 56 186 192
44.754 45 66.45 11.12 51 176 176  47.273 47 79.15 10.60 47 162 164
51.855 54 83.12 10.33 50 166 170  49.156 49 81.42  8.95 44 180 185
40.836 51 69.63 10.95 57 168 172  46.672 51 77.91 10.00 48 162 168
46.774 48 91.63 10.25 48 162 164  50.388 49 73.37 10.08 67 168 168
39.407 57 73.37 12.63 58 174 176  46.080 54 79.38 11.17 62 156 165
45.441 52 76.32  9.63 48 164 166  54.625 50 70.87  8.92 48 146 155
45.118 51 67.25 11.08 48 172 172  39.203 54 91.63 12.88 44 168 172
45.790 51 73.71 10.47 59 186 188  50.545 57 59.08  9.93 49 148 155
48.673 49 76.32  9.40 56 186 188  47.920 48 61.24 11.50 52 170 176
47.467 52 82.78 10.50 53 170 172
;
RUN;

PROC REG DATA=MYLIB.fitness;
	MODEL oxygen = age weight RUNtime rstpulse RUNpulse maxpulse 
                          / SELECTION=BACKWARD SLSTAY=0.15 ;
RUN;

proc corr data=mylib.fitness pearson spearman nosimple;
run;

proc reg data=mylib.fitness;
	model oxygen = age weight RUNtime rstpulse RUNpulse maxpulse 
                          / SELECTION=forward slentry=0.15;
RUN;

PROC REG DATA=MYLIB.fitness;
	MODEL oxygen = age weight RUNtime rstpulse RUNpulse maxpulse 
                          / SELECTION=stepwise slstay=0.15 slentry=0.15;
RUN;

proc reg data=mylib.fitness;
	model oxygen = age weight RUNtime rstpulse RUNpulse maxpulse 
                          / SELECTION=rsquare;
RUN;


/* 다중공선성 */
DATA MYLIB.multico;
    INPUT y x1 x2 x3 x4 @@;
CARDS;
4.00  4.00  4.00  4.00  4.00  4.00  4.00  4.00  3.00  3.50
4.00  4.00  3.00  4.50  4.00  4.00  4.00  4.00  3.00  3.50
5.00  5.00  5.00  4.00  4.50  4.00  4.00  5.00  3.00  3.50
4.00  4.00  4.00  3.00  3.50  5.00  5.00  4.00  4.00  4.50
4.00  4.00  4.00  3.50  4.00  4.00  4.00  5.00  3.00  3.50
3.00  4.00  4.00  3.00  3.50  4.00  3.00  3.00  3.00  3.00
4.00  4.00  4.00  4.00  4.00  4.00  4.00  4.00  3.00  3.50
4.00  4.00  4.00  3.00  3.50  4.00  4.00  3.00  2.50  3.00
4.00  4.00  3.00  3.00  3.50  4.00  4.00  5.00  5.00  4.50
5.00  4.00  4.00  5.00  4.50  5.00  5.00  4.00  4.00  4.50
;
RUN;

PROC REG DATA=MYLIB.multico;
	MODEL y = x1 x2 x3 x4 / VIF COLLIN;
RUN;

PROC REG DATA=MYLIB.multico;
	MODEL y = x1 x2 x3 x4 / selection=rsquare vif collin;
RUN;

/* 가변수 */

DATA MYLIB.dummy;
    INPUT id y age region @@;
CARDS;
 1  46  21  1   2  39  21  3   3  62  21  3   4  38  21  2
 5  39  21  3   6  70  22  2   7  39  22  2   8  35  22  1
 9  41  22  3  10  41  23  2  11  50  23  1  12  71  23  2
13  66  23  3  14  38  24  1  15  68  24  3  16  44  24  3
17  43  24  2  18  44  25  2  19  46  25  3  20  53  25  1
21  41  26  1  22  71  26  3  23  46  26  2  24  76  26  2
25  57  27  1  26  49  28  2  27  58  28  1  28  74  28  3
29  45  28  1  30  48  30  1  31  53  30  2  32  77  30  3
33  79  30  2  34  85  31  2  35  50  31  1  36  56  32  2
37  81  32  3  38  53  33  1  39  88  33  2  40  60  34  2
41  86  35  3  42  93  36  2  43  63  36  2  44  58  36  1
45  64  37  2  46  64  40  1
;
RUN;

data mylib.dummy;
set mylib.dummy;
d1=0; d2=0;
if region = 1 then d1=1;
if region = 2 then d2=1;
run;

proc reg data=mylib.dummy;
model y = age d1 d2;
run;


DATA MYLIB.dummy;
	SET MYLIB.dummy;
	d1=0; d2=0;
	IF region=1 THEN d1=1;
	IF region=2 THEN d2=1;
RUN;

PROC REG DATA=MYLIB.dummy;
	MODEL y= age d1 d2;
RUN;

