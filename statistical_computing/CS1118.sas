/*************
* Chapter 7  *
*************/
LIBNAME MYLIB '/folders/myfolders/mylib';

DATA MYLIB.student;
    INPUT age income expense @@;
CARDS;
25 170 67 28 177 62 20 165 53 16 150 48 19 160 58 21 160 59
22 173 60 16 169 57 20 169 70 19 170 71 20 179 63 26 180 75
23 174 82 16 179 60 25 189 82 17 169 74 30 180 77
;
RUN;

proc corr data=mylib.student nosimple nocorr; /*아무것도 출력x*/
run;

proc corr data=mylib.student nosimple nocorr; /*spearman 만 출력*/
run; /*var 지정안하면 모든 var에 대해 출력*/

PROC CORR DATA=MYLIB.student PEARSON SPEARMAN NOSIMPLE;
/*defalt는 pearson, nosimple 빼면 요약통계량 출력*/
	VAR age income expense; 
RUN;

PROC CORR DATA=MYLIB.student PEARSON SPEARMAN NOSIMPLE;
	VAR income age;
	WITH expense;
RUN;


DATA MYLIB.satis;
    INPUT age age_level satis1 satis2 @@;
CARDS;
28  2   0  70  23  2   0  55  26  2   5  65  27  2   5  65
25  2  10  60  26  2  20  65  29  2  25  70  31  3  25  75
32  3  25  80  34  3  40  85  31  3  40  75  33  3  50  80
39  3  55  95  36  3  60  90  30  3  65  75  36  3  65  90
32  3  80  80  39  3  85  95  31  3  90  75  32  3  95  80
;
RUN;

PROC CORR DATA=MYLIB.satis;
	VAR age satis1 satis2;
RUN;

PROC CORR DATA=MYLIB.satis NOSIMPLE;
	VAR satis1 satis2;
	PARTIAL age;
RUN;

PROC SORT DATA=MYLIB.satis;
	BY age_level;
RUN;
PROC CORR DATA=MYLIB.satis NOSIMPLE;
	VAR satis1 satis2;
	BY age_level;
RUN;


DATA MYLIB.alpha;
    INFILE '/folders/myfolders/alpha.txt';
    INPUT q01-q10;
RUN;

PROC PRINT DATA=MYLIB.ALPHA;
RUN;

PROC CORR DATA=MYLIB.alpha NOCORR ALPHA;
	VAR q01 q02 q03 ;
RUN;

proc corr data=mylib.alpha nosimple nocorr alpha;
	var q02 q03;
run;

data mylib.alpha_t;
set mylib.alpha;
q025=q02+q03;
run;

proc corr data=mylib.alpha_t nosimple pearson spearman;
	var q01 q025;
run;

PROC CORR DATA=MYLIB.alpha NOCORR ALPHA ;
	VAR q04 q05 q06 q07 ;
RUN;

DATA MYLIB.alpha;
SET MYLIB.alpha;
q07=6-q07;
RUN;

PROC CORR DATA=MYLIB.alpha NOCORR ALPHA ;
	VAR q04 q05 q06 q07 ;
RUN;

PROC CORR DATA=MYLIB.alpha NOCORR ALPHA ;
	VAR q08 q09 q10 ;
RUN;


/* 연습문제 7-5    */
DATA MYLIB.student;
     INPUT q01-q10;
CARDS;
2     1     5     2     4     2     3     5     5     2
1     1     2     4     2     1     2     2     4     4
2     2     3     3     3     1     1     2     4     2
4     4     4     1     5     5     5     4     1     1
1     2     5     1     5     2     1     5     5     1
5     5     1     5     1     4     5     2     2     5
5     5     5     1     4     4     5     5     1     1
1     1     3     4     3     1     2     3     4     3
3     3     4     2     5     3     3     5     4     2
5     4     1     4     2     5     5     1     1     5
4     5     4     1     5     4     4     5     2     1
2     2     5     1     4     4     4     5     3     1
3     4     5     2     5     3     3     4     3     2
2     2     2     5     2     1     1     2     5     5
4     3     4     2     4     4     4     5     4     1
5     4     1     4     1     5     5     1     1     5
5     5     3     2     2     4     4     3     1     3
4     5     2     5     1     5     5     1     2     5
3     3     2     5     2     3     3     2     3     4
;
RUN;