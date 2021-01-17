LIBNAME MYLIB '/folders/myfolders/mylib';

/*7-2*/
DATA MYLIB.ANT;
	INPUT X Y @@;
CARDS;
1 4 2 1 3 0 4 1 5 4
;
RUN;
PROC CORR DATA=MYLIB.ANT PEARSON SPEARMAN;
	VAR X Y;
RUN;

PROC UNIVARIATE DATA=MYLIB.ANT NORMAL PLOT;
	VAR X Y;
RUN;

/*7-3*/
DATA MYLIB.DRUG;
	INPUT DOSAGE REACTION @@;
CARDS;
1 3.5 3 2.4 4 2.1 7 1.3
9 1.2 12 2.2 13 2.6 14 4.2
;
RUN;
PROC CORR DATA=MYLIB.DRUG PLOTS=SCATTER;
	VAR DOSAGE REACTION;
RUN;


/*7-4*/
DATA MYLIB.CHOLESTEROL;
	INPUT AGE DECREMENT @@;
CARDS;
45 30 37 55 22 28
43 52 34 25 58 44
46 45 30 30 60 61
49 38 31 40 52 58
50 62 27 45 26 17
;
RUN;
PROC CORR DATA=MYLIB.CHOLESTEROL PLOTS=SCATTER;
RUN;

PROC SORT DATA=MYLIB.CHOLESTEROL;
	BY AGE;
RUN;

DATA MYLIB.CHOLESTEROL2;
	SET MYLIB.CHOLESTEROL;
	if AGE<27 OR AGE>37;
RUN;
PROC CORR DATA=MYLIB.CHOLESTEROL2 PLOTS=SCATTER;
RUN;



/*7-5*/
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

PROC CORR DATA=MYLIB.STUDENT NOCORR ALPHA;
	VAR q01 q02 q06 q07 q09;
RUN;
DATA MYLIB.STUDENT2;
	SET MYLIB.STUDENT;
	q09 = 6 - q09;
RUN;
PROC CORR DATA=MYLIB.STUDENT2 NOCORR ALPHA;
	VAR q01 q02 q06 q07 q09;
RUN;

PROC CORR DATA=MYLIB.STUDENT NOCORR ALPHA;
	VAR q03 q04 q05 q08 q10;
RUN;
DATA MYLIB.STUDENT3;
	SET MYLIB.STUDENT;
	q04 = 6 - q04;
	q10 = 6 - q10;
RUN;
PROC CORR DATA=MYLIB.STUDENT3 NOCORR ALPHA;
	VAR q03 q04 q05 q08 q10;
RUN;







