/*HW1_2017004093_양준우*/
/*2-2*/
DATA mylib.protain;
	INPUT protain @@;
	LABEL protain="단백질 섭취량";
CARDS;
10.1 8.9 13.5 7.8 9.7 10.6 8.4 9.5 18.0 10.2
5.3 13.9 9.0 9.5 9.4 6.9 6.2 6.2 7.1 9.9
13.1 17.4 9.3 11.4 4.4
;
RUN;

ods graphics on;
PROC UNIVARIATE DATA=mylib.protain PLOT;
	VAR protain;
RUN;

/*2-3*/
DATA mylib.king;
	INPUT name $ age @@;
	IF _N_<=14 THEN war='Before'; ELSE war='After'; /*_N_: OBS*/
	agegroup=INT(age/10)*10; /*INT: 정수화*/
CARDS;
태조 73 정종 62 태종 45 세종 53 문종 38
단종 16 세조 51 예종 28 성종 37 연산군 30
중종 56 인종 30 명종 33 선조 56 광해군 66
인조 54 효종 40 현종 33 숙종 59 경종 36
영조 82 정조 48 순조 44 헌종 22 철종 32
고종 67 순종 52
;
RUN;

PROC UNIVARIATE DATA=mylib.king PLOT;
	CLASS war;
	VAR age;
RUN;

PROC MEANS DATA=mylib.king MAX MIN MEDIAN MEAN STD;
	CLASS war;
	VAR age;
	OUTPUT OUT=mylib.outking MAX(age)=max MIN(age)=min MEDIAN(age)=median
	MEAN(age)=mean STD(age)=std;
RUN;

PROC PRINT DATA=mylib.outking;
RUN;

PROC FREQ DATA=mylib.king;
	TABLE war agegroup war*agegroup / NOCOL NOPERCENT;
RUN;

/*2-4*/
DATA mylib.car;
	INPUT size $ manufact $ model $ mileage reliable index;
	LABEL size='차의 크기' manufact='제조회사' model='모델명'
	mileage='주행거리' reliable='신뢰성' index='지수';
CARDS;
Small Chevrolet GeoPrizm 33 5 4
Small Honda Civic 29 5 4
Small Toyata Corolla 30 5 4
Small Ford Escort 27 3 3
Small Dodge Colt 34 . .
Compact Ford Tempo 24 1 3
Compact Chrysler LeBaron 23 3 3
Compact Buick Skylark 21 3 3
Compact Plymouth Acclaim 24 3 3
Compact Chevrolet Corsica 25 2 3
Compact Pontiac Sunbird 24 1 3
Mid-Sized Toyota Camry 24 5 4
Mid-Sized Honda Accord 26 5 4
Mid-Sized Ford Taurus 20 3 3
;
RUN;

PROC MEANS DATA=mylib.car MAXDEC=3 MEAN STD;
	VAR mileage reliable;
	OUTPUT OUT=mylib.carout MEAN(mileage reliable)=m_mean r_mean
	STD(mileage reliable)=m_std r_std;
RUN;

PROC PRINT DATA=mylib.carout;
RUN;

PROC FREQ DATA=mylib.car;
	TABLE size index size*index / NOCOL NOPERCENT;
RUN;


/*2-7*/
DATA mylib.smoke;
	INPUT PPM @@;
	LABEL PPM='분당 맥박 수';
	IF _N_<=22 THEN group='Before'; ELSE group='After';
CARDS;
72 70 68 67 73 71 72 70 69 70 68
72 69 66 73 71 70 72 70 69 72 73
74 72 69 68 72 72 72 71 67 73 69
71 68 74 73 70 74 68 71 74 74 69
;
RUN;

PROC SORT DATA=mylib.smoke;
	BY group PPM;
RUN;

PROC FREQ DATA=mylib.smoke;
	BY group;
	TABLE PPM /NOCOL NOPERCENT;
RUN;

PROC UNIVARIATE DATA=mylib.smoke;
	CLASS group;
	VAR PPM;
	HISTOGRAM PPM /
		OUTHISTOGRAM=mylib.smokeout
		VAXIS=0 TO 25 BY 5
		MIDPOINTS=66 TO 74 BY 1
		NROW=1 NCOL=2;
RUN;