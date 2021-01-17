/*************
* Chapter 9  *
*************/
LIBNAME MYLIB '/folders/myfolders/mylib';

DATA MYLIB.soft;
    INPUT age $ beverage $ count @@;
CARDS;
20대 coke 10 20대 pepsi 14 20대 fanta  4 20대 others 12
30대 coke 13 30대 pepsi  9 30대 fanta 10 30대 others  8
40대 coke 12 40대 pepsi  8 40대 fanta 10 40대 others 10
;
RUN;


PROC FREQ DATA=MYLIB.soft ORDER=DATA;
	WEIGHT count;
	EXACT FISHER; /*fisher 정확검정만 넣어도 chisq 통계량 출력*/
	TABLES age*beverage
		/ NOCOL NOPERCENT EXPECTED CHISQ;
RUN; /*출력 결과 서로 다른 분포*/

DATA MYLIB.bean;
    INPUT type count @@;
CARDS;
1 315 2 108 3 101 4 32
;
RUN;

PROC FREQ DATA=MYLIB.bean;
	WEIGHT count;
	TABLES type / TESTP=(0.5625 0.1875 0.1875 0.0625);
RUN;

DATA MYLIB.edueco;
    INPUT edu eco count @@;
CARDS;
1 1 255   1 2 105   1 3  81
2 1 110   2 2  92   2 3  66
3 1  90   3 2 113   3 3  88
;
RUN;

PROC FREQ DATA=MYLIB.edueco ORDER=DATA;
	WEIGHT count;
	TABLES edu*eco / NOCOL NOPERCENT CHISQ MEASURES;
RUN;



/* 연습문제 9-1  
0  22  0.135
1  53  0.271
2  58  0.271
3  39  0.180
4  20  0.090
5   8  0.053
*/

/* 연습문제 9-2  
48  42
43  49
65  53
59  76
*/

/* 연습문제 9-3 
1683   1637   1833    461
 641   1117    182     52
 800    275     30     23
 326    249    177     76
  23      2    450      1
*/

/* 연습문제 9-4
140    40    20
 70    20    10
160    30    10
 80    10    10
*/

/* 연습문제 9-6 
32     8
28    12
19    21
*/

/* 연습문제 9-8 
19   44  13   3
19   58  31  13
*/

/* 연습문제 9-9
 76   53    59    48
124  147   141   152
*/

