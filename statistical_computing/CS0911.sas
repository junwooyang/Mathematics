/*library 설정: sas university edition 사용 시 반드시 아래와 같은 경로를 사용*/
LIBNAME MYLIB '/folders/myfolders/mylib';

/*data 직접입력*/
DATA mylib.htwt;
	INPUT name $ sex $ dept $ age height weight;
	LABEL name='이름' sex='성별' dept='학과' age='나이' height='키' weight='몸무게';
CARDS;
김철수 M Stat 25 170 67
강민호 M Stat 20 169 70
최병호 M Math 28 177 62
이영희 F Math 19 160 58
박지수 F Econ 21 160 59
;
RUN;

/*data 파일 불러오기*/
DATA mylib.cs;
	INFILE '/folders/myfolders/mylib/cs.txt';
	INPUT number @@;
	LABEL number='숫자';
RUN;

/*data 출력*/
PROC PRINT DATA=mylib.htwt; /*변수 설정 안할 시 전체 출력*/
RUN;

PROC PRINT DATA=mylib.cs LABEL; /*culname을 label로 출력시 ; 전에 label*/
RUN;

PROC SORT DATA=mylib.htwt; /*print 시 by를 사용하기 위해 반드시 sort 작업을 먼저 해줌*/
	BY DESCENDING sex; /*만약 descending sex를 사용하면 내림차순으로 정렬되기 때문에 print 에서 by 사용이 불가*/
RUN; /*DESCENDING SORT와 PRINT BY 둘다 사용해주면 내림차순 출력 가능*/

PROC PRINT DATA=mylib.htwt LABEL; 
	VAR name sex dept age;
	ID sex;
	BY DESCENDING sex; /*출력 결과 성별로 나뉘어 테이블이 두개가 나온 것을 확인할 수 있다.*/
RUN;

PROC PRINT DATA=mylib.htwt LABEL;
	ID dept; /*ID 관측치 순서 대신 사용*/
RUN;

PROC PRINT DATA=mylib.htwt LABEL;
	ID DEPT; /*ID는 SORT와 관계없이 해당변수를 앞쪽으로 나타내줌*/
RUN;


/*SORT*/
PROC SORT DATA=mylib.htwt;
	BY DESCENDING sex age ; /*먼저 sorting 되는 순으로 변수 나열, 역순으로 sorting 할 때 descending 사용*/
RUN;

/*SET 이용하여 새 data 생성, IF 이용해 새 변수 생성*/
DATA mylib.htwt1; /*새로운 data 생성(기존 데이터 사용 가능)*/
	SET mylib.htwt; /*어느 data를 기반으로 사용할건지*/
	stdweight=(height-100)*0.9; /*새 변수 설정*/
	LABEL stdweight="표준체중"; /*라벨이름 같이 설정 가능*/
RUN;

PROC PRINT DATA=mylib.htwt1 label;
RUN;

DATA mylib.htwt2;
	SET mylib.htwt1;
	IF (weight>=stdweight) then obesity='O'; /*if문을 사용하여 새 변수 obesity에 값 할당*/
	ELSE obesity='X';
	LABEL obesity='비만';
RUN;

PROC PRINT DATA=mylib.htwt2 LABEL;
RUN;

/*DO*/
DATA mylib.dotest;
	DO i=1 TO 15 BY 2; /*1부터 15까지 2증분*/
	j=i**3;
	END; /*output 없으면 [i(n+1), j(n)] 한 행만 출력됨*/
RUN;

DATA mylib.dotest;
	DO i=1 TO 15 BY 2;
	j=i**3;
	OUTPUT; /*output을 사용해야 do가 실행하면서 각 j가 계속 저장됨*/
	END;
RUN;




