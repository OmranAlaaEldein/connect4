:- dynamic size/2.
:- dynamic top/2.
:- dynamic piece/3.
:-dynamic col/2.

user_color('R').
comp_color('Y').
empty_color('-').
fact(1).
fact(2).
fact(3).
fact(4).
fact(5).
fact(6).
fact(7).

colmn_Avilable(Colmn ,W):-top(Colmn,NUM),W is NUM+1,size(R,C),W<R+1,Colmn<C+1,Colmn>0.

add_Piece(Colmn,Color):-colmn_Avilable(Colmn,X),retractall(top(Colmn,_)),assert(top(Colmn,X)),
                        retractall(piece(Colmn,X,_)),assert(piece(Colmn,X,Color)) .

removepiece(Colmn):-top(Colmn , NUM),X is NUM-1,retractall( top(Colmn,_) ) ,assert( top(Colmn ,X) ),
        retractall(piece(Colmn , NUM,_)),assert(piece(Colmn , NUM,'-')).

empty_Colmn(Raw , NowColmn):-NowColmn>0,empty_color(C),assert( piece(NowColmn,Raw,C)),ZZ is NowColmn-1,empty_Colmn(Raw,ZZ).
empty_Raw(NowRaw , AllColmn):- NowRaw > 0 , not(empty_Colmn(NowRaw , AllColmn)) , Z is NowRaw-1,empty_Raw(Z,AllColmn).
empty_Table(_):- retractall(piece(_,_,_)) , size(R,C),empty_Raw(R,C).
int_top(Colmn):- Colmn > 0 , assert(top(Colmn,0)) ,X is Colmn-1 , int_top(X).
in_Table(Raw,Colmn):-size(R,C),Raw>0,Raw<R+1,Colmn>0,Colmn<C+1.
/****************************** found create *********************/
play(_):-retractall(size(_,_)),retractall(top(_,_))
    ,write('Write NUMBER OF RAWS pleas?') , read(Raw) , write('Write NUMBER OF COLOMNS pleas? ') ,read(Colmn)
    , assert(size(Raw,Colmn)) ,not(int_top(Colmn)) ,not(empty_Table(_)),print_Table(_),add_user(_) .



  /*****************************************************/

add_user(_):-write('Choose The Colomn ? '),read(W),user_color(X),add_Piece(W ,X)
                           ,top(W,NUM),print_Table(_),check_Win(NUM,W),write('YOU WIN !!'),!;add_comp(_).

add_comp(_):-not(check0('Y')),max(2,1,Z),
	col(1,V),col(2,V2),col(3,V3),col(4,V4),col(5,V5),col(6,V6),col(7,V7),write(V+V2+V3+V4+V5+V6+V7),
	add_Piece(Z,'Y'),top(Z,NUM),print_Table(_),
	     check_Win(NUM,Z),write('YOU WIN !!'),!;add_user(_).

/************************Print The Table**********************************************/
move_Colmn(_,0).
move_Colmn(Raw,Colmn):-piece(Colmn,Raw,COLOR),write(COLOR),X is Colmn-1, move_Colmn(Raw,X).
move_Raw(0,_):-!.
move_Raw(ALL,COLM):- move_Colmn(ALL,COLM),nl, X is ALL-1 ,move_Raw(X,COLM).
print_Table(_):- write('The Table : '),nl,size(R,C),move_Raw(R,C),!.

/************************Check the Winner For the players*****************************************/
check_Win(Raw,Colmn):-check_colmn(Colmn),!;check_raw(Raw,Colmn),!;check_Dialog(Raw,Colmn),!.

check_colmn(Colmn):-top(Colmn,NUM),NUM > 3 ,piece(Colmn,NUM,Color),down(Colmn,NUM,Color,Res),Res>3,!,true.

check_raw(Raw,Colmn):-piece(Colmn , Raw , Color)
                            ,moveRight(Raw ,Colmn,Color,Res1),moveLeft(Raw,Colmn,Color,Res2),
                             RES is Res1+Res2 , RES>4 ,!,true.

check_Dialog(Raw , Colmn):-piece(Colmn,Raw,Color),moveUpRight(Raw,Colmn,Color,Res1) , moveDownLeft(Raw,Colmn,Color,Res2),
                                          RES is Res1 + Res2 , RES > 4 , !,true.
check_Dialog(Raw , Colmn):-piece(Colmn,Raw,Color) ,moveUpLeft(Raw,Colmn,Color,Res1) , moveDownRight(Raw,Colmn,Color,Res2),
                                          RES is Res1 + Res2 , RES > 4 , !,true.


/***********************************THE MOVES*************************************************/

down(_,0,_,0).
down(Colmn,NUM,Color,Res):-Now is NUM-1,piece(Colmn,NUM ,Col ) , Color = Col ,
                            down(Colmn,Now,Color,Z), Res is Z+1,!.

moveRight(_,Col,_,0):-size(_,C),Col>C.
moveRight(_, _,_ ,0).
moveRight(R , C ,COL ,RES):-piece(C,R,COL1),Z is C-1 ,COL1 = COL,moveRight(R,Z,COL,RES1),RES is RES1+1.

moveLeft(_,0,_,0).
moveLeft(_,_,_ ,0).

moveLeft(R , C ,COL ,RES):-piece(C,R,COL1),Z is C+1 ,COL1 = COL,moveLeft(R,Z,COL,RES1),RES is RES1+1.

moveDownRight(_,_,_,0).
moveDownRight(Raw ,Colmn,Color,Res):-in_Table(Raw,Colmn),N1 is Raw-1,N is Colmn-1,piece(Raw,Colmn,Col),Col = Color,
                                     moveDownRight(N1,N,Color,Z),Res is Z+1 .

moveDownLeft(_,_,_,0).
moveDownLeft(Raw ,Colmn,Color,Res):-in_Table(Raw,Colmn),NRaw is Raw-1 , NColmn is Colmn+1,piece(Raw,Colmn,Col),Col = Color,
                                     moveDownLeft(NRaw ,NColmn,Color,Z),Res is Z+1 .

moveUpRight(_,_,_,0).
moveUpRight(Raw ,Colmn,Color,Res):-in_Table(Raw,Colmn),N1 is Raw+1,N is Colmn-1,piece(Raw,Colmn,Col),Col = Color,
                                     moveUpRight(N1,N,Color,Z),Res is Z+1 .

moveUpLeft(_,_,_,0).
moveUpLeft(Raw ,Colmn,Color,Res):-in_Table(Raw,Colmn),N1 is Raw+1,N is Colmn+1,piece(Raw,Colmn,Col),Col = Color,
                                     moveUpLeft(N1 ,N,Color,Z),Res is Z+1 .

/*******************************************************************************/

color2('Y','R').
color2('R','Y').

check(K,Column,Color,L):-L<4,

    checkWinT(K,Color,End),End==0,
    add_Piece(Column,Color),
    color2(Color,X),
    La is L+1
    ,check(K,1,X,La)

    ,check(K,2,X,La),check(K,3,X,La)
    ,check(K,4,X,La),check(K,5,X,La),check(K,6,X,La),check(K,7,X,La)
    ,removepiece(Column)
    ;End is 0,true.

check0(X):-retractall(col(_,_)),assert(col(_,0)),
	   fact(N),not(check(N,N,X,0)),true.
/*check(2,2,X,0),check(3,3,X,0)
	  ,check(4,4,X,0),check(5,5,X,0),check(6,6,X,0),check(7,7,X,0).*/


max(K,T,Z):-K = 8,Z is T,!;K<8,col(K,A),col(T,B),Ka is K+1,A>B,max(Ka,K,Z),!;K<8,max(Ka,T,Z),!.

  /*******************************************************/
/*******************************************************/


/**********************************************/
checkWinT(K,Colmn,End):-checkcolmnT(K,Colmn),End is 1,!;
			    top(Colmn,Raw),checkrawT(K,Raw,Colmn),End is 1,!;
		           top(Colmn,Raw),checkDialogT(K,Raw,Colmn),End is 1,!;End is 0,!.

cc(Color,K):- write('\nmmm\n'),col(K,V),
              Color='Y',
	      Va is V+1,retractall(col(K,_)),assert(col(K,Va)),removepiece(K),!;

	      Color='R',
	      Va is V-1,retractall(col(K,_)),assert(col(K,Va)),removepiece(K),!.


checkcolmnT(K,Colmn):-top(Colmn,NUM),NUM > 3,piece(Colmn,NUM,Color),
down(Colmn,NUM,Color,Occr),Occr>3,cc(Color,K),!.


checkrawT(K,Raw,Colmn):-piece(Colmn , Raw , Color)
                            ,moveRight(Raw ,Colmn,Color,Ocr1),moveLeft(Raw,Colmn,Color,Ocr2),
                             RES is Ocr1+Ocr2 , RES>4 ,cc(Color,K),!.
  checkDialogT(K,Raw , Colmn):-piece(Colmn,Raw,Color) ,moveUpRight(Raw,Colmn,Color,OCR1) , moveDownLeft(Raw,Colmn,Color,OCR2),
                                          RES is OCR1 + OCR2 , RES > 4 ,cc(Color,K),!.
checkDialogT(K,Raw , Colmn):-piece(Colmn,Raw,Color) ,moveUpLeft(Raw,Colmn,Color,OCR1) , moveDownRight(Raw,Colmn,Color,OCR2),
                                          RES is OCR1 + OCR2 , RES>4 ,cc(Color,K),!.


col2(1,2).
col2(2,5).
col2(3,10).









