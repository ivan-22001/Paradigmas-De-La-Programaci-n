natural(cero). 
natural(suc(X)) :- natural(X). 
mayorOIgual(suc(X),suc(Y)) :- mayorOIgual(X, Y).
mayorOIgual(X,X) :- natural(X).


% 1--------------------------------------
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).
hijo(X,Y) :- padre(Y,X).

hermano(X,Y) :- padre(Z,X) , padre(Z,Y), X \= Y.

descendiente(X, Y) :- padre(Y, X).  % X es descendiente de Y si Y es padre de X
descendiente(X, Y) :- padre(Z, X), descendiente(Z, Y).

ancestro(X, Y):- padre(X,Y).
ancestro(X, Y) :- padre(X,Z),ancestro(Z,Y). 


% 3 -------------------------------------------


juntar([],L,L).
juntar([X|L1],L2,[X|L3]) :- juntar(L1,L2,L3).

last([X],X).
last([X|L],Y) :- last(L,Y).

reverse([X],[X]).
reverse([X|L1],L2) :- reverse(L1,L) , juntar(L, [X], L2).

prefijo([],_).
prefijo(L1,L2) :- juntar(L1, _ , L2).

sufijo([],_).
sufijo(L1,L2) :- juntar(_,L1,L2).

sublist( [], _ ).
sublista( [X|L1], [X|L2] ) :- prefijo(L1,L2).
sublista( L, [_|XSS] ) :- sublista( L , XSS ).

pertenece(X,[X|_]).
pertenece(X,[Y|L]) :- X \= Y,pertenece(X,L) .


% Ejercicio 6 ----------------------

aplanar([],[]).
aplanar([[]|T],L):- aplanar(T,L).
aplanar([[X|L1]|L2],L3) :- aplanar([X|L1],L),aplanar(L2,L4), juntar(L,L4,L3).
aplanar([X|L1],L2):- not(is_list(X)), aplanar(L1,L),juntar([X],L,L2).

% Ejercicio 8 -------------------------

interseccion([],[],[]).
interseccion(_,[],[]).
interseccion([],_,[]).
interseccion([X|L1],L2,L) :- pertenece(X,L2), interseccion(L1,L2,L3), juntar([X],L3,L).
interseccion([X|L1],L2,L) :- not(pertenece(X,L2)), interseccion(L1,L2,L).

partir(0,L,[],L).
partir(N,[X|L],[X|L1],L2) :- N2 is N-1, partir(N2,L,L1,L2).

borrar([],_,[]).
borrar([X|L1],X,L2) :- borrar(L1,X,L2).
borrar([X|L1],Y,[X|L2]) :- X \= Y,borrar(L1,Y,L2).

sacarDuplicados([],[]).
sacarDuplicados([X|L1],[X|L2]) :-borrar(L1,X,L), sacarDuplicados(L,L2).

insertar([],[]).
insertar(X,L,L2) :- juntar(I,D,L) , juntar(I,[X|D],L2).

permutacion([],[]).
permutacion([X|L1],P) :- permutacion(L1,L), insertar(X,L,P).


% Ejercicio 10----------------------------
desde(X,X).
desde(X,Y) :- X < Y, N is X+1, desde(N,Y).


desde2(X, Y) :- nonvar(Y), !, Y >= X.
desde2(X, Y) :- var(Y), desde2_gen(X, Y).

desde2_gen(X, X).
desde2_gen(X, Y) :-
    N is X + 1,
    desde2_gen(N, Y).


% Ejercicio 12 --------------------------

vacio(nil).

raiz(bin(_,R,_),R).

altura(nil,0).
altura(bin(I,_,D),N) :- altura(I,N2) , altura(D,N3), N is max(N2,N3)+1.

cantidadDeNodos(nil,0).
cantidadDeNodos(bin(I,_,D),N) :- cantidadDeNodos(I,N1), cantidadDeNodos(D,N2), N is N2+N1+1.

inorder(nil,[]).
inorder(bin(I,R,D),L) :- inorder(I,L1), inorder(D,L2), juntar(L1,[R|L2],L).

arbolConInorder([],nil).
arbolConInorder(L,AB) :- juntar(Izq,[R|Der],L),arbolConInorder(Izq,I),arbolConInorder(Der,D),inorder(AB,L), AB = (bin(I,R,D)).

maxValue(bin(_,R,nil),R).
maxValue(bin(_,_,D),N) :- maxValue(D,N).

minValue(bin(nil,R,_),R).
minValue(bin(I,_,_),N) :- minValue(I,N).

aBB(nil).
aBB(bin(nil,R,nil)).
aBB(bin(I,R,nil)) :- maxValue(I,Ir), R > Ir, aBB(I).
aBB(bin(nil,R,D)) :- minValue(D,Dr), R < Dr, aBB(D).
aBB(bin(I,R,D)) :- minValue(D,Dr), R < Dr, maxValue(I,Ir), R > Ir, aBB(I),aBB(D).