% Inteligencia Artificial: Proyecto 1
% Polinomios en Prolog

% Autores:
% Karen Arteaga Mendoza 190161
% Manuel Hermida Flores 189702

% Febrero 2022

% Base de conocimientos dinámica compuesta de polinomios
%
% el predicado polynomial(A) se compone de:
% A: una lista que guarda los coeficiente del polinomio 
% en los lugares correspondientes al exponente, en orden ascendente
%
% Ejemplo: polynomial([a,b,c,d]).
% Este polinomio es de grado 3 y tiene como coeficientes a, b, c, d
% a es el coeficiente correspondiente a x^0
% b es el coeficiente correspondiente a x^1 ...
% De esta manera, el polinomio escrito como cadena es: dX^3 + cX^2 + bX + d
%

:- dynamic polynomial/1.

% Polinomio cero
polynomial([0]).


% Longitud de una lista : calcula la longitud de la lista correspondiente
%
% Caso base:
% Cuando la lista es vacía la longitud es 0
len([], 0).
% [_|A]: lista con cabeza indiferente de la que se quiere saber la longitud
% L: contador que incrementa cada que se llama al mismo metodo con un elemento menos
len([_|A], L):-
	len(A, L2),
  	L is L2 + 1.


% Grado de un polinomio: calcula el grado del polinomio correspondiente 
%
% A: lista de la que se quiere saber la longitud, excluye la cabeza 
% Deg: variable en la que se guarda el grado del polinomio
degree(polynomial([_|A]), Deg):-
	len(A, L),
  	Deg is L.

% Suma de dos polinomios: calcula en un polinomio S la suma de los polinomios A y B
%
% llama al metodo add() con las listas correspondientes a los polinomios A, B y S
plus(polynomial(A), polynomial(B), polynomial(S)):-
	add(A, B, [], S).
%
% metodos auxiliares para la suma
%
% Caso corte 1: 
% Cuando  la lista B es vacía, combina las listas C y A en la lista D 
% Ocurre cuando el polinomio A es de grado mayor
add(A, [], C, D):-
	combina(C, A, D),
	!.
% Caso corte 2: 
% Cuando  la lista A es vacía, combina las listas C y B en la lista D
% Ocurre cuando el polinomio B es de grado mayor
add([], B, C, D):-
	combina(C, B, D),
	!.
% A1: cabeza de la lista actual del polinomio A
% B1: cabeza de la lista actual del polinomio B
% C: lista actual de elementos sumados 
% C1: nueva cabeza de C
% combina a C y C1 en una lista E que pasa como nueva lista en la llamada recursiva
add([A1|A], [B1|B], C, D):-
	C1 is A1 + B1,
	combina(C, [C1], E),
	add(A, B, E, D).

% Resta de dos polinomios: calcula en un polinomio R la resta de los polinomios A y B
%
% llama al metodo su() con las listas correspondientes a los polinomios A, B y S
minus(polynomial(A), polynomial(B), polynomial(P)):-
	su(A, B, [], P).
%
% Metodos auxiliares para la resta
%
% Caso corte 1: 
% Cuando  la lista B es vacía, combina las listas C y A en la lista D 
% Ocurre cuando el polinomio A es de grado mayor
su(A, [], C, D):-
	combina(C, A, D),
	!.
% Caso corte 2: 
% Cuando la lista A es vacía, combina las listas C y B en la lista D 
% Ocurre cuando el polinomio B es de grado mayor 
% llama al método cambia para invertir el signo de los elementos de B
su([], B, C, D):-
	cambia(B, [], B2),
	combina(C, B2, D),
	!.
% A1: cabeza de la lista actual del polinomio A
% B1: cabeza de la lista actual del polinomio A
% C: lista actual de elementos restados 
% C1: nueva cabeza de C
% combina a C y C1 en una lista E que pasa como nueva lista en la llamada recursiva
su([A1|A], [B1|B], C, D):-
	C1 is A1 - B1,
	combina(C, [C1], E),
	su(A, B, E, D).

% Derivada: Obtiene la derivada del polinomio [A1 | A] y lo guarda en el polinomio L
%
% llama al método deriva con 
% A: los coeficientes con grado mayor a 0
% 1: siendo el grado actual
% []: lista donde se irán guardando los nuevos coeficientes
% L: lista donde se guardará el resultado final
differentiate(polynomial([_ | A]), polynomial(L)):-
	deriva(A, 1, [], L).
%
% Metodos auxiliares para la derivada
%
% Caso corte 1: 
% Cuando  la lista es vacía, iguala el resultado a la lista creada recursivamente
deriva([], _, S, S):-
	!.
%
% A1: es el coeficiente del término a derivar
% A: coeficientes restantes
% X: grado del coeficiente a derivar
% D: nuevo coeficiente
% Se llama a combina para juntar el nuevo coeficiente con los pasados en L2, usando deriva recursivamente
deriva([A1 | A], X, S, L):-
	D is A1 * X,
	X2 is (X + 1), 
	combina(S, [D] , L2),
	deriva(A, X2, L2, L).


% Evaluación de un polinomio: Evalúa el polinomio A con el valor X y guarda el resultado en Y
% 
% llama al metodo degree() y guarda el grado del polinomio en D
% llama al metodo mult() con:
% A: la lista correspondiente al polinomio 
% X: el valor en el que se quiere evaluar
% 0: el exponente en que se evalua el primer término 
% D: el grado del polinomio 
% 0: la suma actual del resultado
% Y: variable a la que se asignará el resultado
evaluate(polynomial(A), X, Y):-
	degree(polynomial(A), D),
	mult(A, X, 0, D, 0, Y),
	!.
%
% Metodos auxiliares de la evaluación
%
% Caso base: 
% Cuando el exponente es cero la suma es 0 
% D: es el contador del grado que va decreciendo con cada llamada recursiva
mult([A1|A], X, 0, Da, 0, Y):-
	D is Da - 1, 
	S is A1,
	mult(A, X, 1, D, S, Y).
% Ea: exponente actual, Da: grado actual, Sa: suma actual
% S: suma resultante de la evaluación del polinomio hasta el exponente actual
% E: exponente siguiente 
% D: contador decreciente 
mult([A1|A], X, Ea, Da, Sa, Y):-
	S is Sa + A1*(X**Ea),
	E is Ea + 1,
	D is Da - 1,
	mult(A, X, E, D, S, Y).
% Caso corte: 
% Cuando el contador D es -1 se asigna el valor de la suma actual 
% a la variable de retorno Y 
mult(_, _, _, -1, Sa, Sa):-
	!.


% Multiplicación
times(polynomial(A), polynomial(B), polynomial(C)):-
	multiplica(A, B, [], C).

multiplica([], _, C, C):-
	!.

multiplica(_, [], C, C):-
	!.

multiplica(A, [B1 | B], C, D):-
	multCoef(A, B1, [] , R),
	add(R, C, [], Res),
	multiplica([0 | A], B, Res, D).

multCoef([], _, C, C):-
	!.

multCoef([A1 | A], Bi, C, R):-
	D is A1 * Bi,
	combina(C, [D], Ci),
	multCoef(A, Bi, Ci, R).

% Composicion
compose(polynomial([A1 | A]), polynomial(B), polynomial(C)):-
	composicion([A1 | A], B, C).

composicion([], _, []):-
	!.

composicion([A1 | A], B, C):-
	composicion(A, B, Pend),
	multiplica(B, Pend, [], M),
	add([A1], M, [], C).


% toString: imprime la cadena correspondiente al polinomio A
%
% A: lista correspondiente al polinomio 
% llama al método escribe() en el grado actual 0
toString(polynomial(A)):-
	invertir(A, [], [I1|I]),
	degree(polynomial(A), D),
	write(I1), write("X"), write("^"), write(D),
	Ds is D - 1,
	escribe(I, Ds),
	!.
%
% Métodos auxiliares toString
% 
% Caso corte:
% Cuando la lista es vacía, regresa
escribe([], _):- !.
% Caso base:
% Cuando el exponente es cero, escribe la variable independiente del polinomio
% A1: cabeza de la lista actual
% pasa la cola de la lista y el exponente 1 al método escribe()
escribe([A1|_], 0):-
	A1 > 0,
	write(" + "), write(A1),
	escribe([], -1).

escribe([A1|_], 0):-
	A1 < 0,
	P is A1 * (-1), write(" - "), write(P),
	escribe([], -1).

escribe([A1|A], 1):-
	A1 > 0,
	write(" + "), write(A1), write("X"),
	escribe(A, 0).

escribe([A1|A], 1):-
	A1 < 0,
	P is A1 * (-1), write(" - "), write(P), write("X"),
	escribe(A, 0).
escribe([0|A], Ea):-
	E is Ea - 1,
	escribe(A, E).
%
% Ea: exponente actual 
% E: exponente siguiente 
% Imprime el signo + seguido de el coeficiente A1, multiplicado por X
% elevado a la potencia del exponente actual 
% se llama recursivamente con la cola de la lista y el exponente que sigue
escribe([A1|A], Ea):-
	A1 > 0,
	write(" + "), write(A1), write("X"), write("^"), write(Ea),
	E is Ea - 1,
	escribe(A, E).

escribe([A1|A], Ea):-
	A1 < 0,
	P is A1 * (-1),
	write(" - "), write(P), write("*"), write("X"), write("^"), write(Ea),
	E is Ea - 1,
	escribe(A, E).


% Métodos auxiliares de las listas 
%
% Combina dos listas L1 y L2 en una tercera L3
combina([], L, L):-
	!.
combina([X|L1], L2, [X|L3]):-
	combina(L1, L2, L3).
%
% Invierte una lista [A1|A] y la guarda en C
invertir([], B, B).
invertir([A1|A], B, C):- 
	invertir(A, [A1|B], C).
% Cambia el signo de los elementos de una lista B y los guarda en C
cambia([], C, C):-
	!.
cambia([B1|B], C, B2):-
	N is B1 * (-1),
	combina(C, [N], D),
	cambia(B, D, B2).



% Main 
main():-
	write("Inteligencia Artificial"), nl,
	write("Proyecto 1: Polinomios"), nl,
	write("	- Karen Arteaga Mendoza"), nl,
	write("	- Manuel Hermida Flores"), nl,
	nl,

	asserta(polynomial([1, 2, 3, 4])),
	asserta(polynomial([5, 0, 3])),

	plus(polynomial([1, 2, 3, 4]), polynomial([5, 0, 3]), polynomial(S)),
	times(polynomial([1, 2, 3, 4]), polynomial([5, 0, 3]), polynomial(T)),
	compose(polynomial([1, 2, 3, 4]), polynomial([5, 0, 3]), polynomial(C)),
	minus(polynomial([0]), polynomial([1, 2, 3, 4]), polynomial(M)),
	evaluate(polynomial([1, 2, 3, 4]), 3, E),
	differentiate(polynomial([1, 2, 3, 4]), polynomial(D)),
	differentiate(polynomial(D), polynomial(D2)),

	asserta(polynomial(S)),
	asserta(polynomial(T)),
	asserta(polynomial(C)),
	asserta(polynomial(M)),
	asserta(polynomial(D)),
	asserta(polynomial(D2)),

	write("p(x)= "), toString(polynomial([1, 2, 3, 4])), nl,
	write("q(x)= "), toString(polynomial([5, 0, 3])), nl, 
	write("p(x) + q(x) = "), toString(polynomial(S)), nl,
	write("p(x) * q(x) = "), toString(polynomial(T)), nl,
	write("p(q(x)) = "), toString(polynomial(C)), nl,
	write("0 - p(x) = "), toString(polynomial(M)), nl,
	write("p(3) = "), write(E), nl,
	write("p'(x) = "), toString(polynomial(D)), nl,
	write("p''(x) = "), toString(polynomial(D2)), nl.

% Instanciar polinomios a la base de conocimientos 
instancia():-
	write("para terminar escribe ya"),
	nl,
	siguiente(-1, []),
	!.
% I es el grado del polinomio actual
% L es la lista de coeficientes del polinomio actual 
siguiente(I, L):-
	D is I + 1,
	write("x^"),
	write(D),
	write(": "), read(Ax), agrega(Ax, D, L).
% para el metodo recursivo y agrega el polinomio actual a la base de conocimiento
agrega(ya, _, P):-
	asserta(polynomial(P)),
	toString(polynomial(P)),
	!.
% agrega el nuevo coeficiente a la lista y pide el siguiente dato
agrega(Ax, D, L):-
	combina(L, [Ax], P),
	write(P), nl,
	siguiente(D, P).









