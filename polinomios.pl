
% Inteligencia Artificial: Proyecto 1
% Polinomios en Prolog

% Autores:
% Karen Arteaga Mendoza 190161
% Manuel Hermida Flores

% Febrero 2022

% Base de conocimientos dinámica compuesta de polinomios
%
% el predicado polynomial(A) se compone de:
% A: una lista que guarda los coeficiente del polinomio 
% en los lugares correspondientes al exponente, en orden ascendente
%
% Ejemplo: polynomial([a,b,c,d]).
% Este polinomio es de grado 3 y tiene como coeficientes a, b, c
% a es el coeficiente correspondiente a x^0
% b es el coeficiente correspondiente a x^1 ...
% De esta manera, el polinomio escrito como cadena es: a*X^0+b*X^1+c*X^2+d*X^3
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
% B1: cabeza de la lista actual del polinomio A
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
minus(polynomial(A), polynomial(B), polynomial(R)):-
	su(A, B, [], R).
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
% FALTA CAMBIAR DE SIGNO A LOS ELEMENTOS DE B 
su([], B, C, D):-
	combina(C, B, D),
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
mult(_, _, _, -1, Sa, Y):-
	Y is Sa,
	!.

% Métodos auxiliares 
%
% Caso corte:
% 
combina([], L, L):-
	!.
% agrega los elementos 
combina([X|L1], L2, [X|L3]):-
	combina(L1, L2, L3).

% toString: imprime la cadena correspondiente al polinomio A
%
% A: lista correspondiente al polinomio 
% llama al método escribe() en el grado actual 0
toString(polynomial(A)):-
	escribe(A, 0),
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
escribe([A1|A], 0):-
	write(A1),
	escribe(A, 1).
% Ea: exponente actual 
% E: exponente siguiente 
% Imprime el signo + seguido de el coeficiente A1, multiplicado por X
% elevado a la potencia del exponente actual 
% se llama recursivamente con la cola de la lista y el exponente que sigue
escribe([A1|A], Ea):-
	write("+"), write(A1), write("*"), write("X"), write("^"), write(Ea),
	E is Ea + 1,
	escribe(A, E).


% main
main():-
	write("para terminar escribe ya"),
	nl,
	siguiente(-1, []),
	!.

% I es el grado del polinomio actual
% L es la lista de coeficientes del polinomio actual 
siguiente(I, L):-
	D is I+1,
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


