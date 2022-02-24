

:- dynamic polynomial/1.

repite.
repite:-
	repite.

max(A, B, A):-
	A > B, !.
max(_, B, B).

min(A, B, A):-
	A < B, !.
min(_, B, B).

len([], 0).
len([_|A], L):-
	len(A, L2),
  	L is L2 + 1.

degree(polynomial([_|A]), Deg):-
	len(A, L),
  	Deg is L.
  
plus(polynomial(A), polynomial(B), polynomial(S)):-
	add(A, B, [], S).
add(A, [], C, D):-
	combina(C, A, D),
	!.
add([], B, C, D):-
	combina(C, B, D),
	!.
add([A1|A], [A2|B], C, D):-
	C1 is A1 + A2,
	combina(C, [C1], E),
	add(A, B, E, D).

minus(polynomial(A), polynomial(B), polynomial(S)):-
	su(A, B, [], S).
su(A, [], C, D):-
	combina(C, A, D),
	!.
su([], B, C, D):-
	combina(C, B, D),
	!.
su([A1|A], [A2|B], C, D):-
	C1 is A1 - A2,
	combina(C, [C1], E),
	su(A, B, E, D).

evaluate(polynomial(A), X, Y):-
	degree(polynomial(A), D),
	mult(A, X, 0, D, 0, Y),
	!.
mult([A1|A], X, 0, Da, 0, Y):-
	D is Da - 1, 
	S is A1,
	mult(A, X, 1, D, S, Y).
mult([A1|A], X, Ea, Da, Sa, Y):-
	S is Sa + A1*(X**Ea),
	E is Ea + 1,
	D is Da - 1,
	mult(A, X, E, D, S, Y).
mult(_, _, _, -1, Sa, Y):-
	Y is Sa,
	!.

combina([], L, L):-
	!.
combina([X|L1], L2, [X|L3]):-
	combina(L1, L2, L3).


toString(polynomial(A)):-
	escribe(A, 0),
	!.
escribe([], _):- !.
escribe([A1|A], 0):-
	write(A1),
	escribe(A, 1).
escribe([A1|A], D):-
	write("+"), write(A1), write("*"), write("X"), write("^"), write(D),
	Dn is D + 1,
	escribe(A, Dn).

main():-
	write("para terminar escribe ya"),
	nl,
	siguiente(-1, []),
	!.
siguiente(I, L):-
	D is I+1,
	write("x^"),
	write(D),
	write(": "), read(Ax), agrega(Ax, D, L).
agrega(ya, _, P):-
	asserta(polynomial(P)),
	toString(polynomial(P)),
	!.
agrega(Ax, D, L):-
	combina(L, [Ax], P),
	write(P), nl,
	siguiente(D, P).


