interpret([],[]) :- !.
interpret([X|_], R) :- \+ (X = a), !. 
interpret([a,a|T], R) :- interpret([a|T], R1), interpret([a|R1], R), !.
interpret([a, d, a, a|T], Y) :- interpret([a|T], Y1), interpret([a,d,a|Y1], Y), !.
interpret([a, d, a, A, B|T], [p(A,B)|T]) :- \+(A = a), \+(B = a), !.
interpret([a,d,a,X,a|T], R) :- interpret([a|T],R1), interpret([a,d,a,X|R1], R), !.
interpret([a,p(C,D)|T], R) :- interpret(T,R1), interpret([a,C,D|R1],R), !.
interpret([a,X,a|T], R) :- interpret([a|T],R1), interpret([a,X|R1], R), !.

interpret([a,A,B,a|T], R) :- interpret([a, A, B], R1), interpret([a|T], R2), append(R1, R2, R), !.
interpret([a,k,X|T], [k(X)|T]) :- !. 
interpret([a, k(X),Y|T], [X|T]) :- !.
interpret([a,s,X|T], [s(X)|T]) :- !.
interpret([a,s(X),Y|T], [s(X,Y)|T]) :- !.
interpret([a, s(X,Y),A|T], R) :- interpret([a,a,X,A,a,Y,A|T], R), !.
interpret([a,i,X|T], [X|T]) :- !.
interpret([a,v,X|T], [v|T]) :- !.
interpret([a, dot(X), Y|T], [Y|T]) :- write(X), !.
interpret([a, r, Y|T], [Y|T]) :- nl, !.

helper([],[]).
helper([96|T], [a|R]) :- helper(T,R), !.
helper([46,120,Y|T],[dot(X)|R]) :- name(X,[Y]), helper(T,R), !.
helper([A|B], [C|D]) :- name(C, [A]), helper(B,D), !.

interpretFromText(A,B) :- helper(A, C), interpret(C,B),  !. 



brackets(X, [], []).
brackets(X, [a|T], [a|R]) :- brackets(X,T, R), !.
brackets(X, [X|T], [i|R]) :- brackets(X,T,R), !.
brackets(X, [Y|T], [a, k, Y|R]) :- brackets(X,T,R), !.

unlambdafy(var(A), [A]).
unlambdafy(app(X,Y), [a|T]) :- unlambdafy(X,T1), unlambdafy(Y,T2), append(T1,T2,T), !.
unlambdafy(abs(var(X),E), R) :- unlambdafy(E,R1), brackets(X,R1,R), !. 
unlambdafy(func(r), [r]) :- !.
unlambdafy(func(dot(A)), [dot(A)]) :- !.
unlambdafy(func(v), [v]) :- !.
unlambdafy(func(d), [d]) :- !.
