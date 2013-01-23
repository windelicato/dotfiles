% A small Prolog file to test prolog.vim
%
%

head :- body.

:- use_module  foobar(faoeu[aoeu]).
:- dynamic foobar/2.

:- statement.
:- statement(x,y,_X,Y,foo(x,Y)).
?- query.
?- query(x,y,_X,Y,foo(x,Y)).

head(_) :- body(foo,Foo,foo(foo),_foo).

head((foobar,baz)booh,X,foo(booh,X)) :- body.
head(booh,X,foo(booh,X)) :- body(foo,Foo,foo(foo),_foo).

head --> body >>.
head --> body(foo,Foo,foo(foo)).

head(booh,X,foo(booh,X)) --> body.
head(booh,X,foo(booh,X)) --> foo,
	body(foo,Foo,foo(foo)), {foo(bar)}.

:- dynamic foo/2.

foobar.

foobar :- !, \+ ,  =@= =:= << >>.

head :- foo(C), foo(B),
	/* something commentary 
	*
	* */

	% more comments
	\+ foo(G).

/* a multiline
* comment
*/

% another multiline comment
% consisting of a block of normal comments

head :- Error(foo), _error(bar). /*predicates must start lower-case*/

interpret((Index,Old),New,World) :-
	foobar (
; uaoeu
).

head :- (error(1); aoeuoe
;  aoeu
	oouaoeu
;
	aoeuaoe
	/*
	* aoeuaoe
	*/
;
	aoeu
% aoeuae
% aoeu
)error(2). /*no comma and a TODO*/

Error
_error

head :- body, Ts =:= =.. [Foo|Bar].

head :- list([a,b,C,foo:Bar|Tail],Foo).
head :- dlist(Huuu-Ts).

head :- foobar('string').
head :- format('fooo ~n bar').

% numbers
head :- X is 2 + 3.4.
