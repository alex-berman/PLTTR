:- use_module(ttr).

% Game of fetch, corresponding to Cooper (2023, p.55)
function([agenda=[]:list(rec_type)],
	 [agenda=[[e:pick_up(a, c)]]:list(rec_type)]).
function([agenda=[[e:pick_up(a, c)]]:list(rec_type)],
	 function([e:pick_up(a, c)],
		  [agenda=[[e:attract_attention(a, b)]]:list(rec_type)])).
function([agenda=[[e:attract_attention(a, b)]]:list(rec_type)],
	 function([e:attract_attention(a, b)],
		  [agenda=[[e:throw(a, c)]]:list(rec_type)])).
function([agenda=[[e:throw(a, c)]]:list(rec_type)],
	 function([e:throw(a, c)],
		  [agenda=[[e:run_after(b, c)]]:list(rec_type)])).
function([agenda=[[e:run_after(b, c)]]:list(rec_type)],
	 function([e:run_after(b, c)],
		  [agenda=[[e:pick_up(b, c)]]:list(rec_type)])).
function([agenda=[[e:pick_up(b, c)]]:list(rec_type)],
	 function([e:pick_up(b, c)],
		  [agenda=[[e:return(b,c,a)]]:list(rec_type)])).
function([agenda=[[e:return(b, c, a)]]:list(rec_type)],
	 function([e:return(b, c, a)],
		  [agenda=[]:list(rec_type)])).

% Corresponds to Cooper (2023, p. 61), 54
e(E, S) :-
    has_type(S, [agenda=[Fst|_]:list(rec_type)]),
    has_type(E, Fst).

% Corresponds to Cooper (2023, p. 61), 55a
s(N, S) :-
    N >= 1,
    function(T_prev, function(EventType, T)),
    N_prev is N - 1,
    s(N_prev, S_prev),
    has_type(S_prev, T_prev),
    e(E, S_prev),
    has_type(E, EventType),
    has_type(S, T).

% Corresponds to Cooper (2023, p. 61), 55b
s(N, S) :-
    N >= 1,
    function(T_prev, T),
    T \= function(_, _),
    N_prev is N - 1,
    s(N_prev, S_prev),
    has_type(S_prev, T_prev),
    has_type(S, T).

s(0, [agenda=[]]).
