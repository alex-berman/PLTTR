:- use_module(ttr).

% Game of fetch, corresponding to Cooper (2023, p.55)
function(
    [agenda=[]:list(rec_type)],
    [agenda=[[e:pick_up(a, c)]]:list(rec_type)]).
function(
    [agenda=[[e:pick_up(a, c)]]:list(rec_type)],
    function(
	[e:pick_up(a, c)],
	[agenda=[[e:attract_attention(a, b)]]:list(rec_type)])).

% Corresponds to Cooper (2023, p. 61), 54
%has_type(first_on_agenda, T) :-
%    has_type(s(_), [agenda=),
    

% Corresponds to Cooper (2023, p. 61), 55b
s(N, V) :-
    N >= 1,
    N_prev is N - 1,
    s(N_prev, S_prev),
    has_type(S_prev, T_prev),
    function(T_prev, T),
    T \= function(_, _),
    has_type(V, T).

s(0, [agenda=[]]).
