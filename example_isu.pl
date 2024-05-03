:- use_module(ttr).

% Game of fetch, corresponding to Cooper (2023, p.55)
f([agenda=[]:list(rec_type)],
  [agenda=[[e:pick_up(a, c)]]:list(rec_type)]).
f([agenda=[[e:pick_up(a, c)]]:list(rec_type)],
  f([e:pick_up(a, c)],
    [agenda=[[e:attract_attention(a, b)]]:list(rec_type)])).

% Corresponds to Cooper (2023, p. 61), 54
% TODO

% Corresponds to Cooper (2023, p. 61), 55b
s(N, S) :-
    N >= 1,
    f(T_prev, T),
    has_type(T, type),
    N_prev is N - 1,
    s(N_prev, S_prev),
    has_type(S_prev, T_prev),
    has_type(S, T).

s(0, [agenda=[]]).
