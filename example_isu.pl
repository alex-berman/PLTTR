% Game of fetch, corresponding to Cooper (2023, p.55)
function(
	[agenda=[]:list(rec_type)],
	[agenda=[[e:pick_up(a, c)]]:list(rec_type)]).

% Corresponds to Cooper (2023, p. 61), 55b
has_type(s(N), TypeN) :-
    N >= 1,
    N_minus_1 is N - 1,
    has_type(s(N_minus_1), T),
    function(T, TypeN).

has_type(s(0), [agenda=[]:list(rec_type)]).
