% Game of fetch, corresponding to Cooper (2023, p.55)

btype(ind).
pred(pick_up, [ind, ind]).

%% update_function(
%% 	R,
%% 	rec_type({agenda: singleton_type(list_type(type), [])}),
%% 	rec_type({agenda: singleton_type(list_type(type), [
%% 					     rec_type({e: pick_up(a, c))

function(
	_R:[agenda=[]:list(rec_type)],
	[agenda=[[e:pick_up(a, c)]]:list(rec_type)]).


% Corresponds to Cooper (2023, p. 61), 54
%% action_rule(
%% 	S:[agenda:[fst:rec_type,
%% 		   rst:list(rec_type)]],
%% 	create(S/agenda/fst)).

% Corresponds to Cooper (2023, p. 61), 55b
action_rule(
	[function(V:T, Type), CurrentState:T],
	[apply(function(V:T, Type), CurrentState, ResultingType), next_state:ResultingType]).



get_next_state :-
    action_rule(Preconds, Effects),
    test_preconds(Preconds),
    apply_effects(Effects).


has_type(current_state, [agenda=[]:list(rec_type)]).


:- get_next_state, has_type(next_state, T), write('Next state: '), write(T), nl.
