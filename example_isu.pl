:- use_module(ttr).
:- dynamic current_event/2, states/2.


% Game of fetch, corresponding to Cooper (2023, p.60)
update_function(
	R, fun([agenda=[]:list(rec_type)],
	       [agenda=[[e:pick_up(R.h, R.s)]]:list(rec_type)])).
update_function(
	R, fun([agenda=[[e:pick_up(R.h, R.s)]]:list(rec_type)],
	       fun([e:pick_up(R.h, R.s)],
		   [agenda=[[e:attract_attention(R.h, R.d)]]:list(rec_type)]))).
update_function(
	R, fun([agenda=[[e:attract_attention(R.h, R.d)]]:list(rec_type)],
	       fun([e:attract_attention(R.h, R.d)],
		   [agenda=[[e:throw(R.h, R.s)]]:list(rec_type)]))).
update_function(
	R, fun([agenda=[[e:throw(R.h, R.s)]]:list(rec_type)],
	       fun([e:throw(R.h, R.s)],
		   [agenda=[[e:run_after(R.d, R.s)]]:list(rec_type)]))).
update_function(
	R, fun([agenda=[[e:run_after(R.d, R.s)]]:list(rec_type)],
	       fun([e:run_after(R.d, R.s)],
		   [agenda=[[e:pick_up(R.d, R.s)]]:list(rec_type)]))).
update_function(
	R, fun([agenda=[[e:pick_up(R.d, R.s)]]:list(rec_type)],
	       fun([e:pick_up(R.d, R.s)],
		   [agenda=[[e:return(R.d, R.s, R.h)]]:list(rec_type)]))).
update_function(
	R, fun([agenda=[[e:return(R.d, R.s, R.h)]]:list(rec_type)],
	       fun([e:return(R.d, R.s, R.h)],
		   [agenda=[]:list(rec_type)]))).


% A dict containing individuals in the roles of human, dog and stick
r(_{h: h1,
    d: d1,
    s: s1}).


% action_rule(+Agent, -Preconds, -Effects)

% Corresponds to Cooper (2023, p. 61), 54
action_rule(
	A,
	[ \+ current_event(A, _),
	  states(A, [S|_]),
	  S = [agenda=[Fst|_]]
	],
	[ create_event_in_world(Fst) ]).

% Corresponds to Cooper (2023, p. 61), 55a
action_rule(
	A,
	[ states(A, [S_prev|_]),
	  r(R),
	  update_function(R, fun(T_prev, fun(EventType, T))),
	  has_type(S_prev, T_prev),
	  current_event(A, E),
	  has_type(E, EventType)
	],
	[ has_type(S, T),
	  retract(states(A, [S_prev|S_tail])),
	  assert(states(A, [S, S_prev|S_tail])),
	  retract(current_event(A, E))
	]).

% Corresponds to Cooper (2023, p. 61), 55b
action_rule(
	A,
	[ states(A, [S_prev|_]),
	  r(R),
	  update_function(R, fun(T_prev, T)),
	  T \= fun(_, _),
	  has_type(S_prev, T_prev)
	],
	[ has_type(S, T),
	  retract(states(A, [S_prev|S_tail])),
	  assert(states(A, [S, S_prev|S_tail]))
	]).


update_state(A) :-
    forall(action_rule(A, Preconds, Effects),
	   ( test_preconds(Preconds) ->
		 apply_effects(Effects)
	    ; true )).


test_preconds([]).
test_preconds([P|Ps]) :-
    P,
    test_preconds(Ps).

apply_effects([]).
apply_effects([E|Es]) :-
    E,
    apply_effects(Es).


agent(0).
agent(1).


create_event_in_world(Type) :-
    % We assume that when a event is created in the world, all agents immediately perceive it.
    create(Type, E),
    forall(agent(A), (
	       retractall(current_event(A, _)),
	       assert(current_event(A, E))
	   )).


print_agent_internals :-
    forall(agent(A), (
	       write('agent '), write(A), nl,
	       write('  current state: '),
	       states(A, [S|_]),
	       write(S), nl,
	       write('  current event: '),
	       ( current_event(A, E) ->
		     write(E)
		; write('none')
	       ), nl,
	       write('----------------------------------------------------------------------'), nl, nl
	   )).


repeat(0, _).
repeat(N, Goal) :-
    N > 0,
    call(Goal),
    N_minus_1 is N - 1,
    repeat(N_minus_1, Goal).


main :-
    retractall(current_event(_, _)),
    retractall(states(_, _)),
    forall(agent(A),
	   assert(states(A, [[agenda=[]]]))),
    print_agent_internals,
    repeat(20, (
	       forall(agent(A),
		      update_state(A)),
	       print_agent_internals
	   )).
