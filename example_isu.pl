:- use_module(ttr).
:- dynamic current_perceived_object/2, state/2, pending_perception/2, pending_action/2.


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


% action_rule(+Name, +Agent, -Precondition, -Effect)
% Unify an action rule's precondition and effect for a given agent.

% Corresponds to Cooper (2023, p. 61), 54
action_rule(
	eventCreation,
	Agent,
	( \+ current_perceived_object(Agent, _),
	  state(Agent, [S|_]),
	  S = [agenda=[Fst|_]]
	),
	assert(pending_action(Agent, create(Fst)))
    ).

% Corresponds to Cooper (2023, p. 61), 55a
action_rule(
	eventBasedUpdate,
	Agent,
	( state(Agent, [S_prev|_]),
	  r(R),
	  update_function(R, fun(T_prev, fun(EventType, T))),
	  has_type(S_prev, T_prev),
	  current_perceived_object(Agent, Event),
	  has_type(Event, EventType)
	),
	( has_type(S, T),
	  retract(state(Agent, [S_prev|S_tail])),
	  assert(state(Agent, [S, S_prev|S_tail]))
	)
    ).

% Corresponds to Cooper (2023, p. 61), 55b
action_rule(
	tacitUpdate,
	Agent,
	( state(Agent, [S_prev|_]),
	  r(R),
	  update_function(R, fun(T_prev, T)),
	  T \= fun(_, _),
	  has_type(S_prev, T_prev)
	),
	( has_type(S, T),
	  retract(state(Agent, [S_prev|S_tail])),
	  assert(state(Agent, [S, S_prev|S_tail]))
	)).


update_state(Agent) :-
    forall(retract(pending_perception(Agent, Object)),
	   ( assert(current_perceived_object(Agent, Object)),
	     apply_rules(Agent),
	     retract(current_perceived_object(Agent, Object))
	   )),
    apply_rules(Agent).


apply_rules(Agent) :-
    write('  current perceived object: '),
    ( current_perceived_object(Agent, Object) ->
	  write(Object) ;
      write(none) ), nl,
    forall(action_rule(RuleName, Agent, Precondition, Effect),
	   ( Precondition ->
		 ( write('  applying '), write(RuleName), nl,
		   Effect,
		   print_agent_internals(Agent)
		 )
	    ; true )).


perceive(Agent, Object) :-
    assert(pending_perception(Agent, Object)).


agent(0).
agent(1).


handle_action(create(Type)) :-
    create_event_in_world(Type).


create_event_in_world(Type) :-
    % We assume that any event can be created at any time, regardless of the state of the world. We also assume that
    % when a event is created in the world, all agents immediately perceive it.
    create(Type, Event),
    forall(agent(Agent), perceive(Agent, Event)).



print_agent_header(A) :-
    write('agent '), write(A), nl.


print_agent_internals(Agent) :-
    write('  state: '),
    state(Agent, [S|Tail]),
    ( Tail = [] -> true ; write('..., ') ),
    write(S),
    nl,
    findall(Action, pending_action(Agent, Action), PendingActions),
    write('  pending actions: '), write(PendingActions), nl.


clear_dynamic_facts :-
    retractall(current_perceived_object(_, _)),
    retractall(state(_, _)),
    retractall(pending_perception(_, _)),
    retractall(pending_action(_, _)).


main :-
    clear_dynamic_facts,
    forall(agent(Agent), assert(state(Agent, [[agenda=[]]]))),
    process_time_steps(0, 20).


process_time_steps(T, MaxT) :-
    forall(agent(Agent),
	   ( print_agent_header(Agent),
	     ( T = 0 -> print_agent_internals(Agent) ; true ),
	     update_state(Agent),
	     forall(retract(pending_action(Agent, Action)),
		    handle_action(Action))
	   )),
    write('----------------------------------------------------------------------'), nl, nl,
    ( T < MaxT ->
	  T1 is T + 1,
	  process_time_steps(T1, MaxT)
     ; true ).



		      
