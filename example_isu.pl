:- use_module(ttr).
:- dynamic current_perceived_object/2, state/2, pending_perception/2, pending_action/2.


% Game of fetch, corresponding to Cooper (2023, p.60)

% R is a dict containing role assignments for h (human), d (dog), and s (stick).
update_function(
	R, fun(_, [agenda=[]:list(rec_type)],
	       [agenda=[[e:pick_up(R.h, R.s)]]:list(rec_type)])).
update_function(
	R, fun(_, [agenda=[[e:pick_up(R.h, R.s)]]:list(rec_type)],
	       fun(_, [e:pick_up(R.h, R.s)],
		   [agenda=[[e:attract_attention(R.h, R.d)]]:list(rec_type)]))).
update_function(
	R, fun(_, [agenda=[[e:attract_attention(R.h, R.d)]]:list(rec_type)],
	       fun(_, [e:attract_attention(R.h, R.d)],
		   [agenda=[[e:throw(R.h, R.s)]]:list(rec_type)]))).
update_function(
	R, fun(_, [agenda=[[e:throw(R.h, R.s)]]:list(rec_type)],
	       fun(_, [e:throw(R.h, R.s)],
		   [agenda=[[e:run_after(R.d, R.s)]]:list(rec_type)]))).
update_function(
	R, fun(_, [agenda=[[e:run_after(R.d, R.s)]]:list(rec_type)],
	       fun(_, [e:run_after(R.d, R.s)],
		   [agenda=[[e:pick_up(R.d, R.s)]]:list(rec_type)]))).
update_function(
	R, fun(_, [agenda=[[e:pick_up(R.d, R.s)]]:list(rec_type)],
	       fun(_, [e:pick_up(R.d, R.s)],
		   [agenda=[[e:return(R.d, R.s, R.h)]]:list(rec_type)]))).
update_function(
	R, fun(_, [agenda=[[e:return(R.d, R.s, R.h)]]:list(rec_type)],
	       fun(_, [e:return(R.d, R.s, R.h)],
		   [agenda=[]:list(rec_type)]))).


roles(_{h: h1,
	d: d1,
	s: s1}).


% action_rule(+Name, +Agent, -Precondition, -Effect)
% Unify an action rule's precondition and effect for a given agent.

% Corresponds to Cooper (2023, p. 61), 54, but with the difference that the creation act uses the information state
% type rather than the state as such. (s_{i,A} : T   T=[agenda:...] ~ : T.agenda.fst!)
action_rule(
	eventCreation,
	Agent,
	( \+ current_perceived_object(Agent, _),
	  state(Agent, [T|_]),
	  T = [agenda=[Fst|_]:list(rec_type)]
	),
	assert(pending_action(Agent, create(Fst)))
    ).

% Corresponds to Cooper (2023, p. 61), 55a
action_rule(
	eventBasedUpdate,
	Agent,
	( state(Agent, [T_prev|_]),
	  roles(R),
	  has_type(S_prev, T_prev),
	  update_function(R, fun(S_prev, T_prev, fun(Event, EventType, T))),
	  current_perceived_object(Agent, Event),
	  has_type(Event, EventType)
	),
	( retract(state(Agent, [T_prev|S_tail])),
	  assert(state(Agent, [T, T_prev|S_tail]))
	)
    ).

% Corresponds to Cooper (2023, p. 61), 55b
action_rule(
	tacitUpdate,
	Agent,
	( state(Agent, [T_prev|_]),
	  roles(R),
	  update_function(R, fun(_, T_prev, T)),
	  T \= fun(_, _, _)
	),
	( retract(state(Agent, [T_prev|T_tail])),
	  assert(state(Agent, [T, T_prev|T_tail]))
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


initial_state_type([agenda=[]:list(rec_type)]).


main :-
    clear_dynamic_facts,
    initial_state_type(T0),
    forall(agent(Agent), assert(state(Agent, [T0]))),
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



		      
