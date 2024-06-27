:- use_module(ttr).
:- dynamic current_perceived_object/2, state/2, pending_perception/2, pending_action/2, fun/3.


% Game of fetch, corresponding to Cooper (2023, p.60)

% R is a dict containing role assignments for h (human), d (dog), and s (stick).
update_function(
	R, fun(_:rectype([agenda=[]:list(rectype(_))]),
	       rectype([agenda=[rectype([e:pick_up(R.h, R.s)])]:list(rectype(_))]))).
update_function(
	R, fun(_:rectype([agenda=[rectype([e:pick_up(R.h, R.s)])]:list(rectype(_))]),
	       fun(_:rectype([e:pick_up(R.h, R.s)]),
		   rectype([agenda=[rectype([e:attract_attention(R.h, R.d)])]:list(rectype(_))])))).
update_function(
	R, fun(_:rectype([agenda=[rectype([e:attract_attention(R.h, R.d)])]:list(rectype(_))]),
	       fun(_:rectype([e:attract_attention(R.h, R.d)]),
		   rectype([agenda=[rectype([e:throw(R.h, R.s)])]:list(rectype(_))])))).
update_function(
	R, fun(_:rectype([agenda=[rectype([e:throw(R.h, R.s)])]:list(rectype(_))]),
	       fun(_:rectype([e:throw(R.h, R.s)]),
		   rectype([agenda=[rectype([e:run_after(R.d, R.s)])]:list(rectype(_))])))).
update_function(
	R, fun(_:rectype([agenda=[rectype([e:run_after(R.d, R.s)])]:list(rectype(_))]),
	       fun(_:rectype([e:run_after(R.d, R.s)]),
		   rectype([agenda=[rectype([e:pick_up(R.d, R.s)])]:list(rectype(_))])))).
update_function(
	R, fun(_:rectype([agenda=[rectype([e:pick_up(R.d, R.s)])]:list(rectype(_))]),
	       fun(_:rectype([e:pick_up(R.d, R.s)]),
		   rectype([agenda=[rectype([e:return(R.d, R.s, R.h)])]:list(rectype(_))])))).
update_function(
	R, fun(_:rectype([agenda=[rectype([e:return(R.d, R.s, R.h)])]:list(rectype(_))]),
	       fun(_:rectype([e:return(R.d, R.s, R.h)]),
		   rectype([agenda=[]:list(rectype(_))])))).


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
	  state(Agent, [rectype([agenda=[Fst|_]:list(rectype(_))])|_])
	),
	assert(pending_action(Agent, create(Fst)))
    ).

% Corresponds to Cooper (2023, p. 61), 55a
action_rule(
	eventBasedUpdate,
	Agent,
	( state(Agent, [T_prev|_]),
	  has_type(S_prev, T_prev),
	  fun(S_prev:T_prev, fun(Event:EventType, T)),
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
	  fun(_:T_prev, T),
	  T = rectype(_)
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


agent(h).
agent(d).


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
    retractall(fun(_, _)),
    retractall(current_perceived_object(_, _)),
    retractall(state(_, _)),
    retractall(pending_perception(_, _)),
    retractall(pending_action(_, _)).


initialize_functions :-
    roles(R),
    forall(update_function(R, fun(Var:VarType, Body)),
	   assert(fun(Var:VarType, Body))).


initial_state_type(rectype([agenda=[]:list(rectype(_))])).


main :-
    clear_dynamic_facts,
    initialize_functions,
    initial_state_type(T0),
    forall(agent(Agent), assert(state(Agent, [T0]))),
    process_time_steps(0, 20).


process_time_steps(T, MaxT) :-
    forall(agent(Agent), process_time_step(T, Agent)),
    potentially_process_next_time_steps(T, MaxT).

process_time_step(T, Agent) :-
    print_agent_header(Agent),
    ( T = 0 -> print_agent_internals(Agent) ; true ),
    update_state(Agent),
    forall(retract(pending_action(Agent, Action)),
	   handle_action(Action)).

potentially_process_next_time_steps(T, MaxT) :-
    write('----------------------------------------------------------------------'), nl, nl,
    ( T < MaxT ->
	  T1 is T + 1,
	  process_time_steps(T1, MaxT)
     ; true ).
