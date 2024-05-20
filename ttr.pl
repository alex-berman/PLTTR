:- module(ttr, [create/2, has_type/2]).

% create(?Type, ?Witness)
create([], []).
create([L=V:list(_)|TypeTail], [L=V|WitnessTail]) :-
    create(TypeTail, WitnessTail).
create([L:V|TypeTail], [L=V|WitnessTail]) :-
    create(TypeTail, WitnessTail).

has_type(Witness, Type) :-
    create(Type, Witness).
