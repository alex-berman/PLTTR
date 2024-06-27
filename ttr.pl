:- module(ttr, [create/2, has_type/2]).

create(rectype(RecTypeContent), rec(RecContent)) :-
    create_record(RecTypeContent, RecContent).

create_record([], []).
create_record([L=V:list(_)|TypeTail], [L=V|WitnessTail]) :-
    create_record(TypeTail, WitnessTail).
create_record([L:V|TypeTail], [L=V|WitnessTail]) :-
    create_record(TypeTail, WitnessTail).

has_type(Witness, Type) :-
    create(Type, Witness).
