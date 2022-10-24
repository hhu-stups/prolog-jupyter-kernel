:- module(jupyter_preferences,[set_preference/2, set_preference/3, 
                               get_preference/2, get_preferences/1,
                               reset_preferences/0]).

:- dynamic preference_value/2.

preference_definition(verbosity,1,natural,'Verbosity level, 0=off, 10=maximal').

set_preference(Name,Value) :-
   set_preference(Name,_Old,Value).

set_preference(Name,OldValue,Value) :-
   preference_definition(Name,_,Type,_Desc),
   check_type(Type,Value),
   retract(preference_value(Name,OldValue)),!,
   %format(user_error,'Changing preference ~w from ~w to ~w~n',[Name,OldValue,Value]),
   assertz(preference_value(Name,Value)).

check_type(natural,Val) :- integer(Val), Val >= 0.
check_type(integer,Val) :- integer(Val).
check_type(boolean,true).
check_type(boolean,false).

get_preference(Name,Value) :- preference_value(Name,Value).

get_preferences(List) :- findall(P-V,get_preference(P,V),L), sort(L,List).

init_preferences :-
   preference_definition(Name,Default,_Type,_Desc),
   \+ preference_value(Name,_), % not already defined
   %format(user_error,'Initialising preference ~w to ~w~n',[Name,Default]),
   assertz(preference_value(Name,Default)),
   fail.
init_preferences.

reset_preferences :-
   retractall(preference_value(_,_)),
   preference_definition(Name,Default,_Type,_Desc),
   %format(user_error,'Resetting preference ~w to ~w~n',[Name,Default]),
   assertz(preference_value(Name,Default)),
   fail.
reset_preferences.

:- initialization init_preferences.
