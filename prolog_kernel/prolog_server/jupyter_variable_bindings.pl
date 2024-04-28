
% This module provides predicates to reuse previous values of variables in a query.
% It is based on the module toplevel_variables from SWI-Prolog (version 8.4.2).
% The SWI-Prolog 9.2.3 update broke Herculog. The built-in SWI-Prolog functionality
% was changed. SWI-Prolog writes Februari 2024 "If you want to hack around, simply call toplevel_variables:toplevel_var/2. Don’t blame anyone if it stops working without notice though as it is a private API."
% For a more future proof solution the built-in SWI-Prolog functionality will no longer be used.
% Using the functionality from this module for SWI-Prolog repaired Herculog.
% This module will be used for both SWI-Prolog an SICStus Prolog.

:- module(jupyter_variable_bindings,
    [store_var_bindings/1,            % store_var_bindings(+Bindings)
     term_with_stored_var_bindings/4, % term_with_stored_var_bindings(+Term, +Bindings, -ExpandedTerm, -UpdatedBindings)
     var_bindings/1                   % var_bindings(-Bindings)
    ]).


:- use_module(library(lists), [delete/3]).
:- use_module(jupyter_logging, [log/1, log/2]).


:- multifile user:generate_message_hook/3.

user:generate_message_hook(jupyter(no_var_binding(VarName))) --> !,
  ['$~w was not bound by a previous query~n'-[VarName]], [nl].


% Define $ to be an operator.
% This is needed so that terms containing terms of the form $Var can be read without any exceptions.
:- op(1, fx, '$').


:- dynamic var_bindings/1. % var_bindings(Bindings)
% Bindings is a list of Name=Var pairs, where Name is the name of the variable Var of the latest query in which a variable of this name was assigned a value.

var_bindings([]).


% Store variable var_bindings

% store_var_bindings(+Bindings)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term Term.
% Updates the previously stored variable bindings with the new values if the variables are instantiated.
store_var_bindings(Bindings) :-
  retract(var_bindings(StoredBindings)),
  !,
  updated_variables(StoredBindings, Bindings, UpdatedBindings),
  assert(var_bindings(UpdatedBindings)).
store_var_bindings(Bindings) :-
  % There are no previously stored variables
  % Call updated_variables/3 anyway to make sure that only instantiated values are stored
  updated_variables([], Bindings, BoundBindings),
  assert(var_bindings(BoundBindings)).


% Reuse stored variable bindings

% term_with_stored_var_bindings(+Term, +Bindings, -ExpandedTerm, -UpdatedBindings)
%
% ExpandedTerm results from expanding the term Term by replacing all terms of the form $Var
%  with the latest value of the variable Var from a previous execution.
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term Term.
% If any term $Var was replaced, UpdatedBindings contains the corresponding value.
% If there is no previous value for one of the variables, an exception is thrown.
term_with_stored_var_bindings(Term, Bindings, ExpandedTerm, UpdatedBindings) :-
  expand_term(Term, Bindings, ExpandedTerm, StoredBindings),
  updated_variables(Bindings, StoredBindings, UpdatedBindings).


% expand_term(+Term, +Bindings, -ExpandedTerm, -StoredBindings)
expand_term(Var, _Bindings, Var, []) :-
  var(Var),
  !.
expand_term(Atomic, _Bindings, Atomic, []) :-
  atomic(Atomic),
  !.
expand_term($(Var), Bindings, Value, [Name=Value]) :-
  !,
  % Get the name of the variable to get the previous value
  var_name(Bindings, Var, Name),
  stored_variable_binding(Name, Value).
expand_term(Term, Bindings, ExpandedTerm, StoredBindings) :-
  functor(Term, Name, Arity),
  !,
  functor(ExpandedTerm, Name, Arity),
  expand_args(1, Bindings, Term, ExpandedTerm, StoredBindings).


% expand_args(+ArgNum, +Bindings, +Term, +ExpandedTerm, -StoredBindings)
expand_args(ArgNum, Bindings, Term, ExpandedTerm, StoredBindings) :-
  arg(ArgNum, Term, Arg),
  arg(ArgNum, ExpandedTerm, ExpandedArg),
  !,
  NextArgNum is ArgNum + 1,
  expand_term(Arg, Bindings, ExpandedArg, TermBindings),
  append(TermBindings, ArgsBindings, StoredBindings),
  expand_args(NextArgNum, Bindings, Term, ExpandedTerm, ArgsBindings).
expand_args(_ArgNum, _Bindings, _Term, _ExpandedTerm, []).


% var_name(+Bindings, +Var, -Name)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var.
var_name([Name=SameVar|_Bindings], Var, Name) :-
  Var == SameVar,
  !.
var_name([_Binding|Bindings], Var, Name) :-
  var_name(Bindings, Var, Name).


% stored_variable_binding(+VarName, -VarValue)
%
% VarValue is the latest value of the variable with name VarName.
% If there is no previous value, an exception is thrown.
stored_variable_binding(VarName, VarValue) :-
  var_bindings(Bindings),
  member(VarName=VarValue, Bindings),
  !.
stored_variable_binding(VarName, _VarValue) :-
  throw(jupyter(no_var_binding(VarName))).


% Update variable list

% updated_variables(+BindingsToUpdate, +BindingsToUpdateWith, -UpdatedBindings)
%
% The arguments are lists of Name=Var pairs, where Name is the name of a variable Var occurring in the term Term.
% UpdatedBindings contains all elements of BindingsToUpdateWith if the corresponding variables are instantiated.
% It also contains those elements of BindingsToUpdate for which there is no instantiated element with the same variable name in BindingsToUpdateWith.
updated_variables([], [], []) :- !.
updated_variables([], [Name=Value|BindingsToUpdateWith], [Name=Value|UpdatedBindings]) :-
  nonvar(Value),
  !,
  updated_variables([], BindingsToUpdateWith, UpdatedBindings).
updated_variables([], [_Name=_Value|BindingsToUpdateWith], UpdatedBindings) :-
  updated_variables([], BindingsToUpdateWith, UpdatedBindings).
updated_variables([Name=_Var|BindingsToUpdate], BindingsToUpdateWith, [Name=Value|UpdatedBindings]) :-
  member(Name=Value, BindingsToUpdateWith),
  nonvar(Value),
  % There is a value Value for the variable with name Name in BindingsToUpdateWith -> use that value
  !,
  % Delete the entry from the list BindingsToUpdateWith as it has been processed
  delete(BindingsToUpdateWith, Name=Value, RemainingBindings),
  updated_variables(BindingsToUpdate, RemainingBindings, UpdatedBindings).
updated_variables([Name=Var|BindingsToUpdate], BindingsToUpdateWith, [Name=Var|UpdatedBindings]) :-
  % There is no new value for the variable with name Name
  updated_variables(BindingsToUpdate, BindingsToUpdateWith, UpdatedBindings).
