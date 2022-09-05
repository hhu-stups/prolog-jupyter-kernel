
% This module provides special predicates which can be used in call requests by the client.
% Some of these predicates need to be the only goal of a query.
% Otherwise, they cannot be determined as special predicates and do not work as expected.


:- module(jupyter,
    [cut/0,
     %halt/0,
     help/0,
     previous_query_time/2,     % previous_query_time(-Goal, -Runtime)
     print_queries/1,           % print_queries(+Ids)
     print_sld_tree/1,          % print_sld_tree(+Goal)
     print_stack/0,             % print_stack
     print_table/1,             % print_table(+Goal)
     print_table/2,             % print_table(+ValuesLists, +VariableNames)
     print_transition_graph/4,  % print_transition_graph(+PredSpec, +FromIndex, +ToIndex, +LabelIndex)
     print_variable_bindings/0,
     retry/0,
     set_prolog_impl/1,         % set_prolog_impl(+PrologImplementationID)
     trace/1,                   % trace(+Goal)
     update_completion_data/0
    ]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(library(lists), [reverse/2]).
:- use_module(library(codesio), [read_term_from_codes/3, write_term_to_codes/3, format_to_codes/3]).
:- use_module(logging, [log/1, log/2]).
:- use_module(output, [query_data/4, debug_mode_for_breakpoints/0]).
:- use_module(variable_bindings, [var_bindings/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following predicates need to appear as a single goal in a query.
% In that case, the execution is handled by the module term_handling.
% Otherwise, an error message is output.

% retry
jupyter:retry :-
  throw(jupyter(no_single_goal(jupyter:retry/0))).

user:retry :-
  throw(jupyter(no_single_goal(jupyter:retry/0))).

% cut
jupyter:cut :-
  throw(jupyter(no_single_goal(jupyter:cut/0))).

user:cut :-
  throw(jupyter(no_single_goal(jupyter:cut/0))).


% jupyter:set_prolog_impl(+PrologImplementationID)
jupyter:set_prolog_impl(_PrologImplementationID) :-
  throw(jupyter(no_single_goal(jupyter:set_prolog_impl/1))).


% jupyter:print_sld_tree(+Goal)
jupyter:print_sld_tree(_Goal) :-
  throw(jupyter(no_single_goal(jupyter:print_sld_tree/1))).


% jupyter:print_stack
jupyter:print_stack :-
  throw(jupyter(no_single_goal(jupyter:print_stack/0))).


% jupyter:print_table(+Goal)
jupyter:print_table(_Goal) :-
  throw(jupyter(no_single_goal(jupyter:print_table/1))).

% jupyter:print_table(+ValuesLists, +VariableNames)
jupyter:print_table(_ValuesLists, _VariableNames) :-
  throw(jupyter(no_single_goal(jupyter:print_table/2))).


% jupyter:print_transition_graph(+PredSpec, +FromIndex, +ToIndex, +LabelIndex)
jupyter:print_transition_graph(_PredSpec, _FromIndex, _ToIndex, _LabelIndex) :-
  throw(jupyter(no_single_goal(jupyter:print_transition_graph/4))).


% jupyter:update_completion_data
jupyter:update_completion_data :-
  throw(jupyter(no_single_goal(jupyter:update_completion_data/0))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Help

% jupyter:predicate_docs(-PredDocs)
%
% PredDocs is a list with elements of the form Pred=Doc, where Pred is a predicate exported by this module and Doc is its documentation as an atom.
jupyter:predicate_docs(PredDocs) :-
  findall(Pred=Doc, predicate_doc(Pred, Doc), PredDocs).


% Prints the documentation for all predicates defined in module jupyter.
jupyter:help :-
  jupyter:predicate_docs(PredDocs),
  log(PredDocs),
  print_pred_docs(PredDocs).


print_pred_docs([]) :- !.
print_pred_docs([_Pred=Doc]) :-
  !,
  format('~w', [Doc]).
print_pred_docs([_Pred=Doc|PredDocs]) :-
  format('~w~n~n--------------------------------------------------------------------------------~n~n', [Doc]),
  print_pred_docs(PredDocs).


predicate_doc('jupyter:cut/0', Doc) :-
  atom_concat([
    'jupyter:cut or cut',
    '\n\n    Cuts off the choicepoints of the latest active query.',
    '\n\n    In general, the previous query is the active one.',
    '\n    However, the previous active query can be activated again.',
    '\n    This can be done by cutting off choicepoints with jupyter:cut/0.',
    '\n    This is also the case if a retry/0 encounters no further solutions.',
    '\n\n    A further retry/0 call causes backtracking of the previous active goal.',
    '\n\n    Needs to be the only goal of a query.'
  ], Doc).
predicate_doc('jupyter:halt/0', Doc) :-
  atom_concat([
  'jupyter:halt or halt',
  '\n\n    Shuts down the running Prolog process.',
  '\n\n    The next time code is to be executed, a new process is started.',
  '\n    Everything defined in the database before does not exist anymore.',
  '\n\n    Corresponds to the functionality of halt/0.',
  '\n    Has the same effect as interrupting or restarting the Jupyter kernel.'
  ], Doc).
predicate_doc('jupyter:help/0', Doc) :-
  atom_concat([
    'jupyter:help',
    '\n\n    Outputs the documentation for all predicates from module jupyter.'
  ], Doc).
predicate_doc('jupyter:previous_query_time/2', Doc) :-
  atom_concat([
    'jupyter:previous_query_time(-Goal, -Runtime)',
    '\n\n    Goal is the previously executed goal.',
    '\n    Time is the time in milliseconds it took the query to complete.'
  ], Doc).
predicate_doc('jupyter:print_queries/1', Doc) :-
  atom_concat([
    'jupyter:print_queries(+Ids)',
    '\n\n    Prints previous queries which were exectued in requests with IDs in Ids.',
    '\n\n    Any $Var terms might be replaced by the variable\'s name.',
    '\n    This is the case if a previous query with ID in Ids contains Var.',
    '\n    Otherwise, $Var is not replaced.'
  ], Doc).
predicate_doc('jupyter:print_sld_tree/1', Doc) :-
  atom_concat([
    'jupyter:print_sld_tree(+Goal)',
    '\n\n    Executes the goal Goal and prints its SLD tree.',
    '\n\n    Needs to be the only goal of a query.'
  ], Doc).
predicate_doc('jupyter:print_stack/0', Doc) :-
  atom_concat([
    'jupyter:print_stack',
    '\n\n    Prints the current stack used for jupyter:retry/0 and jupyter:cut/0.',
    '\n    The active goal is marked by a preceding \'->\'.',
    '\n\n    Needs to be the only goal of a query.'
  ], Doc).
predicate_doc('jupyter:print_table/1', Doc) :-
  atom_concat([
    'jupyter:print_table(+Goal)',
    '\n\n    Computes all results of the goal Goal with findall/3.',
    '\n    These are printed in a table.',
    '\n\n    Needs to be the only goal of a query.',
    '\n\n    Example: jupyter:print_table(prolog_flag(FlagName, Value)).'
  ], Doc).
predicate_doc('jupyter:print_table/2', Doc) :-
  atom_concat([
    'jupyter:print_table(+ValuesLists, +VariableNames)',
    '\n\n    Prints a table of the values in ValuesLists.',
    '\n\n    ValuesLists is a list of lists of the same length.',
    '\n    Each list corresponds to one line of the table.',
    '\n\n    VariableNames is used to fill the header of the table.',
    '\n    If VariableNames=[], capital letters are used.',
    '\n    Otherwise, VariableNames needs to be a list of ground terms.',
    '\n    It needs to be of the same length as the values lists.',
    '\n\n    Needs to be the only goal of a query.',
    '\n\n    Can be used with a predicate like findall/3, but not directly.',
    '\n    Instead, a previous binding can be accessed with a $Var term.',
    '\n\n    Examples:',
    '\n        jupyter:print_table([[10,100],[20,400],[30,900]], [\'X\', \'Y\']).',
    '\n        jupyter:print_table($ResultLists, []).'
  ], Doc).
predicate_doc('jupyter:print_transition_graph/4', Doc) :-
  atom_concat([
    'jupyter:print_transition_graph(+PredSpec, +FromIndex, +ToIndex, +LabelIndex)',
    '\n\n    Finds all solutions of the predicate with specificaion PredSpec.',
    '\n    Prints a graph interpreting the solutions as transitions.',
    '\n\n    PredSpec needs to be of the form PredName/PredArity.',
    '\n    Optionally, it can be module name expanded.',
    '\n\n    FromIndex and ToIndex point to predicate arguments used as nodes.',
    '\n    LabelIndex points to the argument providing a label for an edge.',
    '\n    If LabelIndex=0, no label is shown.',
    '\n\n    Needs to be the only goal of a query.'
  ], Doc).
predicate_doc('jupyter:print_variable_bindings/0', Doc) :-
  atom_concat([
    'jupyter:print_variable_bindings',
    '\n\n    Prints variable bindings from previous queries.',
    '\n    For each variable, the latest value it was bound to is shown.',
    '\n\n    The variable value can be accessed with a $Var term by any query.',
    '\n    In that case, the term is replaced by the value.',
    '\n    If there is no previous value, an error message is printed.'
  ], Doc).
predicate_doc('jupyter:retry/0', Doc) :-
  atom_concat([
    'jupyter:retry or retry',
    '\n\n    Causes backtracking of the latest active query.',
    '\n\n    In general, the previous query is the active one.',
    '\n    However, the previous active query can be activated again.',
    '\n    This can be done by cutting off choicepoints with jupyter:cut/0.',
    '\n    This is also the case if a retry/0 encounters no further solutions.',
    '\n\n    Needs to be the only goal of a query.'
  ], Doc).
predicate_doc('jupyter:set_prolog_impl/1', Doc) :-
  atom_concat([
    'jupyter:set_prolog_impl(+PrologImplementationID)',
    '\n\n    Activates the Prolog implementation with ID PrologImplementationID.',
    '\n\n    Code in the same cell is executed with the previous implementation.',
    '\n\n    Needs to be the only goal of a query.'
  ], Doc).
predicate_doc('jupyter:trace/1', Doc) :-
  atom_concat([
    'jupyter:trace(+Goal)',
    '\n\n    Prints the trace of the goal Goal.',
    '\n\n    By default, no port is leashed so that no user interaction is requested.',
    '\n    All previously set breakpoints are still active.',
    '\n\n    Needs to be the only goal of a query in order to work as expected.'
  ], Doc).
predicate_doc('jupyter:update_completion_data/0', Doc) :-
  atom_concat([
    'jupyter:update_completion_data',
    '\n\n    Updates the predicate data used for code completion using Tab.',
    '\n\n    This is done by retrieving all built-in and exported predicates.',
    '\n    Needed to use completion for predicates from a newly loaded module.',
    '\n\n    Needs to be the only goal of a query.'
  ], Doc).


% atom_concat(+AtomList, -ResultAtom)
%
% ResultAtom is an atom which results from concatenating all atoms in the list AtomList.
atom_concat(Atoms, ResultAtom) :-
  reverse(Atoms, ReversedAtoms),
  atom_concat_(ReversedAtoms, '', ResultAtom).


% atom_concat(+AtomList, +AtomSoFar, -ResultAtom)
atom_concat_([], AtomSoFar, AtomSoFar) :- !.
atom_concat_([Atom|Atoms], AtomSoFar, ResultAtom) :-
  atom_concat(Atom, AtomSoFar, NewAtomSoFar),
  atom_concat_(Atoms, NewAtomSoFar, ResultAtom).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Trace

:- if(swi).

% trace(+Goal)
%
% Switch the tracer on, call the goal Goal and stop the tracer.
% Debug mode is switched on so that any breakpoints which might exist can be activated.
% Because of user:prolog_trace_interception/4 defined in jsonrpc_server, debugging messages are printed to the current output without requesting user interaction.
trace(Goal) :-
  trace,
  call(Goal),
  !,
  notrace.

:- else.

% trace(+Goal)
%
% Switches on trace mode, calls the goal Goal and switches debug mode off.
% Since the last line of the output contains the debugging message of nodebug/1, this line is removed.
% If any breakpoints exist, debug mode is switched back on again so that the debugger can stop at a breakpoint.
% All ports are unleashed so that the debugger does not stop at an invocation to wait for user input.
% However, breakpoints are not affected by this.
trace(Goal) :-
  catch(retractall(output:remove_output_lines_for(trace_debugging_messages)), _Exception, true),
  module_name_expanded(Goal, MGoal),
  switch_trace_mode_on,
  call(MGoal),
  !,
  % Switch off trace mode so that no more debugging messages are printed
  % Afterwards, it needs to be checked if debug mode should be switched on again
  nodebug,
  output:debug_mode_for_breakpoints.


switch_trace_mode_on :-
  current_prolog_flag(debugging, Debugging),
  member(Debugging, [debug, trace]),
  % The debugger is already switched on
  !,
  % When reading the output, some additional lines need to be removed
  % This is done if a clause output:remove_output_lines_for(trace_debugging_messages) exists
  assert(output:remove_output_lines_for(trace_debugging_messages)),
  trace.
switch_trace_mode_on :-
  trace.


% module_name_expanded(+Term, -MTerm)
module_name_expanded((Module:Head:-Body), Module:(Head:-Body)) :- !.
module_name_expanded(Module:Term, Module:Term) :- !.
module_name_expanded(Term, user:Term).

:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Variable bindings

% print_variable_bindings
%
% Print the previous variable bindings which can be reused with a term of the form $Var.
:- if(swi).
print_variable_bindings :-
  print_toplevel_variables. % backtracks until it fails
print_variable_bindings.
:- else.
print_variable_bindings :-
  variable_bindings:var_bindings(Bindings),
  ( Bindings == [] ->
    format('No previous variable bindings~n', [])
  ; print_variable_bindings(Bindings)
  ).

print_variable_bindings([]) :- !.
print_variable_bindings([Name=Value|Bindings]) :-
  format('$~w =~t~12|~p~n', [Name, Value]),
  print_variable_bindings(Bindings).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Previous query data

% previous_query_time(-Goal, -Runtime)
%
% Runtime is the runtime of the latest query, which was a call of Goal
previous_query_time(Goal, Runtime) :-
  findall(Goal-Runtime, output:query_data(_CallRequestId, Runtime, term_data(Goal, _NameVarPairs), _OriginalTermData), GoalRuntimes),
  append(_PreviousGoalRuntimes, [Goal-Runtime], GoalRuntimes).
previous_query_time(_Goal, _Runtime) :-
  format('* There is no previous query', []),
  fail.


% print_queries(+Ids)
%
% Prints the previous queries with ids in Ids in a way that they can be
% - copied to a cell and executed right away or
% - expanded with a head to define a predicate
% If a query contains a term of the form $Var and a previous query contains the variable Var, $Var is replaced by the variable name.
print_queries(Ids) :-
  findall(TermData-OriginalTermData, (member(Id, Ids), output:query_data(Id, _Runtime, TermData, OriginalTermData)), QueriesData),
  print_queries(QueriesData, []).


% print_queries(+QueriesData, +PreviousNameVarPairs)
print_queries([], _PreviousNameVarPairs) :- !.
print_queries([QueryData], PreviousNameVarPairs) :-
  !,
  print_previous_query(QueryData, PreviousNameVarPairs, _NewPreviousNameVarPairs, QueryAtom),
  format('~w.~n', [QueryAtom]).
print_queries([QueryData|QueriesData], PreviousNameVarPairs) :-
  print_previous_query(QueryData, PreviousNameVarPairs, NewPreviousNameVarPairs, QueryAtom),
  format('~w,~n', [QueryAtom]),
  print_queries(QueriesData, NewPreviousNameVarPairs).


% print_previous_query(+QueryData, +PreviousNameVarPairs, -NewPreviousNameVarPairs, -QueryAtom)
print_previous_query(term_data(QueryAtom, NameVarPairs)-same, PreviousNameVarPairs, NewPreviousNameVarPairs, QueryAtom) :-
  % There is no $Var term in the query
  append(NameVarPairs, PreviousNameVarPairs, NewPreviousNameVarPairs),
  !.
print_previous_query(_TermData-OriginalTermData, PreviousNameVarPairs, NewPreviousNameVarPairs, ExpandedTerm) :-
  OriginalTermData = term_data(OriginalTermAtom, OriginalNameVarPairs),
  append(OriginalNameVarPairs, PreviousNameVarPairs, NewPreviousNameVarPairs),
  % Read the original term from the atom
  atom_codes(OriginalTermAtom, OriginalTermCodes),
  append(OriginalTermCodes, [46], OriginalTermCodesWithFullStop),
  read_term_from_codes(OriginalTermCodesWithFullStop, OriginalTerm, [variable_names(OriginalNameVarPairs)]),
  % Expand the term by replacing variables and terms of the form $Var
  expand_term(OriginalTerm, OriginalNameVarPairs, PreviousNameVarPairs, ExpandedTerm).


% expand_term(+Term, +NameVarPairs, +PreviousNameVarPairs, -ExpandedTerm)
%
% NameVarPairs is a list of Name=Var pairs, where Name is the name of a variable Var from the current term.
% PreviousNameVarPairs contains Name=Var pairs from previous queries.
% The term Term is expanded to ExpandedTerm in the following way:
% - If Term is a variable, it is replaced by its Name from NameVarPairs.
% - If Term is of the form $Var:
%   - If the name of the variable Var occurred in one of the previous queries (is contained in PreviousNameVarPairs), $Var is replaced by the variable name.
%   - Otherwise, $Var is replaced by $Name where Name is the name of the variable.
% - If Term is a compound term, its arguments are expanded.
expand_term(Var, NameVarPairs, _PreviousNameVarPairs, Name) :-
  var(Var),
  member(Name=Var, NameVarPairs),
  !.
expand_term(Atomic, _NameVarPairs, _PreviousNameVarPairs, Atomic) :-
  atomic(Atomic),
  !.
expand_term($(Var), NameVarPairs, PreviousNameVarPairs, ExpandedTerm) :-
  !,
  % Get the name of the variable
  var_name(NameVarPairs, Var, Name),
  ( member(Name=_VarValue, PreviousNameVarPairs) ->
    % The variable occurred in one of the previous queries
    ExpandedTerm = Name
  ; otherwise ->
    ExpandedTerm = $(Name)
  ).
expand_term(Term, NameVarPairs, PreviousNameVarPairs, ExpandedTerm) :-
  functor(Term, Name, Arity),
  !,
  functor(ExpandedTerm, Name, Arity),
  expand_args(1, NameVarPairs, PreviousNameVarPairs, Term, ExpandedTerm).


% expand_args(+ArgNum, +NameVarPairs, +PreviousNameVarPairs, +Term, +ExpandedTerm)
expand_args(ArgNum, NameVarPairs, PreviousNameVarPairs, Term, ExpandedTerm) :-
  arg(ArgNum, Term, Arg),
  arg(ArgNum, ExpandedTerm, ExpandedArg),
  !,
  NextArgNum is ArgNum + 1,
  expand_term(Arg, NameVarPairs, PreviousNameVarPairs, ExpandedArg),
  expand_args(NextArgNum, NameVarPairs, PreviousNameVarPairs, Term, ExpandedTerm).
expand_args(_ArgNum, _NameVarPairs, _PreviousNameVarPairs, _Term, _ExpandedTerm).


% var_name(+NameVarPairs, +Var, -Name)
%
% NameVarPairs is a list of Name=Var pairs, where Name is the name of a variable Var.
var_name([Name=SameVar|_NameVarPairs], Var, Name) :-
  Var == SameVar,
  !.
var_name([_NameVarPair|NameVarPairs], Var, Name) :-
  var_name(NameVarPairs, Var, Name).
