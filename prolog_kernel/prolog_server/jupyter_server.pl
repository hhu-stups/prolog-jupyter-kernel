
% This is the main module of the SICStus server.
% The predicate jupyter_server_start/0 can be called to start the server which enters a loop handling requests from a client.
% The requests and corresponding replies are JSON-RPC 2.0 (https://www.jsonrpc.org/specification) messages sent over the standard streams.
% The handling of those is based on code from 'jsonrpc_server.pl' from SICStus 4.5.1


:- module(jupyter_server,
    [jupyter_server_start/0,
     jupyter_server_start/1]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(jupyter_logging, [log/1, log/2]).
:- use_module(jupyter, []).
:- use_module(jupyter_request_handling, [loop/3]).
:- use_module(jupyter_term_handling, [assert_sld_data/4]).
:- use_module(jupyter_preferences, [set_preference/2]).

jupyter_server_start :-
  jupyter_server_start(1).

jupyter_server_start(JupyterKernelVerbosityLevel) :-
  setup,
  set_preference(verbosity,JupyterKernelVerbosityLevel), % useful for testing purposes
  % Start the loop handling requests from the client
  jupyter_request_handling:loop(continue, [], _ContOut).


:- if(swi).
setup :-
  % The tests in jupyter_server_tests.pl need to be started without printing informational messages
  % In order for those messages to be printed during an execution, a corresponding Prolog flag has to be set
  set_prolog_flag(verbose, normal).
:- else.
setup :-
  % Turn leashing off for all ports so that no user interaction is required when a breakpoint is activated
  leash(off),
  % Make sure that redefinitions are performed without user interaction and warnings are issued
  set_prolog_flag(redefine_warnings, proceed),
  % The tests in jupyter_server_tests.pl need to be started without printing informational messages
  % In order for those messages to be printed during an execution, a corresponding Prolog flag has to be set
  set_prolog_flag(informational, on).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Trace interception for SWI-Prolog


:- if(swi).

% user:prolog_trace_interception(+Port, +Frame, +Choice, -Action)
%
% Action=continue corresponds to creeping in the command line debugger, so that no user interaction is required.
user:prolog_trace_interception(_Port, Frame, _PC, continue) :-
  prolog_frame_attribute(Frame, hidden, true),
  % Do nothing for frames hidden from the user
  !.
user:prolog_trace_interception(Port, Frame, _PC, continue) :-
  prolog_frame_attribute(Frame, goal, Goal),
  prolog_frame_attribute(Frame, parent, ParentFrame),
  jupyter_term_handling:assert_sld_data(Port, Goal, Frame, ParentFrame),
  % Succeeds if the current query is a call of jupyter:print_sld_tree/1
  !.
user:prolog_trace_interception(Port, Frame, PC, continue) :-
  % Print the debugging message as output by the tracer to the current output
  % This is needed for juypter:trace/1 calls
  current_output(OutputStream),
  port_functor(Port, PortFunctor),
  phrase('$messages':translate_message(frame(Frame, PortFunctor, PC)), TraceMessageLines),
  print_message_lines(OutputStream, '', TraceMessageLines),
  nl(OutputStream).


% port_functor(+Port, -PortFunctor)
port_functor(call, call).
port_functor(redo(_), redo).
port_functor(unify, unify).
port_functor(exit, exit).
port_functor(fail, fail).
port_functor(exception(_), exception).

:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle error messages which might be sent by the sub modules

:- if(swi).
:- multifile prolog:message/3.
prolog:message(jupyter(JupyterMessageTerm)) --> !,
  juypter_message(JupyterMessageTerm).
:- else.
:- multifile user:generate_message_hook/3.
user:generate_message_hook(jupyter(JupyterMessageTerm)) --> !,
  juypter_message(JupyterMessageTerm).
user:generate_message_hook(existence_error(user:help,0,procedure,user:help/0,0)) --> !,
  ['Existence error in user:help/0'-[]], [nl],
  ['procedure user:help/0 does not exist'-[]], [nl],
  ['goal:  user:help'-[]], [nl],
  [''-[]], [nl],
  ['However, there is the predicate jupyter:help/0'-[]], [nl].
:- endif.


:- if(swi).
:- multifile prolog:message/3.
juypter_message(goal_failed(Goal)) --> !,
  ['Goal (directive) failed: ~w'-[Goal]], [nl].
:- else.
:- multifile user:generate_message_hook/3.
juypter_message(goal_failed(Goal)) --> !,
  ['~w - goal failed'-[Goal]], [nl].
:- endif.
juypter_message(no_var_binding(VarName)) --> !,
  ['$~w was not bound by a previous query~n'-[VarName]], [nl].
juypter_message(invalid_table_values_lists_length) --> !,
  ['The values lists need to be of the same length'-[]], [nl].
juypter_message(invalid_table_variable_names) --> !,
  ['The list of names needs to be empty or of the same length as the values lists and contain ground terms only'-[]], [nl].
juypter_message(leash_pred) --> !,
  ['The leash mode cannot be changed in a Jupyter application as no user interaction can be provided at a breakpoint'-[]], [nl].
juypter_message(no_single_goal(Predicate)) --> !,
  ['~w needs to be the only goal in a term'-[Predicate]], [nl].
juypter_message(print_transition_graph_indices(Arity)) --> !,
  ['All indices need to be less or equal to the provided predicate arity ~w'-[Arity]], [nl].
juypter_message(print_transition_graph_pred_spec(PredSpec)) --> !,
  ['Incorrect predicate specification: ~w'-[PredSpec]], [nl],
  ['It needs to be of the form PredName/PredArity or Module:PredName/PredArity'-[]], [nl].
juypter_message(prolog_impl_id_no_atom) --> !,
  ['The Prolog implementation ID needs to be an atom'-[]], [nl].
juypter_message(single_test_directive) --> !,
  ['The definition of a unit test cannot be split across multiple cells'-[]], [nl].
juypter_message(trace_pred(TracePredSpec)) --> !,
  ['~w cannot be used in a Jupyter application'-[TracePredSpec]], [nl],
  ['However, there is juypter:trace(Goal)'-[]], [nl].
juypter_message(no_answer_given) --> !,
  % Used for the code stub for manually graded tasks of nbgrader assignments
  ['No answer given'-[]], [nl].
