
:- module(logging,
    [create_log_file/0,
     log/1,  % log(+Term)
     log/2   % log(+Control, +Arguments)
    ]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- if(sicstus).
:- use_module(library(lists), [is_list/1]).
:- endif.


:- dynamic
  log_stream/1.


logging(true).


create_log_file :-
  logging(true),
  !,
  % Open a log file (logging to stdout would send the messages to the client)
  open('.prolog_server_log', write, Stream),
  assert(log_stream(Stream)).
create_log_file.


log(List) :-
  is_list(List),
  !,
  log('~w~n', [List]).
log(Term) :-
  log('~w~n', Term).

log(Control, Arguments) :-
  logging(true),
  !,
  % Write to the log file
  log_stream(Stream),
  format(Stream, Control, Arguments),
  flush_output(Stream).
log(_Control, _Arguments).
