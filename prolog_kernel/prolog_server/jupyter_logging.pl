
:- module(jupyter_logging,
    [create_log_file/1,  % create_log_file(-IsSuccess)
     log/1,              % log(+Term)
     log/2               % log(+Control, +Arguments)
    ]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- if(sicstus).
:- use_module(library(lists), [is_list/1]).
:- endif.


:- dynamic
  log_stream/1.


% create_log_file(-IsSuccess)
create_log_file(true) :-
  % Open a log file (jupyter_logging to stdout would send the messages to the client)
  % On Windows platforms, opening a file with SICStus which is alread opened by another process (i.e. another Prolog server) fails
  % Therefore separate log files are created for each Prolog implementation
  catch(current_prolog_flag(dialect, Dialect), _, Dialect = ''),
  atom_concat('.prolog_server_log_', Dialect, LogFileName),
  catch(open(LogFileName, write, Stream), _Exception, fail),
  !,
  assert(log_stream(Stream)).
create_log_file(false).
% No new log file could be opened


log(List) :-
  is_list(List),
  !,
  log('~w~n', [List]).
log(Term) :-
  log('~w~n', Term).

log(Control, Arguments) :-
  % Write to the log file
  log_stream(Stream),
  !,
  format(Stream, Control, Arguments),
  flush_output(Stream).
log(_Control, _Arguments).
% No new log file could be opened
