:- module(jupyter_tools,[set_log_file/1, get_log_file/1, 
                         format_log/2, writeln_log_time/1]).

swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).

% this is another logging; as currently I am not yet sure how to activate the default logging
% and where the logging files appear
% it also opens and closes the file every time

:- dynamic logfile/1.
logfile('/tmp/jupyter_prolog_kernel.log').

set_log_file(F) :- retractall(logfile(_)), assertz(logfile(F)).
get_log_file(F) :- logfile(F).

logging_is_enabled :- logfile(_),!.

open_logfile(Stream) :- logfile(F), open(F,append,Stream,[encoding(utf8)]).

format_log(FormatString,Args) :- %format(FormatString,Args),nl,
    (logfile(F)
      ->  open(F,append,S,[encoding(utf8)]),
          format_datime(S),
          format(S,FormatString,Args),
          close(S)
     ; true
    ).

:- if(swi).

:- use_module(library(system),[ datime/1]).
format_datime(S) :- 
   get_time(TS),
   format_time(S,'%H:%M %S sec @ %D%n',TS).
:- else.
:- use_module(library(system),[ datime/1]).
format_datime(S) :- datime(datme(Y,M,D,H,M,S)),
   format(S,'~w:~w ~w sec @ ~w/~w/~w~n',[H,M,S,D,M,Y]).
:- endif.

writeln_log_time(Term) :-
    (prolog_log_file(_) ->
       statistics(runtime,[Time,_]),
       statistics(walltime,[WTime,_]),
       statistics(memory_used,M), MB is M  / 1000000, % used instead of deprecated 1048576
       Term=..[H|Args],
       append(Args,[Time,WTime,mb(MB)],NArgs),
       NT =.. [H|NArgs],
       writeln_log(NT)
    ;  true).
