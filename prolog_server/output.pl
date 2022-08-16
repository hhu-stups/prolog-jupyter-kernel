
% This module provides predicates to redirect the output of a query execution to a file and read it from the file.
% The main predicates are
% - call_with_output_to_file/3: call a goal and read all its output
% - call_query_with_output_to_file/7: call a goal, read all its output, and assert its runtime and query data
% - exception_message/2: print and read the error message for an error term

% Additionally, it provides the dynamic predicate query_data(CallRequestId, Runtime, TermData, OriginalTermData) where TermData and OriginalTermData are terms of the form term_data(TermAtom, Bindings).
% It is used to remember all queries' IDs, goal and runtime so that the data can be accessed by jupyter:previous_query_time/2 and jupyter:print_previous_queries/1.
% If there was a replacement of $Var terms in the original term, OriginalTermData contains the original term and its bindings.
% Otherwise, OriginalTermData=same


:- module(output,
    [remove_output_lines_for/1,         % remove_output_lines_for(Type),
     send_reply_on_error/0,
     query_data/4,                      % query_data(-CallRequestId, -Runtime, -TermData, -OriginalTermData)
     call_with_output_to_file/3,        % call_with_output_to_file(+Goal, -Output, -ExceptionMessage)
     call_query_with_output_to_file/7,  % call_query_with_output_to_file(+Goal, +CallRequestId, +Bindings, +OriginalTermData, -Output, -ExceptionMessagem -IsFailure)
     redirect_output_to_file/0,
     assert_query_start_time/0,
     switch_debug_mode_on_for_breakpoints/0,
     delete_output_file/1,              % delete_output_file(+DeleteFile)
     exception_message/2                % exception_message(+Exception, -ExceptionMessage)
    ]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(library(codesio), [write_term_to_codes/3]).
:- use_module(logging, [log/1, log/2]).


:- if(swi).
:- use_module(library(lists), [append/2, delete/3]).
:- use_module(library(readutil), [read_line_to_codes/2]).
:- else.
:- use_module(library(lists), [append/2]).
:- use_module(library(file_systems), [delete_file/1]).
:- endif.


:- dynamic
  remove_output_lines_for/1,  % remove_output_lines_for(Type),
  send_reply_on_error/0,
  output_stream/1,            % output_stream(OutputStream)
  query_data/4.               % query_data(CallRequestId, Runtime, TermData, OriginalTermData)
                              % TermData and OriginalTermData are terms of the form term_data(TermAtom, Bindings)


% If send_reply_on_error exists, an error reply is sent to the client if an unhandled error occurs and is printed with print_message/2.
% This predicate is retracted when an error message is to be produced from an error term and therefore printed.
send_reply_on_error.


file_name(stdout, '.server_stdout').
file_name(error, '.server_stderr').
file_name(output, '.server_output').
file_name(test, 'test_definition.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Call a goal and read all output

% call_with_output_to_file(+Goal, -Output, -ExceptionMessage)
%
% Redirects the output of the goal Goal and debugging messages to a file.
% This is done by creating a file which is set as the current output and error stream.
% Calls the goal Goal and reads its output Output (and debugging messages) from the file.
% If an exception is thrown when calling the goal, ExceptionMessage is the corresponding error message.
% If Goal=jupyter:trace(TraceGoal), debug mode has to be switched off afterwards.
call_with_output_to_file(Goal, Output, ExceptionMessage) :-
  goal_with_module(Goal, MGoal),
  prepare_call_with_output_to_file,
  % Call the goal Goal and compute the runtime
  ( call_with_exception_handling(MGoal, ExceptionMessage)
  ; % Goal failed
    reset_output_streams(true),
    fail
  ),
  cleanup_and_read_output_from_file(MGoal, Output).


% call_query_with_output_to_file(+Goal, +CallRequestId, +Bindings, +OriginalTermData, -Output, -ExceptionMessagem -IsFailure)
%
% Like call_with_output_to_file/3.
% Additionally, the runtime of the goal Goal is elapsed and query data is asserted.
call_query_with_output_to_file(Goal, CallRequestId, Bindings, OriginalTermData, Output, ExceptionMessage, IsFailure) :-
  goal_with_module(Goal, MGoal),
  % Compute the atom of the goal Goal before calling it causes variables to be bound
  % The atom is needed for goal_runtime/2 in case ComputeRuntime=true
  write_term_to_codes(Goal, GoalCodes, [variable_names(Bindings)]),
  atom_codes(GoalAtom, GoalCodes),
  prepare_call_with_output_to_file,
  % Call the goal Goal and compute the runtime
  assert_query_start_time,
  ( call_with_exception_handling(MGoal, ExceptionMessage)
  ; % Goal failed
    IsFailure = true
  ),
  assert_query_data(CallRequestId, term_data(GoalAtom, Bindings), OriginalTermData),
  cleanup_and_read_output_from_file(MGoal, Output).


% goal_with_module(+Goal, -MGoal)
goal_with_module(Module:Goal, Module:Goal) :- !.
goal_with_module(Goal, user:Goal).


prepare_call_with_output_to_file :-
  redirect_output_to_file,
  retractall(send_reply_on_error),
  !.


% Redirects the output of a goal and debugging messages to a file
redirect_output_to_file :-
  file_name(output, OutputFileName),
  open(OutputFileName, write, OutputStream),
  assert(output_stream(OutputStream)),
  % Set the stdout stream to which the goal's output is output by default
  tell(OutputStream),
  % Set the stderr stream to which debugging messages are output
  redirect_user_error_output_to_stream(OutputStream).


assert_query_start_time :-
  retractall(query_start_time(_)),
  statistics(walltime, [StartTime|_]),
  assert(query_start_time(StartTime)).


% call_with_exception_handling(+MGoal, -ExceptionMessage)
:- if(swi).
call_with_exception_handling(MGoal, ExceptionMessage) :-
  catch(call(MGoal),
        Exception,
        % In case of an exception, switch debug mode off so that no more debugging messages are printed
        (notrace, exception_message(Exception, ExceptionMessage))).
:- else.
call_with_exception_handling(jupyter:trace(Goal), ExceptionMessage) :-
  !,
  % In case of a call of jupyter:trace/1, the debugger needs to switched off in case of an exception
  % Since in this case, the message "% The debugger is switched off" is output, this is not done for all goals
  % The message is removed from the output before sending it to the client
  catch(call(jupyter:trace(Goal)),
        Exception,
        % In case of an exception, switch the debug mode off so that no more debugging messages are printed
        % If there are breakpoints, switch the debug mode back on
        % Otherwise, no debugging messages are output for those breakpoints
        (nodebug, switch_debug_mode_on_for_breakpoints, exception_message(Exception, ExceptionMessage))).
call_with_exception_handling(MGoal, ExceptionMessage) :-
  catch(call(MGoal),
        Exception,
        exception_message(Exception, ExceptionMessage)).

switch_debug_mode_on_for_breakpoints :-
  % If there are any breakpoints, switch debug mode on
  user:current_breakpoint(_Conditions, _BID, _Status, _Kind, _Type),
  debug,
  !.
:- endif.
switch_debug_mode_on_for_breakpoints.


% assert_query_data(+CallRequestId, +TermData, +OriginalTermData)
assert_query_data(0, _TermData, _OriginalTermData) :- !.
% Do not assert query data for requests with ID 0
% With requests with this ID, the kernel can request additional data (e.g. for inspection in the case of SWI-Prolog)
assert_query_data(CallRequestId, TermData, OriginalTermData) :-
  statistics(walltime, [EndTime|_]),
  nonvar(OriginalTermData),
  !,
  % Remember all queries' IDs, goal and runtime so that it can be accessed by jupyter:previous_query_time/2 and jupyter:print_previous_queries/1
  query_start_time(StartTime),
  Runtime is EndTime - StartTime,
  ( TermData = OriginalTermData ->
    StoreOriginalTermData = same
  ; % there was a replacement of $Var terms in the original term -> store both terms data
    StoreOriginalTermData = OriginalTermData
  ),
  % Assert the data with assertz/1 so that they can be accessed in the correct order with jupyter:print_previous_queries/1
  assertz(query_data(CallRequestId, Runtime, TermData, StoreOriginalTermData)).
assert_query_data(_CallRequestId, _TermData, _OriginalTermData).


% cleanup_and_read_output_from_file(+Goal, -Output)
%
% Output is the output and debugging messages of the goal Goal which was written to the output file.
cleanup_and_read_output_from_file(Goal, Output) :-
  reset_output_streams(false),
  assert(send_reply_on_error),
  file_name(output, OutputFileName),
  read_output_from_file(OutputFileName, Goal, Output),
  delete_output_file(true).


% reset_output_streams(+DeleteFile)
reset_output_streams(DeleteFile) :-
  retract(output_stream(OutputStream)),
  close(OutputStream),
  told,
  delete_output_file(DeleteFile).


% delete_output_file(+DeleteFile)
delete_output_file(true) :-
  !,
  file_name(output, OutputFileName),
  catch(delete_file(OutputFileName), _Exception, true).
delete_output_file(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print and read error messages

% exception_message(+Exception, -ExceptionMessage)
%
% Exception is an error term as thrown by the Prolog system.
% ExceptionMessage is the corresponding error message as output by print_message/2.
% In order to retrieve ExceptionMessage, the error stream is redirected to a file, the error message is printed and the content is read from the file.
exception_message(Exception, ExceptionMessage) :-
  nonvar(Exception),
  !,
  % Open a file to print the error message to it
  file_name(error, ErrorFileName),
  open(ErrorFileName, write, ErrorStream),
  redirect_user_error_output_to_stream(ErrorStream),
  % Do not send an error reply when printing the error message
  % Use catch/3, because send_reply_on_error might have been retracted by call_with_output_to_file/3
  catch(retractall(send_reply_on_error), _Exception, true),
  print_message(error, Exception),
  assert(send_reply_on_error),
  close(ErrorStream),
  % Read the error message from the file
  read_atom_from_file(ErrorFileName, false, ExceptionMessage),
  delete_file(ErrorFileName).
exception_message(_Exception, _ExceptionMessage).


% redirect_user_error_output_to_stream(+Stream)
:- if(swi).
redirect_user_error_output_to_stream(Stream) :-
  set_stream(Stream, alias(user_error)).
:- else.
redirect_user_error_output_to_stream(Stream) :-
  set_prolog_flag(user_error, Stream).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read from a file

% read_output_from_file(+OutputFileName, +Goal, -Output)
:- if(sicstus).
read_output_from_file(OutputFileName, jupyter:trace(_), Output) :-
  !,
  % In case of a jupyter:trace/1 goal, the last line of the output contains the debugging message of nodebug/0
  % Therefore, it is deleted before creating the atom Output
  read_atom_from_file(OutputFileName, true, Output).
:- endif.
read_output_from_file(OutputFileName, _, Output) :-
  read_atom_from_file(OutputFileName, false, Output).


% read_atom_from_file(+FileName, +IsSicstusJupyterTrace, -FileContent)
%
% FileContent is an atom containing the content of the file with name FileName.
% If IsSicstusJupyterTrace=true, some of the lines of the file are not included.
read_atom_from_file(FileName, IsSicstusJupyterTrace, FileContent) :-
  open(FileName, read, Stream),
  read_lines(Stream, AllLinesCodes),
  close(Stream),
  AllLinesCodes \= [],
  !,
  remove_output_lines(IsSicstusJupyterTrace, AllLinesCodes, LineCodes),
  % Create an atom from the line lists
  ( LineCodes == [] ->
    FileContent = ''
  ; append(LineCodes, [_|ContentCodes]), % Cut off the first new line code
    atom_codes(FileContent, ContentCodes)
  ).
read_atom_from_file(_FileName, _DeleteLastLine, '').


% read_lines(+Stream, -Lines)
read_lines(Stream, NewLines) :-
  read_line_to_codes(Stream, Line),
  ( Line == end_of_file ->
    NewLines = []
  ;
    % Add a new line code to the beginning of each line
    NewLines = [[10|Line]|Lines],
    read_lines(Stream, Lines)
  ).


:- if(sicstus).
read_line_to_codes(Stream, Line) :-
  read_line(Stream, Line).
:- endif.


% remove_output_lines(+IsSicstusJupyterTrace, +Lines, -NewLines)
%
% Lines is a list of codes corresponding to lines read from a file to which output of a goal was written.
% In some cases such as for a jupyter:trace/1 or juypter:print_sld_tree/1 call, not all lines should be included in the output sent to the client.
% This is determined by IsSicstusJupyterTrace and the dynamic predicate remove_output_lines_for/1.
remove_output_lines(IsSicstusJupyterTrace, Lines, NewLines) :-
  IsSicstusJupyterTrace == true,
  !,
  % The output was caused by a call of jupyter:trace/1 from SICStus Prolog and contains debugging messages

  % The first element corresponds to the message '% The debugger will first creep -- showing everything (trace)'
  Lines = [_TraceMessage|LinesWithoutTraceMessage],
  % Remove the last two or three elements
  % In case there is a breakpoint, debugging mode was switched back on and the last line contains the corresponding message "% The debugger will first leap -- showing spypoints (debug)"
  % The preceding two elements correspond to the debugging message of nodebug/0 and its message "% The debugger is switched off"
  ( current_prolog_flag(debug, on) ->
    NumRemoveLastLines = 3
  ; NumRemoveLastLines = 2
  ),
  length(RemoveList, NumRemoveLastLines),
  append(LinesWithoutLastLines, RemoveList, LinesWithoutTraceMessage),
  % In case debugging mode was on before trace mode was switched on, a clause remove_output_lines_for(trace_debugging_messages) exists
  % In that case, the remaining first to lines are debugging messages of the exit ports of trace/0 and jupyter:switch_trace_mode_on/0
  ( remove_output_lines_for(trace_debugging_messages) ->
    retractall(remove_output_lines_for(trace_debugging_messages)),
    LinesWithoutLastLines = [_ExitMessage1, _ExitMessage2|NewLines]
  ; NewLines = LinesWithoutLastLines
  ).
remove_output_lines(_IsSicstusJupyterTrace, Lines, NewLines) :-
  remove_output_lines_for(sld_tree_breakpoint_messages),
  !,
  retractall(remove_output_lines_for(sld_tree_breakpoint_messages)),
  % The output was produced by a call of jupyter:print_sld_tree
  % The first two lines are of the following form:
  % "% The debugger will first leap -- showing spypoints (debug)"
  % "% Generic spypoint added, BID=1"
  % The last line is like the following:
  % "% Generic spypoint, BID=1, removed (last)"
  % The lines corresponding to those messages are removed
  append(LinesWithoutLast, [_LastLine], Lines),
  LinesWithoutLast = [_First, _Second|NewLines].
remove_output_lines(_IsSicstusJupyterTrace, Lines, Lines).
