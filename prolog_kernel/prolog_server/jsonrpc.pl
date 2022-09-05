
% This module handles all reading, writing, and parsing of JSON messages.
% It is based on jsonrpc_server.pl and jsonrpc_client.pl from SICStus 4.5.1


:- module(jsonrpc,
    [json_error_term/5,           % json_error_term(+ErrorCode, +ErrorMessageData, +Output, +AdditionalData, -JsonErrorTerm)
     next_jsonrpc_message/1,      % next_jsonrpc_message(-Message)
     parse_json_terms_request/3,  % parse_json_terms_request(+Params, -TermsAndVariables, -ParsingErrorMessageData)
     send_error_reply/3,          % send_error_reply(+Id, +ErrorCode, +ErrorMessage)
     send_json_request/6,         % send_json_request(+Method, +Params, +Id, +InputStream, +OutputStream, -Reply)
     send_success_reply/2         % send_success_reply(+Id, +Result)
    ]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(library(codesio), [open_codes_stream/2]).
:- use_module(output, [retrieve_message/2]).
:- use_module(logging, [log/1, log/2]).


:- if(swi).
:- use_module(library(http/json), [json_write/2, json_write/3, json_read/2]).
:- else.
:- use_module(library(json), [json_write/2, json_write/3, json_read/2]).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Create JSON-RPC objects


% Create a JSON-RPC Request object (http://www.jsonrpc.org/specification#request_object)
jsonrpc_request(Method, Params, Id, json([jsonrpc='2.0',id=Id,method=Method,params=Params])).

jsonrpc_request(Method, Id, json([jsonrpc='2.0',id=Id,method=Method])).


% Create a JSON-RPC success Response object (http://www.jsonrpc.org/specification#response_object)
jsonrpc_response(Result, Id, json([jsonrpc='2.0',id=Id,result=Result])).


% Create a JSON-RPC error Response object (http://www.jsonrpc.org/specification#response_object)
jsonrpc_error_response(Error, Id, json([jsonrpc='2.0',id=Id,error=Error])).

% Create a JSON-RPC error Response object (http://www.jsonrpc.org/specification#response_object)
jsonrpc_error_response(Error, json([jsonrpc='2.0',id= @(null),error=Error])).


% Create a JSON-RPC Error object (http://www.jsonrpc.org/specification#error_object)
jsonrpc_error(Code, Message, Data, json([code=Code,message=Message,data=Data])).

% Create a JSON-RPC Error object (http://www.jsonrpc.org/specification#error_object)
jsonrpc_error(Code, Message, json([code=Code,message=Message])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% json_error_term(+ErrorCode, +ErrorMessageData, +Output, +AdditionalData, -JsonErrorTerm)
%
% ErrorCode is one of the error codes defined by error_object_code/3 (e.g. exception).
% ErrorMessageData is a term of the form message_data(Kind, Term) so that the acutal error message can be retrieved with print_message(Kind, Term)
% Output is the output of the term which was executed.
% AdditionalData is a list containing Key=Value pairs providing additional data for the client.
json_error_term(ErrorCode, ErrorMessageData, Output, AdditionalData, JsonErrorTerm) :-
  output:retrieve_message(ErrorMessageData, PrologMessage),
  error_data(PrologMessage, Output, AdditionalData, ErroData),
  error_object_code(ErrorCode, NumericErrorCode, JsonRpcErrorMessage),
  jsonrpc_error(NumericErrorCode, JsonRpcErrorMessage, ErroData, JsonErrorTerm).


% error_data(+PrologMessage, +Output, +AdditionalData, -ErrorData)
error_data(PrologMessage, Output, AdditionalData, json([prolog_message=PrologMessage|AdditionalData])) :-
  var(Output),
  !.
error_data(PrologMessage, '', AdditionalData, json([prolog_message=PrologMessage|AdditionalData])) :- !.
error_data(PrologMessage, Output, AdditionalData, json([prolog_message=PrologMessage, output=Output|AdditionalData])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Send responses

% send_success_reply(+Id, +Result)
send_success_reply(Id, Result) :-
  nonvar(Id),
  !,
  jsonrpc_response(Result, Id, JSONResponse),
  write_message(JSONResponse).


% send_error_reply(+Id, +ErrorCode, +PrologMessage)
%
% ErrorCode is one of the error codes defined by error_object_code/3 (e.g. exception).
% PrologMessage is an error message as output by print_message/2.
send_error_reply(Id, ErrorCode, PrologMessage) :-
  error_object_code(ErrorCode, NumericErrorCode, JsonRpcErrorMessage),
  json_error_term(NumericErrorCode, JsonRpcErrorMessage, json([prolog_message=PrologMessage]), RPCError),
  jsonrpc_error_response(RPCError, Id, RPCResult),
  write_message(RPCResult).


% json_error_term(+NumericErrorCode, +JsonRpcErrorMessage, +Data, -RPCError)
json_error_term(NumericErrorCode, JsonRpcErrorMessage, Data, RPCError) :-
  nonvar(Data),
  !,
  jsonrpc_error(NumericErrorCode, JsonRpcErrorMessage, Data, RPCError).
json_error_term(NumericErrorCode, JsonRpcErrorMessage, _Data, RPCError) :-
  jsonrpc_error(NumericErrorCode, JsonRpcErrorMessage, RPCError).


% error_object_code(+Name, -Code)
error_object_code(Name, Code) :-
  error_object_code(Name, Code, _Description).

% error_object_code(ErrorCode, NumericErrorCode, JsonRpcErrorMessage)
%
% Pre-defined errorserror_object_code(parse_error, -32700, 'Invalid JSON was received by the server.').
error_object_code(invalid_request, -32600, 'The JSON sent is not a valid Request object.').
error_object_code(method_not_found, -32601, 'The method does not exist / is not available.').
error_object_code(invalid_params, -32602, 'Invalid method parameter(s).').
error_object_code(internal_error, -32603, 'Internal JSON-RPC error.').
% Prolog specific errors
error_object_code(failure, -4711, 'Failure').
error_object_code(exception, -4712, 'Exception').
error_object_code(no_active_call, -4713, 'No active call').
error_object_code(invalid_json_response, -4714, 'The Response object is no valid JSON object').
error_object_code(unhandled_exception, -4715, 'Unhandled exception').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Send json request and read the response

% send_json_request(+Method, +Params, +Id, +InputStream, +OutputStream, -Reply)
%
% Sends a request by writing it to the input stream and reads the response from the output stream.
% Used for the tests in sicstus_jsonrpc_server_tests.pl.
send_json_request(Method, Params, Id, InputStream, OutputStream, Reply) :-
  jsonrpc_request(Method, Params, Id, Request),
  % Send the request
  json_write(InputStream, Request),
  nl(InputStream),
  flush_output(InputStream),
  % Read the response
  json_read(OutputStream, Reply).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read and write json messages

% next_jsonrpc_message(-Message)
%
% Reads the next message from the standard input stream and parses it.
next_jsonrpc_message(Message) :-
  read_message(RPC),
  parse_message(RPC, Message).


% read_message(-JsonRpcMessage)
read_message(JsonRpcMessage) :-
  current_input(In),
  json_read(In, JsonRpcMessage).


% parse_message(+RPC, -Message)
parse_message(RPC, Message) :-
  json_member(RPC, 'method', Method),
  json_member(RPC, 'id', _NoId, Id),
  json_member(RPC, 'params', [], Params),
  !,
  Message = request(Method,Id,Params,RPC).
parse_message(RPC, Message) :-
  % RPC is not valid JSON-RPC 2.0
  Message = invalid(RPC).


% write_message(+JSON)
write_message(JSON) :-
  logging:log(JSON),
  % If sending the JSON message to the client directly fails (because the term JSON might not be parsable to JSON),
  %  the client would receive an imcomplete message.
  % Instead, try writing JSON to a file and send an error reply if this fails.
  % Otherwise, send the JSON message to the client.
  open_null_stream(NullStream),
  % Get the options with which to write the JSON object
  % These should include the option for single-line output so that the client to read each reply as a single line.
  json_write_options(WriteOptions),
  catch(json_write(NullStream, JSON, WriteOptions), Exception, true),
  close(NullStream),
  ( nonvar(Exception) ->
    send_error_reply(@(null), invalid_json_response, '')
  ; otherwise ->
    current_output(Out),
    json_write(Out, JSON, WriteOptions),
    % Terminate the line (assuming single-line output).
    nl(Out),
    flush_output(Out)
  ).


:- if(swi).
json_write_options([width(0)]).
:- else.
json_write_options([compact(true)]).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Parse json messages

% parse_json_terms_request(+Params, -TermsAndVariables, -ParsingErrorMessageData)
%
% Reads Prolog terms from the given 'code' string in Params.
% In general, the code needs to be valid Prolog syntax.
% However, if a missing terminating full-stop causes the only syntax error (in case of SICStus Prolog), the terms can be parsed anyway.
% Does not bind TermsAndVariables if the code parameter in Params is malformed or if there is an error when reading the terms.
% If an error occurred while reading Prolog terms from the 'code' parameter, ParsingErrorMessageData is bound.
parse_json_terms_request(Params, TermsAndVariables, ParsingErrorMessageData) :-
  Params = json(_),
  json_member(Params, code, GoalSpec),
  atom(GoalSpec),
  !,
  terms_from_atom(GoalSpec, TermsAndVariables, ParsingErrorMessageData).
parse_json_terms_request(_Params, _TermsAndVariables, _ParsingErrorMessageData).


% terms_from_atom(+TermsAtom, -TermsAndVariables, -ParsingErrorMessageData)
%
% The atom TermsAtom should form valid Prolog term syntax (the last term does not need to be terminated by a full-stop).
% Reads all Prolog terms from TermsAtom.
% TermsAndVariables is a list with elements of the form Term-Variables.
% Variables is a list of variable name and variable mappings (of the form [Name=Var, ...]) which occur in the corresponding term Term.
% ParsingErrorMessageData is instantiated to a term of the form message_data(Kind, Term) if a syntax error was encountered when reading the terms.
% ParsingErrorMessageData can be used to print the actual error message with print_message(Kind, Term).
% In case of a syntax error, TermsAndVariables is left unbound.
%
% Examples:
% - terms_from_atom("hello(world).", [hello(world)-[]], _ParsingError).
% - terms_from_atom("member(E, [1,2,3]).", [member(_A,[1,2,3])-['E'=_A]], _ParsingError).
% - terms_from_atom("hello(world)", _TermsAndVariables, parsing_error(error(syntax_error('operator expected after expression'),syntax_error(read_term('$stream'(140555796879536),_A,[variable_names(_B)]),1,'operator expected after expression',[atom(hello)-1,'('-1,atom(world)-1,')'-1],0)),'! Syntax error in read_term/3\n! operator expected after expression\n! in line 1\n! hello ( world ) \n! <<here>>')).
:- if(swi).
terms_from_atom(TermsAtom, TermsAndVariables, ParsingErrorMessageData) :-
  catch(read_terms_and_vars(TermsAtom, TermsAndVariables),
        Exception,
        ParsingErrorMessageData = message_data(error, Exception)).

:- else.
terms_from_atom(TermsAtom, TermsAndVariables, ParsingErrorMessageData) :-
  atom_codes(TermsAtom, GoalCodes),
  % Try reading the terms from the codes
  terms_from_codes(GoalCodes, TermsAndVariables, ParsingErrorMessageData),
  ( nonvar(ParsingErrorMessageData)
  -> % No valid Prolog syntax
    % The error might have been caused by a missing terminating full-stop
    ( append(_, [46], GoalCodes)
    ; % If the last code of the GoalCodes list does not represent a full-stop, add one and try reading the term(s) again
      append(GoalCodes, [10, 46], GoalCodesWithFullStop), % The last line might be a comment -> add a new line code as well
      terms_from_codes(GoalCodesWithFullStop, TermsAndVariables, _NewParsingErrorMessageData)
    )
  ; true
  ).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read terms in SWI-Prolog

:- if(swi).

% read_terms_and_vars(+TermsAtom, -TermsAndVariables)
read_terms_and_vars(TermsAtom, NewTermsAndVariables) :-
  % Read the terms with read_term_from_atom/3 so that helpful error messages are produced for syntax errors
  read_term_from_atom(TermsAtom, Term, [variable_names(Variables), subterm_positions(TermPos), comments(Comments)]),
  ( Term == end_of_file ->
    NewTermsAndVariables = []
  ; otherwise ->
    NewTermsAndVariables = [Term-Variables|TermsAndVariables],
    atom_without_processed_term_chars(TermsAtom, TermPos, Comments, RemainingTermsAtom),
    read_terms_and_vars(RemainingTermsAtom, TermsAndVariables)
  ).


% atom_without_processed_term_chars(+TermsAtom, +TermPos, +Comments -RemainingTermsAtom)
%
% TermsAtom is an atom from which a term with the layout TermPos was read.
% TermPos contains information about the last character which belongs to the term.
%  This does not include the terminating full-stop and comments that might precede it.
% Comments is a list of Position-Comment elements, where Position is a stream position object indicating the start of a comment and Comment is the corresponding string representing the comment.
%  Comments contains all comments which were read when reading the term from TermsAtom.
% RemainingTermsAtom is a sub-atom of TermsAtom without the read term including its comments and terminating full-stop.
atom_without_processed_term_chars(TermsAtom, TermPos, Comments, RemainingTermsAtom) :-
  % Get the length of the read term without the terminating full-stop and any whitespace or comments which might preceded the full-stop
  term_length(TermPos, Length),
  % Remove the read term from the atom
  sub_atom(TermsAtom, Length, _Length, 0, AtomWithoutProcessedTerm),
  % The sub-atom might be empty or contain further terms or comments
  % Get the sub-atom from which further terms might be read
  next_term_atom(AtomWithoutProcessedTerm, Length, Comments, RemainingTermsAtom).


% term_length(+TermPos, -Length)
%
% TermPos is the layout information of a term, which is one of:
% - From-To for an atom
% - term_position(From, To, FFrom, FTo, SubPos) for a compound term
% - list_position(From, To, Elms, Tail) for a list (e.g. when consulting a file)
% Length is the length of the term.
term_length(_From-To, To) :- !.
term_length(term_position(_From, To, _FFrom, _FTo, _SubPos), To).
term_length(list_position(_From, To, _Elms, _Tail), To).


% next_term_atom(+AtomWithoutProcessedTerm, +TermLength, +Comments, -RemainingTermsAtom)
%
% RemainingTermsAtom is a sub-atom of AtomWithoutProcessedTerm from which the sub-atom terminating a preceding term (which might contain whitespace, comments and a full-stop) is removed.
next_term_atom('', _TermLength, _Comments, '') :- !. % The term was not terminated with a full-stop and there is no text (such as whitespace or comments) following the term
next_term_atom(AtomWithoutProcessedTerm, _TermLength, _Comments, RemainingTermsAtom) :-
  sub_atom(AtomWithoutProcessedTerm, 0, 1, _After, '.'),
  % The term is directly followed by a full-stop
  !,
  % Remove the full-stop
  sub_atom(AtomWithoutProcessedTerm, 1, _Length, 0, RemainingTermsAtom).
next_term_atom(AtomWithoutProcessedTerm, TermLength, Comments, RemainingTermsAtom) :-
  % There might be a comment before the terminating full-stop
  % If there is a comment starting after the last character of the term read from the atom, it needs to be removed
  remove_comments(AtomWithoutProcessedTerm, TermLength, Comments, AtomWithoutPrecedingComments),
  % Get the number of characters preceding a full-stop
  sub_atom(AtomWithoutPrecedingComments, NumCharsBeforeFullStop, 1, _After, '.'),
  !,
  NumCharsWithFullStop is NumCharsBeforeFullStop + 1,
  % Remove the full-stop
  sub_atom(AtomWithoutPrecedingComments, NumCharsWithFullStop, _RemainingLength, 0, RemainingTermsAtom).
next_term_atom(_AtomWithoutProcessedTerm, _TermLength, _Comments, '').
  % AtomWithoutProcessedTerm does not contain a full-stop, so it does not contain a further term
  % This might be the case if the atom ends with whitespace or a comment


% remove_comments(+AtomWithoutProcessedTerm, +TermLength, +Comments, -AtomWithoutPrecedingComments)
remove_comments(AtomWithoutPrecedingComments, _TermLength, [], AtomWithoutPrecedingComments) :- !.
remove_comments(Atom, TermLength, [Position-Comment|Comments], AtomWithoutPrecedingComments) :-
  !,
  stream_position_data(char_count, Position, CharacterCount),
  CharacterCount >= TermLength,
  % The comment is between the end of the term and the terminating full-stop
  % Get the position of the comment in the atom
  sub_atom(Atom, Before, CommentLength, _After, Comment),
  % Remove the comment and any preceding characters from the atom
  RemoveCharNum is Before + CommentLength,
  sub_atom(Atom, RemoveCharNum, _RemainingLength, 0, AtomWithoutComment),
  remove_comments(AtomWithoutComment, TermLength, Comments, AtomWithoutPrecedingComments).
remove_comments(Atom, TermLength, [_Position-_Comment|Comments], AtomWithoutPrecedingComments) :-
  % The comment was inside the term
  remove_comments(Atom, TermLength, Comments, AtomWithoutPrecedingComments).

:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read terms in SICStus Prolog

:- if(sicstus).

% terms_from_codes(+Codes, -TermsAndVariables, -ParsingErrorMessageData)
terms_from_codes(Codes, TermsAndVariables, ParsingErrorMessageData) :-
  open_codes_stream(Codes, Stream),
  catch(call_cleanup(read_terms_and_vars(Stream, TermsAndVariables), close(Stream)),
        Exception,
        ParsingErrorMessageData = message_data(error, Exception)).


% read_terms_and_vars(+Stream, -TermsAndVariables)
read_terms_and_vars(Stream, NewTermsAndVariables) :-
  read_term(Stream, Term, [variable_names(Variables)]),
  ( Term == end_of_file ->
    NewTermsAndVariables = []
  ; otherwise ->
    NewTermsAndVariables = [Term-Variables|TermsAndVariables],
    read_terms_and_vars(Stream, TermsAndVariables)
  ).

:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% json_member(+Object, +Name, -Value)
%
% If Object is a JSON object, with a member named Name, then bind Value to the corresponding value.
% Otherwise, fail.
json_member(Object, Name, Value) :-
  nonvar(Object),
  Object = json(Members),
  memberchk(Name=V, Members),
  !,
  Value = V.


% json_member(+Object, +Name, +Default, -Value)
%
% If Object is a JSON object, with a member named Name, then bind Value to the corresponding value.
% Otherwise, e.g. if there is no such member or Object is not an object, bind Value to Default.
json_member(Object, Name, _Default, Value) :-
  nonvar(Object),
  Object = json(Members),
  memberchk(Name=V, Members),
  !,
  Value = V.
json_member(_Object, _Name, Default, Value) :-
  Value = Default.
