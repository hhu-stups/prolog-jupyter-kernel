
% This file provides tests for the Prolog server defined in the module jsonrpc_server.
% Some of the tests rely on exact invocation numbers, which is why they might fail when the source code is changed or when a single test is run instead of test unit.


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(library(plunit)).
:- use_module(library(process), [process_create/3, process_release/1]).

:- if(swi).
:- use_module(jsonrpc, [send_json_request/6]).
:- use_module(jsonrpc_server).
:- else.
:- use_module('jsonrpc', [send_json_request/6]).
:- use_module('jsonrpc_server').
:- endif.


:- dynamic process_data/3. % process_data(ProcReference, InputStream, OutputStream)


% process_initialization_data(-Args, -Executable)
:- if(swi).
process_initialization_data(Args, Executable) :-
  Args = ['-l', 'jsonrpc_server',
          '-t', 'jsonrpc_server_start',
          '-q'],
  % The value of the Prolog flag executable is the pathname of the running executable
  current_prolog_flag(executable, Executable).
:- else.
process_initialization_data(Args, Executable) :-
  Args = ['-l', 'jsonrpc_server',
          '--goal', 'jsonrpc_server_start;halt.',
          '--nologo', '--noinfo'],
  % $SP_APP_PATH: path to the SICStus that is running this file
  Executable = '$SP_APP_PATH'.
:- endif.


start_process :-
  % Get the arguments and executable which are needed to start the server process
  process_initialization_data(Args, Executable),
  % Create a new process and assert its reference and the input and output streams so that the process can be released and the streams can be written to and read from
  process_create(Executable, Args, [process(ProcReference), stdin(pipe(InputStream, [encoding(utf8)])), stdout(pipe(OutputStream, [encoding(utf8)]))]),
  assert(process_data(ProcReference, InputStream, OutputStream)).


% release_process(+Halt)
release_process(true) :-
  process_data(ProcReference, InputStream, OutputStream),
  !,
  % Halt the server first, because otherwise closing the input stream results in an error
  send_success_call('halt.', 0, _Result),
  release_process(ProcReference, InputStream, OutputStream).
release_process(_Halt) :-
  process_data(ProcReference, InputStream, OutputStream),
  !,
  release_process(ProcReference, InputStream, OutputStream).
release_process(_Halt).


% release_process(+ProcReference, +InputStream, +OutputStream)
release_process(ProcReference, InputStream, OutputStream) :-
  process_release(ProcReference),
  close(InputStream),
  close(OutputStream),
  retractall(process_data(_, _, _)).


% send_json_request(+Method, +Params, +Id, -Reply)
send_json_request(Method, Params, Id, Reply) :-
  process_data(_ProcReference, InputStream, OutputStream),
  !,
  jsonrpc:send_json_request(Method, Params, Id, InputStream, OutputStream, Reply).
send_json_request(_Method, _Params, _Id, _Reply) :-
  print_message(error, server_tests(server_not_running)).


% send_success_call(+Code, +Id, -Result)
send_success_call(Code, Id, Result) :-
  send_success_request(call, Code, Id, Result).


% send_call_with_single_success_result(+Code, +Id, -ResultList)
send_call_with_single_success_result(Code, Id, ResultList) :-
  send_success_request(call, Code, Id, Result),
  ( Result = json(['1'=json([status=success|ResultList])]) ->
    true
  ; print_message(error, server_tests(expected_success_result(Code, Result))),
    fail
  ).


% send_call_with_single_error_result(+Code, +Id, -Error)
send_call_with_single_error_result(Code, Id, Error) :-
  send_success_request(call, Code, Id, Result),
  ( Result = json(['1'=json([status=error, error=Error])]) ->
    true
  ; print_message(error, server_tests(expected_error_result(Code, Result))),
    fail
  ).


% send_success_request(+Method, +Code, +Id, -Result)
send_success_request(Method, Code, Id, Result) :-
  send_json_request(Method, json([code=Code]), Id, Reply),
  ( Reply = json([jsonrpc='2.0',id=Id,result=Result]) ->
    true
  ; print_message(error, server_tests(expected_success_reply(Code, Reply))),
    fail
  ).


% send_failure_request(+Code, +Id, -Error)
send_failure_request(Code, Id, Error) :-
  send_json_request('call', json([code=Code]), Id, Reply),
  ( Reply = json([jsonrpc='2.0',id=Id,error=Error]) ->
    true
  ; print_message(error, server_tests(expected_failure_reply(Code, Reply))),
    fail
  ).


% check_equality(+Result, +ExpectedResult)
check_equality(ExpectedResult, ExpectedResult) :- !.
check_equality(Result, ExpectedResult) :-
  print_message(error, server_tests(expected_equality(Result, ExpectedResult))),
  fail.


:- if(swi).
:- multifile prolog:message/3.
prolog:message(server_tests(ServerTestsMessageTerm)) --> !,
  server_tests_message(ServerTestsMessageTerm).
:- else.
:- multifile user:generate_message_hook/3.
user:generate_message_hook(server_tests(ServerTestsMessageTerm)) --> !,
  server_tests_message(ServerTestsMessageTerm).
:- endif.


server_tests_message(server_not_running) --> !,
  ['The jsonrpc server is not running'-[]], [nl].
server_tests_message(expected_success_reply(Code, Reply)) --> !,
  ['    Expected success reply for ~q'-[Code]], [nl],
  ['    Got:      ~q'-[Reply]], [nl].
server_tests_message(expected_success_result(Code, Result)) --> !,
  ['    Expected a single success result for ~q'-[Code]], [nl],
  ['    Got:      ~q'-[Result]], [nl].
server_tests_message(expected_error_result(Code, Result)) --> !,
  ['    Expected a single error result for ~q'-[Code]], [nl],
  ['    Got:      ~q'-[Result]], [nl].
server_tests_message(expected_failure_reply(Code, Reply)) --> !,
  ['    Expected failure reply for ~q'-[Code]], [nl],
  ['    Got:      ~q'-[Reply]], [nl].
server_tests_message(expected_equality(Result, ExpectedResult)) --> !,
  ['    Expected: ~q'-[ExpectedResult]], [nl],
  ['    Got:      ~q'-[Result]], [nl].


:- discontiguous
    expected_variable_bindings/2,   % expected_variable_bindings(TestName, ExpectedVariableBindings)
    expected_output/2,              % expected_output(TestName, ExpectedOutput)
    expected_retracted_clauses/2,   % expected_retracted_clauses(TestName, ExpectedOutput)
    expected_error_info_subterm/4.  % expected_error_info_subterm(TestName, Before, Length, ErrorInfoSubterm)

% In some cases, the error information returned for a request cannot be compared as is because it may contain a variable or path name which is not always the same.
% In such cases, only a subterm is compared.

% error_result_message_subterms(+TestName, +Request, +Id, +ExpectedOutput, -ErrorInfoSubterm, -ExpectedErrorInfoSubterm) :-
error_result_message_subterms(TestName, Request, Id, ExpectedOutput, ErrorInfoSubterm, ExpectedErrorInfoSubterm) :-
  % Get the expected error info subterm for the test with name TestName
  expected_error_info_subterm(TestName, Before, Length, ExpectedErrorInfoSubterm),
  Error = json([code= -4712,message='Exception',data=json([error_info=ErrorInfo,output=Output])]),
  % Send the request to get the success response containing the error result
  send_call_with_single_error_result(Request, Id, Error),
  % Compare the output
  check_equality(Output, ExpectedOutput),
%  print(ErrorInfo), nl,
  % Get the subterm of the error info which is to be compared with the expected one
  sub_atom(ErrorInfo, Before, Length, _, ErrorInfoSubterm).

% error_result_message_subterms(+TestName, +Request, +Id, -ErrorInfoSubterm, -ExpectedErrorInfoSubterm) :-
error_result_message_subterms(TestName, Request, Id, ErrorInfoSubterm, ExpectedErrorInfoSubterm) :-
  % Get the expected error info subterm for the test with name TestName
  expected_error_info_subterm(TestName, Before, Length, ExpectedErrorInfoSubterm),
  Error = json([code= -4712,message='Exception',data=json([error_info=ErrorInfo])]),
  % Send the request to get the success response containing the error result
  send_call_with_single_error_result(Request, Id, Error),
%  print(ErrorInfo), nl,
  % Get the subterm of the error info which is to be compared with the expected one
  sub_atom(ErrorInfo, Before, Length, _, ErrorInfoSubterm).


% error_response_message_subterms(+TestName, +Request, +Id, -ErrorInfoSubterm, -ExpectedErrorInfoSubterm) :-
error_response_message_subterms(TestName, Request, Id, ErrorInfoSubterm, ExpectedErrorInfoSubterm) :-
  % Get the expected error info subterm for the test with name TestName
  expected_error_info_subterm(TestName, Before, Length, ExpectedErrorInfoSubterm),
  Error = json([code= -4712,message='Exception',data=json([error_info=ErrorInfo])]),
  % Send the request to get the success response containing the error result
  send_failure_request(Request, Id, Error),
%  print(ErrorInfo), nl,
  % Get the subterm of the error info which is to be compared with the expected one
  sub_atom(ErrorInfo, Before, Length, _, ErrorInfoSubterm).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test definitions


:- begin_tests(halt, [setup(start_process), cleanup(release_process(true))]).

test(halt, [true(Result = ExpectedResult)]) :-
  send_success_call('halt.', 1, Result),
  ExpectedResult = json(['1'=json([status=halt])]),
  % After halting, restart the process
  release_process(false),
  start_process.

test(jupyter_halt, [true(Result = ExpectedResult)]) :-
  send_success_call('jupyter:halt.', 2, Result),
  ExpectedResult = json(['1'=json([status=halt])]),
  % After halting, restart the process
  release_process(false),
  start_process.

:- end_tests(halt).


:- begin_tests(special_methods, [setup(start_process), cleanup(release_process(true))]).

test(dialect) :-
  send_success_request(dialect, '', 1, _Result).

test(predicates) :-
  send_success_request(predicates, '', 2, _Result).

test(jupyter_predicate_docs) :-
  send_success_request(jupyter_predicate_docs, '', 3, _Result).

:- if(sicstus).
test(version) :-
  send_success_request(version, '', 4, _Result).
:- endif.

:- end_tests(special_methods).


:- begin_tests(invalid_request, [setup(start_process), cleanup(release_process(true))]).

test(invalid_params, [true(Reply = ExpectedReply)]) :-
  ExpectedReply = json([jsonrpc='2.0',id=1,error=json([code= -32602,message='Invalid method parameter(s).',data=json([error_info=json([jsonrpc='2.0',id=1,method=call,params=json([cod=''])])])])]),
  send_json_request('call', json([cod='']), 1, Reply).

test(invalid_method, [true(Reply = ExpectedReply)]) :-
  ExpectedReply = json([jsonrpc='2.0',id=2,error=json([code= -32601,message='The method does not exist / is not available.',data=json([error_info=json([jsonrpc='2.0',id=2,method=invalid_method,params=json([])])])])]),
  send_json_request('invalid_method', json([]), 2, Reply).

:- end_tests(invalid_request).


:- begin_tests(single_queries, [setup(start_process), cleanup(release_process(true))]).

test(member_success, [true(Result = ExpectedResult)]) :-
  Request = 'member(M, [1,2,3]).',
  ExpectedResult = [type=query,bindings=json(['M'='1']),output=''],
  send_call_with_single_success_result(Request, 1, Result).

test(member_failure, [true(Error = ExpectedError)]) :-
  Request = 'member(4, [1,2,3]).',
  ExpectedError = json([code= -4711,message='Failure',data=json([error_info=''])]),
  send_call_with_single_error_result(Request, 2, Error).

test(member_with_output, [true(Result = ExpectedResult)]) :-
  Request = 'member(M, [10,20,30]), S is M*M, print(S), nl, nl.',
  ExpectedResult = [type=query,bindings=json(['M'='10','S'='100']),output='100\n'],
  send_call_with_single_success_result(Request, 3, Result).

test(append_success, [true(Result = ExpectedResult)]) :-
  Request = 'X = [1,2,3], Y = [4,5,6], append(X, Y, Z).',
  ExpectedResult = [type=query,bindings=json(['X'='[1,2,3]','Y'='[4,5,6]','Z'='[1,2,3,4,5,6]']),output=''],
  send_call_with_single_success_result(Request, 4, Result).

test(is_list_success, [true(Result = ExpectedResult)]) :-
  Request = 'lists:is_list([1,2,3]).',
  ExpectedResult = [type=query,bindings=json([]),output=''],
  send_call_with_single_success_result(Request, 5, Result).

test(is_list_failure, [true(Error = ExpectedError)]) :-
  Request = 'lists:is_list(atom).',
  ExpectedError = json([code= -4711,message='Failure',data=json([error_info=''])]),
  send_call_with_single_error_result(Request, 6, Error).

test(format_output, [true(Result = ExpectedResult)]) :-
  Request = 'format(\'test ~w~ntest ~w\', [1,2]).',
  ExpectedResult = [type=query,bindings=json([]),output='test 1\ntest 2'],
  send_call_with_single_success_result(Request, 7, Result).

test(no_term, [true(Result = ExpectedResult)]) :-
  Request = '% Comment',
  ExpectedResult = '',
  send_success_call(Request, 8, Result).

test(query_with_preceding_operator, [true(Result = ExpectedResult)]) :-
  Request = '?- member(M, [1,2,3]).',
  ExpectedResult = [type=query,bindings=json(['M'='1']),output=''],
  send_call_with_single_success_result(Request, 9, Result).

:- end_tests(single_queries).


:- begin_tests(multiple_queries, [setup(start_process), cleanup(release_process(true))]).

test(member_twice, [true(Result = ExpectedResult)]) :-
  Request = '?- member(M, [1,2,3]). ?- member(M, [1,2,3]).',
  ExpectedResult = json(['1'=json([status=success,type=query,bindings=json(['M'='1']),output='']),
                         '2'=json([status=success,type=query,bindings=json(['M'='1']),output=''])]),
  send_success_call(Request, 1, Result).

test(multiple_terms_with_halt, [true(Result = ExpectedResult)]) :-
  Request = '?- member(M, [1,2,3]). ?- halt. ?- member(M, [1,2,3]).',
  ExpectedResult = json(['1'=json([status=success,type=query,bindings=json(['M'='1']),output='']),
                         '2'=json([status=halt])]),
  send_success_call(Request, 2, Result),
  % After halting, restart the process
  release_process(false),
  start_process.

:- end_tests(multiple_queries).


:- begin_tests(variable_and_compound_result_terms, [setup(start_process), cleanup(release_process(true))]).

% variables

test(anonymous_variable, [true(Result = ExpectedResult)]) :-
  Request = 'X = _.',
  ExpectedResult = [type=query,bindings=json([]),output=''],
  send_call_with_single_success_result(Request, 1, Result).

test(two_variables, [true(Result = ExpectedResult)]) :-
  Request = 'X = Y.',
  ExpectedResult = [type=query,bindings=json(['Y'='X']),output=''],
  send_call_with_single_success_result(Request, 2, Result).

test(three_variables, [true(Result = ExpectedResult)]) :-
  Request = 'X = Y, Y = Z.',
  ExpectedResult = [type=query,bindings=json(['Y'='X','Z'='X']),output=''],
  send_call_with_single_success_result(Request, 3, Result).

test(anonymous_and_non_anonymous_variable, [true(Result = ExpectedResult)]) :-
  Request = 'X = _, Y = X.',
  ExpectedResult = [type=query,bindings=json(['Y'='X']),output=''],
  send_call_with_single_success_result(Request, 4, Result).

% compound terms

test(compound_member, [true(Result = ExpectedResult)]) :-
  Request = 'member(M, [a(1), b(2), c(3)]).',
  ExpectedResult = [type=query,bindings=json(['M'='a(1)']),output=''],
  send_call_with_single_success_result(Request, 5, Result).

test(compound_findall, [true(Result = ExpectedResult)]) :-
  Request = 'findall(M-S, (member(M,[1,2,3]), S is M*M), Ms).',
  ExpectedResult = [type=query,bindings=json(['Ms'='[1-1,2-4,3-9]']),output=''],
  send_call_with_single_success_result(Request, 6, Result).

test(compound_list, [true(VariableAtomStart = ExpectedVariableAtomStart)]) :-
  Request = 'L = [[A,B,C], [a(A), b(X), [c(Y)|_]]].',
  % VariableAtom cannot be compared as is as it contains a variable name which is always different
  % VariableAtom is something like: '[[A,B,C],[a(A),b(X),[c(Y)|_58661]]]'
  ExpectedVariableAtomStart = '[[A,B,C],[a(A),b(X),[c(Y)|',
  Result = [type=query,bindings=json(['L'=VariableAtom]),output=''],
  send_call_with_single_success_result(Request, 7, Result),
  sub_atom(VariableAtom, 0, 26, _, VariableAtomStart).

:- end_tests(variable_and_compound_result_terms).


:- if(swi).
expected_error_info_subterm(non_existent_predicate, 0, 58, 'ERROR: call/1: Unknown procedure: non_existent_predicate/0').
expected_error_info_subterm(instantiation_error, 0, 56, 'ERROR: is/2: Arguments are not sufficiently instantiated').
expected_error_info_subterm(type_error, 0, 48, 'ERROR: is/2: Arithmetic: `y/0\' is not a function').
expected_error_info_subterm(syntax_error, 0, 90, 'ERROR: Syntax error: Illegal start of term\nERROR: faulty([1,\nERROR: ** here **\nERROR: 2). ').
expected_error_info_subterm(syntax_error_in_second_term, 0, 90, 'ERROR: Syntax error: Illegal start of term\nERROR: faulty([1,\nERROR: ** here **\nERROR: 2). ').
:- else.
expected_error_info_subterm(non_existent_predicate, 0, 144, '! Existence error in user:non_existent_predicate/0\n! procedure user:non_existent_predicate/0 does not exist\n! goal:  user:non_existent_predicate').
expected_error_info_subterm(instantiation_error, 0, 55, '! Instantiation error in argument 2 of (is)/2\n! goal:  ').
                                                      % '! Instantiation error in argument 2 of (is)/2\n! goal:  _24225 is _24231+2'
expected_error_info_subterm(type_error, 0, 90, '! Type error in argument 2 of (is)/2\n! expected evaluable, but found y/0\n! goal:  3 is y+2').
expected_error_info_subterm(syntax_error, 0, 107, '! Syntax error in read_term/3\n! , | or ] expected in list\n! in line 1\n! faulty ( [ 1 , 2 \n! <<here>>\n! ) . ').
expected_error_info_subterm(syntax_error_in_second_term, 0, 107, '! Syntax error in read_term/3\n! , | or ] expected in list\n! in line 1\n! faulty ( [ 1 , 2 \n! <<here>>\n! ) . ').
:- endif.


:- begin_tests(exceptions, [setup(start_process), cleanup(release_process(true))]).

test(non_existent_predicate, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(non_existent_predicate, 'non_existent_predicate.', 1, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(instantiation_error, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(instantiation_error, 'X is Y + 2.', 2, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(type_error, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(type_error, '3 is y + 2.', 3, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(syntax_error, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_response_message_subterms(syntax_error, 'faulty([1,2).', 4, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(syntax_error_in_second_term, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_response_message_subterms(syntax_error_in_second_term, 'valid. faulty([1,2).', 5, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

:- end_tests(exceptions).


:- if(swi).
expected_error_info_subterm(exception_in_retry, 0, 48, 'ERROR: is/2: Arithmetic: `a/0\' is not a function').
expected_error_info_subterm(retry_no_single_goal, 0, 58, 'ERROR: jupyter:retry/0 needs to be the only goal in a term').
expected_error_info_subterm(cut_no_single_goal, 0, 56, 'ERROR: jupyter:cut/0 needs to be the only goal in a term').
:- else.
expected_error_info_subterm(exception_in_retry, 0, 82, '! Type error in argument 2 of (is)/2\n! expected evaluable, but found a/0\n! goal:  ').
                                                     % '! Type error in argument 2 of (is)/2\n! expected evaluable, but found a/0\n! goal:  _46965 is a+1'
expected_error_info_subterm(retry_no_single_goal, 0, 53, '! jupyter:retry/0 needs to be the only goal in a term').
expected_error_info_subterm(cut_no_single_goal, 0, 51, '! jupyter:cut/0 needs to be the only goal in a term').
:- endif.


:- begin_tests(retry_and_cut, [setup(start_process), cleanup(release_process(true))]).

% retry/0 and cut/0 can be called with and without the 'jupyter' module expansion

test(retry_no_active_call, [true(Error = ExpectedError)]) :-
  Request = 'retry.',
  ExpectedError = json([code= -4713,message='No active call',data=json([error_info=''])]),
  send_call_with_single_error_result(Request, 1, Error).

test(cut_no_active_call, [true(Error = ExpectedError)]) :-
  Request = 'jupyter:cut.',
  ExpectedError = json([code= -4713,message='No active call',data=json([error_info=''])]),
  send_call_with_single_error_result(Request, 2, Error).

test(member_retry, [true(RetryResult = ExpectedRetryResult)]) :-
  MemberRequest = 'member(Member, [1,2,3]).',
  ExpectedMemberResult = [type=query,bindings=json(['Member'='1']),output=''],
  send_call_with_single_success_result(MemberRequest, 3, MemberResult),
  check_equality(MemberResult, ExpectedMemberResult),
  RetryRequest = 'jupyter:retry.',
  ExpectedRetryResult = [type=query,bindings=json(['Member'='2']),output='% Retrying goal: member(Member,[1,2,3])\n'],
  send_call_with_single_success_result(RetryRequest, 4, RetryResult).

test(member_cut_and_retry, [true(NumberRetryResult = ExpectedNumberRetryResult)]) :-
  NumberMemberRequest = 'member(Member, [1,2,3]).',
  ExpectedNumberMemberResult = [type=query,bindings=json(['Member'='1']),output=''],
  send_call_with_single_success_result(NumberMemberRequest, 5, NumberMemberResult),
  check_equality(NumberMemberResult, ExpectedNumberMemberResult),
  AtomMemberRequest = 'member(Member, [a,b,c]).',
  ExpectedAtomMemberResult = [type=query,bindings=json(['Member'=a]),output=''],
  send_call_with_single_success_result(AtomMemberRequest, 6, AtomMemberResult),
  check_equality(AtomMemberResult, ExpectedAtomMemberResult),
  % Retry 'member(Member, [a,b,c]).'
  AtomRetryRequest = 'retry.',
  ExpectedAtomRetryResult = [type=query,bindings=json(['Member'=b]),output='% Retrying goal: member(Member,[a,b,c])\n'],
  send_call_with_single_success_result(AtomRetryRequest, 7, AtomRetryResult),
  check_equality(AtomRetryResult, ExpectedAtomRetryResult),
  % Cut the choicepoint of 'member(Member, [a,b,c]).'
  CutRequest = 'cut.',
  ExpectedCutResult = [type=cut,bindings=json([]),output='% Successfully cut\n% The new active goal is: member(Member,[1,2,3])'],
  send_call_with_single_success_result(CutRequest, 8, CutResult),
  check_equality(CutResult, ExpectedCutResult),
  % Retry previous goal 'member(Member, [1,2,3]).'
  NumberRetryRequest = 'retry.',
  ExpectedNumberRetryResult = [type=query,bindings=json(['Member'='2']),output='% Retrying goal: member(Member,[1,2,3])\n'],
  send_call_with_single_success_result(NumberRetryRequest, 9, NumberRetryResult).

test(member_retry_failure, [true(NumberRetryResult = ExpectedNumberRetryResult)]) :-
  NumberMemberRequest = 'member(Member, [1,2,3]).',
  ExpectedNumberMemberResult = [type=query,bindings=json(['Member'='1']),output=''],
  send_call_with_single_success_result(NumberMemberRequest, 10, NumberMemberResult),
  check_equality(NumberMemberResult, ExpectedNumberMemberResult),
  AtomMemberRequest = 'member(Member, [a]).',
  ExpectedAtomMemberResult = [type=query,bindings=json(['Member'=a]),output=''],
  send_call_with_single_success_result(AtomMemberRequest, 11, AtomMemberResult),
  check_equality(AtomMemberResult, ExpectedAtomMemberResult),
  % Retry 'member(Member, [a]).' -> failure because there are no other solutions
  AtomRetryRequest = 'retry.',
  ExpectedAtomRetryError = json([code= -4711,message='Failure',data=json([error_info='',output='% Retrying goal: member(Member,[a])\n'])]),
  send_call_with_single_error_result(AtomRetryRequest, 12, AtomRetryError),
  check_equality(AtomRetryError, ExpectedAtomRetryError),
  % Retry previous goal 'member(Member, [1,2,3]).'
  NumberRetryRequest = 'retry.',
  ExpectedNumberRetryResult = [type=query,bindings=json(['Member'='2']),output='% Retrying goal: member(Member,[1,2,3])\n'],
  send_call_with_single_success_result(NumberRetryRequest, 13, NumberRetryResult).

test(retry_failure_with_output, [true(RetryError = ExpectedRetryError)]) :-
  MemberRequest = 'member(X, [1,a]), print(X), number(X).',
  ExpectedMemberResult = [type=query,bindings=json(['X'='1']),output='1'],
  send_call_with_single_success_result(MemberRequest, 14, MemberResult),
  check_equality(MemberResult, ExpectedMemberResult),
  % Before failing, retrying the goal produces output
  RetryRequest = 'retry.',
  ExpectedRetryError = json([code= -4711,message='Failure',data=json([error_info='',output='% Retrying goal: member(X,[1,a]),print(X),number(X)\na'])]),
  send_call_with_single_error_result(RetryRequest, 15, RetryError).

test(exception_in_retry, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  MemberRequest = 'member(M, [1,a,2]), X is M + 1.',
  ExpectedMemberResult = [type=query,bindings=json(['M'='1','X'='2']),output=''],
  send_call_with_single_success_result(MemberRequest, 16, MemberResult),
  check_equality(MemberResult, ExpectedMemberResult),
  % Retrying the goal throws an exception
  Output = '% Retrying goal: member(M,[1,a,2]),X is M+1\n',
  error_result_message_subterms(exception_in_retry, 'retry.', 17, Output, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(exception_in_retry_with_output, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  MemberRequest = 'member(M, [1,a,2]), print(M), X is M + 1.',
  ExpectedMemberResult = [type=query,bindings=json(['M'='1','X'='2']),output='1'],
  send_call_with_single_success_result(MemberRequest, 18, MemberResult),
  check_equality(MemberResult, ExpectedMemberResult),
  % Before throwing an exception, retrying the goal produces output
  Output = '% Retrying goal: member(M,[1,a,2]),print(M),X is M+1\na',
  error_result_message_subterms(exception_in_retry, 'retry.', 19, Output, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(retry_and_cut_in_one_request, [true(Result = ExpectedResult)]) :-
  Request = '?- member(X, [1,2,3]). ?- retry. ?- member(Y, [a,b,c]). ?- retry. ?- cut. ?- retry.',
  ExpectedResult = json(['1'=json([status=success,type=query,bindings=json(['X'='1']),output='']),
                         '2'=json([status=success,type=query,bindings=json(['X'='2']),output='% Retrying goal: member(X,[1,2,3])\n']),
                         '3'=json([status=success,type=query,bindings=json(['Y'=a]),output='']),
                         '4'=json([status=success,type=query,bindings=json(['Y'=b]),output='% Retrying goal: member(Y,[a,b,c])\n']),
                         '5'=json([status=success,type=cut,bindings=json([]),output='% Successfully cut\n% The new active goal is: member(X,[1,2,3])']),
                         '6'=json([status=success,type=query,bindings=json(['X'='3']),output='% Retrying goal: member(X,[1,2,3])\n'])]),
  send_success_call(Request, 20, Result).

% retry/0 and cut/0 need to be the single goal in a term

test(retry_no_single_goal, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(retry_no_single_goal, 'append([1], [2], A), retry.', 21, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(cut_no_single_goal, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(cut_no_single_goal, 'append([1], [2], A), cut.', 22, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

:- end_tests(retry_and_cut).


:- if(swi).
expected_error_info_subterm(load_test_file, 0, 45, 'ERROR: call/1: Unknown procedure: test_pred/1').
expected_error_info_subterm(load_module, 0, 90, 'ERROR: Syntax error: Operator expected\nERROR: X\nERROR: ** here **\nERROR:  #<= 10, X #> 3. ').
        % 'ERROR: Stream <stream>(0x561666b1db10):1:2 Syntax error: Operator expected').
:- else.
expected_error_info_subterm(load_test_file, 0, 91, '! Existence error in user:test_pred/1\n! procedure user:test_pred/1 does not exist\n! goal:  ').
                                                 % '! Existence error in user:test_pred/1\n! procedure user:test_pred/1 does not exist\n! goal:  user:test_pred(_13169)'
expected_error_info_subterm(load_module, 0, 115, '! Syntax error in read_term/3\n! operator expected after expression\n! in line 1\n! X \n! <<here>>\n! #<= 10 , X #> 3 . ').
:- endif.


:- begin_tests(load_files_and_modules, [setup(start_process), cleanup(release_process(true))]).

test(load_test_file, [true(TestPredResult = ExpectedTestPredResult)]) :-
  % Make sure that the predicate does not exist before loading the file
  error_result_message_subterms(load_test_file, 'test_pred(X).', 1, ErrorInfoSubterm, ExpectedErrorInfoSubterm),
  check_equality(ErrorInfoSubterm, ExpectedErrorInfoSubterm),
  % Load the test file defining test_pred/1
  LoadRequest = 'ensure_loaded(test).',
  ExpectedLoadResult = [type=query,bindings=json([]),output=_LoadOutput],
  send_call_with_single_success_result(LoadRequest, 2, LoadResult),
  check_equality(LoadResult, ExpectedLoadResult),
  % Now the predicate exists
  TestPredRequest = 'test_pred(X).',
  ExpectedTestPredResult = [type=query,bindings=json(['X'='a']),output=''],
  send_call_with_single_success_result(TestPredRequest, 3, TestPredResult).

test(load_module, [true(ClpfdResult = ExpectedClpfdResult)]) :-
  % Make sure that a syntax error is thrown before loading the clpfd module
  error_response_message_subterms(load_module, 'X #<= 10, X #> 3.', 4, ErrorInfoSubterm, ExpectedErrorInfoSubterm),
  check_equality(ErrorInfoSubterm, ExpectedErrorInfoSubterm),
  % Load the clpfd library defining the operators
  LoadRequest = 'use_module(library(clpfd)).',
  ExpectedLoadResult = [type=query,bindings=json([]),output=_LoadOutput],
  send_call_with_single_success_result(LoadRequest, 5, LoadResult),
  check_equality(LoadResult, ExpectedLoadResult),
  % Now the operators exist
  ClpfdRequest = 'X #< 10, X #> 3.',
  ExpectedClpfdResult = [type=query,bindings=json(['X'=json([dom='4..9'])]),output=''],
  send_call_with_single_success_result(ClpfdRequest, 6, ClpfdResult).

:- end_tests(load_files_and_modules).


:- if(swi).
expected_retracted_clauses(predicate_redefinition, ['user:p/1'=':- dynamic p/1.\n\np(1).\np(2).\n']).
expected_retracted_clauses(static_predicate_definition_error_and_redefinition, ['user:p/0'=':- dynamic p/0.\n\np.\n']).

expected_error_info_subterm(app_predicate_definition, 0, 39, 'ERROR: call/1: Unknown procedure: app/3').
expected_error_info_subterm(static_predicate_definition_error_and_redefinition, 0, 94, 'ERROR: assertz/1: No permission to modify static procedure `lists:append/3\'\nERROR: Defined at ').
:- else.
expected_retracted_clauses(predicate_redefinition, ['user:p/1'='p(1).\np(2).\n']).
expected_retracted_clauses(static_predicate_definition_error_and_redefinition, ['user:p/0'='p.\n']).

expected_error_info_subterm(app_predicate_definition, 0, 110, '! Existence error in user:app/3\n! procedure user:app/3 does not exist\n! goal:  user:app([1,2],[3,4],[1,2,3,4])').
expected_error_info_subterm(static_predicate_definition_error_and_redefinition, 0, 99, '! Permission error: cannot assert static user:append/3\n! goal:  assertz(user:append([1],[2],[1,2]))').
:- endif.


:- begin_tests(clause_definitions, [setup(start_process), cleanup(release_process(true))]).

test(app_predicate_definition, [true(DefinitionResult = ExpectedDefinitionResult)]) :-
  % Make sure that the predicate does not exist before defining it
  error_result_message_subterms(app_predicate_definition, 'app([1,2], [3,4], [1,2,3,4]).', 1, ErrorInfoSubterm, ExpectedErrorInfoSubterm),
  check_equality(ErrorInfoSubterm, ExpectedErrorInfoSubterm),
  % Define the predicate and call it in the same request
  DefinitionRequest = 'app([], Res, Res) :- !. app([Head|Tail], List, [Head|Res]) :- app(Tail, List, Res). ?- app([1,2], [3,4], R).',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:app/3\n',retracted_clauses=json([])]),
                                   '2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '3'=json([status=success,type=query,bindings=json(['R'='[1,2,3,4]']),output=''])]),
  send_success_call(DefinitionRequest, 2, DefinitionResult).

test(predicate_redefinition, [true(RetryError = ExpectedRetryError)]) :-
  % Define a predicate p/1 (if a request contains multiple terms, a term without a body is recognized as a clause definition)
  DefinitionRequest = 'p(1). p(2).',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:p/1\n',retracted_clauses=json([])]),
                                   '2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 3, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Define the predicate again (a single term in a request with head and body is a clause definition)
  RedefinitionRequest = 'p(a) :- !.',
  expected_retracted_clauses(predicate_redefinition, ExpectedRetractedClauses),
  ExpectedRedefinitionResult = [type=clause_definition,bindings=json([]),output='% Asserting clauses for user:p/1\n',retracted_clauses=json(RetractedClauses)],
  send_call_with_single_success_result(RedefinitionRequest, 4, RedefinitionResult),
  check_equality(RetractedClauses, ExpectedRetractedClauses),
  check_equality(RedefinitionResult, ExpectedRedefinitionResult),
  % Make sure that only the new clause exists
  CallRequest = 'p(X).',
  ExpectedCallResult = [type=query,bindings=json(['X' = 'a']),output=''],
  send_call_with_single_success_result(CallRequest, 5, CallResult),
  check_equality(CallResult, ExpectedCallResult),
  RetryRequest = 'retry.',
  ExpectedRetryError = json([code= -4711,message='Failure',data=json([error_info='',output='% Retrying goal: p(X)\n'])]),
  send_call_with_single_error_result(RetryRequest, 6, RetryError).

test(static_predicate_definition_error_and_redefinition, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  % Define a predicate p/0
  DefinitionRequest = 'p :- true.',
  ExpectedDefinitionResult = [type=clause_definition,bindings=json([]),output='% Asserting clauses for user:p/0\n',retracted_clauses=json([])],
  send_call_with_single_success_result(DefinitionRequest, 7, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Try to redefine append/3 and p/0 -> output and error message
  Request = 'p. append([1], [2], [1,2]).',
  expected_error_info_subterm(static_predicate_definition_error_and_redefinition, Before, Length, ExpectedErrorInfoSubterm),
  expected_retracted_clauses(static_predicate_definition_error_and_redefinition, ExpectedRetractedClauses),
  Error = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:p/0\n',retracted_clauses=json(RetractedClauses)]),
                '2'=json([status=error,error=json([code= -4712,message='Exception',data=json([error_info=ErrorInfo,retracted_clauses=json([])])])])]),
  send_success_call(Request, 8, Error),
  check_equality(RetractedClauses, ExpectedRetractedClauses),
  % Get the subterm of the error info which is to be compared with the expected one
  sub_atom(ErrorInfo, Before, Length, _, ErrorInfoSubterm).

test(multiple_clauses_for_multiple_predicates_without_retract, [true(DefinitionResult = ExpectedDefinitionResult)]) :-
  DefinitionRequest = 'a(1). a(2). b(1). b(2). b(3). c(1). c(2).',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:a/1\n',retracted_clauses=json([])]),
                                   '2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '3'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:b/1\n',retracted_clauses=json([])]),
                                   '4'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '5'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '6'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:c/1\n',retracted_clauses=json([])]),
                                   '7'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 9, DefinitionResult).

:- end_tests(clause_definitions).


:- if(swi).
expected_retracted_clauses(dcg_definition, ['user:num/3'=':- dynamic num/3.\n\nnum(A, [43|B], C) :-\n    num(A, B, C).\nnum(A, [45|B], C) :-\n    num(D, B, E),\n    A is -D,\n    C=E.\nnum(A, [B|C], D) :-\n    "0"=<B,\n    B=<"9",\n    A is B-"0",\n    D=C.\n']).
:- else.
expected_retracted_clauses(dcg_definition, ['user:num/3'='num(A, B, C) :-\n        B=[43|D],\n        num(A, D, C).\nnum(A, B, C) :-\n        B=[45|D],\n        num(E, D, F),\n        A is-E,\n        C=F.\nnum(A, B, C) :-\n        B=[D|E],\n        [48]=<D,\n        D=<[57],\n        A is D-[48],\n        C=E.\n']).
:- endif.


:- begin_tests(dcgs, [setup(start_process), cleanup(release_process(true))]).

test(dcg_definition, [true(RedefinitionResult = ExpectedRedefinitionResult)]) :-
  % Define the DCG rules
  DefinitionRequest = 'num(N) --> "+", num(N). num(NegativeN) --> "-", num(N), {NegativeN is -N}. num(N) --> [D], {"0"=<D, D=<"9", N is D - "0"}.',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:num/3\n',retracted_clauses=json([])]),'2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),'3'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 1, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Call the predicate
  CallRequest = 'atom_codes(\'-1\', _Codes), phrase(num(N), _Codes).',
  ExpectedCallResult = [type=query,bindings=json(['N' = '-1']),output=''],
  send_call_with_single_success_result(CallRequest, 2, CallResult),
  check_equality(CallResult, ExpectedCallResult),
  % Define the predicate again
  RedefinitionRequest = 'num(N) --> [D], {"0"=<D, D=<"9", N is D - "0"}.',
  expected_retracted_clauses(dcg_definition, ExpectedRetractedClauses),
  ExpectedRedefinitionResult = [type=clause_definition,bindings=json([]),output='% Asserting clauses for user:num/3\n',retracted_clauses=json(RetractedClauses)],
  send_call_with_single_success_result(RedefinitionRequest, 3, RedefinitionResult),
  check_equality(RetractedClauses, ExpectedRetractedClauses).

:- end_tests(dcgs).


:- if(swi).
expected_error_info_subterm(syntax_error_and_missing_full_stop, 0, 92, 'ERROR: Syntax error: Operator expected\nERROR: member(1\nERROR: ** here **\nERROR:  [1,2,3]) . ').
                               % 'ERROR: Stream <stream>(0x5566da6e48b0):1:17 Syntax error: Unexpected end of file'
:- else.
expected_error_info_subterm(syntax_error_and_missing_full_stop, 0, 116, '! Syntax error in read_term/3\n! , or ) expected in arguments\n! in line 1\n! member ( 1 \n! <<here>>\n! [ 1 , 2 , 3 ] ) ').
:- endif.


:- begin_tests(missing_full_stop, [setup(start_process), cleanup(release_process(true))]).

test(query_with_missing_full_stop, [true(Result = ExpectedResult)]) :-
  Request = 'member(M, [1,2,3])',
  ExpectedResult = [type=query,bindings=json(['M'='1']),output=''],
  send_call_with_single_success_result(Request, 1, Result).

test(query_with_missing_full_stop_ending_with_whitespace, [true(Result = ExpectedResult)]) :-
  Request = 'member(M, [1,2,3]) \n ',
  ExpectedResult = [type=query,bindings=json(['M'='1']),output=''],
  send_call_with_single_success_result(Request, 2, Result).

test(syntax_error_and_missing_full_stop, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_response_message_subterms(syntax_error_and_missing_full_stop, 'member(1 [1,2,3])', 3, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(app_predicate_definition_with_missing_full_stop, [true(DefinitionResult = ExpectedDefinitionResult)]) :-
  DefinitionRequest = 'app([], Res, Res) :- !. app([Head|Tail], List, [Head|Res]) :- app(Tail, List, Res)',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:app/3\n',retracted_clauses=json([])]),'2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 4, DefinitionResult).

test(query_with_missing_full_stop_ending_with_comment, [true(Result = ExpectedResult)]) :-
  Request = 'append([1], [2], X) % Comment',
  ExpectedResult = [type=query,bindings=json(['X'='[1,2]']),output=''],
  send_call_with_single_success_result(Request, 5, Result).

:- end_tests(missing_full_stop).


:- if(swi).
expected_retracted_clauses(predicate_redefinition_inside_unit_test, ['user:print_test/1'='print_test(_) :-\n        nl.\n']).

expected_error_info_subterm(single_begin_tests, 0, 74, 'ERROR: The definition of a unit test cannot be split across multiple cells').
expected_error_info_subterm(single_end_tests, 0, 74, 'ERROR: The definition of a unit test cannot be split across multiple cells').
:- else.
expected_retracted_clauses(predicate_redefinition_inside_unit_test, ['user:print_test/1'='print_test(_) :-\n        nl.\n']).
expected_error_info_subterm(single_begin_tests, 0, 69, '! The definition of a unit test cannot be split across multiple cells').
expected_error_info_subterm(single_end_tests, 0, 69, '! The definition of a unit test cannot be split across multiple cells').
:- endif.


:- begin_tests(plunit_tests, [setup(start_process), cleanup(release_process(true))]).

:- if(sicstus).
test(load_test_definition_file, [true(DefinitionResult = ExpectedDefinitionResult)]) :-
  % Load a test definition file
  LoadRequest = '[test].',
  ExpectedLoadResult = [type=query,bindings=json([]),output=_LoadOutput],
  send_call_with_single_success_result(LoadRequest, 1, LoadResult),
  check_equality(LoadResult, ExpectedLoadResult),
  % Run the tests
  RunTestsRequest = 'run_tests.',
  % RunTestsOutput cannot be compared as is as it contains the absolute path to the test file
  % RunTestsOutput is something like: '% PL-Unit: test \n% .../test.pl:8:\n% \ttest a: succeeded (det) in 0.00 seconds\n% .../test.pl:11:\n% \ttest b: succeeded (det) in 0.00 seconds\n% .../test.pl:14:\n% \ttest c: succeeded (det) in 0.00 seconds\n% done\n% 3 tests passed'
  RunTestsResult = [type=query,bindings=json([]),output=RunTestsOutput],
  ExpectedLine1Start = '% PL-Unit: test ',
  ExpectedLastLines = '% done\n% 3 tests passed',
  send_call_with_single_success_result(RunTestsRequest, 2, RunTestsResult),
  %print(RunTestsOutput), nl, nl,
  sub_atom(RunTestsOutput, 0, 16, _, Line1Start),
  check_equality(Line1Start, ExpectedLine1Start),
  atom_length(RunTestsOutput, RunTestsOutputLength),
  LastLinesStart is RunTestsOutputLength - 23,
  sub_atom(RunTestsOutput, LastLinesStart, 23, 0, LastLines),
  check_equality(LastLines, ExpectedLastLines),
  % Defining other tests results in a redefinition message
  DefinitionRequest = ':- begin_tests(list). test(list) :- lists:is_list([]).',
  ExpectedDefinitionResult = [type=directive,bindings=json([]),output=_LoadOutput2],
  send_call_with_single_success_result(DefinitionRequest, 3, DefinitionResult).
:- endif.

test(multiple_units, [true(RunTestsResult = ExpectedRunTestsResult)]) :-
  DefinitionRequest = ':- begin_tests(list1, [condition(true)]). test(list) :- lists:is_list([]). :- end_tests(list1). :- begin_tests(list2). test(list_fail, [fail]) :- lists:is_list(1). :- end_tests(list2).',
  ExpectedDefinitionResult = [type=directive,bindings=json([]),output=_LoadOutput],
  send_call_with_single_success_result(DefinitionRequest, 4, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Run the tests
  RunTestsRequest = 'run_tests.',
  ExpectedRunTestsResult = [type=query,bindings=json([]),output=_Output],
  send_call_with_single_success_result(RunTestsRequest, 5, RunTestsResult).

test(single_begin_tests, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(single_begin_tests, ':- begin_tests(list).', 6, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(single_end_tests, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(single_end_tests, ':- end_tests(list).', 7, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(test_1_definition_outside_unit_test, [true(CallResult = ExpectedCallResult)]) :-
  % Defining a test/1 predicate outside a unit test
  DefinitionRequest = 'test(1). test(2).',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:test/1\n',retracted_clauses=json([])]),
                                   '2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 8, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Call the predicate
  CallRequest = 'test(X).',
  ExpectedCallResult = [type=query,bindings=json(['X' = '1']),output=''],
  send_call_with_single_success_result(CallRequest, 9, CallResult).

test(test_2_definition_outside_unit_test, [true(CallResult = ExpectedCallResult)]) :-
  % Defining a test/2 predicate outside a unit test
  DefinitionRequest = 'test(X, Y) :- print(X), print(Y).',
  ExpectedDefinitionResult = [type=clause_definition,bindings=json([]),output='% Asserting clauses for user:test/2\n',retracted_clauses=json([])],
  send_call_with_single_success_result(DefinitionRequest, 10, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Call the predicate
  CallRequest = 'test(a, b).',
  ExpectedCallResult = [type=query,bindings=json([]),output='ab'],
  send_call_with_single_success_result(CallRequest, 11, CallResult).

:- if(sicstus).
test(test_definition_and_run_tests, [true(Result = ExpectedResult)]) :-
  Request = ':- begin_tests(list1, [condition(true)]).\n test(list) :-\n  lists:is_list([]).\n :- end_tests(list1).\n ?- run_tests.\n :- begin_tests(list2).\n test(list_fail, [fail]) :-\n  lists:is_list(1).\n :- end_tests(list2).\n ?- run_tests.',
  ExpectedResult = json(['1'=json([status=success,type=directive,bindings=json([]),output=_LoadOutput1]),
                         '2'=json([status=success,type=query,bindings=json([]),output=_Output1]),
                         '3'=json([status=success,type=directive,bindings=json([]),output=_LoadOutput2]),
                         '4'=json([status=success,type=query,bindings=json([]),output=_Output2])]),
  send_success_call(Request, 12, Result).

test(predicate_redefinition_inside_unit_test, [true(RunTestsResult = ExpectedRunTestsResult)]) :-
  % Define a predicate
  PredicateDefinitionRequest = 'print_test(_X) :- nl.',
  ExpectedPredicateDefinitionResult = [type=clause_definition,bindings=json([]),output='% Asserting clauses for user:print_test/1\n',retracted_clauses=json([])],
  send_call_with_single_success_result(PredicateDefinitionRequest, 13, PredicateDefinitionResult),
  check_equality(PredicateDefinitionResult, ExpectedPredicateDefinitionResult),
  % Define tests and redefine the predicate
  RedefinitionRequest = ':- begin_tests(print). print_test(X) :- nl. test(1) :- print_test(1). :- end_tests(print).',
  expected_retracted_clauses(predicate_redefinition_inside_unit_test, ExpectedRetractedClauses),
  ExpectedRedefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:print_test/1\n',retracted_clauses=json(RetractedClauses)]),
                                     '2'=json([status=success,type=directive,bindings=json([]),output=_LoadOutput])]),
  send_success_call(RedefinitionRequest, 14, RedefinitionResult),
  check_equality(RetractedClauses, ExpectedRetractedClauses),
  check_equality(RedefinitionResult, ExpectedRedefinitionResult),
  % Run the tests
  RunTestsRequest = 'run_tests.',
  ExpectedRunTestsResult = [type=query,bindings=json([]),output=_Output],
  send_call_with_single_success_result(RunTestsRequest, 15, RunTestsResult).
:- endif.

:- end_tests(plunit_tests).

:- if(swi).
expected_output(use_module_directive, '').
expected_output(directive_failure, 'ERROR: Goal (directive) failed: member(4,[1,2,3])').
expected_output(directive_failure_with_output, 'test\nERROR: Goal (directive) failed: print(test),fail').
expected_output(multiple_directives_failure, 'ERROR: Goal (directive) failed: append(1,2,Res),print(Res)').
:- else.
expected_output(use_module_directive, '% module lists imported into user').
expected_output(directive_failure, '* member(4,[1,2,3]) - goal failed').
expected_output(directive_failure_with_output, 'test\n* print(test),fail - goal failed').
expected_output(multiple_directives_failure, '* append(1,2,Res),print(Res) - goal failed').
:- endif.


:- begin_tests(directives, [setup(start_process), cleanup(release_process(true))]).

test(use_module_directive, [true(Result = ExpectedResult)]) :-
  Request = ':- use_module(library(lists)).',
  expected_output(use_module_directive, ExpectedOutput),
  ExpectedResult = [type=directive,bindings=json([]),output=ExpectedOutput],
  send_call_with_single_success_result(Request, 1, Result).

test(directive_with_output, [true(RetryError = ExpectedRetryError)]) :-
  Request = ':- member(M, [1,2,3]), write(M).',
  ExpectedResult = [type=directive,bindings=json([]),output='1'],
  send_call_with_single_success_result(Request, 2, Result),
  check_equality(Result, ExpectedResult),
  % A retry is not possible for directives
  RetryRequest = 'retry.',
  ExpectedRetryError = json([code= -4713,message='No active call',data=json([error_info=''])]),
  send_call_with_single_error_result(RetryRequest, 3, RetryError).

test(directive_failure, [true(Error = ExpectedError)]) :-
  Request = ':- member(4, [1,2,3]).',
  expected_output(directive_failure, ExpectedOutput),
  ExpectedError = json([code= -4711,message='Failure',data=json([error_info='',output=ExpectedOutput])]),
  send_call_with_single_error_result(Request, 4, Error).

test(directive_failure_with_output, [true(Error = ExpectedError)]) :-
  Request = ':- print(test), fail.',
  expected_output(directive_failure_with_output, ExpectedOutput),
  ExpectedError = json([code= -4711,message='Failure',data=json([error_info='',output=ExpectedOutput])]),
  send_call_with_single_error_result(Request, 5, Error).

test(multiple_directives, [true(Result = ExpectedResult)]) :-
  Request = ':- member(M, [1,2,3]), write(M). :- member(M, [a,b,c]), write(M).',
  ExpectedResult = json(['1'=json([status=success,type=directive,bindings=json([]),output='1']),
                         '2'=json([status=success,type=directive,bindings=json([]),output=a])]),
  send_success_call(Request, 6, Result).

test(multiple_directives_failure, [true(Result = ExpectedResult)]) :-
  Request = ':- append([1], [2], Res), print(Res). :- append(1, 2, Res), print(Res).',
  expected_output(multiple_directives_failure, ExpectedOutput),
  ExpectedResult = json(['1'=json([status=success,type=directive,bindings=json([]),output='[1,2]']),
                         '2'=json([status=error,error=json([code= -4711,message='Failure',data=json([error_info='',output=ExpectedOutput])])])]),
  send_success_call(Request, 7, Result).

test(halt_directive, [true(Result = ExpectedResult)]) :-
  Request = ':- print(1). :- halt. :- print(2)',
  ExpectedResult = json(['1'=json([status=success,type=directive,bindings=json([]),output='1']),
                         '2'=json([status=halt])]),
  send_success_call(Request, 8, Result),
  % After halting, restart the process
  release_process(false),
  start_process.

test(retry_and_cut_directives_for_directive, [true(Result = ExpectedResult)]) :-
  Request = '?- member(X, [1,2,3]). :- member(Y, [a,b,c]). :- retry. :- cut. :- retry.',
  ExpectedResult = json(['1'=json([status=success,type=query,bindings=json(['X'='1']),output='']),
                         '2'=json([status=success,type=directive,bindings=json([]),output='']),
                         '3'=json([status=success,type=query,bindings=json(['X'='2']),output='% Retrying goal: member(X,[1,2,3])\n']),
                         '4'=json([status=success,type=cut,bindings=json([]),output='% Successfully cut\n% There is no previous active goal']),
                         '5'=json([status=error,error=json([code= -4713,message='No active call',data=json([error_info=''])])])]),
  send_success_call(Request, 9, Result).

test(retry_and_cut_directives, [true(Result = ExpectedResult)]) :-
  Request = '?- member(X, [1,2,3]). ?- member(Y, [a,b,c]). :- retry. :- cut. :- retry.',
  ExpectedResult = json(['1'=json([status=success,type=query,bindings=json(['X'='1']),output='']),
                         '2'=json([status=success,type=query,bindings=json(['Y'=a]),output='']),
                         '3'=json([status=success,type=query,bindings=json(['Y'=b]),output='% Retrying goal: member(Y,[a,b,c])\n']),
                         '4'=json([status=success,type=cut,bindings=json([]),output='% Successfully cut\n% The new active goal is: member(X,[1,2,3])']),
                         '5'=json([status=success,type=query,bindings=json(['X'='2']),output='% Retrying goal: member(X,[1,2,3])\n'])]),
  send_success_call(Request, 10, Result).

:- end_tests(directives).


:- begin_tests(comments, [setup(start_process), cleanup(release_process(true))]).

test(comment_preceding_terminating_full_stop, [true(Result = ExpectedResult)]) :-
  Request = ':- print(1)  % comment\n. ',
  ExpectedResult = [type=directive,bindings=json([]),output='1'],
  send_call_with_single_success_result(Request, 1, Result).

test(multiple_terms_with_comment_preceding_terminating_full_stop, [true(Result = ExpectedResult)]) :-
  Request = ':- print(1)  % comment\n.   \n:- print(2)% comment',
  ExpectedResult = json(['1'=json([status=success,type=directive,bindings=json([]),output='1']),
                         '2'=json([status=success,type=directive,bindings=json([]),output='2'])]),
  send_success_call(Request, 2, Result).

:- end_tests(comments).


% jupyter specific predicates


:- if(swi).
expected_output(exception_in_trace, '   Call: (22) 3 is 1+x\n   Exception: (22) 3 is 1+x').
expected_output(jupyter_trace, '   Call: (26) app([1], [2], [1, 2])\n   Call: (27) app([], [2], [2])\n   Exit: (27) app([], [2], [2])\n   Exit: (26) app([1], [2], [1, 2])\n   Call: (26) print(done)\ndone   Exit: (26) print(done)').

expected_error_info_subterm(trace_0, 0, 99, 'ERROR: trace/0 cannot be used in a Jupyter application\nERROR: However, there is juypter:trace(Goal)').
expected_error_info_subterm(leash_1, 0, 119, 'ERROR: The leash mode cannot be changed in a Jupyter application as no user interaction can be provided at a breakpoint').
expected_error_info_subterm(exception_in_trace, 0, 48, 'ERROR: is/2: Arithmetic: `x/0\' is not a function').
expected_error_info_subterm(trace_1, 0, 99, 'ERROR: trace/1 cannot be used in a Jupyter application\nERROR: However, there is juypter:trace(Goal)').
expected_error_info_subterm(trace_2, 0, 99, 'ERROR: trace/2 cannot be used in a Jupyter application\nERROR: However, there is juypter:trace(Goal)').
:- else.
expected_output(exception_in_trace, '        1      1 Call: 3 is 1+x\n! Type error in argument 2 of (is)/2\n! expected evaluable, but found x/0\n! goal:  3 is 1+x\n        1      1 Exception: 3 is 1+x').
expected_output(jupyter_trace, '        3      1 Call: app([1],[2],[1,2])\n        4      2 Call: app([],[2],[2])\n        4      2 Exit: app([],[2],[2])\n        3      1 Exit: app([1],[2],[1,2])\n        5      1 Call: print(done)\ndone\n        5      1 Exit: print(done)').

expected_error_info_subterm(trace_0, 0, 89, '! trace/0 cannot be used in a Jupyter application\n! However, there is juypter:trace(Goal)').
expected_error_info_subterm(leash_1, 0, 114, '! The leash mode cannot be changed in a Jupyter application as no user interaction can be provided at a breakpoint').
expected_error_info_subterm(exception_in_trace, 0, 90, '! Type error in argument 2 of (is)/2\n! expected evaluable, but found x/0\n! goal:  3 is 1+x').
:- endif.


define_app_predicates :-
  % Define the predicates app/3 and app/4
  DefinitionRequest = 'app([], Res, Res) :- !. app([Head|Tail], List, [Head|Res]) :- app(Tail, List, Res). app(L1, L2, L3, Res) :- app(L2, L3, R1), app(L1, R1, Res).',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:app/3\n',retracted_clauses=json([])]),
                                   '2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '3'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:app/4\n',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 0, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult).


% Some of these tests rely on exact invocation numbers, which is why they might fail when the source code is changed or when a single test is run instead of test unit.

:- begin_tests(debugging, [setup((start_process, define_app_predicates)), cleanup(release_process(true))]).

test(trace_0, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(trace_0, 'trace.', 1, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(leash_1, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(leash_1, 'leash(off).', 2, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(exception_in_trace, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  expected_output(exception_in_trace, ExpectedOutput),
  error_result_message_subterms(exception_in_trace, 'jupyter:trace((3 is 1 + x)).', 3, ExpectedOutput, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

:- if(swi).

test(trace_1, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(trace_1, 'trace(pred).', 4, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(trace_2, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(trace_2, 'trace(pred, call).', 5, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(jupyter_trace, [true(RetryError = ExpectedRetryError)]) :-
  % Print the trace of the predicate app/3
  TraceRequest = 'jupyter:trace((app([1], [2], [1,2]), print(done))).',
  expected_output(jupyter_trace, ExpectedOutput),
  ExpectedTraceResult = [type=query,bindings=json([]),output=ExpectedOutput],
  send_call_with_single_success_result(TraceRequest, 6, TraceResult),
  check_equality(TraceResult, ExpectedTraceResult),
  % jupyter:trace/1 cannot be retried
  RetryRequest = 'retry.',
  ExpectedRetryError = json([code= -4711,message='Failure',data=json([error_info='',output='% Retrying goal: jupyter:trace((app([1],[2],[1,2]),print(done)))\n'])]),
  send_call_with_single_error_result(RetryRequest, 7, RetryError),
  check_equality(RetryError, ExpectedRetryError).

test(spypoint_and_trace, [true(Call3Result = ExpectedCall3Result)]) :-
  % Add a spypoint for app/3
  AddBreakpointRequest = 'spy(app/3).',
  ExpectedAddBreakpointResult = [type=query,bindings=json([]),output='% Spy point on app/3'],
  send_call_with_single_success_result(AddBreakpointRequest, 8, AddBreakpointResult),
  check_equality(AddBreakpointResult, ExpectedAddBreakpointResult),
  % Call the predicate app/3
  CallRequest = 'app([1], [2], [1,2]).',
  ExpectedCallResult = [type=query,bindings=json([]),output='   Call: (33) app([1], [2], [1, 2])\n   Call: (34) app([], [2], [2])\n   Exit: (34) app([], [2], [2])\n   Exit: (33) app([1], [2], [1, 2])'],
  send_call_with_single_success_result(CallRequest, 9, CallResult),
  check_equality(CallResult, ExpectedCallResult),
  % Call jupyter:trace/1
  TraceRequest = 'jupyter:trace(app([1], [2], [1,2])).',
  ExpectedTraceResult = [type=query,bindings=json([]),output='   Call: (42) app([1], [2], [1, 2])\n   Call: (43) app([], [2], [2])\n   Exit: (43) app([], [2], [2])\n   Exit: (42) app([1], [2], [1, 2])'],
  send_call_with_single_success_result(TraceRequest, 10, TraceResult),
  check_equality(TraceResult, ExpectedTraceResult),
  % Since there is a breakpoint, after the jupyter:trace/1 call, debug mode is still on and debugging messages are printed
  Call2Request = 'app([1], [2], [1,2]).',
  ExpectedCall2Result = [type=query,bindings=json([]),output='   Call: (49) app([1], [2], [1, 2])\n   Call: (50) app([], [2], [2])\n   Exit: (50) app([], [2], [2])\n   Exit: (49) app([1], [2], [1, 2])'],
  send_call_with_single_success_result(Call2Request, 11, Call2Result),
  check_equality(Call2Result, ExpectedCall2Result),
  % After an exception, debug mode is still on and debugging messages are printed
  ExceptionRequest = 'jupyter:trace((3 is 1 + x)).',
  ExpectedExceptionOutput = '   Call: (58) 3 is 1+x\n   Exception: (58) 3 is 1+x\n   Exception: (57) jupyter:trace(3 is 1+x)',
  ExpectedErrorInfo = 'ERROR: is/2: Arithmetic: `x/0\' is not a function',
  Error = json([code= -4712,message='Exception',data=json([error_info=ErrorInfo, output=ExceptionOutput])]),
  send_call_with_single_error_result(ExceptionRequest, 12, Error),
  check_equality(ExceptionOutput, ExpectedExceptionOutput),
  check_equality(ErrorInfo, ExpectedErrorInfo),
  Call3Request = 'app([1], [2], [1,2]).',
  ExpectedCall3Result = [type=query,bindings=json([]),output='   Call: (58) app([1], [2], [1, 2])\n   Call: (59) app([], [2], [2])\n   Exit: (59) app([], [2], [2])\n   Exit: (58) app([1], [2], [1, 2])'],
  send_call_with_single_success_result(Call3Request, 13, Call3Result).

:- else.

test(jupyter_trace, [true(DebuggingResult = ExpectedDebuggingResult)]) :-
  % Print the trace of the predicate app/3
  TraceRequest = 'jupyter:trace((app([1], [2], [1,2]), print(done))).',
  expected_output(jupyter_trace, ExpectedOutput),
  ExpectedTraceResult = [type=query,bindings=json([]),output=ExpectedOutput],
  send_call_with_single_success_result(TraceRequest, 4, TraceResult),
  check_equality(TraceResult, ExpectedTraceResult),
  % jupyter:trace/1 cannot be retried
  RetryRequest = 'retry.',
  ExpectedRetryError = json([code= -4711,message='Failure',data=json([error_info='',output='% Retrying goal: jupyter:trace((app([1],[2],[1,2]),print(done)))\n'])]),
  send_call_with_single_error_result(RetryRequest, 5, RetryError),
  check_equality(RetryError, ExpectedRetryError),
  % debug mode is switched off because there is no breakpoint
  DebuggingRequest = 'debugging.',
  ExpectedDebuggingResult = [type=query,bindings=json([]),output='The debugger is switched off\nNo leashing\nUndefined predicates will raise an exception (error)\nThere are no breakpoints'],
  send_call_with_single_success_result(DebuggingRequest, 6, DebuggingResult).

test(breakpoint_and_trace, [true(Call3Result = ExpectedCall3Result)]) :-
  % Add a breakpoint which causes only the first argument of a app/3 goal to be printed
  AddBreakpointRequest = 'add_breakpoint(pred(app/3)-[print-[1], proceed], _BID).',
  ExpectedAddBreakpointResult = [type=query,bindings=json([]),output='% The debugger will first zip -- showing spypoints (zip)\n% Conditional spypoint for user:app/3 added, BID=1'],
  send_call_with_single_success_result(AddBreakpointRequest, 7, AddBreakpointResult),
  check_equality(AddBreakpointResult, ExpectedAddBreakpointResult),
  % Call the predicate app/3
  CallRequest = 'app([1], [2], [1,2]).',
  ExpectedCallResult = [type=query,bindings=json([]),output=' *      7      1 Call: ^1 [1]\n *      8      2 Call: ^1 []\n *      8      2 Exit: ^1 []\n *      7      1 Exit: ^1 [1]'],
  send_call_with_single_success_result(CallRequest, 8, CallResult),
  check_equality(CallResult, ExpectedCallResult),
  % Created breakpoints are activated during a jupyter:trace/1 call
  TraceRequest = 'jupyter:trace(app([1], [2], [3], [1,2,3])).',
  ExpectedTraceResult = [type=query,bindings=json([]),output='        9      1 Call: app([1],[2],[3],[1,2,3])\n *     10      2 Call: ^1 [2]\n *     11      3 Call: ^1 []\n *     11      3 Exit: ^1 []\n *     10      2 Exit: ^1 [2]\n *     12      2 Call: ^1 [1]\n *     13      3 Call: ^1 []\n *     13      3 Exit: ^1 []\n *     12      2 Exit: ^1 [1]\n        9      1 Exit: app([1],[2],[3],[1,2,3])'],
  send_call_with_single_success_result(TraceRequest, 9, TraceResult),
  check_equality(TraceResult, ExpectedTraceResult),
  % Since there is a breakpoint, after the jupyter:trace/1 call, debug mode is still on and debugging messages are printed
  Call2Request = 'app([1], [2], [1,2]).',
  ExpectedCall2Result = [type=query,bindings=json([]),output=' *   3380     13 Call: ^1 [1]\n *   3381     14 Call: ^1 []\n *   3381     14 Exit: ^1 []\n *   3380     13 Exit: ^1 [1]'],
  send_call_with_single_success_result(Call2Request, 10, Call2Result),
  check_equality(Call2Result, ExpectedCall2Result),
  % After an exception, debug mode is still on and debugging messages are printed
  ExceptionRequest = 'jupyter:trace((3 is 1 + x)).',
  ExpectedExceptionOutput = '     5417     22 Call: 3 is 1+x\n! Type error in argument 2 of (is)/2\n! expected evaluable, but found x/0\n! goal:  3 is 1+x\n     5417     22 Exception: 3 is 1+x\n! Type error in argument 2 of (is)/2\n! expected evaluable, but found x/0\n! goal:  3 is 1+x\n     5408     21 Exception: jupyter:trace(3 is 1+x)\n! Type error in argument 2 of (is)/2\n! expected evaluable, but found x/0\n! goal:  3 is 1+x\n     5407     20 Exception: call(jupyter:trace(3 is 1+x))',
  ExpectedErrorInfo = '! Type error in argument 2 of (is)/2\n! expected evaluable, but found x/0\n! goal:  3 is 1+x',
  Error = json([code= -4712,message='Exception',data=json([error_info=ErrorInfo, output=ExceptionOutput])]),
  send_call_with_single_error_result(ExceptionRequest, 11, Error),
  check_equality(ExceptionOutput, ExpectedExceptionOutput),
  check_equality(ErrorInfo, ExpectedErrorInfo),
  Call3Request = 'app([1], [2], [1,2]).',
  ExpectedCall3Result = [type=query,bindings=json([]),output=' *  10370     22 Call: ^1 [1]\n *  10371     23 Call: ^1 []\n *  10371     23 Exit: ^1 []\n *  10370     22 Exit: ^1 [1]'],
  send_call_with_single_success_result(Call3Request, 12, Call3Result).
:- endif.

:- end_tests(debugging).


:- if(swi).
expected_variable_bindings(reuse_stored_values, ['X'='[1,2,3]','Z'='3','X'='1','Y'='2']).

expected_output(no_stored_variable_bindings, 'No defined toplevel variables').
expected_output(uninstantiated_value, 'No defined toplevel variables').
expected_output(reuse_stored_values, '$Y =        2\n$X =        1\n$Z =        3').

expected_error_info_subterm(variable_value_not_stored, 0, 43, 'ERROR: $X was not bound by a previous query').
:- else.
expected_variable_bindings(reuse_stored_values, ['X'='1','Z'='3','Y'='2']).

expected_output(no_stored_variable_bindings, 'No previous variable bindings').
expected_output(uninstantiated_value, 'No previous variable bindings').
expected_output(reuse_stored_values, '$X =        1\n$Y =        2\n$Z =        3').

expected_error_info_subterm(variable_value_not_stored, 0, 38, '! $X was not bound by a previous query').
:- endif.


:- begin_tests(stored_variable_bindings, [setup(start_process), cleanup(release_process(true))]).

test(variable_value_not_stored, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(variable_value_not_stored, 'Z is $X + $Y.', 1, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(no_stored_variable_bindings, [true(CallResult = ExpectedCallResult)]) :-
  CallRequest = 'jupyter:print_variable_bindings.',
  expected_output(no_stored_variable_bindings, ExpectedOutput),
  ExpectedCallResult = [type=query,bindings=json([]),output=ExpectedOutput],
  send_call_with_single_success_result(CallRequest, 2, CallResult).

test(uninstantiated_value, [true(StoredVariablesResult = ExpectedStoredVariablesResult)]) :-
  % Uninstantiated values are not stored
  CallRequest = 'X = _.',
  ExpectedCallResult = [type=query,bindings=json([]),output=''],
  send_call_with_single_success_result(CallRequest, 3, CallResult),
  check_equality(CallResult, ExpectedCallResult),
  % There are no stored variable values
  StoredVariablesRequest = 'jupyter:print_variable_bindings.',
  expected_output(uninstantiated_value, ExpectedOutput),
  ExpectedStoredVariablesResult = [type=query,bindings=json([]),output=ExpectedOutput],
  send_call_with_single_success_result(StoredVariablesRequest, 4, StoredVariablesResult).

test(reuse_stored_values, [true(StoredVariablesResult = ExpectedStoredVariablesResult)]) :-
  CallRequest = 'X = 1, Y = 2.',
  ExpectedCallResult = [type=query,bindings=json(['X'='1','Y'='2']),output=''],
  send_call_with_single_success_result(CallRequest, 5, CallResult),
  check_equality(CallResult, ExpectedCallResult),
  % The same variable name can occur with and without $ in a query
  ReuseRequest = 'X = [1,2,3], Z is $X + $Y.',
  expected_variable_bindings(reuse_stored_values, ExpectedVariableBindings),
  ExpectedReuseResult = [type=query,bindings=json(ExpectedVariableBindings),output=''],
  send_call_with_single_success_result(ReuseRequest, 6, ReuseResult),
  check_equality(ReuseResult, ExpectedReuseResult),
  % Now there are stored variable values
  StoredVariablesRequest = 'jupyter:print_variable_bindings.',
  expected_output(reuse_stored_values, ExpectedOutput),
  ExpectedStoredVariablesResult = [type=query,bindings=json([]),output=ExpectedOutput],
  send_call_with_single_success_result(StoredVariablesRequest, 7, StoredVariablesResult).

test(non_ground_value, [true(OutputStart = ExpectedAtomStart)]) :-
  CallRequest = 'A = f(B).',
  ExpectedCallResult = [type=query,bindings=json(['A' = 'f(B)']),output=''],
  send_call_with_single_success_result(CallRequest, 8, CallResult),
  check_equality(CallResult, ExpectedCallResult),
  ReuseRequest = 'print($A).',
  % VariableAtom and Output cannot be compared as is as they contain a variable name which is always different
  % VariableAtom and Output are something like: 'f(_71189)'
  ExpectedAtomStart = 'f(_',
  ReuseResult = [type=query,bindings=json(['A'=VariableAtom]),output=Output],
  send_call_with_single_success_result(ReuseRequest, 9, ReuseResult),
  sub_atom(VariableAtom, 0, 3, _, VariableAtomStart),
  check_equality(VariableAtomStart, ExpectedAtomStart),
  sub_atom(Output, 0, 3, _, OutputStart).

:- end_tests(stored_variable_bindings).


:- if(swi).
expected_error_info_subterm(print_table_1_no_single_goal, 0, 64, 'ERROR: jupyter:print_table/1 needs to be the only goal in a term').
expected_error_info_subterm(print_table_2_with_unbound_variable_name, 0, 114, 'ERROR: The list of names needs to be empty or of the same length as the values lists and contain ground terms only').
expected_error_info_subterm(print_table_2_different_length_values_lists_and_names, 0, 114, 'ERROR: The list of names needs to be empty or of the same length as the values lists and contain ground terms only').
expected_error_info_subterm(print_table_2_different_length_values_lists, 0, 53, 'ERROR: The values lists need to be of the same length').
expected_error_info_subterm(print_table_2_no_single_goal, 0, 64, 'ERROR: jupyter:print_table/2 needs to be the only goal in a term').
:- else.
expected_error_info_subterm(print_table_1_no_single_goal, 0, 59, '! jupyter:print_table/1 needs to be the only goal in a term').
expected_error_info_subterm(print_table_2_with_unbound_variable_name, 0, 109, '! The list of names needs to be empty or of the same length as the values lists and contain ground terms only').
expected_error_info_subterm(print_table_2_different_length_values_lists_and_names, 0, 109, '! The list of names needs to be empty or of the same length as the values lists and contain ground terms only').
expected_error_info_subterm(print_table_2_different_length_values_lists, 0, 48, '! The values lists need to be of the same length').
expected_error_info_subterm(print_table_2_no_single_goal, 0, 59, '! jupyter:print_table/2 needs to be the only goal in a term').
:- endif.


:- begin_tests(print_table, [setup(start_process), cleanup(release_process(true))]).

% jupyter:print_table/1

test(print_table_1_member, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table((member(Member, [10,20,30]), Square is Member*Member)).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[['10','100'],['20','400'],['30','900']],'VariableNames'=['Member','Square']])],
  send_call_with_single_success_result(Request, 1, Result).

test(print_table_1_member_with_output, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table((member(Member, [10,20,30]), Square is Member*Member, print(Square), nl)).',
  ExpectedResult = [type=print_table,bindings=json([]),output='100\n400\n900',print_table=json(['ValuesLists'=[['10','100'],['20','400'],['30','900']],'VariableNames'=['Member','Square']])],
  send_call_with_single_success_result(Request, 2, Result).

test(print_table_1_member_with_variables, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table(member(Member, [A,B,C])).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[['Member','Member','B','C'],['Member','A','Member','C'],['Member','A','B','Member']],'VariableNames'=['Member','A','B','C']])],
  send_call_with_single_success_result(Request, 3, Result).

test(print_table_1_member_with_variables_and_binding, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table((member(Member, [A, B]), B=2)).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[['Member','Member','2'],['2','A','2']],'VariableNames'=['Member','A','B']])],
  send_call_with_single_success_result(Request, 4, Result).

test(print_table_1_member_and_retry, [true(RetryError = ExpectedRetryError)]) :-
  MemberRequest = 'jupyter:print_table((member(Member, [10,20,30]), Square is Member*Member)).',
  MemberExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[['10','100'],['20','400'],['30','900']],'VariableNames'=['Member','Square']])],
  send_call_with_single_success_result(MemberRequest, 5, MemberResult),
  check_equality(MemberResult, MemberExpectedResult),
  % retry
  RetryRequest = 'retry.',
  ExpectedRetryError = json([code= -4713,message='No active call',data=json([error_info=''])]),
  send_call_with_single_error_result(RetryRequest, 6, RetryError).

test(print_table_1_without_result, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table(lists:is_list([])).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[[]],'VariableNames'=[]])],
  send_call_with_single_success_result(Request, 7, Result).

test(print_table_1_no_single_goal, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(print_table_1_no_single_goal, 'jupyter:print_table((member(Member, [10,20,30]), Square is Member*Member)), nl.', 8, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

% jupyter:print_table/2

test(print_table_2, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table([[10,100],[20,400],[30,900]], [\'X\', \'Y\']).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[['10','100'],['20','400'],['30','900']],'VariableNames'=['X','Y']])],
  send_call_with_single_success_result(Request, 9, Result).

test(print_table_2_reuse_var_value, [true(PrintTableResult = ExpectedPrintTableResult)]) :-
  FindallRequest = 'findall([Member,Square], (member(Member, [10,20,30]), Square is Member*Member), ResultLists).',
  ExpectedFindallResult = [type=query,bindings=json(['ResultLists'='[[10,100],[20,400],[30,900]]']),output=''],
  send_call_with_single_success_result(FindallRequest, 10, FindallResult),
  check_equality(FindallResult, ExpectedFindallResult),
  % Reuse variable value
  PrintTableRequest = 'jupyter:print_table($ResultLists, [\'Member\', \'Square\']).',
  ExpectedPrintTableResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[['10','100'],['20','400'],['30','900']],'VariableNames'=['Member','Square']])],
  send_call_with_single_success_result(PrintTableRequest, 11, PrintTableResult).

test(print_table_2_variable, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table([[A,B],[C,D],[E,F]], [\'X\', \'Y\']).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[['A','B'],['C','D'],['E','F']],'VariableNames'=['X','Y']])],
  send_call_with_single_success_result(Request, 12, Result).

test(print_table_2_no_values, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table([], []).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[],'VariableNames'=[]])],
  send_call_with_single_success_result(Request, 13, Result).

test(print_table_2_no_values_but_names, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table([], [a, b]).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[],'VariableNames'=[a,b]])],
  send_call_with_single_success_result(Request, 14, Result).

test(print_table_2_no_variable_names, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_table([[10,100],[20,400],[30,900]], []).',
  ExpectedResult = [type=print_table,bindings=json([]),output='',print_table=json(['ValuesLists'=[['10','100'],['20','400'],['30','900']],'VariableNames'=['A','B']])],
  send_call_with_single_success_result(Request, 15, Result).

test(print_table_2_with_unbound_variable_name, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(print_table_2_with_unbound_variable_name, 'jupyter:print_table([[10,100],[20,400],[30,900]], [\'A\', B]).', 16, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(print_table_2_different_length_values_lists_and_names, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(print_table_2_different_length_values_lists_and_names, 'jupyter:print_table([[10,100],[20,400],[30,900]], [\'A\']).', 17, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(print_table_2_different_length_values_lists, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(print_table_2_different_length_values_lists, 'jupyter:print_table([[10,100],[20],[30,900]], [\'X\', \'Y\']).', 18, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(print_table_2_no_single_goal, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(print_table_2_no_single_goal, 'findall([Member], member(Member, [10,20,30]), ResultLists), jupyter:print_table(ResultLists, [\'Member\', \'Square\']).', 19, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

:- end_tests(print_table).


:- if(swi).
expected_output(previous_query_time_sleep, '').
:- else.
expected_output(previous_query_time_sleep, '% module system imported into user').
:- endif.


:- begin_tests(previous_query_time, [setup(start_process), cleanup(release_process(true))]).

test(previous_query_time_no_previous_query, [true(Error = ExpectedError)]) :-
  Request = 'jupyter:previous_query_time(Goal, Time).',
  ExpectedError = json([code= -4711,message='Failure',data=json([error_info='',output='* There is no previous query'])]),
  send_call_with_single_error_result(Request, 1, Error).

test(previous_query_time_member, [true(LastQueryTimeResult = ExpectedLastQueryTimeResult)]) :-
  MemberRequest = 'member(M, [1,2,3]).',
  ExpectedMemberResult = [type=query,bindings=json(['M'='1']),output=''],
  send_call_with_single_success_result(MemberRequest, 2, MemberResult),
  check_equality(MemberResult, ExpectedMemberResult),
  % Get the goal and runtime of the previous query
  LastQueryTimeRequest = 'jupyter:previous_query_time(Goal, Time).',
  ExpectedLastQueryTimeResult = [type=query,bindings=json(['Goal'='member(M,[1,2,3])','Time'=_Time]),output=''],
  send_call_with_single_success_result(LastQueryTimeRequest, 3, LastQueryTimeResult).

test(previous_query_time_sleep, [true(LastQueryTimeResult = ExpectedLastQueryTimeResult)]) :-
  SleepRequest = 'use_module(library(system)), sleep(1).',
  expected_output(previous_query_time_sleep, ExpectedOutput),
  ExpectedSleepResult = [type=query,bindings=json([]),output=ExpectedOutput],
  send_call_with_single_success_result(SleepRequest, 4, SleepResult),
  check_equality(SleepResult, ExpectedSleepResult),
  % Get the goal and runtime of the previous query
  LastQueryTimeRequest = 'jupyter:previous_query_time(Goal, Time).',
  ExpectedLastQueryTimeResult = [type=query,bindings=json(['Goal'='use_module(library(system)),sleep(1)','Time'=_Time]),output=''],
  send_call_with_single_success_result(LastQueryTimeRequest, 5, LastQueryTimeResult).

:- end_tests(previous_query_time).


:- begin_tests(print_previous_queries, [setup(start_process), cleanup(release_process(true))]).

test(previous_queries_with_multiple_variables, [true(PrevQueriesResult = ExpectedPrevQueriesResult)]) :-
  AppendRequest = 'X = [1,2,3], Y = [4,5,6], append(X, Y, Z).',
  ExpectedAppendResult = [type=query,bindings=json(['X'='[1,2,3]','Y'='[4,5,6]','Z'='[1,2,3,4,5,6]']),output=''],
  send_call_with_single_success_result(AppendRequest, 1, AppendResult),
  check_equality(AppendResult, ExpectedAppendResult),
  % Print the query with id 1
  PrevQueriesRequest = 'jupyter:print_previous_queries([1]).',
  ExpectedPrevQueriesResult = [type=query,bindings=json([]),output='  X=[1,2,3],Y=[4,5,6],append(X,Y,Z).'],
  send_call_with_single_success_result(PrevQueriesRequest, 2, PrevQueriesResult).

test(previous_queries_with_previous_variable_binding, [true(PrevQueriesResult = ExpectedPrevQueriesResult)]) :-
  BindingXRequest = 'X = [1,2,3].',
  ExpectedBindingXResult = [type=query,bindings=json(['X'='[1,2,3]']),output=''],
  send_call_with_single_success_result(BindingXRequest, 3, BindingXResult),
  check_equality(BindingXResult, ExpectedBindingXResult),
  MemberRequest = 'member(Member, [1,2,3]).',
  ExpectedMemberResult = [type=query,bindings=json(['Member'='1']),output=''],
  send_call_with_single_success_result(MemberRequest, 4, MemberResult),
  check_equality(MemberResult, ExpectedMemberResult),
  PrintRequest = 'print($Member), nl, print($X).',
  ExpectedPrintResult = [type=query,bindings=json(['Member'='1','X'='[1,2,3]']),output='1\n[1,2,3]'],
  send_call_with_single_success_result(PrintRequest, 5, PrintResult),
  check_equality(PrintResult, ExpectedPrintResult),
  % Print the two previous queries -> '$Member' is replaced by 'Member' and '$X' is not replaced (does not occur in one of the queries which are printed)
  PrevQueriesRequest = 'jupyter:print_previous_queries([4,5]).',
  ExpectedPrevQueriesResult = [type=query,bindings=json([]),output='  member(Member,[1,2,3]),\n  print(Member),nl,print($X).'],
  send_call_with_single_success_result(PrevQueriesRequest, 6, PrevQueriesResult).

test(previous_queries_with_non_existent_ids, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_previous_queries([10,11,12]).',
  ExpectedResult = [type=query,bindings=json([]),output=''],
  send_call_with_single_success_result(Request, 7, Result).

test(previous_queries_no_ids, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_previous_queries([]).',
  ExpectedResult = [type=query,bindings=json([]),output=''],
  send_call_with_single_success_result(Request, 8, Result).

:- end_tests(print_previous_queries).


:- if(swi).
expected_variable_bindings(multiple_ranges_for_one_variable, ['X'=json([dom='1..6']),'Y'=json([dom='2\\/4..12'])]).
:- else.
expected_variable_bindings(multiple_ranges_for_one_variable, ['X'=json([dom='1..6']),'Y'=json([dom='{2}\\/(4..12)'])]).
:- endif.


load_clpfd_module :-
  send_call_with_single_success_result('use_module(library(clpfd)).', 0, [type=query,bindings=json([]),output=_LoadOutput]).


:- begin_tests(clpfd, [setup((start_process, load_clpfd_module)), cleanup(release_process(true))]).

test(ranges, [true(Result = ExpectedResult)]) :-
  Request = 'X in 1..5, Y in 2..8, X+Y #= Z.',
  ExpectedResult = [type=query,bindings=json(['X'=json([dom='1..5']),'Y'=json([dom='2..8']),'Z'=json([dom='3..13'])]),output=''],
  send_call_with_single_success_result(Request, 1, Result).

test(ranges_and_bind, [true(Result = ExpectedResult)]) :-
  Request = 'X in 1..5, Y #= 4 #\\/ Y #= 8, X + Y #< 8.',
  ExpectedResult = [type=query,bindings=json(['X'=json([dom='1..3']),'Y'='4']),output=''],
  send_call_with_single_success_result(Request, 2, Result).

test(multiple_ranges_for_one_variable, [true(Result = ExpectedResult)]) :-
  Request = 'X in 1..6, Y #= X+X, Y #\\= 3.',
  expected_variable_bindings(multiple_ranges_for_one_variable, ExpectedVariableBindings),
  ExpectedResult = [type=query,bindings=json(ExpectedVariableBindings),output=''],
  send_call_with_single_success_result(Request, 3, Result).

test(missing_lower_bound, [true(Result = ExpectedResult)]) :-
  Request = 'X #< 10.',
  ExpectedResult = [type=query,bindings=json(['X'=json([dom='inf..9'])]),output=''],
  send_call_with_single_success_result(Request, 4, Result).

:- if(swi).
test(truth_value, [true(Result = ExpectedResult)]) :-
  Request = 'X+Y #= Z #<==> B, X=1, Z=6, Y in 1..10, Y #\\= 5.',
  ExpectedResult = [type=query,bindings=json(['X'='1','Y'=json([dom='1..4\\/6..10']),'Z'='6','B'=json([dom='0..1'])]),output=''],
  send_call_with_single_success_result(Request, 4, Result).
:- else.
test(truth_value, [true(Result = ExpectedResult)]) :-
  Request = 'X+Y #= Z #<=> B, X=1, Z=6, Y in 1..10, Y #\\= 5.',
  ExpectedResult = [type=query,bindings=json(['X'='1','Y'=json([dom='(1..4)\\/(6..10)']),'Z'='6','B'=json([dom='0..1'])]),output=''],
  send_call_with_single_success_result(Request, 4, Result).
:- endif.

:- end_tests(clpfd).


:- if(swi).
expected_error_info_subterm(print_sld_tree_no_single_goal, 0, 67, 'ERROR: jupyter:print_sld_tree/1 needs to be the only goal in a term').
expected_error_info_subterm(sld_tree_exception, 0, 48, 'ERROR: is/2: Arithmetic: `a/0\' is not a function').

expected_output(sld_tree_exception, '1').

expected_print_sld_tree(sld_tree_with_variable_bindings, 'digraph {\n    "1" [label="pred(A,B)"]\n    "2" [label="g1(A,C)"]\n    "3" [label="g11(A,D)"]\n    "4" [label="g12(b,C)"]\n    "5" [label="g2(c,B)"]\n    "1" -> "2"\n    "2" -> "3"\n    "2" -> "4"\n    "1" -> "5"\n}').
expected_print_sld_tree(sld_tree_with_multiple_goals_and_output, 'digraph {\n    "1" [label="print(test)"]\n    "2" [label="app([1,2],[3],[4],[1,2,3,4])"]\n    "3" [label="app([3],[4],A)"]\n    "4" [label="print(3)"]\n    "5" [label="app([],[4],B)"]\n    "6" [label="app([1,2],[3,4],[1,2,3,4])"]\n    "7" [label="print(1)"]\n    "8" [label="app([2],[3,4],[2,3,4])"]\n    "9" [label="print(2)"]\n    "10" [label="app([],[3,4],[3,4])"]\n    "11" [label="print(done)"]\n    "2" -> "3"\n    "3" -> "4"\n    "3" -> "5"\n    "2" -> "6"\n    "6" -> "7"\n    "6" -> "8"\n    "8" -> "9"\n    "8" -> "10"\n}').
expected_print_sld_tree(sld_tree_failure, 'digraph {\n    "1" [label="print(failure_test)"]\n    "2" [label="lists:append([1],[2],[3])"]\n}').
expected_print_sld_tree(sld_tree_exception, 'digraph {\n    "1" [label="member_square([1,a,3])"]\n    "2" [label="lists:member(A,[1,a,3])"]\n    "3" [label="B is 1*1"]\n    "4" [label="print(1)"]\n    "5" [label="fail"]\n    "6" [label="B is a*a"]\n    "1" -> "2"\n    "1" -> "3"\n    "1" -> "4"\n    "1" -> "5"\n    "1" -> "6"\n}').
:- else.
expected_error_info_subterm(print_sld_tree_no_single_goal, 0, 62, '! jupyter:print_sld_tree/1 needs to be the only goal in a term').
expected_error_info_subterm(sld_tree_exception, 0, 82, '! Type error in argument 2 of (is)/2\n! expected evaluable, but found a/0\n! goal:  ').
%                                                      '! Type error in argument 2 of (is)/2\n! expected evaluable, but found a/0\n! goal:  _702439 is a*a'

expected_output(sld_tree_exception, '1\n% The debugger is switched off').

expected_print_sld_tree(sld_tree_with_variable_bindings, 'digraph {\n    "4" [label="pred(A,B)"]\n    "5" [label="g1(A,C)"]\n    "6" [label="g11(A,D)"]\n    "7" [label="g12(b,C)"]\n    "8" [label="g2(c,B)"]\n    "4" -> "5"\n    "5" -> "6"\n    "5" -> "7"\n    "4" -> "8"\n}').
expected_print_sld_tree(sld_tree_with_multiple_goals_and_output, 'digraph {\n    "6934" [label="print(test)"]\n    "6935" [label="app([1,2],[3],[4],[1,2,3,4])"]\n    "6936" [label="app([3],[4],A)"]\n    "6937" [label="print(3)"]\n    "6938" [label="app([],[4],B)"]\n    "6939" [label="app([1,2],[3,4],[1,2,3,4])"]\n    "6940" [label="print(1)"]\n    "6941" [label="app([2],[3,4],[2,3,4])"]\n    "6942" [label="print(2)"]\n    "6943" [label="app([],[3,4],[3,4])"]\n    "6944" [label="print(done)"]\n    "6935" -> "6936"\n    "6936" -> "6937"\n    "6936" -> "6938"\n    "6935" -> "6939"\n    "6939" -> "6940"\n    "6939" -> "6941"\n    "6941" -> "6942"\n    "6941" -> "6943"\n}').
expected_print_sld_tree(sld_tree_failure, 'digraph {\n    "12495" [label="print(failure_test)"]\n    "12496" [label="append([1],[2],[3])"]\n}').
expected_print_sld_tree(sld_tree_exception, 'digraph {\n    "16757" [label="member_square([1,a,3])"]\n    "16758" [label="member(A,[1,a,3])"]\n    "16759" [label="B is 1*1"]\n    "16760" [label="print(1)"]\n    "16761" [label="B is a*a"]\n    "16757" -> "16758"\n    "16757" -> "16759"\n    "16757" -> "16760"\n    "16757" -> "16761"\n}').
:- endif.


% These tests rely on exact invocation numbers, which is why they might fail when the source code is changed or when a single test is run instead of test unit.

:- begin_tests(print_sld_tree, [setup((start_process)), cleanup(release_process(true))]).

test(sld_tree_with_variable_bindings, [true(Result = ExpectedResult)]) :-
  % Define clauses
  DefinitionRequest = 'pred(X, Z) :- g1(X, Y), g2(Y, Z). g1(X, Z) :- g11(X, Y), g12(Y, Z). g11(a, b). g12(b, c). g2(c, d).',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:pred/2\n',retracted_clauses=json([])]),
                                   '2'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:g1/2\n',retracted_clauses=json([])]),
                                   '3'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:g11/2\n',retracted_clauses=json([])]),
                                   '4'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:g12/2\n',retracted_clauses=json([])]),
                                   '5'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:g2/2\n',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 1, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Print the SLD tree
  Request = 'jupyter:print_sld_tree(pred(X, Y)).',
  expected_print_sld_tree(sld_tree_with_variable_bindings, ExpectedSldData),
  ExpectedResult = [type=query,bindings=json(['X'=a,'Y'=d]),output='',print_sld_tree=SldData],
  check_equality(SldData, ExpectedSldData),
  send_call_with_single_success_result(Request, 2, Result).

test(sld_tree_with_multiple_goals_and_output, [true(Result = ExpectedResult)]) :-
  % Define clauses
  DefinitionRequest = 'app([], Res, Res) :- !. app([Head|Tail], List, [Head|Res]) :-  print(Head),  app(Tail, List, Res). app(L1, L2, L3, Res) :-  app(L2, L3, R1),  app(L1, R1, Res).',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:app/3\n',retracted_clauses=json([])]),
                                   '2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '3'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:app/4\n',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 3, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Print the SLD tree
  Request = 'jupyter:print_sld_tree((print(test), app([1, 2], [3], [4], [1,2,3,4]), print(done))).',
  expected_print_sld_tree(sld_tree_with_multiple_goals_and_output, ExpectedSldData),
  ExpectedResult = [type=query,bindings=json([]),output=test312done,print_sld_tree=SldData],
  check_equality(SldData, ExpectedSldData),
  send_call_with_single_success_result(Request, 4, Result).

test(sld_tree_failure, [true(Result = ExpectedResult)]) :-
  % When printing the SLD tree, everything computed before the failure is output
  Request = 'jupyter:print_sld_tree((print(failure_test), append([1], [2], [3]), print(not_reached))).',
  expected_print_sld_tree(sld_tree_failure, ExpectedSldData),
  ExpectedResult = json([code= -4711,message='Failure',data=json([error_info='',output=failure_test,print_sld_tree=SldData])]),
  check_equality(SldData, ExpectedSldData),
  send_call_with_single_error_result(Request, 5, Result).

test(sld_tree_exception, [true(SldData = ExpectedSldData)]) :-
  % Define a predicate
  DefinitionRequest = 'member_square(List) :- member(M, List), S is M*M, print(S), fail.',
  ExpectedDefinitionResult = [type=clause_definition,bindings=json([]),output='% Asserting clauses for user:member_square/1\n',retracted_clauses=json([])],
  send_call_with_single_success_result(DefinitionRequest, 6, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % When printing the SLD tree, everything computed before the exception is output
  Request = 'jupyter:print_sld_tree(member_square([1,a,3])).',
  expected_error_info_subterm(sld_tree_exception, Before, Length, ExpectedErrorInfoSubterm),
  expected_output(sld_tree_exception, ExpectedOutput),
  expected_print_sld_tree(sld_tree_exception, ExpectedSldData),
  Result = json([code= -4712,message='Exception',data=json([error_info=ErrorInfo,output=Output,print_sld_tree=SldData])]),
  send_call_with_single_error_result(Request, 7, Result),
  % Get the subterm of the error info which is to be compared with the expected one
  sub_atom(ErrorInfo, Before, Length, _, ErrorInfoSubterm),
  check_equality(ErrorInfoSubterm, ExpectedErrorInfoSubterm),
  check_equality(Output, ExpectedOutput).

test(print_sld_tree_no_single_goal, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(print_sld_tree_no_single_goal, 'jupyter:print_sld_tree(print(test)), print(exception).', 8, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

:- end_tests(print_sld_tree).


:- if(swi).
expected_error_info_subterm(incorrect_pred_spec, 0, 128, 'ERROR: Incorrect predicate specification: edge\nERROR: It needs to be of the form PredName/PredArity or Module:PredName/PredArity').
expected_error_info_subterm(incorrect_index, 0, 77, 'ERROR: All indices need to be less or equal to the provided predicate arity 3').
expected_error_info_subterm(print_transition_graph_no_single_goal, 0, 75, 'ERROR: jupyter:print_transition_graph/4 needs to be the only goal in a term').
:- else.
expected_error_info_subterm(incorrect_pred_spec, 0, 118, '! Incorrect predicate specification: edge\n! It needs to be of the form PredName/PredArity or Module:PredName/PredArity').
expected_error_info_subterm(incorrect_index, 0, 72, '! All indices need to be less or equal to the provided predicate arity 3').
expected_error_info_subterm(print_transition_graph_no_single_goal, 0, 70, '! jupyter:print_transition_graph/4 needs to be the only goal in a term').
:- endif.


define_transition_predicates :-
  % Define the predicates edge/3 and two_way_edge/3
  DefinitionRequest = 'edge(a, 71, b). edge(a, 151, c). edge(b, 80, c). edge(c, 99, d). edge(d, 75, b). edge(d, 140, a). two_way_edge(N1, L, N2) :- edge(N1, L, N2). two_way_edge(N1, L, N2) :- edge(N2, L, N1).',
  ExpectedDefinitionResult = json(['1'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:edge/3\n',retracted_clauses=json([])]),
                                   '2'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '3'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '4'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '5'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '6'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])]),
                                   '7'=json([status=success,type=clause_definition,bindings=json([]),output='% Asserting clauses for user:two_way_edge/3\n',retracted_clauses=json([])]),
                                   '8'=json([status=success,type=clause_definition,bindings=json([]),output='',retracted_clauses=json([])])]),
  send_success_call(DefinitionRequest, 0, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult).


:- begin_tests(print_transition_graph, [setup((start_process, define_transition_predicates)), cleanup(release_process(true))]).

test(incorrect_pred_spec, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(incorrect_pred_spec, 'jupyter:print_transition_graph(edge, 1, 3, 2).', 1, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(incorrect_from_index, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(incorrect_index, 'jupyter:print_transition_graph(user:edge/3, 4, 3, 2).', 2, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(incorrect_to_index, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(incorrect_index, 'jupyter:print_transition_graph(edge/3, 1, 5, 2).', 3, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(incorrect_label_index, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(incorrect_index, 'jupyter:print_transition_graph(edge/3, 1, 3, 7).', 4, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(incorrect_label_index, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(incorrect_index, 'jupyter:print_transition_graph(edge/3, 1, 3, 7).', 5, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(print_transition_graph_no_single_goal, [true(ErrorInfoSubterm = ExpectedErrorInfoSubterm)]) :-
  error_result_message_subterms(print_transition_graph_no_single_goal, 'jupyter:print_transition_graph(edge/3, 1, 3, 2), print(exception).', 6, ErrorInfoSubterm, ExpectedErrorInfoSubterm).

test(transition_graph, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_transition_graph(edge/3, 1, 3, 2).',
  ExpectedResult = [type=query,bindings=json([]),output='',print_transition_graph='digraph {\n    "a" -> "b" [label="71"]\n    "a" -> "c" [label="151"]\n    "b" -> "c" [label="80"]\n    "c" -> "d" [label="99"]\n    "d" -> "b" [label="75"]\n    "d" -> "a" [label="140"]\n}'],
  send_call_with_single_success_result(Request, 7, Result).

test(transition_graph_without_labels, [true(Result = ExpectedResult)]) :-
  Request = 'jupyter:print_transition_graph(user:two_way_edge/3, 1, 3, 0).',
  ExpectedResult = [type=query,bindings=json([]),output='',print_transition_graph='digraph {\n    "a" -> "b"\n    "a" -> "c"\n    "b" -> "c"\n    "c" -> "d"\n    "d" -> "b"\n    "d" -> "a"\n    "b" -> "a"\n    "c" -> "a"\n    "c" -> "b"\n    "d" -> "c"\n    "b" -> "d"\n    "a" -> "d"\n}'],
  send_call_with_single_success_result(Request, 8, Result).

:- end_tests(print_transition_graph).


:- begin_tests(help, [setup((start_process)), cleanup(release_process(true))]).

:- if(sicstus).
test(help_0, [true(HelpResult = ExpectedHelpResult)]) :-
  % If user:help/0 is called instead of jupyter:help/0 and the predicate is not defined, an error message is output
  HelpRequest = 'help.',
  ExpectedError = json([code= -4712,message='Exception',data=json([error_info='! Existence error in user:help/0\n! procedure user:help/0 does not exist\n! goal:  user:help\n! \n! However, there is the predicate jupyter:help/0'])]),
  send_call_with_single_error_result(HelpRequest, 1, HelpError),
  check_equality(HelpError, ExpectedError),
  % Define the predicate help/0
  DefinitionRequest = 'help :- print(help).',
  ExpectedDefinitionResult = [type=clause_definition,bindings=json([]),output='% Asserting clauses for user:help/0\n',retracted_clauses=json([])],
  send_call_with_single_success_result(DefinitionRequest, 2, DefinitionResult),
  check_equality(DefinitionResult, ExpectedDefinitionResult),
  % Once the predicate is defined, calling it works as expected
  AppRequest = 'help.',
  ExpectedHelpResult = [type=query,bindings=json([]),output='help'],
  send_call_with_single_success_result(AppRequest, 3, HelpResult).
:- endif.

:- end_tests(help).
