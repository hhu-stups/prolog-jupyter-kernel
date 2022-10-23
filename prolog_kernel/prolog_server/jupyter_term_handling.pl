
% This module provides predicates to handle terms received from the client, compute their results and assert them with term_response/1.
% There are three main types of terms.
% For each of the types there are terms which need to be handled specially.
% The following types of terms are differentiated:
% - directives:
%   - begin_tests/1
%   - begin_tests/2
%   - end_tests/1
%   - any other directive
% - clause definitions:
%   - test(Name) :- Body
%   - test(Name, Options) :- Body
%   - Head :- Body
%   - Head --> Body
%   - Head (if the request contains more than one term)
% - queries (including terms following '?-'):
%   - retry or jupyter:retry
%   - cut or jupyter:cut
%   - halt or jupyter:halt
%   - a call of a special jupyter predicate:
%     - jupyter:print_table/1 or jupyter:print_table/2
%     - jupyter:print_sld_tree/1
%     - jupyter:print_transition_graph/1,3,4
%     - jupyter:show_graph/2  % alternative name for print_transition_graph with Node and Edge predicate
%     - jupyter:set_prolog_impl/1
%     - jupyter:update_completion_data/0
%     - jupyter:print_stack/0
%   - a call of run_tests: run_tests/0, run_tests/1 or run_tests/2
%   - a call of trace: trace/0, trace/1 or trace/2
%   - a call of leash/1
%   - a call of abolish/1 or abolish/2(in case of SICStus Prolog)
%   - any other term which is the only one of a request


:- module(jupyter_term_handling,
    [assert_sld_data/4,        % assert_sld_data(Port, Goal, Frame, ParentFrame)
     declaration_end/1,        % declaration_end(+LoadFile)
     handle_term/6,            % handle_term(+Term, +IsSingleTerm, +CallRequestId, +Stack, +Bindings, -Cont)
     pred_definition_specs/1,  % pred_definition_specs(PredDefinitionSpecs)
     term_response/1,          % term_response(JsonResponse)
     test_definition_end/1     % test_definition_end(+LoadFile)
    ]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(library(codesio), [write_term_to_codes/3, format_to_codes/3, read_term_from_codes/3]).
:- use_module(library(lists), [delete/3, reverse/2, nth1/3, append/2]).
:- use_module(jupyter_logging, [log/1, log/2]).
:- use_module(jupyter_query_handling, [call_with_output_to_file/3, call_query_with_output_to_file/7, redirect_output_to_file/0]).
:- use_module(jupyter_jsonrpc, [send_error_reply/3]).
:- use_module(jupyter_request_handling, [loop/3]).
:- use_module(jupyter_preferences, [set_preference/3, get_preference/2, get_preferences/1]).

:- if(sicstus).
:- use_module(library(aggregate), [forall/2]).
:- use_module(library(file_systems), [delete_file/1]).
:- use_module(jupyter_variable_bindings, [term_with_stored_var_bindings/4, store_var_bindings/1]).
:- endif.


:- dynamic
  declaration_stream/1,     % declaration_stream(DeclarationStream)
                            % DeclarationStream is a write stream if the current request contains declaration directives.
  is_retry/1,               % is_retry(IsRetry)
  pred_definition_specs/1,  % pred_definition_specs(PredDefinitionSpecs)
                            % PredDefinitionSpecs is a list of PredName/PredArity elements for every predicate which is defined by the current request.
  term_response/1,          % term_response(JsonResponse)
  test_definition_stream/1. % test_definition_stream(TestDefinitionStream)
                            % TestDefinitionStream is a write stream if the current request contains a begin_tests request.


test_file_base_name('jupyter_tests').


% test_file_name(?Unit, -TestFileName)
%
% PL-Unit tests need to be defined in a file which is loaded.
% Unit is the name of the test unit.
% TestFileName is the name of the file to which test definitions are written.
:- if(swi).

% In SWI-Prolog, the following causes an error:
% - load a file defining a test unit U
% - run the tests
% - change the file so that test unit U is not defined anymore
% - load the same file again
% - run the tests
% Therefore, the same test file name cannot be reused for several requests.
% However, when a test unit was loaded from a file X, a test unit with the same name cannot be loaded from file Y afterwards.
% Thus, every test unit is written to a file of which the name contains the unit name.

:- dynamic current_test_file_name/2.  % current_test_file_name(Unit, TestFileName)

% Unit is bound when this predicate is called for an encountered begin_tests directive.
% In that case, a new file name is computed.
test_file_name(Unit, TestFileName) :-
  nonvar(Unit),
  !,
  % Compute the test file name and assert it so that it can be loaded when handling a different term
  test_file_name_(Unit, TestFileName),
  catch(retractall(current_test_file_name(_Unit, _TestFileName)), _Exception, true),
  assert(current_test_file_name(Unit, TestFileName)).
test_file_name(Unit, TestFileName) :-
  current_test_file_name(Unit, TestFileName).


% test_file_name(+Unit, -TestFileName)
%
% Computes TestFileName by appending '_' and the unit name Unit to the base file name retrieved with test_file_base_name/1.
test_file_name_(Unit, TestFileName) :-
  test_file_base_name(TestFileBaseName),
  atom_concat(Unit, '.pl', UnitNameWithFileExtension),
  atom_concat('_', UnitNameWithFileExtension, UnitNameWithFileExtensionAndUnderscore),
  atom_concat(TestFileBaseName, UnitNameWithFileExtensionAndUnderscore, TestFileName).

:- else.
% In SICStus Prolog, a file containing `plunit` test clauses defines the predicates `'unit body'/4` and `'unit test'/5`.
% If test files with different names are loaded, these predicates are redefined and corresponding messages are output.
% Therefore, the test file should always be called the same.
test_file_name(_Unit, TestFileName) :-
  test_file_base_name(TestFileBaseName),
  atom_concat(TestFileBaseName, '.pl', TestFileName).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle_term(+Term, +IsSingleTerm, +CallRequestId, +Stack, +Bindings, -Cont)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term Term.
% Check which type of term Term is and handle it accordingly.
% Term can be either a directive, a clause definition (which might be a DCG rule), or a query
% Directives
handle_term((:- Directive), IsSingleTerm, CallRequestId, Stack, Bindings, Cont) :- !,
  handle_directive((:- Directive), IsSingleTerm, CallRequestId, Stack, Bindings, Cont).
% Clause definitions
handle_term((Head :- Body), _IsSingleTerm, _CallRequestId, _Stack, Bindings, continue) :- !,
  handle_clause_definition_term((Head :- Body), Bindings).
handle_term((Head --> Body), _IsSingleTerm, _CallRequestId, _Stack, _Bindings, continue) :- !,
  handle_dcg((Head --> Body)).
% Queries
handle_term(?-(Query), _IsSingleTerm, CallRequestId, Stack, Bindings, Cont) :- !,
  handle_query_term(Query, false, CallRequestId, Stack, Bindings, continue, Cont).
handle_term(Query, true, CallRequestId, Stack, Bindings, Cont) :-
  handle_query_term(Query, false, CallRequestId, Stack, Bindings, continue, Cont).
% Clause definitions
handle_term(Head, false, _CallRequestId, _Stack, Bindings, continue) :-
  handle_clause_definition_term(Head, Bindings).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Directives

% The directives begin_tests and end_tests are handled specially.
% All other directives are called with jupyter_query_handling:call_with_output_to_file/3.
% The runtime of the exeuction and additional query data is not asserted as it is the case for queries.
% Furthermore, a retry is not possible and a directive's variable bindings are not sent to the client.

% For SICStus, declarations need to be handled specially as they must not appear in a query.
% Therefore, all declarations of a request are written to a file which is loaded.
% Thus, all declarations which are to be valid at the same time, need to be defined in a single request.
% The declaration file is loaded when all terms of a request have been handled.
% When declaring a property of a predicate for which clauses had been asserted before, these do not exist anymore after the declaration.
% Therefore, a cell declaring predicate properties cannot contain clauses for the same predicate.

% handle_directive(+Term, +IsSingleTerm, +CallRequestId, +Stack, +Bindings, +Cont)
%
% begin_tests/1
handle_directive((:- begin_tests(_Unit)), true, _CallRequestId, _Stack, _Bindings, continue) :- !,
  handle_single_test_directive.
handle_directive((:- begin_tests(Unit)), _IsSingleTerm, _CallRequestId, _Stack, Bindings, continue) :- !,
  handle_begin_tests((:- begin_tests(Unit)), Unit, Bindings).
% begin_tests/2
handle_directive((:- begin_tests(_Unit, _Options)), true, _CallRequestId, _Stack, _Bindings, continue) :- !,
  handle_single_test_directive.
handle_directive((:- begin_tests(Unit, Options)), _IsSingleTerm, _CallRequestId, _Stack, Bindings, continue) :- !,
  handle_begin_tests((:- begin_tests(Unit, Options)), Unit, Bindings).
% end_tests/1
handle_directive((:- end_tests(_Unit)), true,_CallRequestId, _Stack,  _Bindings, continue) :- !,
  handle_single_test_directive.
handle_directive((:- end_tests(Unit)), _IsSingleTerm, _CallRequestId, _Stack, _Bindings, continue) :- !,
  handle_end_tests((:- end_tests(Unit))).
:- if(sicstus).
handle_directive((:- Declaration), _IsSingleTerm, _CallRequestId, _Stack, _Bindings, continue) :-
  functor(Declaration, DeclarationName, DeclarationArity),
  declaration_name_arity(DeclarationName, DeclarationArity),
  !,
  handle_declaration_directive(DeclarationName, (:- Declaration)).
:- endif.
% Any other directive
handle_directive((:- Directive), _IsSingleTerm, CallRequestId, Stack, Bindings, Cont) :- !,
  handle_query_term(Directive, true, CallRequestId, Stack, Bindings, cut, Cont).


:- if(sicstus).
% declaration_name_arity(-Name,-Arity)
%
% Name and Arity are the name and arity of any declaration listed by the Predicate Index website: https://sicstus.sics.se/sicstus/docs/4.7.1/html/sicstus.html/Predicate-Index.html
% Since the begin_tests and end_tests directives are handled differently, they are not listed here.
declaration_name_arity(attribute, 1).
declaration_name_arity(block, 1).
declaration_name_arity(chr_constraint, 1).
declaration_name_arity(chr_option, 2).
declaration_name_arity(chr_type, 1).
declaration_name_arity(discontiguous, 1).
declaration_name_arity(dynamic, 1).
declaration_name_arity(include, 1).
declaration_name_arity(initialization, 1).
declaration_name_arity(is, 2).
declaration_name_arity(meta_predicate, 1).
declaration_name_arity(mode, 1).
declaration_name_arity(multifile, 1).
declaration_name_arity(public, 1).
declaration_name_arity(volatile, 1).


:- dynamic jupyter_discontiguous/1.  % jupyter_discontiguous(PredSpec)
                                     % The predicate with predicate spec PredSpec was declared discontiguous
                                     % As this predicate property cannot be retrieved with predicate_property (as is the case for SWI), a dynamic predicate needs to be used instead


declaration_file_name('jupyter_declaration.pl').


% handle_declaration_directive(+DeclarationName, +Declaration)
handle_declaration_directive(discontiguous, (:- Declaration)) :-
  Declaration =.. [discontiguous, PredSpec],
  assert(jupyter_discontiguous(PredSpec)),
  handle_declaration_directive((:- Declaration)).
handle_declaration_directive(_DeclarationName, DeclarationDirective) :-
  handle_declaration_directive(DeclarationDirective).


% handle_declaration_directive(+Declaration)
handle_declaration_directive(DeclarationDirective) :-
  declaration_stream(DeclarationStream),
  % Not the first declaration directive -> write to the existing file
  !,
  write_term_to_stream(DeclarationDirective, [], DeclarationStream).
handle_declaration_directive(DeclarationDirective) :-
  % First declaration directive of the request -> create a new file
  declaration_file_name(DeclarationFileName),
  open(DeclarationFileName, write, DeclarationStream),
  assert(declaration_stream(DeclarationStream)),
  write_term_to_stream(DeclarationDirective, [], DeclarationStream).


% declaration_end(+LoadFile)
%
% Closes and retracts the stream to which declaration_directives were written.
% If LoadFile=true, loads that file.
declaration_end(LoadFile) :-
  declaration_stream(DeclarationStream),
  !,
  close(DeclarationStream),
  retractall(declaration_stream(_)),
  declaration_file_name(DeclarationFileName),
  % When loading the file, an exception or warning might be output
  ( LoadFile==true ->
    % Disable the printing of informational messages so that the messages of the following form are not printed:
    % "% compiling cwd/jupyter_declaration.pl...""
    % "% compiled cwd/jupyter_declaration.pl in module user, 0 msec 112 bytes"
    prolog_flag(informational, PreviousInformationalValue, off),
    % When loading the file, an exception or warning might be output
    jupyter_query_handling:call_with_output_to_file(load_files(DeclarationFileName), Output, ErrorMessageData),
    % Reset the value of the Prolog flag 'informational'
    prolog_flag(informational, _, PreviousInformationalValue)
  ; otherwise ->
    Output = ''
  ),
  delete_file(DeclarationFileName),
  ( nonvar(ErrorMessageData) ->
    assert_error_response(exception, message_data(error, ErrorMessageData), Output, [])
  ; otherwise ->
    atom_concat(Output, '\n% Loaded the declaration file', OutputWithLoadMessage),
    assert_success_response(directive, [], OutputWithLoadMessage, [])
  ).
declaration_end(_LoadFile).
:- else.
declaration_end(_LoadFile).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Clause definitions

% In order to be able to redefine a predicate without having to remove its clauses first, previous clauses are retracted automatically.
% Whenever a clause is to be defined for a predicate for which there already are clauses, these are retracted and a message saying so is sent to the client.
% This implies that all clauses of a predicate need to be defined by one request.
% New clauses are added to the database with assertz/1.

% handle_clause_definition_term(+Clause, +Bindings)
%
% Test definitions
handle_clause_definition_term((test(Name) :- Body), Bindings) :-
  test_definition_stream(TestDefinitionStream),
  % If test_definition_stream/1 succeeds, there was a begin_tests directive
  % In that case, this test definition belongs to the unit test and is written to the test definition file
  !,
  write_term_to_stream((test(Name) :- Body), Bindings, TestDefinitionStream).
handle_clause_definition_term((test(Name, Options) :- Body), Bindings) :-
  test_definition_stream(TestDefinitionStream),
  % If test_definition_stream/1 succeeds, there was a begin_tests directive
  % In that case, this test definition belongs to the unit test and is written to the test definition file
  !,
  write_term_to_stream((test(Name, Options) :- Body), Bindings, TestDefinitionStream).
% Any other clause definition
handle_clause_definition_term(Clause, _Bindings) :-
  handle_clause_definition(Clause).


% handle_clause_definition(+Clause)
handle_clause_definition(Clause) :-
  module_name_expanded(Clause, Module:ClauseWithoutModule),
  clause_head(ClauseWithoutModule, Head),
  functor(Head, PredName, PredArity),
  retract_previous_clauses(Module:PredName/PredArity, RetractedClauses, Output),
  % Assert the clause and check if it was successful
  catch(assertz(Module:ClauseWithoutModule), Exception, true),
  ( nonvar(Exception) ->
    assert_error_response(exception, message_data(error, Exception), Output, [retracted_clauses=RetractedClauses])
  ; otherwise ->
    assert_success_response(clause_definition, [], Output, [retracted_clauses=RetractedClauses])
  ).


% module_name_expanded(+Term, -MTerm)
module_name_expanded((Module:Head:-Body), Module:(Head:-Body)) :- !.
module_name_expanded(Module:Term, Module:Term) :- !.
module_name_expanded(Term, user:Term).


% clause_head(+Clause, -Head)
clause_head((Head :- _Body), Head) :- !.
clause_head(Head, Head).


% retract_previous_clauses(+MPredSpec, -RetractedClauses, -Output)
retract_previous_clauses(MPredSpec, json([]), AssertMessage) :-
  % In case the predicate was declared as discontiguous, no previous clauses are retracted
  discontiguous_pred(MPredSpec),
  !,
  current_pred_definition_specs(PredDefinitionSpecs),
  ( member(MPredSpec, PredDefinitionSpecs) ->
    % For the current request, clauses have been defined for the given predicate
    AssertMessage = ''
  ; compute_assert_message(MPredSpec, AssertMessage),
    % Update the defined predicate definition specs
    catch(retractall(pred_definition_specs(_)), _Exception, true),
    assert(pred_definition_specs([MPredSpec|PredDefinitionSpecs]))
  ).
retract_previous_clauses(MPredSpec, RetractedClausesJson, Output) :-
  current_pred_definition_specs(PredDefinitionSpecs),
  retract_previous_clauses(MPredSpec, PredDefinitionSpecs, NewPredDefinitionSpecs, RetractedClauses, Output),
  % Update the defined predicate definition specs
  catch(retractall(pred_definition_specs(_)), _Exception, true),
  assert(pred_definition_specs(NewPredDefinitionSpecs)),
  ( var(RetractedClauses) ->
    RetractedClausesJson = json([])
  ; RetractedClausesJson = json([RetractedClauses])
  ).


% discontiguous_pred(+MPredSpec)
%
% Succeeds if the predicate with pred spec MPredSpec was declared discontiguous.
:- if(swi).
discontiguous_pred(Module:PredName/PredArity) :-
  functor(PredTerm, PredName, PredArity),
  predicate_property(Module:PredTerm, discontiguous).
:- else.
discontiguous_pred(Module:PredName/PredArity) :-
  jupyter_discontiguous(Module:PredName/PredArity).
discontiguous_pred(_Module:PredName/PredArity) :-
  jupyter_discontiguous(PredName/PredArity).
:- endif.


% current_pred_definition_specs(-PredDefinitionSpecs)
%
% PredDefinitionSpecs is a list of Module:PredName/PredArity elements for every predicate which is defined by the current request.
current_pred_definition_specs(PredDefinitionSpecs) :-
  pred_definition_specs(PredDefinitionSpecs),
  !.
current_pred_definition_specs([]).


% retract_previous_clauses(+MPredSpec, +PredDefinitionSpecs, -NewPredDefinitionSpecs, -RetractedClauses, -Output)
%
% MPredSpec is of the form Module:PredName/PredArity.
% PredDefinitionSpecs is a list of Module:PredName/PredArity elements for every predicate which is defined by the current request.
% The predicate specs need to be remembered so that the first time a clause for a new predicate is encountered, all previous clauses can be retracted.
% RetractedClauses is of the form MPredSpecAtom=ListingOutput.
%  MPredSpecAtom is an atom representing MPredSpec.
%  ListingOutput is an atom of all the previously defined clauses if there were any which had to be retracted.
%  Otherwise, RetractedClauses is not bound to anything.
% When the first clause of a predicate is asserted, Output is an atom saying which predicate is being asserted.
%  Otherwise, Output=''.
retract_previous_clauses(MPredSpec, PredDefinitionSpecs, PredDefinitionSpecs, _RetractedClauses, '') :-
  member(MPredSpec, PredDefinitionSpecs),
  % This is not the first clause of the PredName/PredArity predicate for the current request -> no clauses have to be retracted
  !.
retract_previous_clauses(Module:PredName/PredArity, PredDefinitionSpecs, [Module:PredName/PredArity|PredDefinitionSpecs], _RetractedClauses, AssertMessage) :-
  functor(Term, PredName, PredArity),
  \+ predicate_property(Module:Term, _Property),
  % The predicate does not exist yet -> no clauses have to be retracted
  !,
  compute_assert_message(Module:PredName/PredArity, AssertMessage).
retract_previous_clauses(Module:PredName/PredArity, PredDefinitionSpecs, PredDefinitionSpecs, _RetractedClauses, '') :-
  functor(Term, PredName, PredArity),
  \+ predicate_property(Module:Term, dynamic),
  % The predicate is not dynamic -> no clauses can be asserted
  % Try asserting anyway so that the corresponding error reply is sent to the client
  !.
retract_previous_clauses(Module:PredName/PredArity, PredDefinitionSpecs, [Module:PredName/PredArity|PredDefinitionSpecs], MPredSpecAtom=ListingOutput, AssertMessage) :-
  functor(Head, PredName, PredArity),
  clause(Module:Head, _Body),
  % Use listing/1 to get all the clauses that are to be retracted.
  jupyter_query_handling:call_with_output_to_file(listing(Module:PredName/PredArity), ListingOutput, ExceptionMessage),
  var(ExceptionMessage),
  !,
  % Create an atom of MPredSpec so that it is JSON parsable
  write_term_to_atom(Module:PredName/PredArity, [], MPredSpecAtom),
  % Create a new unbound term to retract all clauses
  functor(Term, PredName, PredArity),
  retractall(Module:Term),
  compute_assert_message(Module:PredName/PredArity, AssertMessage).
retract_previous_clauses(PredSpec, PredDefinitionSpecs, [PredSpec|PredDefinitionSpecs], _RetractedClauses, AssertMessage) :-
  % There are no clauses to retract
  compute_assert_message(PredSpec, AssertMessage).


% compute_assert_message(+MPredSpec, -AssertMessage)
compute_assert_message(PredSpec, AssertMessage) :-
  format_to_atom('% Asserting clauses for ~w~n', [PredSpec], AssertMessage).

format_to_atom(_,_,Atom) :-  get_preference(verbosity,L), L<1,!, Atom=''.
format_to_atom(Msg,Args,Atom) :- 
  format_to_codes(Msg, Args, Codes),
  atom_codes(Atom, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DCG

% DCGs are transformed into clauses with term expansion and handled like other clause definitions with handle_clause_definition/1.

% handle_dcg(+DCG)
handle_dcg(DCG) :-
  expand_dcg_term(DCG, ExpandedDCG),
  handle_clause_definition(ExpandedDCG).


% expand_dcg_term(+DCG, -ExpandedDCG)
:- if(swi).
expand_dcg_term(DCG, ExpandedDCG) :-
  dcg_translate_rule(DCG, ExpandedDCG).
:- else.
expand_dcg_term(DCG, ExpandedDCG) :-
  expand_term(DCG, ExpandedDCG).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Queries

% In case of any other query not handled by any of the predicates defined above, the query is called by jupyter_query_handling:call_query_with_output_to_file/7.
% Before calling it, any $Var terms are replaced by corresponding values from previous queries.
% Additionally, the output of the goal and debugging messages are redirected to a file so that it can be read in and sent to the client.

% jupyter_query_handling:call_query_with_output_to_file/7 leaves a choice point.
% This way, when a 'retry' term is encountered in a future request, its failing causes the goal to be retried.

% handle_query_term(+Term, +IsDirective, +CallRequestId, +Stack, +Bindings, +LoopCont, -Cont)
handle_query_term(Term, IsDirective, CallRequestId, Stack, Bindings, LoopCont, Cont) :-
  % Before executing a query, replace any of its subterms of the form $Var by the latest value of the variable Var from a previous query.
  replace_previous_variable_bindings(Term, Bindings, UpdatedTerm, UpdatedBindings, Exception),
  ( nonvar(Exception) ->
    assert_error_response(exception, message_data(error, Exception), '', []),
    Cont = continue
  ; otherwise ->
    % Create a term_data(TermAtom, Bindings) term.
    % If the term is a query, the term_data term is used to assert the original term data in case terms of the form $Var were replaced.
    % The term data is needed when accessing previous queries (e.g. with jupyter:print_queries/1).
    % Bindings needs to be copied so that the term can be read from the atom without any of the variables being instantiated by calling the term.
    copy_term(Bindings, BindingsCopy),
    write_term_to_atom(Term, Bindings, TermAtom),
    handle_query_term_(UpdatedTerm, IsDirective, CallRequestId, Stack, UpdatedBindings, term_data(TermAtom, BindingsCopy), LoopCont, Cont)
  ).


% replace_previous_variable_bindings(+Term, +Bindings, -UpdatedTerm, -UpdatedBindings, -Exception)
:- if(swi).
replace_previous_variable_bindings(Term, Bindings, UpdatedTerm, UpdatedBindings, Exception) :-
  catch(toplevel_variables:expand_query(Term, UpdatedTerm, Bindings, UpdatedBindings), Exception, true).
:- else.
replace_previous_variable_bindings(Term, Bindings, UpdatedTerm, UpdatedBindings, Exception) :-
  catch(jupyter_variable_bindings:term_with_stored_var_bindings(Term, Bindings, UpdatedTerm, UpdatedBindings), Exception, true).
:- endif.


% handle_query_term_(+Query, +IsDirective, +CallRequestId, +Stack, +Bindings, +OriginalTermData, +LoopCont, -Cont)
% retry
handle_query_term_(retry, _IsDirective, _CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_retry(Stack).
handle_query_term_(jupyter:retry, _IsDirective, _CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_retry(Stack).
% cut
handle_query_term_(jupyter:cut, _IsDirective, _CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_cut(Stack, Cont).
handle_query_term_(cut, _IsDirective, _CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_cut(Stack, Cont).
% halt
handle_query_term_(jupyter:halt, _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, done) :- !,
  % By unifying Cont=done, the loop reading and handling messages is stopped
  handle_halt.
handle_query_term_(halt, _IsDirective,_CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, done) :- !,
  % By unifying Cont=done, the loop reading and handling messages is stopped
  handle_halt.
% jupyter predicates
handle_query_term_(jupyter:print_sld_tree(Goal), _IsDirective, _CallRequestId, _Stack, Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_sld_tree(Goal, Bindings).
handle_query_term_(jupyter:print_stack, _IsDirective, CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_stack(Stack, CallRequestId).
handle_query_term_(jupyter:print_table(Goal), _IsDirective, _CallRequestId, _Stack, Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_table_with_findall(Bindings, Goal).
handle_query_term_(jupyter:print_table(ValuesLists, VariableNames), _IsDirective, _CallRequestId, _Stack, Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_table(Bindings, ValuesLists, VariableNames).
handle_query_term_(jupyter:print_transition_graph(PredSpec, FromIndex, ToIndex, LabelIndex),
                   _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_transition_graph(true,PredSpec, FromIndex, ToIndex, LabelIndex).
handle_query_term_(jupyter:print_transition_graph(PredSpec, FromIndex, ToIndex),
                  _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_transition_graph(true,PredSpec, FromIndex, ToIndex, 0).
handle_query_term_(jupyter:show_graph(NodeSpec,PredSpec),
                  _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_transition_graph(NodeSpec,PredSpec).
handle_query_term_(jupyter:set_prolog_impl(PrologImplementationID), _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_set_prolog_impl(PrologImplementationID).
handle_query_term_(jupyter:update_completion_data, 
                   _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_update_completion_data.
handle_query_term_(jupyter:set_preference(Pref,Value), 
                   _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_set_preference(Pref,Value).
% run_tests
handle_query_term_(run_tests, _IsDirective, CallRequestId, Stack, Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_run_tests(run_tests, CallRequestId, Stack, Bindings, Cont).
handle_query_term_(run_tests(Spec), _IsDirective, CallRequestId, Stack, Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_run_tests(run_tests(Spec), CallRequestId, Stack, Bindings, Cont).
handle_query_term_(run_tests(Spec, Options), _IsDirective, CallRequestId, Stack, Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_run_tests(run_tests(Spec, Options), CallRequestId, Stack, Bindings, Cont).
% trace
handle_query_term_(trace, _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_trace(trace/0).
:- if(swi).
handle_query_term_(trace(_Pred), _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_trace(trace/1).
handle_query_term_(trace(_Pred, _Ports), _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_trace(trace/2).
:- endif.
% leash/1
handle_query_term_(leash(_Ports), _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  assert_error_response(exception, message_data(error, jupyter(leash_pred)), '', []).
:- if(sicstus).
% abolish
handle_query_term_(abolish(Predicates), _IsDirective, CallRequestId, _Stack, _Bindings, OriginalTermData, _LoopCont, continue) :- !,
  handle_abolish(abolish(Predicates), CallRequestId, OriginalTermData).
handle_query_term_(abolish(Predicates, Options), _IsDirective, CallRequestId, _Stack, _Bindings, OriginalTermData, _LoopCont, continus) :- !,
  handle_abolish(abolish(Predicates, Options), CallRequestId, OriginalTermData).
:- endif.
% Any other query
handle_query_term_(Query, IsDirective, CallRequestId, Stack, Bindings, OriginalTermData, LoopCont, Cont) :-
  handle_query(Query, IsDirective, CallRequestId, Stack, Bindings, OriginalTermData, LoopCont, Cont).


% handle_query(+Goal, +IsDirective, +CallRequestId, +Stack, +Bindings, +OriginalTermData, +LoopCont, -Cont)
%
% Goal is the current term of the request which is to be handled.
% IsDirective=true if the Goal was encountered as a directive.
%  In that case, no variable bindings are sent to the client.
% CallRequestId is the ID of the current call request.
%  It is needed for juypter:print_queries/1.
% Stack is a list containing atoms representing the previous queries which might have exited with a choicepoint and can therefore be retried.
%  It is needed for retry/0 and cut/0 queries.
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal Goal.
% LoopCont is one of continue and cut.
%  If LoopCont = cut, the recurse loop (jupyter_request_handling:loop/3) will exit right away without making retrys of a term possible.
% Cont will be processed by jupyter_request_handling:loop/3.
handle_query(Goal, IsDirective, CallRequestId, Stack, Bindings, OriginalTermData, LoopCont, Cont) :-
  % In order to send the goal to the client, it has to be converted to an atom
  % This has to be done before calling it causes variables to be bound
  write_term_to_atom(Goal, Bindings, GoalAtom),
  RecStack = [GoalAtom|Stack],
  retractall(is_retry(_)),
  asserta(is_retry(false)),
  % Call the goal Goal
  jupyter_query_handling:call_query_with_output_to_file(Goal, CallRequestId, Bindings, OriginalTermData, Output, ErrorMessageData, IsFailure),
  retry_message_and_output(GoalAtom, Output, RetryMessageAndOutput),
  % Exception, failure or success from Goal
  ( nonvar(ErrorMessageData) -> % Exception
    !,
    assert_error_response(exception, ErrorMessageData, RetryMessageAndOutput, []),
    Cont = continue
  ; IsFailure == true -> % Failure
    !,
    % Also happens when 'retry' message requested a new solution and found none.
    assert_query_failure_response(IsDirective, GoalAtom, RetryMessageAndOutput),
    Cont = continue
  ; otherwise -> % Success
    handle_result_variable_bindings(Bindings, ResultBindings),
    assert_query_success_response(IsDirective, ResultBindings, RetryMessageAndOutput),
    % Start a new recursive loop so that the current goal can be retried
    % The loop will
    % - exit right away if LoopCont=cut
    % - fail if it receives a request to retry Goal
    jupyter_request_handling:loop(LoopCont, RecStack, RecCont),
    ( RecCont = cut,
      !,
      Cont = continue
    ; otherwise -> % Possibly 'done'
      Cont = RecCont
    )
  ),
  !.


% assert_query_failure_response(+IsDirective, +GoalAtom, +Output)
assert_query_failure_response(true, GoalAtom, Output) :-
  % For directives, output an error message displaying the failure
  !,
  assert_error_response(failure, message_data(warning, jupyter(goal_failed(GoalAtom))), Output, []).
assert_query_failure_response(_IsDirective, _GoalAtom, Output) :-
  assert_error_response(failure, null, Output, []).


% output_and_failure_message(+Output, +FailureMessage, -OutputAndFailureMessage)
output_and_failure_message('', FailureMessage, FailureMessage) :- !.
output_and_failure_message(Output, FailureMessage, OutputAndFailureMessage) :-
  atom_concat('\n', FailureMessage, FailureMessageWithNl),
  atom_concat(Output, FailureMessageWithNl, OutputAndFailureMessage).


% assert_query_success_response(+IsDirective, +ResultBindings, +Output)
assert_query_success_response(true, _ResultBindings, Output) :-
  % For directives, no bindings are output
  !,
  assert_success_response(directive, [], Output, []).
assert_query_success_response(_IsDirective, ResultBindings, Output) :-
  assert_success_response(query, ResultBindings, Output, []).


% update_variable_bindings(+BindingsWithoutSingletons)
:- if(swi).
update_variable_bindings(BindingsWithoutSingletons) :-
  toplevel_variables:expand_answer(BindingsWithoutSingletons, _NewBindings).
:- else.
update_variable_bindings(BindingsWithoutSingletons) :-
  jupyter_variable_bindings:store_var_bindings(BindingsWithoutSingletons).
:- endif.


% retry_message_and_output(+GoalAtom, +Output, -RetryMessageAndOutput)
%
% If the current term was 'retry', a retry message is prepended to the output of the goal.
retry_message_and_output(GoalAtom, Output, RetryMessageAndOutput) :-
  % The Id can be from the initial 'call' request or from a subsequent 'retry' request.
  retract(is_retry(IsRetry)),
  retry_message(IsRetry, GoalAtom, RetryMessage),
  atom_concat(RetryMessage, Output, RetryMessageAndOutput).


% retry_message(+IsRetry, +GoalAtom, -RetryMessage)
%
% If the current term was a 'retry' term (IsRetry=true), a retry message is sent to the client.
% This message contains the goal which was retried.
retry_message(true, GoalAtom, RetryMessage) :-
  !,
  format_to_atom('% Retrying goal: ~w~n', [GoalAtom], RetryMessage).
retry_message(_IsRetry, _GoalAtom, '').


% handle_result_variable_bindings(+Bindings, -ResultBindings)
handle_result_variable_bindings(Bindings, ResultBindings) :-
  % Update the stored variable bindings
  remove_singleton_variables(Bindings, BindingsWithoutSingletons),
  update_variable_bindings(BindingsWithoutSingletons),
  % Convert the variable values to json parsable terms
  json_parsable_vars(BindingsWithoutSingletons, Bindings, ResultBindings).


% remove_singleton_variables(+Bindings, -BindingsWithoutSingletons)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term currently being handled.
% BindingsWithoutSingletons contains the elements of Bindings except for (named) singleton variables starting with '_'
remove_singleton_variables([], []) :- !.
remove_singleton_variables([Name=_Var|Bindings], BindingsWithoutSingletons) :-
  % Name starts with '_'
  sub_atom(Name, 0, 1, _After, '_'),
  !,
  remove_singleton_variables(Bindings, BindingsWithoutSingletons).
remove_singleton_variables([Binding|Bindings], [Binding|BindingsWithoutSingletons]) :-
  remove_singleton_variables(Bindings, BindingsWithoutSingletons).


% json_parsable_vars(+NonParsableVars, +Bindings, -JsonParsableVars)
%
% NonParsableVars and Bindings are lists of Name=Var pairs, where Name is the name of a variable Var occurring in the term currently being handled.
% As not all of the values can be parsed to JSON (e.g. uninstantiated variables and compounds), they need to be made JSON parsable first.
% Bindings is needed in case any variable term needs to be converted to an atom and contains other variables.
% By using Bindings, the names of the variables can be preserved.
% In case of domain variables with bounded domains (lower and upper bound exist) which are not bound to a single value,
%  the value returned to the client is a list of lists where each of those lists contains a lower and upper bound of a range the variable can be in.
json_parsable_vars([], _Variables, []) :- !.
json_parsable_vars([VarName=Var|RemainingBindings], Bindings, [VarName=json([dom=DomAtom])|JsonParsableBindings]) :-
  var(Var),
  % Check if the predicate clpfd:fd_dom/2 exists
  % If it does not, the library clpfd has not been loaded
  % In that case, there cannot be any domain variables
  current_predicate(clpfd:fd_dom/2),
  clpfd:fd_dom(Var, Dom),
  write_term_to_atom(Dom, [], DomAtom),
  DomAtom \= 'inf..sup',
  % cannot compare the term unless loading the library
  !,
  % The variable is a domain variable with a bounded domain (lower and upper bound exist)
  json_parsable_vars(RemainingBindings, Bindings, JsonParsableBindings).
json_parsable_vars([VarName=Var|RemainingBindings], Bindings, JsonParsableBindings) :-
  var(Var),
  same_var(RemainingBindings, Var),
  !,
  % The list of Name=Var pairs contains at least one element OtherName=Var where Var is uninstantiated
  % Unify the variable Var with VarName
  Var=VarName,
  json_parsable_vars(RemainingBindings, Bindings, JsonParsableBindings).
json_parsable_vars([_VarName=Var|RemainingBindings], Bindings, JsonParsableBindings) :-
  var(Var),
  !,
  % The variable is uninstantiated and therefore not included in the result list
  json_parsable_vars(RemainingBindings, Bindings, JsonParsableBindings).
json_parsable_vars([VarName=Var|RemainingBindings], Bindings, [VarName=VarAtom|JsonParsableBindings]) :-
  % Convert the value to an atom as it may be compound and cannot be parsed to JSON otherwise
  write_term_to_atom(Var, Bindings, VarAtom),
  json_parsable_vars(RemainingBindings, Bindings, JsonParsableBindings).


% same_var(+BindingsWithoutSingletons, +Var)
%
% BindingsWithoutSingletons is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term currently being handled.
% Fails if BindingsWithoutSingletons does not contain any elenent VarName=Var1 where Var1 and Var are identical (==).
same_var([], _Var) :- fail.
same_var([_VarName=Var1|_BindingsWithoutSingletons], Var2) :-
  Var1 == Var2, !.
same_var([_Binding|BindingsWithoutSingletons], Var) :-
  same_var(BindingsWithoutSingletons, Var).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handling the different types of queries

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retry

% If there is no active goal, an error message is sent to the client.
% Otherwise, in order to retry an active previous goal, fails into the caller (jupyter_query_handling:call_query_with_output_to_file/7).
% The goal which is retried is output.

% handle_retry(+CallRequestId, +Stack)
handle_retry(Stack) :-
  ( Stack = [_ActiveGoal|_RemainingStack] ->
    % Tell caller that the current query is a retry
    asserta(is_retry(true)),
    % Redirect all output to a file and call statistics/2 to compute the runtime as would normally be done before calling a query
    jupyter_query_handling:redirect_output_to_file,
    statistics(walltime, _Value),
    fail
  ; otherwise -> % No active call
    assert_error_response(no_active_call, null, '', [])
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cut

% If there is no active goal, an error message is sent to the client.
% Otherwise, Cont=cut causes possible choice points of jupyter_query_handling:call_query_with_output_to_file/7 in handle_query/8 to be cut.
% A message informing the user about the new active query is displayed.

% handle_cut(+Stack, -Cont)
handle_cut(Stack, Cont) :-
  ( Stack = [_Active|RemainingStack] ->
    cut_message(RemainingStack, CutMessage),
    assert_success_response(query, [], CutMessage, []),
    Cont = cut
  ; otherwise -> % No active call
    assert_error_response(no_active_call, null, '', []),
    Cont = continue
  ).


% cut_message(+RemainingStack, -CutMessage)
cut_message([], '% There is no previous active goal') :- !.
cut_message([ActiveGoalAtom|_RemainingStack], CutMessage) :-
  format_to_codes('% The new active goal is: ~w', [ActiveGoalAtom], MessageCodes),
  atom_codes(CutMessage, MessageCodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Halt

% If the server is to be halted, the loop reading and handling messages is stopped so that the server process is stopped.
% The type of the success reply sent to the client indicates that the server was halted and needs to be restarted for the next execution request.

handle_halt :-
  assertz(term_response(json([status=halt]))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print result table

% The client requested a response which can be used to print a table.
% The client expects the result to contain a member 'print_table' of which the value is a dictionary with the following members:
% - ValuesLists: a list of lists where each list corresponds to one line of the table
% - VariableNames: a list of names used as the header for the table; one of
%   - []: if no names are provided, the header will contain capital letters as names
%   - a list of ground terms of the same length as the values lists


% handle_print_table_with_findall(+Bindings, +Goal)
%
% The values need to be computed with findall/3 for the goal Goal.
% The header of the table will contain the names of the variables occurring in Goal.
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal Goal.
handle_print_table_with_findall(Bindings, Goal) :-
  ( jupyter_query_handling:call_with_output_to_file(jupyter_term_handling:findall_results_and_var_names(Goal, Bindings, Results, VarNames), Output, ErrorMessageData),
    % Success or exception from findall_results_and_var_names/4
    ( nonvar(ErrorMessageData) ->
      !,
      assert_error_response(exception, ErrorMessageData, '', [])
    ; otherwise -> % success
      % Return the additional 'print_table' data
      assert_success_response(query, [], Output, [print_table=json(['ValuesLists'=Results, 'VariableNames'=VarNames])])
    ),
    !
  ; % findall_results_and_var_names/4 failed
    assert_error_response(failure, null, '', [])
  ).


% handle_print_table(+Bindings, +ValuesLists, +VariableNames)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal Goal.
% ValuesLists is expected to be a list of lists of the same lengths.
% It contains the data which is to be printed in the table.
% VariableNames is [] or a list of ground terms which need to be of the same length as the values lists.
handle_print_table(_Bindings, [], VariableNames) :- !,
  assert_success_response(query, [], '', [print_table=json(['ValuesLists'=[], 'VariableNames'=VariableNames])]).
handle_print_table(Bindings, ValuesLists, VariableNames) :-
  % Get the length of the first list and make sure that all other lists have the same length
  ValuesLists = [ValuesList|RemainingValuesLists],
  length(ValuesList, Length),
  ( forall(member(List, RemainingValuesLists), length(List, Length)) ->
    % Make sure that VariableNames is valid
    ( table_variable_names(VariableNames, Length, TableVariableNames) ->
      % As not all of the values can be parsed to JSON (e.g. uninstantiated variables and compounds), they need to be made JSON parsable first by converting them to atoms
      findall(ValuesAtomList, (member(Values, ValuesLists), convert_to_atom_list(Values, Bindings, ValuesAtomList)), JsonParsableValuesLists),
      % Return the additional 'print_table' data
      assert_success_response(query, [], '', [print_table=json(['ValuesLists'=JsonParsableValuesLists, 'VariableNames'=TableVariableNames])])
    ; otherwise -> % The variable names are invalid
      assert_error_response(exception, message_data(error, jupyter(invalid_table_variable_names)), '', [])
    )
  ; otherwise -> % Not all lists in ValuesLists are of the same length
    assert_error_response(exception, message_data(error, jupyter(invalid_table_values_lists_length)), '', [])
  ).


% table_variable_names(+VariableNames, +Length, -TableVariableNames)
table_variable_names([], Length, TableVariableNames) :-
  % If no variable names are provided, capital letters are used instead
  Letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'],
  % TableVariableNames is a list containing the first Length letters
  length(TableVariableNames, Length),
  append(TableVariableNames, _, Letters).
table_variable_names(VariableNames, Length, VariableNames) :-
  % Check that the number of variable names is correct and that all of them are ground
  length(VariableNames, Length),
  forall(member(VariableName, VariableNames), ground(VariableName)),
  !.


% convert_to_atom_list(+List, +Bindings, -AtomList)
%
% AtomList contains the elements of List after converting them to atoms.
convert_to_atom_list(List, Bindings, AtomList) :-
  findall(ElementAtom, (member(Element, List), write_term_to_atom(Element, Bindings, ElementAtom)), AtomList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PlUnit tests

% In order to use PlUnit tests with the SICStus JSON-RPC server, the tests need to be written to a file which can be loaded.
% SICStus: When a the first begin_tests directive is encountered, a file is created and opened for writing.
% SWI: For every unit, a file is created and opened for writing.
% The corresponding stream is asserted as test_write_stream/1 so that it can be used to write test definitions test directives to the file.
% Each following test/1 or test/2 clause (and in case of SICStus also begin_tests/end_tests directives) is written to the file.
% When all terms of a request were handled or a run_tests query is encountered (additionally in case of SWI: a new begin_tests directive), the file is loaded.

% SICStus: All tests which are to be run at the same time need to be defined by the same request. Otherwise, the definition is overwritten.

% handle_single_test_directive
handle_single_test_directive :-
  assert_error_response(exception, message_data(error, jupyter(single_test_directive)), '', []).


% handle_begin_tests(+Directive, +Unit +Bindings)
%
% Unit is the name of the test unit.
:- if(swi).
handle_begin_tests(Directive, Unit, Bindings) :-
  % Create a new file for each test unit
  % First load the previous test unit file if there is one
  test_definition_end(true),
  catch(retractall(test_definition_stream(_TestDefinitionStream)), _Exception, true),
  begin_new_test_file(Directive, Unit, Bindings).
:- else.
handle_begin_tests(Directive, _Unit, Bindings) :-
  test_definition_stream(TestDefinitionStream),
  % Not the first begin_tests directive -> write to the existing file
  !,
  write_term_to_stream(Directive, Bindings, TestDefinitionStream).
handle_begin_tests(Directive, Unit, Bindings) :-
  % First begin_tests directive of the request or after a run_tests query -> create a new file
  begin_new_test_file(Directive, Unit, Bindings).
:- endif.


% begin_new_test_file(+Directive, +Unit +Bindings)
begin_new_test_file(Directive, Unit, Bindings) :-
  test_file_name(Unit, TestFileName),
  open(TestFileName, write, TestDefinitionStream),
  assert(test_definition_stream(TestDefinitionStream)),
  % Load the module plunit in the file
  % Otherwise, if the module was not loaded, the loading of the test definition file fails with an existence error because of user:begin_test/1
  write_term(TestDefinitionStream, ':- use_module(library(plunit)).\n', []),
  write_term_to_stream(Directive, Bindings, TestDefinitionStream).


% handle_end_tests(+Directive)
handle_end_tests(Directive) :-
  test_definition_stream(TestDefinitionStream),
  % Otherwise, there was no begin_tests directive -> there is no file to write to
  write_term_to_stream(Directive, [], TestDefinitionStream).


% handle_run_tests(+Term, +CallRequestId, +Stack, +Bindings, -Cont)
%
% If in the current query tests were defined, the test definition file is loaded.
% Afterwards, this is handled the same as any other query.
handle_run_tests(Term, CallRequestId, Stack, Bindings, Cont) :-
  test_definition_end(true),
  handle_query(Term, false, CallRequestId, Stack, Bindings, _OriginalTermData, cut, Cont).


% write_term_to_stream(+Term, +Bindings, +TestDefinitionStream)
write_term_to_stream(Term, Bindings, TestDefinitionStream) :-
  write_term(TestDefinitionStream, Term, [variable_names(Bindings)]),
  write_term(TestDefinitionStream, '.\n', []).


% test_definition_end(+LoadFile)
%
% Closes and retracts the stream to which test definitions were written.
% If LoadFile=true, loads the test definition file.
test_definition_end(LoadFile) :-
  test_definition_stream(TestDefinitionStream),
  !,
  close(TestDefinitionStream),
  retractall(test_definition_stream(_)),
  test_file_name(Unit, TestFileName),
  % When loading the file, an exception or warning might be output
  ( LoadFile==true ->
    % When loading the file, an exception or warning might be output
    jupyter_query_handling:call_with_output_to_file(load_files(TestFileName), Output, ErrorMessageData)
  ; otherwise ->
    Output = ''
  ),
  delete_file(TestFileName),
  ( nonvar(ErrorMessageData) ->
    assert_error_response(exception, message_data(error, ErrorMessageData), Output, [])
  ; otherwise ->
    ( nonvar(Unit) ->
      % In case of SWI-Prolog, the Unit is bound
      % Since each Unit is written to and loaded from a separate file, a message should be output for each file
      format_to_codes('% Defined test unit ~w', [Unit], LoadMessageCodes),
      atom_codes(LoadMessage, LoadMessageCodes),
      atom_concat(Output, LoadMessage, OutputWithLoadMessage)
    ; atom_concat(Output, '\n% Loaded the test file', OutputWithLoadMessage)
    ),
    assert_success_response(directive, [], OutputWithLoadMessage, [])
  ).
test_definition_end(_LoadFile).
% The request did not contain any begin_tests directive -> there is no file or stream which has to be dealt with


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% trace

% trace/0, trace/1 and trace/2 cannot be used with this server, because the debugging messages are not printed in a way that they can be read in and sent to the client.
% Instead, jupyter:trace(Goal) can be used to print the trace of the goal Goal.

% handle_trace(+TracePredSpec)
handle_trace(TracePredSpec) :-
  assert_error_response(exception, message_data(error, jupyter(trace_pred(TracePredSpec))), '', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print tables

% findall_results_and_var_names(+Goal, +Bindings, -Results, -VarNames)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal Goal.
% Uses findall to find all results (ResultsLists) of the goal Goal.
% ResultsLists contains lists containing values for each variable in Bindings.
% VarNames is the list of variable names from Bindings.
findall_results_and_var_names(Goal, Bindings, JsonParsableResultsLists, VarNames) :-
  var_names_and_values(Bindings, VarNames, Vars),
  findall(Vars, Goal, ResultsLists),
  json_parsable_results_lists(ResultsLists, VarNames, Bindings, JsonParsableResultsLists).


% var_names_and_values(+Bindings, -VarNames, -Vars)
var_names_and_values([], [], []) :- !.
var_names_and_values([VarName=Var|Bindings], [VarName|VarNames], [Var|Vars]) :-
  var_names_and_values(Bindings, VarNames, Vars).


% json_parsable_results_lists(+ResultsLists, +VarNames, +Bindings, -JsonParsableResultsLists)
%
% ResultsLists is a list containing lists of values of the variables with names in VarNames.
% As not all of the terms in ResultsLists can be parsed to JSON (e.g. uninstantiated variables and compounds), they need to be made JSON parsable first.
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal which was called to get the results.
% Bindings is needed to preserve the variable names when converting a result to an atom.
json_parsable_results_lists([], _VarNames, _Bindings, []) :- !.
json_parsable_results_lists([Results|ResultsLists], VarNames, Bindings, [JsonParsableResults|JsonParsableResultsLists]) :-
  json_parsable_results(Results, VarNames, Bindings, JsonParsableResults),
  json_parsable_results_lists(ResultsLists, VarNames, Bindings, JsonParsableResultsLists).


% json_parsable_results(+Results, +VarNames, +Bindings, -JsonParsableResult)
json_parsable_results([], _VarNames, _Bindings, []) :- !.
json_parsable_results([Result|Results], [VarName|VarNames], Bindings, [Result|JsonParsableResults]) :-
  % If the result is a variable, unify it with its name
  var(Result),
  !,
  Result = VarName,
  json_parsable_results(Results, VarNames, Bindings, JsonParsableResults).
json_parsable_results([Result|Results], [_VarName|VarNames], Bindings, [ResultAtom|JsonParsableResults]) :-
  % Convert the value to an atom as it may be compound and cannot be parsed to JSON otherwise
  write_term_to_atom(Result, Bindings, ResultAtom),
  json_parsable_results(Results, VarNames, Bindings, JsonParsableResults).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print SLD Trees

% Create content for a file representing a graph resembling the SLD tree of an execution that can be rendered with DOT.
% The collection of the data needs to be handled differntly for SWI- and SICStus Prolog:
% - SICStus: a breakpoint which is removed after the execution.
% - SWI: user:prolog_trace_interception/4 is used that calls assert_sld_data/4 which succeeds if SLD data is to be collected (if a clause collect_sld_data/0 exists).

% So far, data is collected for call ports only and no leaves are shown marking a successful or failing branch.
% In order to add such leaves, data needs to be collected for other ports as well.
% Then, to add a failure/success leaf, the first fail/exit port for a call needs to be determined.

% The graph file content is created like the following.
% Nodes are defined by their ID and labelled with the goal.
% Directed edges are added from a parent invocation to the child invocation.
% This may look like the following:
%   digraph {
%       "4" [label="app([1,2],[3],A)"]
%       "5" [label="app([2],[3],B)"]
%       "6" [label="app([],[3],C)"]
%       "4" -> "5"
%       "5" -> "6"
%   }


:- dynamic
    collect_sld_data/0,
    sld_data/3.          % sld_data(GoalCodes, Current, Parent)


% assert_sld_data(+Port, +MGoal, +Current, +Parent)
%
% Assert the data which is needed to create a dot file representing the SLD tree.
% For SICStus Prolog, Current and Parent are invocation numbers of the current invovation and the parent invocation.
% For SWI-Prolog, Current and Parent are integer references to the frame.
assert_sld_data(call, MGoal, Current, Parent) :-
  collect_sld_data, % SLD data is to be colleted
  !,
  % If the goal is module name expanded with the user module, remove the module expansion
  ( MGoal = user:Goal ->
    true
  ; Goal = MGoal
  ),
  % Assert the goal as character codes so that the variable names can be preserved and replaced consistently
  write_term_to_codes(Goal, GoalCodes, []),
  assertz(sld_data(GoalCodes, Current, Parent)).
assert_sld_data(_Port, _MGoal, _Current, _Parent) :-
  collect_sld_data. % SLD data is to be colleted, but not for ports other than call


% handle_print_sld_tree(+Goal, +Bindings)
handle_print_sld_tree(Goal, Bindings) :-
  % Assert collect_sld_data/0 so that SLD data is collected during tracing (needed for SWI-Prolog)
  assert(collect_sld_data),
  % Retract previous data
  catch(retractall(sld_data(_GoalCodes, _Inv, _ParentInv)), _GoalInvDataException, true),
  % Call the goal and collect the needed data
  jupyter_query_handling:call_query_with_output_to_file(
       jupyter_term_handling:call_with_sld_data_collection(Goal, Exception, IsFailure), 0, Bindings,
                                                   _OriginalTermData, Output, _ExceptionMessage, _IsFailure),
  retractall(collect_sld_data),
  % Compute the graph file content
  sld_graph_file_content(GraphFileContentAtom),
  % Assert the result response
  ( nonvar(Exception) -> % Exception
    !,
    assert_error_response(exception, message_data(error, Exception), Output, [print_sld_tree=GraphFileContentAtom])
  ; IsFailure == true -> % Failure
    !,
    assert_error_response(failure, null, Output, [print_sld_tree=GraphFileContentAtom])
  ; otherwise -> % Success
    handle_result_variable_bindings(Bindings, ResultBindings),
    assert_success_response(query, ResultBindings, Output, [print_sld_tree=GraphFileContentAtom])
  ).


:- if(swi).

% call_with_sld_data_collection(+Goal, -Exception -IsFailure)
call_with_sld_data_collection(Goal, Exception, IsFailure) :-
  module_name_expanded(Goal, MGoal),
  catch(call_with_sld_failure_handling(MGoal, IsFailure), Exception, notrace).


% call_with_sld_failure_handling(+Goal, -IsFailure)
call_with_sld_failure_handling(Goal, IsFailure) :-
  trace,
  ( call(Goal) ->
    notrace,
    IsFailure = false
  ; notrace,
    IsFailure = true
  ).

:- else.

% call_with_sld_data_collection(+Goal, -Exception -IsFailure)
call_with_sld_data_collection(Goal, Exception, IsFailure) :-
  module_name_expanded(Goal, MGoal),
  BreakpointConditions = [call]-[proceed, goal(Module:DebuggerGoal), inv(Inv), parent_inv(ParentInv), true(assert_sld_data(call, Module:DebuggerGoal, Inv, ParentInv))],
  % Make sure that when the output is read in, some informational messages are removed
  assert(jupyter_query_handling:remove_output_lines_for(sld_tree_breakpoint_messages)),
  % Calling debug/0 makes sure that an informational message is always output which can then be removed
  debug,
  catch(
    call_with_sld_failure_handling(MGoal, BreakpointConditions, IsFailure),
    Exception,
    % In case of an exception, first turn of tracing so that no more data is asserted
    % Then, remove the created breakpoint
    ( notrace, current_breakpoint(BreakpointConditions, BID, _Status, _Kind, _Type), remove_breakpoints([BID]) )
  ).


% call_with_sld_failure_handling(+Goal, +BreakpointConditions, -IsFailure)
%
% Adds a breakpoint which collects data, calls the goal and removes the breakpoint.
call_with_sld_failure_handling(Goal, BreakpointConditions, IsFailure) :-
  add_breakpoint(BreakpointConditions, BID),
  ( call(Goal) ->
    remove_breakpoints([BID]),
    IsFailure = false
  ; remove_breakpoints([BID]),
    IsFailure = true
  ).

:- endif.


% sld_graph_file_content(-GraphFileContentAtom)
%
% GraphFileContentAtom is an atom representing the content of a graph file which would represent the SLD tree of the current execution.
% Collects the data which was asserted as sld_data/3.
% For each element (except the ones for the toplevel call and remove_breakpoints/1), an atom is created representing one of the lines of the file.
sld_graph_file_content(GraphFileContentAtom) :-
  findall(GoalCodes-Id-ParentId, sld_data(GoalCodes, Id, ParentId), SldData),
  clean_sld_data(SldData, CleanSldData),
  % Compute nodes content
  sld_tree_node_atoms(CleanSldData, 'A', [], Nodes),
  % Compute edges content
  % The first element corresponds to a call from the toplevel
  % SldDataWithoutToplevelCalls contains all elements from CleanSldData which do not correspond to teplevel calls with the same ParentId
  CleanSldData = [_Goal-_CurrentId-ToplevelId|_],
  delete_all_occurrences(CleanSldData, _G-_Id-ToplevelId, SldDataWithoutToplevelCalls),
  sld_tree_edge_atoms(SldDataWithoutToplevelCalls, Edges),
  % Build the file content atom
  % Append elements to the list with which the remaining file content is added
  append(Edges, ['}'], EdgesWithClosingBracket),
  append(Nodes, EdgesWithClosingBracket, NodesAndEdgesWithClosingBracket),
  atom_concat(['digraph {\n'|NodesAndEdgesWithClosingBracket], GraphFileContentAtom).


% clean_sld_data(+SldData, -CleanSldData)
%
% For SIW- and SICStus Prolog, the list of SLD tree data needs to be handled differently before it can be used to compute graph file content.
:- if(swi).

clean_sld_data(SldData, CleanSldData) :-
  compute_unique_ids(SldData, 1, [], CleanSldData).


% compute_unique_ids(+SldData, +CurrentId, +ActiveIds, -SldDataWithUniqueIds)
%
% SldData is a list with elements of the form GoalCodes-CurrentFrame-ParentFrame.
% CurrentFrame and ParentFrame are integer references to the local stack frame.
% Since these are not unique, they cannot be used to compute the graph file and instead, unique IDs need to be computed.
% ActiveIds is a list of Frame-Id pairs.
% Every element in SldData is assigned a ID (CurrentId).
% For every element, checks if an element CurrentFrame-Id is contained in ActiveIds.
%   If so, there was another goal on the same level as the current one.
%   In that case, the element in SldData is "replaced" with CurrentFrame-CurrentId.
%   Otherwise, a new element CurrentFrame-NewID is added to SldData.
% For every element in SldData, ActiveIds contains an element ParentFrame-ParentId (except for the toplevel goals)
% SldDataWithUniqueIds contains GoalCodes-CurrentId-ParentId elements.
compute_unique_ids([], _CurrentId, _ActiveIds, []).
compute_unique_ids([GoalCodes-CurrentFrame-ParentFrame|SldData], CurrentId, ActiveIds, [GoalCodes-CurrentId-ParentId|SldDataWithUniqueIds]) :-
  ( member(CurrentFrame-PreviousId, ActiveIds) ->
    % A goal on the same level was already encountered
    % The corresponding element needs to be replaced in the active ID list
    delete(ActiveIds, CurrentFrame-PreviousId, ReaminingActiveIds),
    NewActiveIds = [CurrentFrame-CurrentId|ReaminingActiveIds]
  ; NewActiveIds = [CurrentFrame-CurrentId|ActiveIds]
  ),
  % Retrieve the parent's ID
  ( member(ParentFrame-Id, ActiveIds) ->
    ParentId = Id
  ; % For the toplevel calls, there is no parent ID
    ParentId = 0
  ),
  NextId is CurrentId + 1,
  compute_unique_ids(SldData, NextId, NewActiveIds, SldDataWithUniqueIds).

:- else.

clean_sld_data(SldData, CleanSldData) :-
  % Remove the last element because it corresponds to the call of remove_breakpoints/1
  append(CleanSldData, [_RemoveBreakpointsData], SldData).

:- endif.


% sld_tree_node_atoms(+SldData, +CurrentReplacementAtom +VariableNameReplacements, -NodeAtoms)
%
% SldData is a list with elements of the form GoalCodes-Current-Parent.
% For each of the elements, NodeAtoms contains an atom of the following form: '"Current" [label="Goal"]'
% All variable names, which are of the form _12345 are replaced by names starting with 'A'.
% CurrentReplacementAtom is the atom the next variable name is to be replaced with.
% In order to keep the renaming consistent for all terms, VariableNameReplacements is a list with VarName=NameReplacement pairs for name replacements which were made for the previous terms.
sld_tree_node_atoms([], _CurrentReplacementAtom, _VariableNameReplacements, []) :- !.
sld_tree_node_atoms([GoalCodes-Current-_Parent|SldData], CurrentReplacementAtom, VariableNameReplacements, [Node|Nodes]) :-
  % Read the goal term from the codes with the option variable_names/1 so that variable names can be replaced consistently
  append(GoalCodes, [46], GoalCodesWithFullStop),
  read_term_from_codes(GoalCodesWithFullStop, GoalTerm, [variable_names(VariableNames)]),
  % Replace the variable names
  replace_variable_names(VariableNames, CurrentReplacementAtom, VariableNameReplacements, NextReplacementAtom, NewVariableNameReplacements),
  % Create the atom
  format_to_codes('    \"~w\" [label=\"~w\"]~n', [Current, GoalTerm], NodeCodes),
  atom_codes(Node, NodeCodes),
  sld_tree_node_atoms(SldData, NextReplacementAtom, NewVariableNameReplacements, Nodes).


% replace_variable_names(+VariableNames, +CurrentReplacementAtom, +VariableNameReplacements, -NextReplacementAtom, -NewVariableNameReplacements)
replace_variable_names([], CurrentReplacementAtom, VariableNameReplacements, CurrentReplacementAtom, VariableNameReplacements) :- !.
replace_variable_names([Var=Name|VariableNames], CurrentReplacementAtom, VariableNameReplacements, NextReplacementAtom, NewVariableNameReplacements) :-
  member(Var=ReplacementAtom, VariableNameReplacements),
  !,
  % The variable has already been assigned a new name
  Name = ReplacementAtom,
  replace_variable_names(VariableNames, CurrentReplacementAtom, VariableNameReplacements, NextReplacementAtom, NewVariableNameReplacements).
replace_variable_names([Var=Name|VariableNames], CurrentReplacementAtom, VariableNameReplacements, OutputReplacementAtom, NewVariableNameReplacements) :-
  % The variable has not been assigned a new name
  Name = CurrentReplacementAtom,
  next_replacement_atom(CurrentReplacementAtom, NextReplacementAtom),
  replace_variable_names(VariableNames, NextReplacementAtom, [Var=CurrentReplacementAtom|VariableNameReplacements], OutputReplacementAtom, NewVariableNameReplacements).


% next_replacement_atom(+CurrentReplacementAtom, -NextReplacementAtom)
%
% In order to compute the next replacement atom, CurrentReplacementAtom is converted into a list of character codes.
% If the last code does not equal 90 (i.e. 'Z'), the code is increased and the codes are converted into NextReplacementAtom.
% Otherwise, the code cannot be increased further, so a new character code for 'A' is added to the list.
next_replacement_atom(CurrentReplacementAtom, NextReplacementAtom) :-
  atom_codes(CurrentReplacementAtom, CurrentCodes),
  append(PrecedingCodes, [LastCode], CurrentCodes),
  % Compute the next last code(s)
  ( LastCode == 90 ->
    % The code 90 corresponds to 'Z' and cannot simply be increased
    % Instead, an additional character code needs to be added
    NextCodeList = [90, 65]
  ; NextCode is LastCode + 1,
    NextCodeList = [NextCode]
  ),
  % Compute the new code list and atom
  ( PrecedingCodes == [] ->
    NextReplacementCodes = NextCodeList
  ; append(PrecedingCodes, NextCodeList, NextReplacementCodes)
  ),
  atom_codes(NextReplacementAtom, NextReplacementCodes).


% delete_all_occurrences(+List, +DeleteElement, -NewList)
%
% NewList is a list containing all elements of the List which are not equal to DeleteElement.
% In order to not bind any variables, copy_term/2 is used.
delete_all_occurrences([], _DeleteElement, []) :- !.
delete_all_occurrences([Element|List], DeleteElement, NewList) :-
  copy_term(DeleteElement, CopyDeleteElement),
  Element = CopyDeleteElement,
  !,
  delete_all_occurrences(List, DeleteElement, NewList).
delete_all_occurrences([Element|List], DeleteElement, [Element|NewList]) :-
  delete_all_occurrences(List, DeleteElement, NewList).


% sld_tree_edge_atoms(+SldData, -Edges)
%
% SldData is a list with elements of the form GoalCodes-Current-Parent.
% For each of these elements, Edges contains an atom of the following form: '    "Parent" -> "Current"~n'
sld_tree_edge_atoms([], []) :- !.
sld_tree_edge_atoms([_GoalCodes-Current-Parent|SldData], [EdgeAtom|Edges]) :-
  format_to_codes('    \"~w\" -> \"~w\"~n', [Parent, Current], EdgeCodes),
  atom_codes(EdgeAtom, EdgeCodes),
  sld_tree_edge_atoms(SldData, Edges).


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

% Print Transition Graph

% Create content for a file representing a graph of transitions between the clauses of a given predicate.
% In addition to a predicate specification, indices need to be provided for the arguments which are used for the edges and their optional labels.
% The predicate results are used to create lines for the edges of one of the following forms:
% - '    "From" -> "To" [label="Label"]~n'
% - '    "From" -> "To"~n'


% handle_print_transition_graph(+NodePredSpec, +EdgePredSpec, +FromIndex, +ToIndex, +LabelIndex)
%
% NodePredSpec needs to be a predicate spec of the form true or Module:PredName/PredArity or PredName/PredArity.
% The first argument is the internal name of the node, passed on to EdgePredSpec, the second arg can be a dot attribute list
% EdgePredSpec needs to be a predicate specification of the form Module:PredName/PredArity or PredName/PredArity.
% FromIndex, ToIndex, and LabelIndex need to be less or equal to PredArity.
% FromIndex and ToIndex point to predicate arguments used as nodes.
% LabelIndex points to the argument providing a label for an edge.
% If LabelIndex=0, the edges are not labelled.
handle_print_transition_graph(NodePredSpec, EdgePredSpec, FromIndex, ToIndex, LabelIndex) :-
  % Check that the predicate specification and indices are correct
  module_name_expanded_pred_spec(EdgePredSpec, Module:PredName/PredArity,PredTerm),
  check_indices(PredArity, FromIndex, ToIndex, LabelIndex),
  !,
  PredTerm =.. [PredName|ArgList],
  % compute all possible nodes
  (NodePredSpec=true
    -> EdgeCall = Module:PredTerm
    ;  findall(NodeName,get_transition_graph_node_atom(NodePredSpec,NodeName,_),Nodes),
       findall(NodeDotDesc,get_transition_graph_node_atom(NodePredSpec,_,NodeDotDesc),NodeDescAtoms),
       nth1(FromIndex, ArgList, FromNode),
       nth1(ToIndex, ArgList, ToNode),
       EdgeCall = (member(FromNode,Nodes),Module:PredTerm,member(ToNode,Nodes)) 
       % only take nodes into account which are declared, % TO DO: we could only apply restriction to FromNode
  ),
  % Compute all possible transitions
  findall(ArgList, EdgeCall, Results),
  % Compute the graph file content
  transition_graph_edge_atoms(Results, FromIndex, ToIndex, LabelIndex, EdgeDescAtoms),
  append([NodeDescAtoms,EdgeDescAtoms, ['}']], EdgesWithClosingBracket),
  atom_concat(['digraph {\n'|EdgesWithClosingBracket], GraphFileContentAtom),
  % Assert the result response
  assert_success_response(query, [], '', [print_transition_graph=GraphFileContentAtom]).
handle_print_transition_graph(_EdgePredSpec, _FromIndex, _ToIndex, _LabelIndex).
  % If some requirements are not fulfilled, the first clause asserts an error response and fails

% generate dot node name and dot description atom
% example fact for NodePredSpec:
% node(a,[label/'A',shape/rect, style/filled, fillcolor/yellow]).
get_transition_graph_node_atom(NodePredSpec,NodeName,NodeDotDesc) :-
  module_name_expanded_pred_spec(NodePredSpec, Module:PredName/_PredArity, NodeCall),
  NodeCall =.. [PredName|ArgList],
  ArgList = [NodeName|ArgTail], % first argument is the identifier/name of the node
  call(Module:NodeCall), % generate solutions for the node predicate
  (ArgTail = [DotList|_], % we have a potential argument with infos about the style, label, ...
   findall(dot_attr(Attr,Val),get_dot_node_attribute(Attr,Val,DotList),Attrs),
   Attrs = [_|_] % we have found at least one attribute
   -> gen_dot_node_desc(NodeName,Attrs,Codes,[]),
      atom_codes(NodeDotDesc,Codes)
   ; NodeDotDesc = ''
  ).



% provide a default version of the command which automatically sets from,to and label index.
% e.g. we can call jupyter:print_transition_graph(edge/2).
handle_print_transition_graph(NodePredSpec,EdgePredSpec) :-
  module_name_expanded_pred_spec(EdgePredSpec, _Module:_PredName/PredArity,_),
  FromIndex=1, ToIndex=PredArity,
  (PredArity =< 2 -> LabelIndex=0
   ; LabelIndex=2),
  handle_print_transition_graph(NodePredSpec,EdgePredSpec, FromIndex, ToIndex, LabelIndex).

% expand module name to determine arity and provide a predicate call
% can be called with M:p/n or p/n or M:p or M:p(arg1,...)
% in the latter case the call arguments are passed through
% TODO: maybe get rid of this using meta_predicate annotations
% module_name_expanded_pred_spec(+PredSpec, -MPredSpec)
module_name_expanded_pred_spec(PredSpec, Module:PredName/PredArity,PredCall) :- 
   get_module(PredSpec,Module,PredName/PredArity),!,
   functor(PredCall,PredName,PredArity).
module_name_expanded_pred_spec(PredSpec, Module:PredName/PredArity,PredCall) :-
   get_module(PredSpec,Module,PredName),
   atom(PredName),  % just predicate name w/o arity
   current_predicate(Module:PredName/Arity),!,
   PredArity=Arity,
   functor(PredCall,PredName,PredArity).
module_name_expanded_pred_spec(PredSpec, Module:PredName/PredArity,PredCall) :-
   get_module(PredSpec,Module,PredCall),
   functor(PredCall,PredName,PredArity),
   PredArity>0,!.
module_name_expanded_pred_spec(PredSpec, _ , _) :-
   assert_error_response(exception, message_data(error, jupyter(print_transition_graph_pred_spec(PredSpec))), '', []),
   fail.

get_module(Module:Term,M,T) :- !, M=Module,T=Term.
get_module(Term,user,Term).

% check_indices(+PredArity, +FromIndex, +ToIndex, +LabelIndex)
check_indices(PredArity, FromIndex, ToIndex, LabelIndex) :-
  % All indices need to be less or equal to the predicate arity
  integer(FromIndex), FromIndex =< PredArity,
  integer(ToIndex), ToIndex =< PredArity,
  (atom(LabelIndex) -> true ; integer(LabelIndex), LabelIndex >= 0, LabelIndex =< PredArity),
  !.
check_indices(PredArity, _FromIndex, _ToIndex, _LabelIndex) :-
  assert_error_response(exception, message_data(error, jupyter(print_transition_graph_indices(PredArity))), '', []),
  fail.


% transition_graph_edge_atoms(+Results, +FromIndex, +ToIndex, +LabelIndex, -EdgeAtoms)
%
% Results is a list of lists where each of those lists corresponds to the arguments of a clause.
% FromIndex, ToIndex, and LabelIndex are pointers to these arguments.
% For each of the lists, the list EdgeAtoms contains an atom.
% If LabelIndex=0, EdgeAtoms contains atoms of the following form: '    "From" -> "To"~n'
% Otherwise, the atoms are of the following form:                  '    "From" -> "To" [label="Label"]~n'
transition_graph_edge_atoms([], _FromIndex, _ToIndex, _LabelIndex, []) :- !.
transition_graph_edge_atoms([Result|Results], FromIndex, ToIndex, LabelIndex, [EdgeAtom|EdgeAtoms]) :-
  nth1(FromIndex, Result, From),
  nth1(ToIndex, Result, To),
  (get_label(LabelIndex, Result, Label) ->
      (get_line_colour_style(LabelIndex, Result, Color,Style)
       -> format_to_codes('    \"~w\" -> \"~w\" [label=\"~w\", color=\"~w\", style=\"~w\"]~n',
                          [From, To, Label, Color, Style], EdgeCodes)
        ; format_to_codes('    \"~w\" -> \"~w\" [label=\"~w\"]~n', [From, To, Label], EdgeCodes)
      )
   ;  %Label=0 -> do not show any label
      format_to_codes('    \"~w\" -> \"~w\"~n', [From, To], EdgeCodes)
  ),
  %TODO: we should probably escape the labels, ...
  atom_codes(EdgeAtom, EdgeCodes),
  transition_graph_edge_atoms(Results, FromIndex, ToIndex, LabelIndex, EdgeAtoms).

% we also accept graph definitions of the following form, where LabelIndex=2
% edg(a,[label/i, color/red, style/dotted],b).
% edg(b,[label/j, color/chartreuse, style/solid], c).
get_label(0,_,_) :- !, fail.
get_label(LabelIndex,_,Label) :- atom(LabelIndex),!, % allows one to use an atom as label index
  Label=LabelIndex.
get_label(List,_,Label) :- List=[_|_], !, get_line_label(List,Label).
get_label(LabelIndex,Result,Label) :-
   nth1(LabelIndex, Result, ArgVal),
   (get_line_label(ArgVal,ListLabel) -> Label=ListLabel ; Label=ArgVal).

get_line_label(List,Label) :- bind_member(label,Label,List).

get_line_colour_style(List,_,Col,Style) :- List=[_|_], !, % style list provided directly in jupyter call
   get_line_colour(List,Col),
   get_line_style(List,Style).
get_line_colour_style(LabelIndex,Result,Col,Style) :- integer(LabelIndex),
   nth1(LabelIndex, Result, List), % the LabelIndex argument is a list containing dot/graphviz infos
   get_line_colour(List,Col),
   get_line_style(List,Style).

get_line_colour(List,Col) :- bind_member(colour,C,List),!,Col=C.
get_line_colour(List,Col) :- bind_member(color,C,List),!,Col=C.
get_line_colour(_,'black'). % default

get_line_style(List,Style) :- bind_member(style,C,List),valid_dot_line_style(C), !,Style=C.
get_line_style(_,'solid'). % default

valid_dot_line_style(bold).
valid_dot_line_style(dashed).
valid_dot_line_style(dotted).
valid_dot_line_style(invis).
valid_dot_line_style(solid).

%get_shape(List,Style) :- bind_member(Style,C,List),!,Style=C.
%get_shape(_,'none').

bind_member(Label,Value,List) :- member(Binding,List), binding(Binding,Label,Value).
% we accept various ways to specify bindings:
binding('='(Label,Value),Label,Value).
binding('/'(Label,Value),Label,Value).
binding('-'(Label,Value),Label,Value).

get_dot_node_attribute(Attr2,Value,List) :- bind_member(Attr,Value,List),
   valid_dot_node_attribute(Attr,Attr2).

valid_dot_node_attribute(label,label).
valid_dot_node_attribute(color,color).
valid_dot_node_attribute(colour,color).
valid_dot_node_attribute(fillcolor,fillcolor).
valid_dot_node_attribute(shape,shape).
valid_dot_node_attribute(style,style).

valid_dot_shape('Mcircle').
valid_dot_shape('Mdiamond').
valid_dot_shape('Msquare').
valid_dot_shape(box).
valid_dot_shape(box3d). % requires newer version of graphviz
valid_dot_shape(cds). % requires newer version of graphviz
valid_dot_shape(circle).
valid_dot_shape(component). % requires newer version of graphviz
valid_dot_shape(cylinder). % requires newer version of graphviz
valid_dot_shape(diamond).
valid_dot_shape(doublecircle).
valid_dot_shape(doubleoctagon).
valid_dot_shape(egg).
valid_dot_shape(ellipse).
valid_dot_shape(folder). % requires newer version of graphviz
valid_dot_shape(hexagon).
valid_dot_shape(house).
valid_dot_shape(invhouse).
valid_dot_shape(invtrapez).
valid_dot_shape(invtrapezium).
valid_dot_shape(invtriangle).
valid_dot_shape(larrow). % requires newer version of graphviz
valid_dot_shape(lpromoter). % requires newer version of graphviz
valid_dot_shape(none).
valid_dot_shape(note). % requires newer version of graphviz
valid_dot_shape(octagon).
valid_dot_shape(oval).
valid_dot_shape(parallelogram).
valid_dot_shape(pentagon).
valid_dot_shape(plain).
valid_dot_shape(plaintext).
valid_dot_shape(point).
valid_dot_shape(promoter). % requires newer version of graphviz
valid_dot_shape(record).
valid_dot_shape(rarrow). % requires newer version of graphviz
valid_dot_shape(rect).
valid_dot_shape(rectangle).
valid_dot_shape(rpromoter). % requires newer version of graphviz
valid_dot_shape(septagon).
valid_dot_shape(square).
valid_dot_shape(star). % requires newer version of graphviz
valid_dot_shape(tab). % requires newer version of graphviz
valid_dot_shape(trapezium).
valid_dot_shape(triangle).
valid_dot_shape(tripleoctagon).

valid_dot_node_style(bold).
valid_dot_node_style(dashed).
valid_dot_node_style(diagonals).
valid_dot_node_style(dotted).
valid_dot_node_style(filled).
valid_dot_node_style(rounded).
valid_dot_node_style(solid).
valid_dot_node_style(striped).
valid_dot_node_style(wedged).
valid_dot_node_style(none).

% generate a node description as list of codes
% | ?- jupyter_term_handling:gen_node_desc(a,[dot_attr(label,b),dot_attr(color,c)],A,[]), format("~s~n",[A]).
% a [label="b", color="c"]
gen_dot_node_desc(NodeName,Attrs) --> "\"", gen_atom(NodeName),"\" [", gen_node_attr_codes(Attrs),"]",[10].
gen_node_attr_codes([]) --> "".
gen_node_attr_codes([dot_attr(Attr,Val)]) --> !, gen_atom(Attr),"=\"",gen_atom(Val),"\"".
gen_node_attr_codes([dot_attr(Attr,Val)|Tail]) --> 
   gen_atom(Attr),"=\"",gen_atom(Val),"\", ",
   gen_node_attr_codes(Tail).
gen_atom(Atom,In,Out) :- format_to_codes('~w',Atom,Codes), append(Codes,Out,In).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Change the Prolog implementation

% The user requested to change the active Prolog implementation.
% The actual changing of the implementation is handled by the client (the Jupyter kernel).
% It expects an 'set_prolog_impl_id' item to be part of the result.

% handle_set_prolog_impl(+PrologImplementationID)
handle_set_prolog_impl(PrologImplementationID) :-
  atom(PrologImplementationID),
  !,
  assert_success_response(query, [], '', [set_prolog_impl_id=PrologImplementationID]).
handle_set_prolog_impl(_PrologImplementationID) :-
  assert_error_response(exception, message_data(error, jupyter(prolog_impl_id_no_atom)), '', []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Change a Jupyter Prolog preference

handle_set_preference(Pref,Value) :-
  set_preference(Pref,Old,Value),
  !,
  format_to_atom('% Changing preference ~w from ~w to ~w~n', [Pref,Old,Value], Msg),
  assert_success_response(query, [], Msg, []).
handle_set_preference(Pref,Value) :- 
  assert_error_response(exception, message_data(error, jupyter(set_preference(Pref,Value))), '', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Reload the completion data

% The user requested to reload the data used for code completion.
% Finds all predicates which are built-in or exported by a loaded module.
% The client expects these to be part of the result as 'predicate_atoms'.


handle_update_completion_data :-
  % Find all callable (built-in and exported) predicates and send them to the client
  findall(Pred, generate_built_in_pred(Pred), BuiltInPreds),
  findall(Pred, generate_exported_pred(Pred), ExportedPreds),
  append(ExportedPreds, BuiltInPreds, CurrentPreds),
  % convert the predicates into atoms so that they are JSON parsable
  findall(PredAtom, (member(CurPred, CurrentPreds), predicate_atom(CurPred, PredAtom)), PredAtoms),
  assert_success_response(query, [], '', [predicate_atoms=PredAtoms]).


% generate_built_in_pred(-PredicateHead)
:- if(swi).
generate_built_in_pred(Head) :-
  predicate_property(system:Head, built_in),
  functor(Head, Name, _Arity),
  % Exclude reserved names
  \+ sub_atom(Name, 0, _, _, $).
:- else.
generate_built_in_pred(Head) :-
  predicate_property(Head, built_in),
  functor(Head, Name, _Arity),
  % Exclude the 255 call predicates
  Name \= call.
generate_built_in_pred(call(_)).
:- endif.


% generate_exported_pred(-ModuleNameExpandedPredicateHead)
generate_exported_pred(Module:Pred) :-
  ServerModules = [jupyter_jsonrpc, jupyter_logging, jupyter_query_handling, jupyter_request_handling, jupyter_server, jupyter_term_handling, jupyter_variable_bindings],
  predicate_property(Module:Pred, exported),
  % Exclude exported predicates from any of the modules used for this server except for 'jupyter'
  \+ member(Module, ServerModules).


% predicate_atom(+Predicate, -PredicateAtom)
%
% PredicateAtom is an atom created from Predicate by replacing all variables in it with atoms starting from 'A'.
predicate_atom(Predicate, PredicateAtom) :-
  % Create a Name=Var pairs list as can be used for write_term_to_codes/3
  term_variables(Predicate, Variables),
  name_var_pairs(Variables, 65, Bindings), % 65 is the ASCII code for 'A'
  write_term_to_codes(Predicate, PredicateCodes, [variable_names(Bindings)]),
  atom_codes(PredicateAtom, PredicateCodes).


% name_var_pairs(+Variables, +CurrentCharacterCode, -Bindings)
name_var_pairs([], _CurrentCharacterCode, []) :- !.
name_var_pairs([Variable|Variables], CurrentCharacterCode, [NameAtom=Variable|Bindings]) :-
  atom_codes(NameAtom, [CurrentCharacterCode]),
  NextCharacterCode is CurrentCharacterCode + 1,
  name_var_pairs(Variables, NextCharacterCode, Bindings).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print stack

% Prints the stack used for juypter:retry/0 and jupyter:cut/0.
% The currently active query is printed at the top and indicated by a preceding '->'.

% handle_print_stack(+Stack, +CallRequestId)
handle_print_stack(Stack, CallRequestId) :-
  handle_query(jupyter_term_handling:print_stack(Stack), false, CallRequestId, Stack, [], _OriginalTermData, cut, _Cont).


% print_stack(+Stack)
print_stack([]) :- !.
print_stack([Query|Queries]) :-
  format('->  ~w~n', [Query]),
  print_stack_(Queries).

% print_stack_(+Stack)
print_stack_([]) :- !.
print_stack_([Query|Queries]) :-
  format('    ~w~n', [Query]),
  print_stack_(Queries).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% abolish

% In case of SWI-Prolog, a predicate declared as discontiguous can be determined as such by predicate_property(P, discontiguous).
% As this is not the case for SICStus Prolog, the dynamic predicate jupyter_discontiguous(PredSpec) is used instead.
% PredSpec is the predicate spec of a predicate which was declared discontiguous.

% When removing a predicate from the dataase with abolish, its properties are removed as well.
% In that case, the corresponding jupyter_discontiguous/1 clause also needs to be removed.
% In order for this to work, abolish needs to be called as the single goal of a query.

:- if(sicstus).
% handle_abolish(+Goal, +CallRequestId, +Stack, +Bindings, +OriginalTermData, +LoopCont)
handle_abolish(Goal, CallRequestId, OriginalTermData) :-
  jupyter_query_handling:call_query_with_output_to_file(Goal, CallRequestId, [], OriginalTermData, Output, ErrorMessageData, IsFailure),
  % Exception, failure or success from Goal
  ( nonvar(ErrorMessageData) -> % Exception
    !,
    assert_error_response(exception, ErrorMessageData, Output, [])
  ; IsFailure == true -> % Failure
    !,
    assert_error_response(failure, ErrorMessageData, Output, [])
  ; otherwise -> % Success
    Goal =.. [abolish, Predicates|_Options],
    retract_jupyter_discontiguous(Predicates),
    assert_success_response(query, [], Output, [])
  ).


% retract_jupyter_discontiguous(+Predicates)
%
% Predicates is a predicate specification or a list of such.
retract_jupyter_discontiguous(Module:Name/Arity) :- !,
  catch(retractall(jupyter_discontiguous(Module:Name/Arity)), _Exception, true).
retract_jupyter_discontiguous(Name/Arity) :- !,
  catch(retractall(jupyter_discontiguous(Name/Arity)), _Exception, true).
retract_jupyter_discontiguous([]) :- !.
retract_jupyter_discontiguous([PredSpec|PredSpecs]) :-
  retract_jupyter_discontiguous(PredSpec),
  retract_jupyter_discontiguous(PredSpecs).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Assert the term responses

% For each term which is processed and produces a result, this result is asserted.
% This way, all results can be sent to the client when all terms of a request have been handled.

% assert_success_response(+Type, +Bindings, +Output, +AdditionalData)
%
% Type is the type of the term read from the client.
% It is one of: directive, clause_definition, query
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term.
% Output is the output of the term which was executed.
% AdditionalData is a list containing Key=Value pairs providing additional data for the client.
assert_success_response(Type, Bindings, Output, AdditionalData) :-
  %format('Success ~w:~n ~w~n~w~n ~w~n',[Type,Bindings,Output,AdditionalData]),
  assertz(term_response(json([status=success, type=Type, bindings=json(Bindings), output=Output|AdditionalData]))).


% assert_error_response(+ErrorCode, +ErrorMessageData, +Output, +AdditionalData)
%
% ErrorCode is one of the error codes defined by error_object_code/3 (e.g. exception).
% ErrorMessageData is a term of the form message_data(Kind, Term) so that the acutal error message can be retrieved with print_message(Kind, Term)
% Output is the output of the term which was executed.
% AdditionalData is a list containing Key=Value pairs providing additional data for the client.
assert_error_response(ErrorCode, ErrorMessageData, Output, AdditionalData) :-
  %format('ERROR ~w:~n ~w~n~w~n ~w~n',[ErrorCode,ErrorMessageData,Output,AdditionalData]),
  jupyter_jsonrpc:json_error_term(ErrorCode, ErrorMessageData, Output, AdditionalData, ErrorData),
  assertz(term_response(json([status=error, error=ErrorData]))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write_term_to_atom(+Term, +Bindings, -Atom)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term Term.
write_term_to_atom(Term, Bindings, Atom) :-
  write_term_to_codes(Term, TermCodes, [variable_names(Bindings)]),
  atom_codes(Atom, TermCodes).
