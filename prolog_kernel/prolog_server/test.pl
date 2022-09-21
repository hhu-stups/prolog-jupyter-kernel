
% This file defines PL-Unit tests.
% It is used by the tests in jupyter_server_tests.pl for loading and running a test file with the Prolog server.


test_pred(a).
test_pred(b).


:- use_module(library(plunit)).


:- begin_tests(test).

test(a) :-
  test_pred(a).

test(b) :-
  test_pred(b).

test(c, [fail]) :-
  test_pred(c).

:- end_tests(test).
