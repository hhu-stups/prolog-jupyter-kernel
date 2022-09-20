% This file defines PL-Unit tests.
% It is used to show how these tests can be run from a Jupyter notebook (Using_Jupyter_Notebooks_with_SWI-Prolog.ipynb).


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

