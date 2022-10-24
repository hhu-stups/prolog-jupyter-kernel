
install:
	pip install -e .
	python -m prolog_kernel.install

sics_tests:
	sicstus -l prolog_kernel/prolog_server/jupyter_server_tests.pl --goal "run_tests,halt."
swi_tests:
	swipl -l prolog_kernel/prolog_server/jupyter_server_tests.pl -t "run_tests,halt."


clean:
	pip uninstall prolog_kernel
	jupyter kernelspec remove prolog_kernel
