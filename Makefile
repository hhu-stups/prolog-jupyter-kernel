
install:
	pip install -e .
	python -m prolog_kernel.install

clean:
	pip uninstall prolog_kernel
	jupyter kernelspec remove prolog_kernel
