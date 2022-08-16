from distutils.core import setup

setup(
	name="Prolog",
	version="0.0.0",
	packages=["prolog_kernel"],
	description="Jupyter kernel for (SWI- and SICStus) Prolog",
	author="Anne Brecklinghaus",
	install_requires=[
		"jupyter_client",
		"IPython",
		"ipykernel",
		"graphviz",
        "beautifulsoup4",
	],
	classifiers=[
		"Intended Audience :: Developers",
		"Programming Language :: Python :: 3",
	],
)
