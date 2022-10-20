
# Herculog - A Jupyter Kernel for Prolog

A [Jupyter](https://jupyter.org/) kernel for Prolog based on the [IPython kernel](https://github.com/ipython/ipykernel).

By default, [SICStus Prolog](https://sicstus.sics.se/) and [SWI-Prolog](https://www.swi-prolog.org/) (which is the actual **default**) are supported. The kernel is implemented in a way that basically all functionality except the loading of configuration files can easily be overridden. This is especially useful for **extending the kernel for further Prolog implementations** or running code with a different version of an implementation. For further information about this, see [Configuration](#configuration).

Also see the [JupyterLab Prolog CodeMirror Extension](https://github.com/anbre/jupyterlab-prolog-codemirror-extension) for **syntax highlighting** of Prolog code in JupyterLab.

**Note:** The project is still under development. Even though major changes are unlikely, the possibility cannot be excluded. Furthermore, no liability is accepted for correctness and completeness.


## Examples

The directory [notebooks](./notebooks) contains some example Juypter notebooks. Note that all of them can be viewed with [nbviewer](https://nbviewer.org/) without having to install the kernel.

For instance, the notebooks in [notebooks/feature_introduction](./notebooks/feature_introduction) can be accessed via:
- [using_jupyter_notebooks_with_swi_prolog.ipynb](https://nbviewer.org/github/anbre/prolog-jupyter-kernel/blob/master/notebooks/feature_introduction/swi/using_jupyter_notebooks_with_swi_prolog.ipynb)
- [using_jupyter_notebooks_with_sicstus_prolog.ipynb](https://nbviewer.org/github/anbre/prolog-jupyter-kernel/blob/master/notebooks/feature_introduction/sicstus/using_jupyter_notebooks_with_sicstus_prolog.ipynb)

These notebooks serve as an **introduction to the features** of the kernel for SWI and SICStus Prolog. They also point out some peculiarities of the implementations.

The notebook in [notebooks/slides](./notebooks/slides) was created for a **slideshow** giving a rough overview of the kernel's features and its implementation.

The directory [notebooks/nbgrader_example](./notebooks/nbgrader_example) provides an example of a **course directory** for the [nbgrader extension](https://nbgrader.readthedocs.io/en/stable/).

Additionally, the file [jupyter_server_tests.pl](./prolog_server/jupyter_server_tests.pl) defines some **PL-Unit tests**. They provide further examples of what kind of code the Prolog server (and therefore the kernel) can handle and what the expected behavior is.


## Installation

### Requirements

- At least **Python** 3.5
  - Tested with Python 3.8.10
- **Jupyter** installation with JupyterLab and/or Juypter Notebook
  - Tested with
    - jupyter_core: 4.10.0
    - jupyterlab: 3.2.9
    - notebook: 6.4.8
- A **Prolog** installation for the configured implementation
  - In order to use the default configuration, SWI-Prolog is needed and needs to be on the PATH
  - Tested with version 8.4.3 of SWI-Prolog and SICStus 4.5.1
- For Windows, installing **graphviz** with pip does not suffice
  - Instead, it can be installed from [here](https://graphviz.org/download/) and added to the PATH (a reboot is required afterwards)

The installation was tested with Ubuntu 20.04 and Windows 10.


### Install

The kernel is provided as a Python package on the [Python Package Index](https://pypi.org/) and can be installed with `pip`:
1. Download the kernel:<br/> `python -m pip install prolog_kernel`
2. Install the kernel specification directory:
    - `python -m prolog_kernel.install`
    - There are the following options which can be seen when running `python -m prolog_kernel.install --help`
      - `--user`: install to the per-user kernel registry (default if not root and no prefix is specified)
      - `--sys-prefix`: install to Python's sys.prefix (e.g. virtualenv/conda env)
      - `--prefix PREFIX`: install to the given prefix: PREFIX/share/jupyter/kernels/ (e.g. virtualenv/conda env)


### Uninstall
1. `pip uninstall prolog_kernel`
2. `jupyter kernelspec remove prolog_kernel`


### Configuration

The kernel can be configured by defining a Python config file named `prolog_kernel_config.py`. The kernel will look for this file in the Jupyter config path (can be retrieved with `jupyter --paths`) and the current working directory. An **example** of such a configuration file with an explanation of the options and their default values commented out can be found [here](./prolog_kernel/prolog_kernel_config.py).

**Note:** If a config file exists in the current working directory, it overrides values from other configuration files.



In general, the kernel can be configured to use a different Prolog server (which is responsible for code execution) or kernel implementation. Furthermore, it can be configured to use another Prolog implementation altogether which might not be supported by default. The following options can be configured:
- `jupyter_logging`: If set to `True`, the logging level is set to DEBUG by the kernel so that **Python debugging messages** are logged.
  - Note that this way, logging debugging messages can only be enabled after reading a configuration file. Therefore, for instance, the user cannot be informed that no configuration file was loaded if none was defined at one of the expected locations.
  - In order to switch on debugging messages by default, the development installation described in the GitHub repository can be followed and the logging level set to `DEBUG` in the file `kernel.py` (which contains a corresponding comment).
  - However, note that this causes messages to be printed in the Jupyter console applications, which interferes with the other output.

- `server_logging`: If set to `True`, a **Prolog server log file** is created.
  - The name of the file consists of the implementation ID preceded by `.prolog_server_log_`.
- `implementation_id`: The ID of the **Prolog implementation** with which the server is started.
  - In order to use the default SWI- or SICStus Prolog implementation, the ID `swi` or `sicstus` is expected respectively.
- `implementation_data`: The **Prolog implementation-specific data** which is needed to run the server for code execution.
  - This is required to be a dictionary containing at least an entry for the configured `implementation_id`.
  - Each entry needs to define values for
    - `failure_response`: The output which is displayed if a query **fails**
    - `success_response`: The output which is displayed if a query **succeeds without any variable bindings**
    - `error_prefix`: The prefix that is output for **error messages**
    - `informational_prefix`: The prefix that is output for **informational messages**
    - `program_arguments`: **Command line arguments** with which the Prolog server can be started
      - For SICStus and SWI-Prolog, the default Prolog server provided by the kernel can be used by configuring the string `"default"`.
      - In that case, the following arguments are used (where the file path is extended to be absolute):
        - SWI-Prolog:     `["swipl", "-l", "prolog_server/jupyter_server.pl", "-t", "jupyter_server_start"]`
        - SICStus Prolog: `["sicstus", "-l", "prolog_server/jupyter_server.pl", "--goal", "jupyter_server_start;halt.", "--nologo"]`
  - Additionally, a `kernel_implementation_path` can be provided, which needs to be an **absolute path to a Python file**:
    - The corresponding module is required to define a subclass of `PrologKernelBaseImplementation` named `PrologKernelImplementation`. This can be used to override some of the kernel's basic behavior (see [Overriding the Kernel Implementation](#overriding-the-kernel-implementation)).

In addition to configuring the Prolog implementation to be used, the Prolog server implements the predicate `jupyter:set_prolog_impl(+PrologImplementationID)` to **change the Prolog implementation on the fly**. In order for this to work, the configured `implementation_data` dictionary needs to contain data for more than one Prolog implementation.


**Troubleshooting:**
In case of SICStus Prolog, if the given **`program_arguments` are invalid** (e.g. if the Prolog code file does not exist), the kernel waits for a response from the server which it will never receive. In that state it is **not able to log any exception** and instead, nothing happens.
To facilitate finding the cause of the error, before trying to start the Prolog server, the arguments and the directory from which they are tried to be executed are logged.


#### Overriding the Kernel Implementation

The actual kernel code determining the handling of requests is not implemented by the kernel class itself. Instead, there is the file [prolog_kernel_base_implementation.py](./prolog_kernel/prolog_kernel_base_implementation.py) which defines the class `PrologKernelBaseImplementation`. When the kernel is started, a (sub)object of this class is created. It handles the starting of and communication with the Prolog server. For all requests (execution, shutdown, completion, inspection) the kernel receives, a `PrologKernelBaseImplementation` method is called. By **creating a subclass** of this and defining the path to it as `kernel_implementation_path`, the **actual implementation code can be replaced**.

If no such path is defined, the path itself or the defined class is invalid, a **default implementation** is used instead. In case of SICStus and SWI-Prolog, the files [swi_kernel_implementation.py](./prolog_kernel/swi_kernel_implementation.py) and [sicstus_kernel_implementation.py](./prolog_kernel/sicstus_kernel_implementation.py) are used. Otherwise, the base implementation from the file [prolog_kernel_base_implementation.py](./prolog_kernel/prolog_kernel_base_implementation.py) is loaded.


## Development

### Development Install

1. `git clone https://github.com/anbre/prolog-jupyter-kernel.git`
2. Change to the root directory of the repository
3. `pip install .`
4. Install the kernel specification directory:
    - `python -m prolog_kernel.install`
    - For available installation options, see [Install](#install)


### Local Changes

In general, in order for local code adjustments to take effect, the kernel needs to be reinstalled. When installing the local project in *editable* mode with `pip install -e .` (e.g. by running `make`), restarting the kernel suffices.

Adjustments of the Prolog server code are loaded when the server is restarted. Thus, when changing Prolog code only, instead of restarting the whole kernel, it can be interrupted, which causes the Prolog server to be restarted.


### Upload to PyPI

This kernel is available as a Python package on the [Python Package Index](https://pypi.org/project/prolog-kernel/). A new version of the package can be published in the following way:
1. Install the requirements build and twine: <br/> `pip install build twine`
2. Increase the version in [pyproject.toml](./pyproject.toml)
3. Create the distribution files: <br/> `python -m build`
4. Upload the package to PyPI: <br/> `twine upload dist/*`

For further information, see the [Packaging Python Projects Tutorial](https://packaging.python.org/en/latest/tutorials/packaging-projects/).


### Debugging

Usually, if the execution of a goal causes an exception, the corresponding Prolog error message is computed and displayed in the Jupyter frontend. However, in case something goes wrong unexpectedly or the query does not terminate, the **Prolog server might not be able to send a response to the client**. In that case, the user can only see that the execution does not terminate without any information about the error or output that might have been produced. However, it is possible to write logging messages and access any potential output, which might facilitate finding the cause of the error.

Debugging the server code is not possible in the usual way by tracing invocations. Furthermore, all messages exchanged with the client are written to the standard streams. Therefore, printing helpful debugging messages does not work either. Instead, if `server_logging` is configured, **messages can be written to a log file** by calling `log/1` or `log/2` from the module `jupyter_logging`. By default, only the responses sent to the client are logged.

When a query is executed, all its output is written to a file named `.server_output`, which is deleted afterwards by `jupyter_query_handling:delete_output_file`. If an error occurs during the actual execution, the file cannot be deleted and thus, the **output of the goal can be accessed**. Otherwise, the deletion might be prevented.

Furthermore, the server might send a response which the client cannot handle. In that case, **logging for the Python code** can be enabled by configuring `jupyter_logging`. For instance, the client logs the responses received from the server.
