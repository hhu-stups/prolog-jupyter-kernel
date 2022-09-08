"""
A Prolog Jupyter kernel communicating with a Prolog server with JSON-RPC 2.0 messages.
The communication is based on 'jsonrpc_client.py' from SICStus Prolog 4.5.1.

By default, SICStus Prolog and SWI-Prolog (which is the actual default) are supported.
By defining a 'prolog_kernel_config.py' file, the Prolog implementation to be used can be defined.
In addition to providing an implementation_id (for SICStus und SWI-Prolog, the IDs 'sicstus' and 'swi' are expected),
further implementation specific data (a dictionary 'implementation_data' with the implementation_id as key) can be defined.
This includes the command line arguments with which the Prolog server can be started.

Additionally, there is the Prolog predicate 'jupyter:set_prolog_impl(+PrologImplementationID)' with which the implementation can be changed.
In order for this to work, the configured 'implementation_data' dictionary needs to contain data for more than one Prolog implementation.

An example of a configuration file with an explanation of the options and their default values commented out can be found in the current directory.
When defined, this file needs to be present in one of the Jupyter config paths (can be retrieved with 'jupyter --paths') or the current working directory.

The actual kernel code is not implemented by this kernel class itself.
Instead, there is the file 'prolog_kernel_base_implementation.py' which defines the class 'PrologKernelBaseImplementation'.
When the kernel is started, a (sub)object of this class is created.
It handles the starting of and communication with the Prolog server.
For all requests (execution, shutdown, completion, inspection) the kernel receives, a 'PrologKernelBaseImplementation' method is called.
By creating a subclass of this and defining the path to it as 'kernel_implementation_path', the actual implementation code can be replaced.

If no such path is defined, the path itself or the defined class is invalid, a default implementation is used instead.
In case of SWI- and SICStus Prolog, the files 'swi_kernel_implementation.py' and 'sicstus_kernel_implementation.py' are used, which can be found in the current directory.
Otherwise, the base implementation from the file 'prolog_kernel_base_implementation.py' is loaded.

The Prolog Jupyter kernel is implemented in a way that basically all functionality except the loading of the configuration can easily be overriden.
This is especially useful for extending the kernel for further Prolog implementations.
"""


import importlib.util
import logging
import os
import sys

from inspect import getmembers, isclass
from ipykernel.kernelbase import Kernel
from jupyter_core.paths import jupyter_config_path
from traitlets import Bool, Dict, Unicode
from traitlets.config.loader import ConfigFileNotFound, PyFileConfigLoader

import prolog_kernel.swi_kernel_implementation
import prolog_kernel.sicstus_kernel_implementation

from prolog_kernel.prolog_kernel_base_implementation import PrologKernelBaseImplementation


# Set the logging format
logging.basicConfig(format='[%(asctime)s] {%(filename)s:%(lineno)d} %(levelname)s - %(message)s')


class PrologKernel(Kernel):
    kernel_name = 'prolog_kernel'
    implementation = kernel_name
    implementation_version = '1.0'
    language_info = {
        'name': 'Prolog',
        'file_extension': '.pl',
        'mimetype': 'text/x-prolog',
        'codemirror_mode': 'prolog',
    }
    banner = kernel_name


    # Define default configuration options

    # If set to True, the logging level is set to DEBUG by the kernel so that debugging messages are logged.
    jupyter_logging = Bool(False).tag(config=True)

    # If set to True, a log file is created by the Prolog server.
    server_logging = Bool(False).tag(config=True)

    # The ID of the Prolog implementation which is used to execute code.
    # It is required that the implementation_data dictionary contains an item with this key.
    implementation_id = Unicode('swi').tag(config=True)

    # The default program argutments for SWI- and SICStus Prolog
    default_program_arguments = {
        "swi": ["swipl",
                "-l", "prolog_server/jsonrpc_server.pl",
                "-t", "jsonrpc_server_start"],
        "sicstus": ["sicstus",
                    "-l", "prolog_server/jsonrpc_server.pl",
                    "--goal", "jsonrpc_server_start;halt.",
                    "--nologo"]
    }

    # The implementation specific data which is needed to run the Prolog server for code execution.
    # This is required to be a dictionary containing at least an entry for the configured implementation_id.
    # Each entry needs to define values for
    # - "failure_response": The output which is displayed if a query fails
    # - "success_response": The output which is displayed if a query succeeds without any variable bindings
    # - "error_prefix": The prefix output for error messages
    # - "informational_prefix": The prefix output for informational messages
    # - "program_arguments": The command line arguments (a list of strings) with which the Prolog server can be started
    #                        For SWI- and SICStus Prolog, the default Prolog server can be used by configuring the string "default"
    #                        In that case, the following arguments are used (where the file path is extended to be absolute)
    #                        SWI-Prolog:     ["swipl", "-l", "prolog_server/jsonrpc_server.pl", "-t", "jsonrpc_server_start"]
    #                        SICStus Prolog: ["sicstus", "-l", "prolog_server/jsonrpc_server.pl", "--goal", "jsonrpc_server_start;halt.", "--nologo"]
    # Additionally, a "kernel_implementation_path" can be provided, which needs to be an absolute path to a Python file.
    # The corresponding module is required to define a subclass of PrologKernelBaseImplementation named PrologKernelImplementation.
    # This can be used to override some of the kernel's basic behavior.
    implementation_data = Dict({
        "swi": {
            "failure_response": "false",
            "success_response": "true",
            "error_prefix": "ERROR: ",
            "informational_prefix": "% ",
            "program_arguments": "default"
        },
        "sicstus": {
            "failure_response": "no",
            "success_response": "yes",
            "error_prefix": "! ",
            "informational_prefix": "% ",
            "program_arguments": "default"
        }
    }).tag(config=True)

    # The keys which are required for each entry in the implementation_data dict.
    required_implementation_data_keys = [
        "failure_response",
        "success_response",
        "error_prefix",
        "informational_prefix",
        "program_arguments"
    ]

    logger = None

    # A dictionary with implementation ids as keys and the corresponding PrologKernelBaseImplementation as value.
    # When a Prolog implementation is started, it is added to the dictionary.
    # On kernel shutdown or interruption, all implementations are shutdown/interrupted
    active_kernel_implementations = {}


    def __init__(self, **kwargs):
        super().__init__(**kwargs)

        self.logger = logging.getLogger()
        # For development, the logging level can be set to level DEBUG, so that all debug messages (including the ones about loading a configuration file) are output
        #self.logger.setLevel(logging.DEBUG)

        # Load the configuration and configured implementation specific data
        self.load_config_file()
        load_exception_message = self.load_implementation_data(self.implementation_id)
        if load_exception_message:
            # The configured implementation_data is invalid
            raise Exception(load_exception_message)

        # Create an implementation object which starts the Prolog server
        self.load_kernel_implementation()


    def load_config_file(self):
        """
        Searches the search paths for Jupyter config files and the current working directory for prolog_kernel_config.py files.
        If such files exist, they are loaded.
        If a config file exists in the current working directory, its configuration overrides values from other config files.

        Based on _load_config_files(cls, basefilename, path, log, raise_config_file_errors) from traitlets.config.application.Application.
        """

        config_file_name = 'prolog_kernel_config.py'

        # Get the search path for Jupyter config files and add the current working directory
        config_paths = jupyter_config_path()
        config_paths.insert(0, os.getcwd())

        # List all existing Prolog kernel config files
        existing_file_paths = [p for p in config_paths if os.path.exists(os.path.join(p, config_file_name))]
        # existing_file_paths list is in descending priority order, so it needs to be reversed
        existing_file_paths.reverse()

        if not existing_file_paths:
            self.logger.debug("No " + config_file_name + " file found in one of these paths: " + str(config_paths))
            self.logger.debug("Using the default configuration")
            return

        # For all paths, load the config file and upadte the configuration
        for existing_file_path in existing_file_paths:
            loader = PyFileConfigLoader(config_file_name, path=existing_file_path, log=self.logger)

            config = None
            try:
                config = loader.load_config()
            except ConfigFileNotFound:
                self.logger.error("Could not find the config file " + os.path.join(existing_file_path, config_file_name), exc_info=True)
            except Exception:
                self.logger.error("Exception while loading config file " + os.path.join(existing_file_path, config_file_name), exc_info=True)
            else:
                # Update the configuration
                self.update_config(config)

                # If configured, enable the logging of debug messages
                if self.jupyter_logging:
                    self.logger.setLevel(logging.DEBUG)

                self.logger.debug("Loaded config file: " + loader.full_filename)


    def load_implementation_data(self, implementation_id):
        """
        Tries to set the implementation data for the Prolog implementation with ID implementation_id.
        If no such data is provided for the given implementation ID or the dictionary does not contain all required keys, a corresponding message is returned.
        """

        # Check if there is an item for the implementation_id
        if not implementation_id in self.implementation_data:
            return "There is no configured implementation_data entry for the Prolog implementation ID '" + implementation_id + "'"

        # Check if all required keys are contained in the dictionary
        missing_keys = []
        for key in self.required_implementation_data_keys:
            if not key in self.implementation_data[implementation_id]:
                missing_keys.append(key)

        if missing_keys == []:
            # The implementation data is valid
            self.active_implementation_data = self.implementation_data[implementation_id]
        elif len(missing_keys) == 1:
            return "The configured implementation_data dict for the Prolog implementation ID '" + implementation_id + "' needs to contain an entry for '" + missing_keys[0] + "'"
        else:
            return "The configured implementation_data dict for the Prolog implementation ID '" + implementation_id + "' needs to contain entries for '" + "', '".join(missing_keys) + "'"


    def load_kernel_implementation(self):
        """
        In order for the kernel to be able to execute code, a (sub)object of 'PrologKernelBaseImplementation' is needed.
        If the configured implementation_data contains an entry for 'kernel_implementation_path', tries to load the corresponding module and create a 'PrologKernelImplementation' defined in it.
        This causes the Prolog server to be started so that code can be executed.

        If no 'kernel_implementation_path' is given or it is invalid, a default implementation is used instead.
        For the Prolog implementations with ID 'swi' or 'sicstus', there is a module defining the class 'PrologKernelImplementation' in the current directory.
        Otherwise, the 'PrologKernelBaseImplementation' is used.
        """

        use_default = False

        if 'kernel_implementation_path' in self.active_implementation_data:
            file_path = self.active_implementation_data['kernel_implementation_path']

            if not os.path.exists(file_path):
                use_default = True
                self.logger.debug("The configured kernel_implementation_path '" + str(file_path) + "' does not exist")
            else:
                self.logger.debug("Loading kernel specific code from '" + str(file_path) + "'")
                # Load the module from the specified file
                (module_name, file_extension)= os.path.splitext(os.path.basename(file_path))
                spec = importlib.util.spec_from_file_location(module_name, file_path)
                kernel_implementation_module = importlib.util.module_from_spec(spec)
                sys.modules[module_name] = kernel_implementation_module
                spec.loader.exec_module(kernel_implementation_module)

                # Try to get the class with name 'PrologKernelImplementation' and check if it is valid
                implementation_classes = list(class_pair[1] for class_pair in getmembers(kernel_implementation_module, isclass) if class_pair[0]=='PrologKernelImplementation')
                if len(implementation_classes) == 0:
                    use_default = True
                    self.logger.debug("The module at the configured kernel_implementation_path needs to define the class 'PrologKernelImplementation'")
                else:
                    # Try loading the specific implementation
                    try:
                        self.active_kernel_implementation = kernel_implementation_module.PrologKernelImplementation(self)
                        if not isinstance(self.active_kernel_implementation, kernel_implementation_module.PrologKernelBaseImplementation):
                            use_default = True
                            self.logger.debug("The class 'PrologKernelImplementation' needs to be a subclass of 'PrologKernelBaseImplementation'")
                    except Exception:
                        use_default = True
                        self.logger.debug("Exception while creating a 'PrologKernelImplementation' object" , exc_info=True)
        else:
            use_default = True
            self.logger.debug('No kernel_implementation_path configured')

        if use_default:
            # The configured implementation could not be loaded
            # A default implementation is used instead
            if self.implementation_id == 'swi':
                self.logger.debug("Using the default implementation for SWI-Prolog")
                self.active_kernel_implementation = prolog_kernel.swi_kernel_implementation.PrologKernelImplementation(self)
            elif self.implementation_id == 'sicstus':
                self.logger.debug("Using the default implementation for SICStus Prolog")
                self.active_kernel_implementation = prolog_kernel.sicstus_kernel_implementation.PrologKernelImplementation(self)
            else:
                self.logger.debug("Using the base implementation")
                self.active_kernel_implementation = PrologKernelBaseImplementation(self)

        # Add the Prolog implementation specific implementation class to the dictionary of active implementations
        self.active_kernel_implementations[self.implementation_id] = self.active_kernel_implementation


    def change_prolog_implementation(self, prolog_impl_id):
        """
        Change the Prolog implementation to the one with ID prolog_impl_id.
        If there is a running server for that implementation, it is activated.
        Otherwise, the implementation-specific data is loaded (which starts a new server) and set as the active one.
        Returns True if something goes wrong and the new implementation cannot be used.
        """

        self.logger.debug('Change Prolog implementation to ' + str(prolog_impl_id))

        if prolog_impl_id in self.active_kernel_implementations:
            # There is a running Prolog server for the provided implementation ID
            # Make it the active one
            self.implementation_id = prolog_impl_id
            self.active_kernel_implementation = self.active_kernel_implementations[self.implementation_id]
        else:
            # Try to load the implementation specific data
            load_exception_message = self.load_implementation_data(prolog_impl_id)
            if load_exception_message:
                self.logger.debug(load_exception_message)
                # The configured implementation_data is invalid
                # Display an error message
                load_exception_message = self.active_implementation_data["error_prefix"] + load_exception_message

                display_data = {
                    'data': {
                        'text/plain': load_exception_message,
                        'text/markdown': '<pre style="' + self.active_kernel_implementation.output_text_style + 'color:red">' + load_exception_message + '</pre>',
                        'text/latex': load_exception_message.replace('\n', '\\\\\n ') # TODO adjust for latex
                    },
                    'metadata': {}}
                self.send_response(self.iopub_socket, 'display_data', display_data)
                return True
            else:
                self.implementation_id = prolog_impl_id
                # Create an implementation object which starts the Prolog server
                self.load_kernel_implementation()


    def interrupt_all(self):
        # Interrupting the kernel interrupts the running Prolog processes, so all of them need to be restarted
        for implementation_id, kernel_implementation in self.active_kernel_implementations.items():
            kernel_implementation.kill_prolog_server()


    ############################################################################
    # Overriden kernel methods
    ############################################################################


    def do_shutdown(self, restart):
        # Shutdown all active Prolog servers so that no processes are kept running
        for kernel_implementation in self.active_kernel_implementations.values():
            kernel_implementation.do_shutdown(restart)

        return {'status': 'ok', 'restart': restart}


    def do_execute(self, code, silent, store_history=True, user_expressions=None, allow_stdin=False):
        return self.active_kernel_implementation.do_execute(code, silent, store_history, user_expressions, allow_stdin)


    def do_complete(self, code, cursor_pos):
        return self.active_kernel_implementation.do_complete(code, cursor_pos)


    def do_inspect(self, code, cursor_pos, detail_level=0, omit_sections=()):
        return self.active_kernel_implementation.do_inspect(code, cursor_pos, detail_level, omit_sections)
