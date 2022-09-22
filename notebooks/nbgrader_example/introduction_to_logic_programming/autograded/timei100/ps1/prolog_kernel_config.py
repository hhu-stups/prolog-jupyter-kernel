c = get_config()

## If set to True, the logging level is set to DEBUG by the kernel so that debugging messages are logged.
# Default:
# c.PrologKernel.jupyter_logging = False

## If set to True, a log file is created by the Prolog server
# Default:
# c.PrologKernel.server_logging = False

## The ID of the Prolog implementation with which the server is started.
# Default:
# c.PrologKernel.implementation_id = "swi"
c.PrologKernel.implementation_id = "sicstus"

## The implementation specific data which is needed to run the Prolog server for code execution.
## This is required to be a dictionary containing at least an entry for the configured implementation_id.
## Each entry needs to define values for
## - "failure_response": The output which is displayed if a query fails
## - "success_response": The output which is displayed if a query succeeds without any variable bindings
## - "error_prefix": The prefix output for error messages
## - "informational_prefix": The prefix output for informational messages
## - "program_arguments": The command line arguments (a list of strings) with which the Prolog server can be started
##                        For SWI- and SICStus Prolog, the default Prolog server can be used by configuring the string "default"
##                        In that case, the following arguments are used (where the file path is extended to be absolute)
##                        SWI-Prolog:     ["swipl", "-l", "prolog_server/jupyter_server.pl", "-t", "jupyter_server_start"]
##                        SICStus Prolog: ["sicstus", "-l", "prolog_server/jupyter_server.pl", "--goal", "jupyter_server_start;halt.", "--nologo"]
## Additionally, a "kernel_implementation_path" can be provided, which needs to be an absolute path to a Python file.
## The corresponding module is required to define a subclass of PrologKernelBaseImplementation named PrologKernelImplementation.
## This can be used to override some of the kernel's basic behavior.
# Default:
# c.PrologKernel.implementation_data = {
#    "swi": {
#        "failure_response": "false",
#        "success_response": "true",
#        "error_prefix": "ERROR: ",
#        "informational_prefix": "% ",
#        "program_arguments": "default"
#    },
#    "sicstus": {
#        "failure_response": "no",
#        "success_response": "yes",
#        "error_prefix": "! ",
#        "informational_prefix": "% ",
#        "program_arguments": "default"
#    }
# }
