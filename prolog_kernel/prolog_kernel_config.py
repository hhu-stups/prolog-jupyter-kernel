c = get_config()

## The ID of the Prolog implementation which is used to execute code.
# Default:
# c.PrologKernel.implementation_id = "swi"

## The implementation specific data which is needed to run the Prolog server for code execution.
## This needs to be a dictionary which needs to at least contain an entry for the configured implementation_id.
## Each entry needs to define values for
## - "failure_response": The output which is displayed if a query fails
## - "success_response": The output which is displayed if a query succeeds without any variable bindings
## - "error_prefix": The prefix output for error messages
## - "informational_prefix": The prefix output for informational messages
## - "program_arguments": The command line arguments with which the Prolog server can be started
##                        If this differs from the default value, an absolute path or one relative to the location of the Jupyter notebook needs to be provided
## Additionally, a "kernel_implementation_path" (which needs to be absolute) can be provided.
## The corresponding module needs to define a class PrologKernelImplementation as a subclass of PrologKernelBaseImplementation.
## It can be used to override the kernel's behavior.
# Default:
# c.PrologKernel.implementation_data = {
#    "swi": {
#        "failure_response": "false",
#        "success_response": "true",
#        "error_prefix": "ERROR: ",
#        "informational_prefix": "% ",
#        "program_arguments": ["swipl",
#                              "-l", "prolog_server/jsonrpc_server.pl",
#                              "-t", "jsonrpc_server_start"]
#    },
#    "sicstus": {
#        "failure_response": "no",
#        "success_response": "yes",
#        "error_prefix": "! ",
#        "informational_prefix": "% ",
#        "program_arguments": ["sicstus",
#                              "-l", "prolog_server/jsonrpc_server.pl",
#                              "--goal", "jsonrpc_server_start;halt.",
#                              "--nologo"]
#    }
# }
