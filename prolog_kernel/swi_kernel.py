"""
Default SWI-Prolog kernel implementation.

Defines the inspection for other predicates than the ones defined in the module jupyter.
"""


from prolog_kernel.prolog_kernel_base_implementation import PrologKernelBaseImplementation

# When overriding this at a different location, add the location of the prolog_kernel_base_implementation.py file defining the PrologKernelBaseImplementation class to the search path for modules
#sys.path.append('/path/to/kernel/prolog_kernel/prolog_kernel')
#from prolog_kernel_base_implementation import PrologKernelBaseImplementation


class PrologKernelImplementation(PrologKernelBaseImplementation):

    def do_inspect(self, code, cursor_pos, detail_level=0, omit_sections=()):
        """
        For SWI-Prolog, help for a predicate can be accessed with help/1.
        When inspecting a token, the output of this predicate precedes the docs for predicates from module jupyter.
        """
        # Get the matching predicates from module jupyter
        token, jupyter_data = self.get_token_and_jupyter_predicate_inspection_data(code, cursor_pos)

        if not token:
            # There is no token which can be inspected
            return {'status': 'ok', 'data': {}, 'metadata': {}, 'found': False}

        try:
            # Request predicate help with help/1
            response_dict = self.server_request(0, 'call', {'code':'help(' + token + ')'})
            help_output = response_dict["result"]["1"]["output"]

        except Exception as exception:
            self.logger.error(exception, exc_info=True)
            help_output = ''

        found = True

        if help_output == '':
            # There is no help/1 ouput
            if jupyter_data == {}:
                data = {}
                found = False
            else:
                data = jupyter_data
        else:
            # There is help/1 ouput
            jupyter_docs_plain = help_output
            jupyter_docs_md = '<pre>' + help_output.replace('\n', '<br>').replace('$', '&#36;') + '</pre>'

            if jupyter_data != {}:
                # Append the jupyter docs
                jupyter_docs_plain += '\n\n----------------------------------------------------------------------------\n\n' + jupyter_data['text/plain']
                jupyter_docs_md += '<br>----------------------------------------------------------------------------<br><br>' + jupyter_data['text/markdown']

            data = {'text/plain': jupyter_docs_plain, 'text/markdown': jupyter_docs_md}

        return {'status': 'ok', 'data': data, 'metadata': {}, 'found': found}
