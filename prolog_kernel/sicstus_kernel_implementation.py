"""
Default SICStus Prolog kernel implementation.

Defines the inspection for other predicates than the ones defined in the module jupyter.
"""


import requests

from bs4 import BeautifulSoup, Tag

from prolog_kernel.prolog_kernel_base_implementation import PrologKernelBaseImplementation

# When overriding this at a different location, add the location of the prolog_kernel_base_implementation.py file defining the PrologKernelBaseImplementation class to the search path for modules
#sys.path.append('/path/to/kernel/prolog_kernel/prolog_kernel')
#from prolog_kernel_base_implementation import PrologKernelBaseImplementation


class PrologKernelImplementation(PrologKernelBaseImplementation):

    def retrieve_predicate_information(self):
        """When inspecting a built-in predicate, the SICStus Prolog version is needed for retrieving the documentation URLs"""
        try:
            version_response_dict = self.server_request(0, 'version', log_response=False)
            self.sicstus_version = version_response_dict["result"]

            self.predicate_doc_links = self.get_predicate_doc_links()
        except Exception as exception:
            self.logger.error(exception, exc_info=True)

        super().retrieve_predicate_information()


    def get_predicate_doc_links(self):
        """
        Retrieves the links to the documentation of all predicates for the current Prolog version from the Predicate Index page (e.g. https://sicstus.sics.se/sicstus/docs/latest/html/sicstus.html/Predicate-Index.html for the latest version).
        The link texts look like the following: 'zip/0 (built-in):', 'assert/[1,2] (built-in, ref page):'

        Returns a dictionary where the keys are the names and arities of the built-in predicates (the part of the link text before the bracket).
        The values are lists containing dictionaries with elements 'link_text' and 'link'.

        Additionally, the dictionary contains elements where the key is the predicate name and arity of a predicate exported by the Prolog module 'jupyter'.
        For such a predicate the corresponding value is the documentation of the predicate as a string.

        Example
        ------
        The dictionary containing the first built-in element only might look like the following:
        {'!/0': [{'link_text': '!/0 (built-in, ref page)', 'link': 'https://sicstus.sics.se/sicstus/docs/4.5.1/html/sicstus.html/mpg_002dref_002dcut.html#index-_0021_002f0-_0028built_002din_002c-ref-page_0029-1'}]}
        """
        try:
            # Get the html content and parse it
            base_url = 'https://sicstus.sics.se/sicstus/docs/' + self.sicstus_version + '/html/sicstus.html/'

            predicate_index_link = base_url + '/Predicate-Index.html'
            response = requests.get(predicate_index_link, timeout=1)
            soup = BeautifulSoup(response.text, 'html.parser')

            predicate_data = {}

            # The table "index-pl" contains all the predicate links
            # Find the hyperlink objects
            tables = soup.find_all('table', class_='index-pl')
            if len(tables) > 0:
                for link in tables[0].find_all('a'):
                    if len(link.contents) > 0:
                        child = link.contents[0]
                        if isinstance(child, Tag):
                            # Remove the part in brackets
                            predicate_string = child.text.split('(')[0].strip()
                            predicate_link_dict = {'link_text': child.text, 'link': base_url + link.get('href')}
                            if predicate_string in predicate_data:
                                link_dicts = predicate_data[predicate_string]
                                link_dicts.append(predicate_link_dict)
                                predicate_data[predicate_string] = link_dicts
                            else:
                                predicate_data[predicate_string] = [predicate_link_dict]

            return predicate_data
        except Exception as exception:
            self.logger.error(exception, exc_info=True)
            return None


    def do_inspect(self, code, cursor_pos, detail_level=0, omit_sections=()):
        """
        For SICStus Prolog, the website https://sicstus.sics.se/sicstus/docs/latest/html/sicstus.html/Predicate-Index.html lists links to the documentation of predicates.
        When inspecting a token, links for matching predicates are shown preceding the docs for predicates from module jupyter.
        """
        # Get the matching predicates from module jupyter
        token, jupyter_data = self.get_token_and_jupyter_predicate_inspection_data(code, cursor_pos)

        if not token:
            # There is no token which can be inspected
            return {'status': 'ok', 'data': {}, 'metadata': {}, 'found': False}

        # If the links could not be retrieved when starting the kernel, try reading them again
        if self.predicate_doc_links is None:
            self.predicate_doc_links = self.get_predicate_doc_links()

        if self.predicate_doc_links is None and jupyter_data == {}:
            # There is no matching predicate
            return {'status': 'ok', 'data': {}, 'metadata': {}, 'found': False}

        # Find the matching predicate links
        # If a key of the dictionary contains the current token, the element is assumed to match
        matching_predicate_data = {pred:self.predicate_doc_links[pred] for pred in self.predicate_doc_links if (token in pred)}

        found = True

        if len(matching_predicate_data) == 0:
            # There is no matching predicate from the Predicate Index website
            if jupyter_data == {}:
                data = {}
                found = False
            else:
                data = jupyter_data
        else:
            # There is a matching predicate from the Predicate Index website
            # Compute the link texts
            jupyter_docs_plain = ''
            jupyter_docs_md = ''

            for pred, data in matching_predicate_data.items():
                if isinstance(data, list):
                    for link_dict in data:
                        jupyter_docs_plain += '\x1b[0m' + link_dict['link_text'] + ':\n\x1b[0;34m' + link_dict['link'] + '\n\n'
                        jupyter_docs_md += '<pre><a href="' + link_dict['link'] + '">' + link_dict['link_text'] + '</a><br><br></pre>'

            if jupyter_data != {}:
                # Append the jupyter docs
                jupyter_docs_plain += '\x1b[0m' + '_'*80 + '\n\n' + jupyter_data['text/plain']
                jupyter_docs_md += '' + '_'*80 + '<br><br>' + jupyter_data['text/markdown']

            data = {'text/plain': jupyter_docs_plain, 'text/markdown': jupyter_docs_md}

        return {'status': 'ok', 'data': data, 'metadata': {}, 'found': found}
