import argparse
import json
import logging
import os
import shutil
import sys
import tempfile

from jupyter_client.kernelspec import KernelSpecManager

logger = logging.getLogger(__name__)

KERNELSPEC_FILES = [
    'kernel.js',
]

def get_kernelspec_dir_path():
    """
    Get the path of the kernelspec directory where the static files needed for the installation are.
    This currently only includes the kernel.js file,
    because the kernelspec (kernel.json) is generated dynamically.
    """
    dirname = os.path.dirname(__file__)
    kernelspec_dir_path = os.path.join(dirname, 'kernelspec')
    return kernelspec_dir_path

def create_kernelspec(dest_dir):
    with open(os.path.join(dest_dir, 'kernel.json'), 'w', encoding='utf-8') as f:
        kernel_json = {
            'argv': [sys.executable, '-m', 'prolog_kernel', '-f', '{connection_file}'],
            'display_name': 'Prolog',
            'language': 'prolog',
        }
        json.dump(kernel_json, f, ensure_ascii=False, indent=4)

    kernelspec_dir = get_kernelspec_dir_path()
    for file in KERNELSPEC_FILES:
        shutil.copyfile(os.path.join(kernelspec_dir, file), os.path.join(dest_dir, file))

def _is_root():
    try:
        return os.geteuid() == 0
    except AttributeError:
        # non-Unix platform -> assume not root
        return False

def main(argv=None):
    logging.basicConfig(
        format='%(levelname)s: %(message)s',
        level=logging.INFO,
    )

    ap = argparse.ArgumentParser()
    ap.add_argument(
        '--user',
        action='store_true',
        help="install to the per-user kernel registry (default if not root and no prefix is specified)")
    ap.add_argument(
        '--sys-prefix',
        action='store_true',
        help="install to Python's sys.prefix (e.g. virtualenv/conda env)")
    ap.add_argument(
        '--prefix',
        help="install to the given prefix: PREFIX/share/jupyter/kernels/ (e.g. virtualenv/conda env)")
    args = ap.parse_args(argv)

    if args.sys_prefix:
        args.prefix = sys.prefix
    if not args.prefix and not _is_root():
        args.user = True

    with tempfile.TemporaryDirectory() as temp_dir:
        create_kernelspec(temp_dir)
        KernelSpecManager().install_kernel_spec(temp_dir, 'prolog_kernel', user=args.user, prefix=args.prefix)

if __name__ == '__main__':
    main()
