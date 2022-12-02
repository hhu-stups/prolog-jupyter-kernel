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

def main(argv=None):
    logging.basicConfig(
        format='%(levelname)s: %(message)s',
        level=logging.INFO,
    )

    ap = argparse.ArgumentParser()
    ap.add_argument(
        '--user',
        action='store_true',
        help="install to the per-user kernel registry instead of sys.prefix (use if you get permission errors during installation)")
    ap.add_argument(
        '--prefix',
        help="install to the given prefix: PREFIX/share/jupyter/kernels/")
    args = ap.parse_args(argv)

    if not args.user and not args.prefix:
        args.prefix = sys.prefix

    with tempfile.TemporaryDirectory() as temp_dir:
        create_kernelspec(temp_dir)
        KernelSpecManager().install_kernel_spec(temp_dir, 'prolog_kernel', user=args.user, prefix=args.prefix)

if __name__ == '__main__':
    main()
