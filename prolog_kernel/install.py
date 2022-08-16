import argparse
import os

from jupyter_client.kernelspec import KernelSpecManager

def get_kernelspec_dir_path():
    """
    Get the path of the kernelspec directory where the files needed for the installation are.
    These include the kernel.json and kernel.js files.
    """
    dirname = os.path.dirname(__file__)
    kernelspec_dir_path = os.path.join(dirname, '..', 'kernelspec')
    return kernelspec_dir_path

def _is_root():
    try:
        return os.geteuid() == 0
    except AttributeError:
        # non-Unix platform -> assume not root
        return False

def main(argv=None):
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

    print('Installing Prolog kernel spec')
    KernelSpecManager().install_kernel_spec(get_kernelspec_dir_path(), 'prolog_kernel', user=args.user, prefix=args.prefix)

if __name__ == '__main__':
    main()
