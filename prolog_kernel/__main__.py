from ipykernel.kernelapp import IPKernelApp
from prolog_kernel.kernel import PrologKernel


def entry_point():
    IPKernelApp.launch_instance(kernel_class=PrologKernel)


IPKernelApp.launch_instance(kernel_class=PrologKernel)
