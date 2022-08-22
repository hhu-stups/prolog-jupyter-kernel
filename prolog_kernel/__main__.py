from ipykernel.kernelapp import IPKernelApp
from prolog_kernel.kernel import PrologKernel

IPKernelApp.launch_instance(kernel_class=PrologKernel)
