from ipykernel.kernelapp import IPKernelApp
from . import PrologKernel

IPKernelApp.launch_instance(kernel_class=PrologKernel)
