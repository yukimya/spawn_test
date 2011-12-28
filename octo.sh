#!/bin/sh
#$ -S /bin/sh
#$ -cwd
#$ -q all.q
#$ -pe ompi 51
#export OMP_NUM_THREADS=1
#export OMP_MCA_btl=tcp,self
#export OMPI_MCA_btl_tcp_if_include=eth0
#export PATH LD_LIBRARY_PATH OMPI_MCA_btl OMPI_MCA_btl_tcp_if_include
mpiexec -n 1 ./get_m 
