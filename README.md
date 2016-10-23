# MPMD-with-Coarray-Fortran-Load-Balancing-Example-Program
THIS REPOSITORY CONTAINS A FORTRAN (2008) COARRAY EXAMPLE PROGRAM THAT ILLUSTRATES A SIMPLE PGAS LOAD BALANCING TECHNIQUE

# Overview
This Repository contains a MPMD-style Fortran (2008) coarray example program to illustrate a simple PGAS load balancing technique.

Effectively, the Load Balancing example program does execute the same computations, no matter if it uses 11 or 13 coarray images, for an example. To do so with only 11 images, it does reuse two TeamManager images to execute TeamMember objects afterwards.

Nevertheless, the crucial point here is not just the simple reuse of coarray images. More importantly, the Load Balancing example program does use the same parallel programming code to initiate both, the remote execution as well as the local execution of TeamMember objects, even at the same time. To achieve this, it uses a coarray wrapper for both purposes, remote data transfer as well as local data transfer through PGAS memory. 

The example program is still very primitive. Nevertheless, using such a coding style routinely might be very helpful for easy development of dynamic load balancing code later on.

The example program was derived from our original 'MPMD with Fortran 2008 Coarrays' example program: https://github.com/MichaelSiehl/MPMD-with-Fortran-2008-Coarrays. For a brief description of the modifications see our short paper 'MPMD with Coarray Fortran (PGAS): Load Balancing – Example Program' (pdf format) in the Repositorys main folder or at http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf.

# Important Update (16/10/23)
Please be aware that the original source code in the 'src' folder is not conforming with the Fortran (2008/2015) standard since it does not use any Fortran (2008) means for the synchronizations (between coarray images) in the MPMD-style part of the source codes.<br />
I just added a new source code folder 'src_atomic_version', containing a very similar example program which does utilize Fortran 2008 SYNC MEMORY and atomic subroutines. Within that new version of the example program the SYNC MEMORY statements do form user-defined unordered execution segments (see Modern Fortran explained, 2011, Appendix B.10.1 for a brief description). To compile that code you may need a more recent Fortran compiler since the code uses scalar integer components of derived type coarrays together with atomic subroutines (atomic_define, atomic_ref) (ifort 15 did not support this, while ifort 17 as well as gfortran7/OpenCoarrays do support it).<br />
Compared to the original src version, the source code of the new src_atomic_version contains only very few changes which are marked with '160904' and '160907' comments (time stamps).
