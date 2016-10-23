! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

MODULE Main_Sub
!
CONTAINS
!
!**********
!
SUBROUTINE Entry_Main_Sub
  !
  USE OOOPimma_admImageManager
  !
  IMPLICIT NONE
  !
  CALL OOOPimma_Start (OOOPimmaImageManager_1) ! start the ImageManager on all images
  !
END SUBROUTINE Entry_Main_Sub
!
!**********
!
END MODULE Main_Sub
