! This file is part of:
! MPMD with Coarray Fortran (2008): an Example Program (I)
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_with_Coarray_Fortran_2008.pdf

MODULE OOOEerro_admError

USE OOOGglob_Globals

IMPLICIT NONE
!___________________________________________________________
!
PRIVATE
!
PUBLIC :: OOOEerroc_AddObject
!___________________________________________________________
!
TYPE, PUBLIC :: OOOEerroc_colError
  PRIVATE
  !
END TYPE OOOEerroc_colError
!___________________________________________________________

CONTAINS
!
!___________________________________________________________
!
SUBROUTINE OOOEerroc_AddObject (Collection, chrErrorDescription, &
  intErrorType)
  TYPE (OOOEerroc_colError), INTENT (INOUT) :: Collection
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(KIND=OOOGglob_kint), INTENT(IN) :: intErrorType
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len200) :: chrSelection
  !
  !
Write(*,*) "CurrentProc: ", OOOGglob_chrCurrentProcedure
!
write(*,*) "ErrDescr: ", TRIM(chrErrorDescription)
!
write(*,*)  "ErrTyp: ", intErrorType
!
Write(*,*)
Write(*,*) "CallingProc: ", TRIM(OOOGglob_chrCallingProcedure)
!
Write(*,*) "ReturningProc: ", TRIM(OOOGglob_chrReturningProcedure)
!
PRINT *
Write(*,*) "     *** An error occured ! *** "
PRINT *, ' Please select: '
PRINT *
PRINT *, ' x  - Exit Program, or'
PRINT *, ' c  - Continue Execution'
PRINT *
!
PRINT *, ' -> '
READ *, chrSelection
PRINT *
chrSelection = TRIM(chrSelection)
SELECT CASE (chrSelection)
  CASE ('x', 'X')
  !
      ERROR STOP ! end of execution
  !
END SELECT
!
END SUBROUTINE OOOEerroc_AddObject
!___________________________________________________________
!
END MODULE OOOEerro_admError
