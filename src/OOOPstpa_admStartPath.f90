! This file is part of:
! MPMD with Coarray Fortran (2008): an Example Program (I)
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_with_Coarray_Fortran_2008.pdf

MODULE OOOPstpa_admStartPath
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPstpa
!********************************************************
! Abstract Data Type (ADT):         OOOPstpa_adtStartPath
! Abstract Data Type Module (adm):  OOOPstpa_admStartPath.f90
!********************************************************
! Purpose:                    StartPath-Object
! Language:                   mainly Fortran 95 with Fortran 2008 coarrays
! Programmer:                 Michael Siehl
! Date:                       January 2016
!********************************************************
! Naming Conventions:
!
!  for scalar members:
!                             m: ADT member
!                             S: property set, G: property get,
!                             CopyImgToImg: copy an ADT member image to image
!  for array members:
!                             A: array
!                             mA: ADT array member
!                             SA: set array property, GA: get array property,
!                             CopyAImgToImg: copy an ADT array member image to image
!
!  for elements of array members:
!                             SAElement: set only one array element property
!                             GAElement: get only one array element property
!                             CopyAElementImgToImg: copy only one element of an ADT array member image to image
!
!                             99: signals a static array member which has an upper array bound
!                                 larger than necessary; the upper bound is given by a global parameter
!
!  other naming conventions:
!                             _CA: coarray routine / coarray declaration
!                             _SYNC_: synchronization routine
!                             CopyCoarrayObjImgToImg: copy a coarray ADT object image to image
!
!                             DC: deep copy routine
!                             Enum: enumeration
!
!                             OO: public (outer) scope (the two leading namespace letters)
!                             II: private (inner) scope
!                             UU: sub-object
!********************************************************
!___________________________________________________________

USE OOOGglob_Globals
USE OOOEerro_admError
!
!___________________________________________________________

IMPLICIT NONE
!___________________________________________________________

PRIVATE
!___________________________________________________________
!
!*****************************
! access routines for scalar *
! and static array members:  *
!*****************************
PUBLIC :: OOOPstpaS_chrPath, OOOPstpaG_chrPath

!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
PUBLIC :: OOOPstpa_LoadPath

!___________________________________________________________
!
!*********************
!** Error Handling: **
!*********************
!
PRIVATE :: IIstpa_ErrorHandler

!___________________________________________________________
!
!********************************************************
!*** Abstract Data Type Declaration: ********************
!********************************************************
TYPE, PUBLIC :: OOOPstpa_adtStartPath
  PRIVATE
  !*****
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len200) :: m_chrPath = ""
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! Error-Collection
  !
END TYPE OOOPstpa_adtStartPath
!__________________________________________________________
!


CONTAINS




!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!___________________________________________________________

SUBROUTINE OOOPstpaS_chrPath (Object, chrPath)
  TYPE (OOOPstpa_adtStartPath), INTENT (INOUT) :: Object
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), INTENT (IN) :: chrPath
                                                                CALL OOOGglob_subSetProcedures ("OOOPstpaS_chrPath")
  Object % m_chrPath = chrPath
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPstpaS_chrPath
!**********
SUBROUTINE OOOPstpaG_chrPath (Object, chrPath)
  TYPE (OOOPstpa_adtStartPath), INTENT (IN) :: Object
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), INTENT (OUT) :: chrPath
                                                                CALL OOOGglob_subSetProcedures ("OOOPstpaG_chrPath")
  chrPath = Object % m_chrPath
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPstpaG_chrPath
!__________________________________________________________







!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________
!
SUBROUTINE OOOPstpa_LoadPath (Object)
  ! method, loads the start-Path from file
  TYPE (OOOPstpa_adtStartPath), INTENT (INOUT) :: Object
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenStatus = 'OLD'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenAccess  = 'SEQUENTIAL'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenForm = 'FORMATTED'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenPosition = 'REWIND'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenAction = 'READ'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenBlank = 'NULL'
  !
  ! for INQUIRE:
  LOGICAL(KIND=OOOGglob_klog) :: logExist
  INTEGER(OOOGglob_kint) :: intRecl = 0
  !
  INTEGER(OOOGglob_kint) :: FileStatus = 0 ! File-error-Status
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len200) :: chrPathAndFileName = ""
  INTEGER(OOOGglob_kint) :: FileUnit = 0
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPstpa_LoadPath")
  !
  chrPathAndFileName = 'start.txt' ! the file must be in the program directory
  FileUnit = OOOGglob_FileUnitA
  !
  ! check the existence of the file:
  INQUIRE (FILE=chrPathAndFileName, EXIST=logExist)
  IF (.NOT.logExist) THEN
    ! File does not exist
                                                                CALL IIstpa_ErrorHandler (Object, &
                                                                  "INQUIRE: File does not exist", &
                                                                  OOOGglob_warning, OOOGglob_NoErrNumber)
                                                                CALL OOOGglob_subResetProcedures
    Return
  END IF
  !
  OPEN (UNIT=FileUnit, IOSTAT=FileStatus, FILE=TRIM(chrPathAndFileName), &
      STATUS=TRIM(OpenStatus), ACCESS=TRIM(OpenAccess), FORM=TRIM(OpenForm), &
      POSITION=TRIM(OpenPosition), ACTION=TRIM(OpenAction), &
      BLANK=TRIM(OpenBlank), DELIM='APOSTROPHE')
                                                                IF (FileStatus .NE. 0) THEN
                                                                  CALL IIstpa_ErrorHandler (Object, "File-Open-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF

  !
  READ (UNIT=FileUnit, FMT=*, IOSTAT=FileStatus) Object % m_chrPath
                                                                IF (FileStatus .NE. 0) THEN
                                                                  CALL IIstpa_ErrorHandler (Object, "File-Read-error 1", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
 !
  CLOSE (UNIT=FileUnit, IOSTAT=FileStatus, STATUS='KEEP')
                                                                IF (FileStatus .NE. 0) THEN
                                                                  CALL IIstpa_ErrorHandler (Object, "File-Close-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
 !
                                                                CALL OOOGglob_subResetProcedures
 !
END SUBROUTINE OOOPstpa_LoadPath
!___________________________________________________________






!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!Private
SUBROUTINE IIstpa_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  TYPE(OOOPstpa_adtStartPath), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IIstpa_ErrorHandler
!__________________________________________________________



END MODULE OOOPstpa_admStartPath
