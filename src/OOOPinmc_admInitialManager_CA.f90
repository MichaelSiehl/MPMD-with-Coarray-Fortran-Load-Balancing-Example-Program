! This file is part of:
! MPMD with Coarray Fortran (2008): an Example Program (I)
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_with_Coarray_Fortran_2008.pdf

MODULE OOOPinmc_admInitialManager_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPinmc
!********************************************************
! Abstract Data Type (ADT):         OOOPinmc_adtInitialManager_CA
! Abstract Data Type Module (adm):  OOOPinmc_admInitialManager_CA.f90
!********************************************************
! Purpose:                    InitialManager_CA-Object
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
!___________________________________________________________

IMPLICIT NONE
!___________________________________________________________

PRIVATE
!___________________________________________________________
!
!*******************************
!****  Local ADT Routines:  ****
!*******************************
!***
! access routines for scalar
! and static array members:

!***
! access routines for
! dynamic array members:
!
!***
! local ADT management:
PUBLIC :: OOOPinmc_StructureConstructor
!__________________________________________________________
!
!*********************************
!****  Coarray ADT Routines:  ****
!*********************************
!***
! access routines for scalar
! and static array members:
PUBLIC :: OOOPinmcS_intNumberOfTeamManagers_CA, OOOPinmcG_intNumberOfTeamManagers_CA, &
          OOOPinmcCopyImgToImg_intNumberOfTeamManagers_CA
PUBLIC :: OOOPinmcSA_intTeamManagerImages99_CA, OOOPinmcGA_intTeamManagerImages99_CA, &
          OOOPinmcCopyAImgToImg_intTeamManagerImages99_CA
PUBLIC :: OOOPinmcSA_chrTeamMembersFiles99_CA, OOOPinmcGA_chrTeamMembersFiles99_CA, &
          OOOPinmcCopyAImgToImg_chrTeamMembersFiles99_CA
!***
! access routines for
! dynamic array members:

!***
! coarray ADT management:
PUBLIC :: OOOPinmcDC_CopyCoarrayObjImgToImg_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
PRIVATE :: IIinmc_ErrorHandler
!***
! coarray ADT:
PRIVATE :: IIinmc_ImageNumberBoundError_CA
!___________________________________________________________
!
!************************
!****  Enumerations:  ***
!************************
!
!___________________________________________________________
!
!********************************************************
!***  Abstract Data Type Specification: *****************
!********************************************************
TYPE, PUBLIC :: OOOPinmc_adtInitialManager_CA
  PRIVATE
  !*****
  INTEGER(OOOGglob_kint) :: m_intNumberOfTeamManagers = 0
  !*****
  INTEGER(OOOGglob_kint), DIMENSION (1:OOOGglob_TeamManagers_UpperBound) :: mA_intTeamManagerImages99
  !*****
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40), DIMENSION (1:OOOGglob_TeamManagers_UpperBound) :: mA_chrTeamMembersFiles99
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
END TYPE OOOPinmc_adtInitialManager_CA
!__________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
TYPE (OOOPinmc_adtInitialManager_CA), PUBLIC, CODIMENSION[*], SAVE :: OOOPinmcInitialManager_CA_1
!
!__________________________________________________________





CONTAINS


!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Local ADT Routines: ***********************
!**********************************************************

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!__________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Local ADT Routines: ***********************
!**********************************************************

!***************************
! access routines for      *
! dynamic array members:   *
!***************************

!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Local ADT Routines: ***********************
!**********************************************************

!*************************
! local ADT management:  *
!*************************

!___________________________________________________________


!
SUBROUTINE OOOPinmc_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPinmc_adtInitialManager_CA), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPinmc_StructureConstructor")
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmc_StructureConstructor
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Coarray ADT Routines: *********************
!**********************************************************

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!___________________________________________________________
!
SUBROUTINE OOOPinmcS_intNumberOfTeamManagers_CA (Object_CA, intNumberOfTeamManagers, &
                                                   intImageNumber)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intNumberOfTeamManagers
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcS_intNumberOfTeamManagers_CA")
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (intNumberOfTeamManagers > OOOGglob_TeamManagers_UpperBound) &
                                                                THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, "to many elements", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  Object_CA[intImageNumber] % m_intNumberOfTeamManagers = intNumberOfTeamManagers
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcS_intNumberOfTeamManagers_CA
!**********
SUBROUTINE OOOPinmcG_intNumberOfTeamManagers_CA (Object_CA, intNumberOfTeamManagers, &
                                                intImageNumber)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (OUT) :: intNumberOfTeamManagers
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPinmcG_intNumberOfTeamManagers_CA")
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  intNumberOfTeamManagers = Object_CA[intImageNumber] % m_intNumberOfTeamManagers
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcG_intNumberOfTeamManagers_CA
!**********
SUBROUTINE OOOPinmcCopyImgToImg_intNumberOfTeamManagers_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPinmcCopyImgToImg_intNumberOfTeamManagers_CA")
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  Object_CA[intImageNumberToCopyTo] % m_intNumberOfTeamManagers = Object_CA[intImageNumberToCopyFrom] % m_intNumberOfTeamManagers
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcCopyImgToImg_intNumberOfTeamManagers_CA
!___________
!
SUBROUTINE OOOPinmcSA_intTeamManagerImages99_CA (Object_CA, intTeamManagerImages99)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), DIMENSION(1:OOOGglob_TeamManagers_UpperBound), INTENT (IN) :: intTeamManagerImages99
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcSA_intTeamManagerImages99_CA")
  Object_CA % mA_intTeamManagerImages99 = intTeamManagerImages99
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcSA_intTeamManagerImages99_CA
!**********
SUBROUTINE OOOPinmcGA_intTeamManagerImages99_CA (Object_CA, intTeamManagerImages99)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), DIMENSION(1:OOOGglob_TeamManagers_UpperBound), INTENT (OUT) :: intTeamManagerImages99
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcGA_intTeamManagerImages99_CA")
  intTeamManagerImages99 = Object_CA % mA_intTeamManagerImages99
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcGA_intTeamManagerImages99_CA
!**********
SUBROUTINE OOOPinmcCopyAImgToImg_intTeamManagerImages99_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPinmcCopyAImgToImg_intTeamManagerImages99_CA")
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  ! copy only the required part of the array:
                                                                !
                                                                IF (Object_CA % m_intNumberOfTeamManagers &
                                                                  .LT. 1) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid number of TeamManagers", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  Object_CA[intImageNumberToCopyTo] % mA_intTeamManagerImages99 (1 : Object_CA % m_intNumberOfTeamManagers) &
   = Object_CA[intImageNumberToCopyFrom] % mA_intTeamManagerImages99 (1 : Object_CA % m_intNumberOfTeamManagers)
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcCopyAImgToImg_intTeamManagerImages99_CA
!__________________________________________________________
!
SUBROUTINE OOOPinmcSA_chrTeamMembersFiles99_CA (Object_CA, chrTeamMembersFiles99)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), DIMENSION(1:OOOGglob_TeamManagers_UpperBound), INTENT (IN) :: chrTeamMembersFiles99
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcSA_chrTeamMembersFiles99_CA")
  Object_CA % mA_chrTeamMembersFiles99 = chrTeamMembersFiles99
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcSA_chrTeamMembersFiles99_CA
!**********
SUBROUTINE OOOPinmcGA_chrTeamMembersFiles99_CA (Object_CA, chrTeamMembersFiles99)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), DIMENSION(1:OOOGglob_TeamManagers_UpperBound), INTENT (OUT) :: chrTeamMembersFiles99
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcGA_chrTeamMembersFiles99_CA")
  chrTeamMembersFiles99 = Object_CA % mA_chrTeamMembersFiles99
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcGA_chrTeamMembersFiles99_CA
!**********
SUBROUTINE OOOPinmcCopyAImgToImg_chrTeamMembersFiles99_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPinmcCopyAImgToImg_chrTeamMembersFiles99_CA")
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  ! copy only the required part of the array:
                                                                !
                                                                IF (Object_CA % m_intNumberOfTeamManagers &
                                                                  .LT. 1) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid number of TeamManagers", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  Object_CA[intImageNumberToCopyTo] % mA_chrTeamMembersFiles99 (1 : Object_CA % m_intNumberOfTeamManagers) &
   = Object_CA[intImageNumberToCopyFrom] % mA_chrTeamMembersFiles99 (1 : Object_CA % m_intNumberOfTeamManagers)
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcCopyAImgToImg_chrTeamMembersFiles99_CA
!__________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Coarray ADT Routines:  ********************
!**********************************************************

!***************************
! access routines for      *
! dynamic array members:   *
!***************************

!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Coarray ADT Routines:  ********************
!**********************************************************

!**************************
! coarray ADT management: *
!**************************
!___________________________________________________________
!
SUBROUTINE OOOPinmcDC_CopyCoarrayObjImgToImg_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  ! copy from any (remote) image to any other (remote) image
  ! (static members only)
  TYPE (OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
  !******************************************
                                                                CALL OOOGglob_subSetProcedures  &
                                                                ("OOOPinmcDC_CopyCoarrayObjImgToImg_CA")
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                !
                                                                IF (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  !*******************************************
  ! copy the properties:
  !****************
  CALL OOOPinmcCopyImgToImg_intNumberOfTeamManagers_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  CALL OOOPinmcCopyAImgToImg_intTeamManagerImages99_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  CALL OOOPinmcCopyAImgToImg_chrTeamMembersFiles99_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  ! if necessary further properties must be copied here
  !*******************************************
 !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmcDC_CopyCoarrayObjImgToImg_CA
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
SUBROUTINE IIinmc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  TYPE(OOOPinmc_adtInitialManager_CA), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IIinmc_ErrorHandler
!__________________________________________________________
!
!Private
LOGICAL(OOOGglob_klog) FUNCTION IIinmc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  TYPE(OOOPinmc_adtInitialManager_CA), CODIMENSION[*], INTENT(INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT(IN) :: intImageNumber
  !
  IIinmc_ImageNumberBoundError_CA = .FALSE.
  !
  IF (intImageNumber .LT. 1) THEN ! image number is too small
    IIinmc_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
  IF (intImageNumber .GT. NUM_IMAGES()) THEN ! image number is too large
    IIinmc_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
END FUNCTION IIinmc_ImageNumberBoundError_CA
!__________________________________________________________

END MODULE OOOPinmc_admInitialManager_CA
