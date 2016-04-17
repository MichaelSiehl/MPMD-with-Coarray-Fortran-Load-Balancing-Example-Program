! This file is part of:
! MPMD with Coarray Fortran (2008): an Example Program (I)
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_with_Coarray_Fortran_2008.pdf

MODULE OOOPtemc_admTeamManager_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPtemc
!********************************************************
! Abstract Data Type (ADT):         OOOPtemc_adtTeamManager_CA
! Abstract Data Type Module (adm):  OOOPtemc_admTeamManager_CA.f90
!********************************************************
! Purpose:                    TeamManager_CA-Object
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

!***
! local ADT management:
PUBLIC :: OOOPtemc_StructureConstructor
!__________________________________________________________
!
!*********************************
!****  Coarray ADT Routines:  ****
!*********************************
!***
! access routines for scalar
! and static array members:
PUBLIC :: OOOPtemcS_chrTeamMembersFileName_CA, OOOPtemcG_chrTeamMembersFileName_CA, &
          OOOPtemcCopyImgToImg_chrTeamMembersFileName_CA
PUBLIC :: OOOPtemcS_intNumberOfTeamMembers_CA, OOOPtemcG_intNumberOfTeamMembers_CA, &
          OOOPtemcCopyImgToImg_intNumberOfTeamMembers_CA
PUBLIC :: OOOPtemcSA_intTeamMemberImages99_CA, OOOPtemcGA_intTeamMemberImages99_CA, &
          OOOPtemcCopyAImgToImg_intTeamMemberImages99_CA
!***
! access routines for
! dynamic array members:

!***
! coarray ADT management:
PUBLIC :: OOOPtemcDC_CopyCoarrayObjImgToImg_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
PRIVATE :: IItemc_ErrorHandler
!***
! coarray ADT:
PUBLIC :: IItemc_ImageNumberBoundError_CA
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
TYPE, PUBLIC :: OOOPtemc_adtTeamManager_CA
  PRIVATE
  !*****
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: m_chrTeamMembersFileName
  !*****
  INTEGER(OOOGglob_kint) :: m_intNumberOfTeamMembers = 0
  !*****
  INTEGER(OOOGglob_kint), DIMENSION (1:OOOGglob_TeamMembers_UpperBound) :: mA_intTeamMemberImages99
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
END TYPE OOOPtemc_adtTeamManager_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
TYPE (OOOPtemc_adtTeamManager_CA), PUBLIC, CODIMENSION[*], SAVE :: OOOPtemcTeamManager_CA_1
!___________________________________________________________




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



!
SUBROUTINE OOOPtemc_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPtemc_adtTeamManager_CA), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPtemc_StructureConstructor")
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemc_StructureConstructor
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
!__________________________________________________________
!
SUBROUTINE OOOPtemcS_chrTeamMembersFileName_CA (Object_CA, chrTeamMembersFileName, intImageNumber)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), INTENT (IN) :: chrTeamMembersFileName
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPtemcS_chrTeamMembersFileName_CA")
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  Object_CA[intImageNumber] % m_chrTeamMembersFileName = chrTeamMembersFileName
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcS_chrTeamMembersFileName_CA
!**********
SUBROUTINE OOOPtemcG_chrTeamMembersFileName_CA (Object_CA, chrTeamMembersFileName, intImageNumber)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), INTENT (OUT) :: chrTeamMembersFileName
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPtemcG_chrTeamMembersFileName_CA")
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  chrTeamMembersFileName = Object_CA[intImageNumber] % m_chrTeamMembersFileName
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcG_chrTeamMembersFileName_CA
!**********
SUBROUTINE OOOPtemcCopyImgToImg_chrTeamMembersFileName_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPtemcCopyImgToImg_chrTeamMembersFileName_CA")
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  ! copy the array from image to image
  Object_CA[intImageNumberToCopyTo] % m_chrTeamMembersFileName &
   = Object_CA[intImageNumberToCopyFrom] % m_chrTeamMembersFileName
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcCopyImgToImg_chrTeamMembersFileName_CA
!__________________________________________________________
!
SUBROUTINE OOOPtemcS_intNumberOfTeamMembers_CA (Object_CA, intNumberOfTeamMembers, &
                                                   intImageNumber)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intNumberOfTeamMembers
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPtemcS_intNumberOfTeamMembers_CA")
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (intNumberOfTeamMembers > OOOGglob_TeamMembers_UpperBound) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, "to many elements", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  Object_CA[intImageNumber] % m_intNumberOfTeamMembers = intNumberOfTeamMembers
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcS_intNumberOfTeamMembers_CA
!**********
SUBROUTINE OOOPtemcG_intNumberOfTeamMembers_CA (Object_CA, intNumberOfTeamMembers, &
                                                intImageNumber)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (OUT) :: intNumberOfTeamMembers
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPtemcG_intNumberOfTeamMembers_CA")
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  intNumberOfTeamMembers = Object_CA[intImageNumber] % m_intNumberOfTeamMembers
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcG_intNumberOfTeamMembers_CA
!**********
SUBROUTINE OOOPtemcCopyImgToImg_intNumberOfTeamMembers_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPtemcCopyImgToImg_intNumberOfTeamMembers_CA")
  !
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  Object_CA[intImageNumberToCopyTo] % m_intNumberOfTeamMembers = Object_CA[intImageNumberToCopyFrom] % m_intNumberOfTeamMembers
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcCopyImgToImg_intNumberOfTeamMembers_CA
!___________
!
SUBROUTINE OOOPtemcSA_intTeamMemberImages99_CA (Object_CA, intTeamMemberImages99)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), DIMENSION(1:OOOGglob_TeamMembers_UpperBound), INTENT (IN) :: intTeamMemberImages99
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPtemcSA_intTeamMemberImages99_CA")
  Object_CA % mA_intTeamMemberImages99 = intTeamMemberImages99
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcSA_intTeamMemberImages99_CA
!**********
SUBROUTINE OOOPtemcGA_intTeamMemberImages99_CA (Object_CA, intTeamMemberImages99)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), DIMENSION(1:OOOGglob_TeamMembers_UpperBound), INTENT (OUT) :: intTeamMemberImages99
                                                                CALL OOOGglob_subSetProcedures &
                                                                ("OOOPtemcGA_intTeamMemberImages99_CA")
  intTeamMemberImages99 = Object_CA % mA_intTeamMemberImages99
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcGA_intTeamMemberImages99_CA
!**********
SUBROUTINE OOOPtemcCopyAImgToImg_intTeamMemberImages99_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPtemcCopyAImgToImg_intTeamMemberImages99_CA")
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  ! copy only the required part of the array:
  Object_CA[intImageNumberToCopyTo] % mA_intTeamMemberImages99 (1 : Object_CA % m_intNumberOfTeamMembers) &
   = Object_CA[intImageNumberToCopyFrom] % mA_intTeamMemberImages99 (1 : Object_CA % m_intNumberOfTeamMembers)
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcCopyAImgToImg_intTeamMemberImages99_CA
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
SUBROUTINE OOOPtemcDC_CopyCoarrayObjImgToImg_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  ! copy from any (remote) image to any other (remote) image
  ! (static members only)
  TYPE (OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
  !******************************************
                                                                CALL OOOGglob_subSetProcedures  &
                                                                ("OOOPtemcDC_CopyCoarrayObjImgToImg_CA")
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IItemc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  !*******************************************
  ! copy the properties:
  !****************
  CALL OOOPtemcCopyImgToImg_chrTeamMembersFileName_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  CALL OOOPtemcCopyImgToImg_intNumberOfTeamMembers_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  CALL OOOPtemcCopyAImgToImg_intTeamMemberImages99_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  ! if necessary further properties must be copied here
  !*******************************************
 !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemcDC_CopyCoarrayObjImgToImg_CA
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
SUBROUTINE IItemc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  TYPE(OOOPtemc_adtTeamManager_CA), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IItemc_ErrorHandler
!__________________________________________________________
!
!Private
LOGICAL(OOOGglob_klog) FUNCTION IItemc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  TYPE(OOOPtemc_adtTeamManager_CA), CODIMENSION[*], INTENT(INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT(IN) :: intImageNumber
  !
  IItemc_ImageNumberBoundError_CA = .FALSE.
  !
  IF (intImageNumber .LT. 1) THEN ! image number is too small
    IItemc_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
  IF (intImageNumber .GT. NUM_IMAGES()) THEN ! image number is too large
    IItemc_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
END FUNCTION IItemc_ImageNumberBoundError_CA
!__________________________________________________________

END MODULE OOOPtemc_admTeamManager_CA
