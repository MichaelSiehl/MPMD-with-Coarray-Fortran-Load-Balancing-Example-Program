! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

MODULE OOOPimmc_admImageManager_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimmc
!********************************************************
! Abstract Data Type (ADT):         OOOPimmc_adtImageManager_CA
! Abstract Data Type Module (adm):  OOOPimmc_admImageManager_CA.f90
!********************************************************
! Purpose:                    ImageManager_CA-Object
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
USE OOOPimsc_admImageStatus_CA
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
PUBLIC :: OOOPimmc_StructureConstructor
!__________________________________________________________
!
!*********************************
!****  Coarray ADT Routines:  ****
!*********************************
!***
! access routines for scalar
! and static array members:
PUBLIC :: OOOPimmcS_intImageActivityFlag_CA, OOOPimmcG_intImageActivityFlag_CA, &
          OOOPimmcCopyImgToImg_intImageActivityFlag_CA
PUBLIC :: OOOPimmcS_chrTeamMembersFileName_CA, OOOPimmcG_chrTeamMembersFileName_CA, &
          OOOPimmcCopyImgToImg_chrTeamMembersFileName_CA
!***
! access routines for
! dynamic array members:

!***
! coarray ADT management:
PUBLIC :: OOOPimmcDC_CopyCoarrayObjImgToImg_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
PRIVATE :: IIimmc_ErrorHandler
!***
! coarray ADT:
PUBLIC :: IIimmc_ImageNumberBoundError_CA
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
TYPE, PUBLIC :: OOOPimmc_adtImageManager_CA
  PRIVATE
  !*****
  INTEGER(OOOGglob_kint) :: m_intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % InitialWaiting
  !*****
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: m_chrTeamMembersFileName
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
END TYPE OOOPimmc_adtImageManager_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
TYPE (OOOPimmc_adtImageManager_CA), PUBLIC, CODIMENSION[*], SAVE :: OOOPimmcImageManager_CA_1
!
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
SUBROUTINE OOOPimmc_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPimmc_adtImageManager_CA), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPimmc_StructureConstructor")
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimmc_StructureConstructor
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
!
SUBROUTINE OOOPimmcS_intImageActivityFlag_CA (Object_CA, intImageActivityFlag, &
                                                   intImageNumber)
  TYPE (OOOPimmc_adtImageManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageActivityFlag
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPimmcS_intImageActivityFlag_CA")
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  Object_CA[intImageNumber] % m_intImageActivityFlag = intImageActivityFlag
                                                                !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimmcS_intImageActivityFlag_CA
!**********
SUBROUTINE OOOPimmcG_intImageActivityFlag_CA (Object_CA, intImageActivityFlag, &
                                                intImageNumber)
  TYPE (OOOPimmc_adtImageManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (OUT) :: intImageActivityFlag
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPimmcG_intImageActivityFlag_CA")
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  intImageActivityFlag = Object_CA[intImageNumber] % m_intImageActivityFlag
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimmcG_intImageActivityFlag_CA
!**********
SUBROUTINE OOOPimmcCopyImgToImg_intImageActivityFlag_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPimmc_adtImageManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPimmcCopyImgToImg_intImageActivityFlag_CA")
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                  (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                  (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                               !
  Object_CA[intImageNumberToCopyTo] % m_intImageActivityFlag = Object_CA[intImageNumberToCopyFrom] % m_intImageActivityFlag
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimmcCopyImgToImg_intImageActivityFlag_CA
!__________________________________________________________
!
SUBROUTINE OOOPimmcS_chrTeamMembersFileName_CA (Object_CA, chr_TeamMembersFileName, intImageNumber)
  TYPE (OOOPimmc_adtImageManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), INTENT (IN) :: chr_TeamMembersFileName
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPimmcS_chrTeamMembersFileName_CA")
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  Object_CA[intImageNumber] % m_chrTeamMembersFileName = chr_TeamMembersFileName
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimmcS_chrTeamMembersFileName_CA
!**********
SUBROUTINE OOOPimmcG_chrTeamMembersFileName_CA (Object_CA, chr_TeamMembersFileName, intImageNumber)
  TYPE (OOOPimmc_adtImageManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), DIMENSION(1), INTENT (OUT) :: chr_TeamMembersFileName
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                  ("OOOPimmcG_chrTeamMembersFileName_CA")
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  chr_TeamMembersFileName = Object_CA[intImageNumber] % m_chrTeamMembersFileName
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimmcG_chrTeamMembersFileName_CA
!**********
SUBROUTINE OOOPimmcCopyImgToImg_chrTeamMembersFileName_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPimmc_adtImageManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPimmcCopyImgToImg_chrTeamMembersFileName_CA")
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                  (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                  (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                               !
  ! copy image to image
  Object_CA[intImageNumberToCopyTo] % m_chrTeamMembersFileName &
   = Object_CA[intImageNumberToCopyFrom] % m_chrTeamMembersFileName
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimmcCopyImgToImg_chrTeamMembersFileName_CA
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
SUBROUTINE OOOPimmcDC_CopyCoarrayObjImgToImg_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  ! copy from any (remote) image to any other (remote) image
  ! (static members only)
  TYPE (OOOPimmc_adtImageManager_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
  !******************************************
                                                                CALL OOOGglob_subSetProcedures  &
                                                                ("OOOPimmcDC_CopyCoarrayObjImgToImg_CA")
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                  (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IIimmc_ImageNumberBoundError_CA &
                                                                  (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIimmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                               !
  !*******************************************
  ! copy the properties:
  !****************
  CALL OOOPimmcCopyImgToImg_intImageActivityFlag_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  CALL OOOPimmcCopyImgToImg_chrTeamMembersFileName_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  ! if necessary further properties must be copied here
  !*******************************************
 !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimmcDC_CopyCoarrayObjImgToImg_CA
!___________________________________________________________
!





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
SUBROUTINE IIimmc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  TYPE(OOOPimmc_adtImageManager_CA), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IIimmc_ErrorHandler
!__________________________________________________________
!
!Private
LOGICAL(OOOGglob_klog) FUNCTION IIimmc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  TYPE(OOOPimmc_adtImageManager_CA), CODIMENSION[*], INTENT(INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT(IN) :: intImageNumber
  !
  IIimmc_ImageNumberBoundError_CA = .FALSE.
  !
  IF (intImageNumber .LT. 1) THEN ! image number is too small
    IIimmc_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
  !********
  !
  IF (intImageNumber .GT. NUM_IMAGES()) THEN ! image number is too large
    IIimmc_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
END FUNCTION IIimmc_ImageNumberBoundError_CA
!__________________________________________________________

END MODULE OOOPimmc_admImageManager_CA
