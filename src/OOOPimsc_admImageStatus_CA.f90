! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

MODULE OOOPimsc_admImageStatus_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimsc
!********************************************************
! Abstract Data Type (ADT):         OOOPimsc_adtImageStatus_CA
! Abstract Data Type Module (adm):  OOOPimsc_admImageStatus_CA.f90
!********************************************************
! Purpose:                    ImageStatus_CA-Object
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
PUBLIC :: OOOPimsc_StructureConstructor
!__________________________________________________________
!
!*********************************
!****  Coarray ADT Routines:  ****
!*********************************
!***
! access routines for scalar
! and static array members:
PUBLIC :: OOOPimscS_intImageActivityFlag_CA, OOOPimscG_intImageActivityFlag_CA, &
          OOOPimscCopyImgToImg_intImageActivityFlag_CA
!***
! access routines for
! dynamic array members:

!***
! coarray ADT management:
PUBLIC :: OOOPimscDC_CopyCoarrayObjImgToImg_CA
PUBLIC :: OOOPimsc_StructureConstructor_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
PRIVATE :: IIimsc_ErrorHandler
!***
! coarray ADT:
PRIVATE :: IIimsc_ImageNumberBoundError_CA
!___________________________________________________________
!
!************************
!****  Enumerations:  ***
!************************
!***  ImageActivityFlag:
TYPE, PUBLIC :: OOOPimsc_DontUse1
  INTEGER(KIND=OOOGglob_kint) :: InitialWaiting ! = 1
  INTEGER(KIND=OOOGglob_kint) :: TeamManager ! = 2
  INTEGER(KIND=OOOGglob_kint) :: TeamMember ! = 3
  ! 160414:
  INTEGER(KIND=OOOGglob_kint) :: ExecutionFinished ! = 4
END TYPE OOOPimsc_DontUse1
!
TYPE (OOOPimsc_DontUse1), PUBLIC, PARAMETER :: OOOPimscEnum_ImageActivityFlag &
                                           = OOOPimsc_DontUse1 (1,2,3,4)
!
!___________________________________________________________
!
!********************************************************
!***  Abstract Data Type Specification: *****************
!********************************************************
TYPE, PUBLIC :: OOOPimsc_adtImageStatus_CA
  PRIVATE
  !*****
  INTEGER(OOOGglob_kint) :: m_intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % InitialWaiting
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
END TYPE OOOPimsc_adtImageStatus_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
TYPE (OOOPimsc_adtImageStatus_CA), PUBLIC, CODIMENSION[*], SAVE :: OOOPimscImageStatus_CA_1
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
SUBROUTINE OOOPimsc_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPimsc_adtImageStatus_CA), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPimsc_StructureConstructor")
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimsc_StructureConstructor
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
SUBROUTINE OOOPimscS_intImageActivityFlag_CA (Object_CA, intImageActivityFlag, &
                                                   intImageNumber)
  TYPE (OOOPimsc_adtImageStatus_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageActivityFlag
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPimscS_intImageActivityFlag_CA")
                                                                !
                                                                IF (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  Object_CA[intImageNumber] % m_intImageActivityFlag = intImageActivityFlag
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimscS_intImageActivityFlag_CA
!**********
SUBROUTINE OOOPimscG_intImageActivityFlag_CA (Object_CA, intImageActivityFlag, &
                                                intImageNumber)
  TYPE (OOOPimsc_adtImageStatus_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (OUT) :: intImageActivityFlag
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumber
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPimscG_intImageActivityFlag_CA")
                                                                !
                                                                IF (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) THEN
                                                                  CALL IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  intImageActivityFlag = Object_CA[intImageNumber] % m_intImageActivityFlag
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimscG_intImageActivityFlag_CA
!**********
SUBROUTINE OOOPimscCopyImgToImg_intImageActivityFlag_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  TYPE (OOOPimsc_adtImageStatus_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures &
                                                                 ("OOOPimscCopyImgToImg_intImageActivityFlag_CA")
                                                                !
                                                                IF (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  !
  Object_CA[intImageNumberToCopyTo] % m_intImageActivityFlag = Object_CA[intImageNumberToCopyFrom] % m_intImageActivityFlag
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimscCopyImgToImg_intImageActivityFlag_CA
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
SUBROUTINE OOOPimscDC_CopyCoarrayObjImgToImg_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  ! copy from any (remote) image to any other (remote) image
  ! (static members only)
  TYPE (OOOPimsc_adtImageStatus_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyFrom
  INTEGER(OOOGglob_kint), INTENT (IN) :: intImageNumberToCopyTo
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
  !******************************************
                                                                CALL OOOGglob_subSetProcedures  &
                                                                ("OOOPimscDC_CopyCoarrayObjImgToImg_CA")
                                                                !
                                                                IF (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyFrom)) THEN
                                                                  CALL IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 1", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
                                                                IF (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumberToCopyTo)) THEN
                                                                  CALL IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number 2", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
                                                                !
  !*******************************************
  ! copy the properties:
  !****************
  CALL OOOPimscCopyImgToImg_intImageActivityFlag_CA (Object_CA, intImageNumberToCopyFrom, intImageNumberToCopyTo)
  !****************
  ! if necessary further properties must be copied here
  !*******************************************
 !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimscDC_CopyCoarrayObjImgToImg_CA
!___________________________________________________________
!
SUBROUTINE OOOPimsc_StructureConstructor_CA (Object_CA)
  TYPE (OOOPimsc_adtImageStatus_CA), CODIMENSION[*], INTENT (INOUT) :: Object_CA
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPimsc_StructureConstructor_CA")

  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimsc_StructureConstructor_CA
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
SUBROUTINE IIimsc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  TYPE(OOOPimsc_adtImageStatus_CA), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IIimsc_ErrorHandler
!__________________________________________________________
!
!Private
LOGICAL(OOOGglob_klog) FUNCTION IIimsc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  TYPE(OOOPimsc_adtImageStatus_CA), CODIMENSION[*], INTENT(INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT(IN) :: intImageNumber
  !
  IIimsc_ImageNumberBoundError_CA = .FALSE.
  !
  IF (intImageNumber .LT. 1) THEN ! image number is too small
    IIimsc_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
  IF (intImageNumber .GT. NUM_IMAGES()) THEN ! image number is too large
    IIimsc_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
END FUNCTION IIimsc_ImageNumberBoundError_CA
!__________________________________________________________

END MODULE OOOPimsc_admImageStatus_CA
