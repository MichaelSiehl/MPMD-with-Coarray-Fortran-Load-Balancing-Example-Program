! This file is part of:
! MPMD with Coarray Fortran (2008): an Example Program (I)
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_with_Coarray_Fortran_2008.pdf

MODULE OOOPtmec_admTeamMember_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPtmec
!********************************************************
! Abstract Data Type (ADT):         OOOPtmec_adtTeamMember_CA
! Abstract Data Type Module (adm):  OOOPtmec_admTeamMember_CA.f90
!********************************************************
! Purpose:                    TeamMember_CA-Object
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
PUBLIC :: OOOPtmec_StructureConstructor
!__________________________________________________________
!
!*********************************
!****  Coarray ADT Routines:  ****
!*********************************
!***
! access routines for scalar
! and static array members:

!***
! access routines for
! dynamic array members:

!***
! coarray ADT management:

!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
PRIVATE :: IItmec_ErrorHandler
!***
! coarray ADT:
PUBLIC :: IItmec_ImageNumberBoundError_CA
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
TYPE, PUBLIC :: OOOPtmec_adtTeamMember_CA
  PRIVATE
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
END TYPE OOOPtmec_adtTeamMember_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
TYPE (OOOPtmec_adtTeamMember_CA), PUBLIC, CODIMENSION[*], SAVE :: OOOPtmecTeamMember_CA_1
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
SUBROUTINE OOOPtmec_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPtmec_adtTeamMember_CA), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPtmec_StructureConstructor")
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtmec_StructureConstructor
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





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
SUBROUTINE IItmec_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  TYPE(OOOPtmec_adtTeamMember_CA), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IItmec_ErrorHandler
!__________________________________________________________
!
!Private
LOGICAL(OOOGglob_klog) FUNCTION IItmec_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  TYPE(OOOPtmec_adtTeamMember_CA), CODIMENSION[*], INTENT(INOUT) :: Object_CA
  INTEGER(OOOGglob_kint), INTENT(IN) :: intImageNumber
  !
  IItmec_ImageNumberBoundError_CA = .FALSE.
  !
  IF (intImageNumber .LT. 1) THEN ! image number is too small
    IItmec_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
  IF (intImageNumber .GT. NUM_IMAGES()) THEN ! image number is too large
    IItmec_ImageNumberBoundError_CA = .TRUE.
  END IF
  !
END FUNCTION IItmec_ImageNumberBoundError_CA
!__________________________________________________________

END MODULE OOOPtmec_admTeamMember_CA
