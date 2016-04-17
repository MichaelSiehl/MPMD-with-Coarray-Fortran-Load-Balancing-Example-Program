! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

MODULE OOOPtmem_admTeamMember
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPtmem
!********************************************************
! Abstract Data Type (ADT):         OOOPtmem_adtTeamMember
! Abstract Data Type Module (adm):  OOOPtmem_admTeamMember.f90
!********************************************************
! Purpose:                    TeamMember-Object
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

USE OOOGglob_Globals ! KIND-Values
USE OOOEerro_admError ! error-Collection
!
USE OOOPtmec_admTeamMember_CA
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
!

!**********************************
! access routines for dynamic     *
! array and derived type members: *
!**********************************

!
!****************************
! access routines for the   *
! coarray wrapper member:   *
!****************************

!
!*******************
! ADT-Management: **
!*******************
PUBLIC :: OOOPtmem_StructureConstructor
!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
PUBLIC :: OOOPtmem_Start
!___________________________________________________________
!
!*********************
!** Error Handling: **
!*********************
PRIVATE :: IItmem_ErrorHandler
!___________________________________________________________
!
!*********************
!**  Enumerations:  **
!*********************
!

!___________________________________________________________
!
!********************************************************
!*** Abstract Data Type Declaration: ********************
!********************************************************
TYPE, PUBLIC :: OOOPtmem_adtTeamMember
  PRIVATE
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error-Collection
  !
END TYPE OOOPtmem_adtTeamMember
!___________________________________________________________
!
!****************************************************
!***  Corresponding Local Object Declaration:  ******
!****************************************************
!***
TYPE (OOOPtmem_adtTeamMember), PUBLIC, SAVE :: OOOPtmemTeamMember_1
!___________________________________________________________




CONTAINS


!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!****************************
! access routines for the   *
! coarray wrapper member:   *
!****************************
!___________________________________________________________
!





!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! ADT-Management:  *
!*******************
!___________________________________________________________
!

SUBROUTINE OOOPtmem_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPtmem_adtTeamMember), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPtmem_StructureConstructor")
  ! initialize something here
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtmem_StructureConstructor
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________

SUBROUTINE OOOPtmem_Start (Object)
  USE OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  TYPE (OOOPtmem_adtTeamMember), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPtmem_Start")
  !
  CALL OOOPtmem_StructureConstructor (Object)
  !
write(*,*) 'TeamMember started on Image: ', THIS_IMAGE()
  !
  !160414:
  ! communicate with local PGAS memory to finish image execution:
  CALL OOOPimscS_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                                                      ExecutionFinished, THIS_IMAGE())
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtmem_Start
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
SUBROUTINE IItmem_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  TYPE(OOOPtmem_adtTeamMember), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IItmem_ErrorHandler
!__________________________________________________________





END MODULE OOOPtmem_admTeamMember
