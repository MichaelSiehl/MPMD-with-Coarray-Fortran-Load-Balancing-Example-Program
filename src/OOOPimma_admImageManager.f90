! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

MODULE OOOPimma_admImageManager
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimma
!********************************************************
! Abstract Data Type (ADT):         OOOPimma_adtImageManager
! Abstract Data Type Module (adm):  OOOPimma_admImageManager.f90
!********************************************************
! Purpose:                    ImageManager-Object
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
USE OOOPimmc_admImageManager_CA
USE OOOPtema_admTeamManager
USE OOOPtmem_admTeamMember
USE OOOPinma_admInitialManager
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
PUBLIC :: OOOPimma_StructureConstructor
!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
PUBLIC :: OOOPimma_Start
PRIVATE :: IIimma_SYNC_CheckActivityFlag ! synchronization routine
!___________________________________________________________
!
!********************
!** Error Handling: *
!********************
PRIVATE :: IIimma_ErrorHandler
!___________________________________________________________
!
!*********************
!**  Enumerations:  **
!*********************

!___________________________________________________________
!
!********************************************************
!*** Abstract Data Type Declaration: ********************
!********************************************************
TYPE, PUBLIC :: OOOPimma_adtImageManager
  PRIVATE
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error-Collection
  !
END TYPE OOOPimma_adtImageManager
!__________________________________________________________
!
!****************************************************
!***  Corresponding Local Object Declaration:  ******
!****************************************************
!***
TYPE (OOOPimma_adtImageManager), PUBLIC, SAVE :: OOOPimmaImageManager_1
!
!___________________________________________________________




CONTAINS


!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************





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

SUBROUTINE OOOPimma_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPimma_adtImageManager), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPimma_StructureConstructor")
  !
  ! initialize something here
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimma_StructureConstructor
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________
!
SUBROUTINE OOOPimma_Start (Object)
  TYPE (OOOPimma_adtImageManager), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPimma_Start")
  !
  CALL OOOPimma_StructureConstructor (Object)
  SYNC ALL
  !
  IF (THIS_IMAGE() == 1) THEN
    ! on image 1 only: check if the number of images is not less 4,
    ! as a minimum requirement:
    IF (NUM_IMAGES() .LT. 4) THEN
      WRITE(*,*) ' **************************************'
      WRITE(*,*) ' ** Total number of images is to small: ', NUM_IMAGES(), '. '
      WRITE(*,*) ' ** Total number of images must be greater'
      WRITE(*,*) ' ** than 3 for the program to execute !'
      WRITE(*,*) ' **** program execution stopped *******'
      WRITE(*,*) ' **************************************'
      ERROR STOP ! STOP only, crashes the Linux terminal window
    END IF
    !
    SYNC IMAGES(*) ! all other images will wait for the executing image (image 1)
                    ! to reach this (but do not wait for each other)
  ELSE
    SYNC IMAGES(1) ! image 1 will wait for each of the other (executing) images to reach this
  END IF
  !********
  IF (THIS_IMAGE() .GT. 1) THEN
    CALL IIimma_SYNC_CheckActivityFlag (Object) ! synchronization routine
  ELSE
    CALL OOOPinma_Start (OOOPinmaInitialManager_1) ! start the InitialManager on image 1
    ! 160414:
    CALL IIimma_SYNC_CheckActivityFlag (Object) ! synchronization routine, will finish execution on image 1
                                                ! due to the call of subroutine IIinma_FinishExecution earlier
                                                ! in subroutine OOOPinma_Start
  END IF
    !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPimma_Start
!___________________________________________________________
!
SUBROUTINE IIimma_SYNC_CheckActivityFlag (Object)
  !!! synchronization routine !!!!!
  !!! synchronization counterpart routines are:  !!!!
  !!! IIinma_ActivateTeamManagerImage  !!!!
  !!! IItema_ActivateTeamMemberImage   !!!!
  !
  USE OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  TYPE (OOOPimma_adtImageManager), INTENT (IN) :: Object
  INTEGER(OOOGglob_kint) :: intImageActivityFlag
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: chrTeamMembersFileName
  !
                                                                CALL OOOGglob_subSetProcedures ("IIimma_SYNC_CheckActivityFlag")
  !
  !
  DO ! check the ActivityFlag of the OOOPimscImageStatus_CA standalone coarray wrapper
     ! in local PGAS memory permanently until it is not InitialWaiting any more:
    CALL OOOPimscG_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, intImageActivityFlag, THIS_IMAGE())
    IF (intImageActivityFlag /= OOOPimscEnum_ImageActivityFlag % InitialWaiting) THEN
      EXIT
    END IF
  END DO
  !
  !
  DO ! 160414: check the ImageActivityFlag in local PGAS memory permanently until it has
     !         value OOOPimscEnum_ImageActivityFlag % ExecutionFinished
    ! 160414:
    CALL OOOPimscG_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, intImageActivityFlag, THIS_IMAGE())
    !
    IF (intImageActivityFlag == OOOPimscEnum_ImageActivityFlag % TeamManager) THEN   ! start a TeamManager
      CALL OOOPimmcG_chrTeamMembersFileName_CA (OOOPimmcImageManager_CA_1, chrTeamMembersFileName, THIS_IMAGE())
      CALL OOOPtema_Start (OOOPtemaTeamManager_1, chrTeamMembersFileName)
    END IF
    !
    IF (intImageActivityFlag == OOOPimscEnum_ImageActivityFlag % TeamMember) THEN   ! start a TeamMember
      CALL OOOPtmem_Start (OOOPtmemTeamMember_1)
    END IF
    ! 160414: finish image execution:
    IF (intImageActivityFlag == OOOPimscEnum_ImageActivityFlag % ExecutionFinished) THEN
      write(*,*) 'Execution finished on image', THIS_IMAGE()
      EXIT
    END IF
    !
  END DO ! 160414
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE IIimma_SYNC_CheckActivityFlag

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
SUBROUTINE IIimma_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  TYPE(OOOPimma_adtImageManager), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IIimma_ErrorHandler
!__________________________________________________________






END MODULE OOOPimma_admImageManager
