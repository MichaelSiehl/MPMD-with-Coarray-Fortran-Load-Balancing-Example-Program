! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

MODULE OOOPinma_admInitialManager
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPinma
!********************************************************
! Abstract Data Type (ADT):         OOOPinma_adtInitialManager
! Abstract Data Type Module (adm):  OOOPinma_admInitialManager.f90
!********************************************************
! Purpose:                    InitialManager-Object
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
USE OOOPstpa_admStartPath ! load the start path from file
!
USE OOOPinmc_admInitialManager_CA
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
PUBLIC :: OOOPinmaS_intNumberOfTeamManagers, OOOPinmaG_intNumberOfTeamManagers
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
PUBLIC :: OOOPinma_StructureConstructor
!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
PUBLIC :: OOOPinma_Start
PRIVATE :: IIinma_LoadTeamManagers, IIinma_ActivateTeamManagerImage
!___________________________________________________________
!
!*********************
!** Error Handling: **
!*********************
PRIVATE :: IIinma_ErrorHandler
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
TYPE, PUBLIC :: OOOPinma_adtInitialManager
  PRIVATE
  !****
  INTEGER(OOOGglob_kint) :: m_intNumberOfTeamManagers = 0
  !***** array containing the image numbers of the TeamManager images:
  INTEGER(OOOGglob_kint), DIMENSION (1:OOOGglob_TeamManagers_UpperBound) :: mA_intTeamManagerImages99
  !***** array containing the file names of the TeamMembers files for each TeamManager:
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40), DIMENSION (1:OOOGglob_TeamManagers_UpperBound) :: mA_chrTeamMembersFiles99
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error-Collection
  !
END TYPE OOOPinma_adtInitialManager
!__________________________________________________________
!
!****************************************************
!***  Corresponding Local Object Declaration:  ******
!****************************************************
!***
TYPE (OOOPinma_adtInitialManager), PUBLIC, SAVE :: OOOPinmaInitialManager_1
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
!__________________________________________________________
!
SUBROUTINE OOOPinmaS_intNumberOfTeamManagers (Object, intNumberOfTeamManagers)
  TYPE (OOOPinma_adtInitialManager), INTENT (INOUT) :: Object
  INTEGER(OOOGglob_kint), INTENT (IN) :: intNumberOfTeamManagers
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures ("OOOPinmaS_intNumberOfTeamManagers")
                                                                !
                                                                IF (intNumberOfTeamManagers > OOOGglob_TeamManagers_UpperBound) THEN
                                                                  CALL IIinma_ErrorHandler (Object, "to many elements", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  Object % m_intNumberOfTeamManagers = intNumberOfTeamManagers
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmaS_intNumberOfTeamManagers
!**********
SUBROUTINE OOOPinmaG_intNumberOfTeamManagers (Object, intNumberOfTeamManagers)
  TYPE (OOOPinma_adtInitialManager), INTENT (IN) :: Object
  INTEGER(OOOGglob_kint), INTENT (OUT) :: intNumberOfTeamManagers
                                                                CALL OOOGglob_subSetProcedures ("OOOPinmaG_intNumberOfTeamManagers")
  intNumberOfTeamManagers = Object % m_intNumberOfTeamManagers
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinmaG_intNumberOfTeamManagers
!__________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!****************************
! access routines for the   *
! coarray wrapper member:   *
!****************************

!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! ADT-Management:  *
!*******************
!___________________________________________________________

SUBROUTINE OOOPinma_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPinma_adtInitialManager), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPinma_StructureConstructor")
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinma_StructureConstructor
!___________________________________________________________






!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________

SUBROUTINE OOOPinma_Start (Object)
  USE OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  TYPE (OOOPinma_adtInitialManager), INTENT (INOUT) :: Object
  !
  INTEGER(OOOGglob_kint) :: intCounter = 0
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPinma_Start")
  !
  CALL OOOPinma_StructureConstructor (Object)
  !
  !*******
  CALL IIinma_LoadTeamManagers (Object) ! from TeamManagers.txt
  !
  ! activate the TeamManagers on the (remote) images as given by TeamManagers.txt:
  DO intCounter = 1, Object % m_intNumberOfTeamManagers
    ! (Object % mA_intTeamManagerImages99(intCounter) gives the (remote) image number)
    ! (Object % mA_chrTeamMembersFiles99(intCounter) gives the name of the file,
    !       containing the TeamManager's number of TeamMembers as well as the TeamMembers
    !       image numbers) :
    CALL IIinma_ActivateTeamManagerImage (Object, Object % mA_intTeamManagerImages99(intCounter), &
                                     Object % mA_chrTeamMembersFiles99(intCounter))
  END DO
  !
  !160414:
  ! communicate with local PGAS memory to finish image execution:
  CALL OOOPimscS_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                                                    ExecutionFinished, THIS_IMAGE())
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPinma_Start
!___________________________________________________________
!
SUBROUTINE IIinma_LoadTeamManagers (Object)
  ! method, loads the data from TeamManagers.txt
  TYPE (OOOPinma_adtInitialManager), INTENT (INOUT) :: Object
  !
  TYPE (OOOPstpa_adtStartPath) :: UUStartPath1
  !
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenStatus = 'OLD'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenAccess  = 'SEQUENTIAL'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenForm = 'FORMATTED'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenPosition = 'REWIND'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenAction = 'READ'
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: OpenBlank = 'NULL'
  !
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len200) :: chrStartPath
  !
  ! for INQUIRE:
  LOGICAL(KIND=OOOGglob_klog) :: logExist
  INTEGER(OOOGglob_kint) :: intRecl = 0
  !
  INTEGER(OOOGglob_kint) :: FileUnit = 0
  INTEGER(OOOGglob_kint) :: FileStatus = 0 ! File-error-Status
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len200) :: chrPathAndFileName = ""
  INTEGER(OOOGglob_kint) :: intCounter ! do loop counter
  INTEGER(OOOGglob_kint) :: intNumberOfTeamManagers
  !
                                                                CALL OOOGglob_subSetProcedures ("IIinma_LoadTeamManagers")
  !
  FileUnit = OOOGglob_FileUnitA
  !
  CALL OOOPstpa_LoadPath (UUStartPath1)
  CALL OOOPstpaG_chrPath (UUStartPath1, chrStartPath)
  !
  chrPathAndFileName = TRIM(chrStartPath) // 'TeamManagers.txt'
  !
  OPEN (UNIT=FileUnit, IOSTAT=FileStatus, FILE=TRIM(chrPathAndFileName), &
      STATUS=TRIM(OpenStatus), ACCESS=TRIM(OpenAccess), FORM=TRIM(OpenForm), &
      POSITION=TRIM(OpenPosition), ACTION=TRIM(OpenAction), &
      BLANK=TRIM(OpenBlank), DELIM='APOSTROPHE')
                                                                !
                                                                IF (FileStatus /= 0) THEN
                                                                  CALL IIinma_ErrorHandler (Object, "File Open-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  READ (UNIT=FileUnit, FMT=*, IOSTAT=FileStatus) intNumberOfTeamManagers
                                                                !
                                                                IF (FileStatus /= 0) THEN
                                                                  CALL IIinma_ErrorHandler (Object, "File READ-Error 1", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  CALL OOOPinmaS_intNumberOfTeamManagers (Object, intNumberOfTeamManagers) ! contains error handling
  !
  ! read the TeamManagerImages and there TeamMembers file name from the TeamManagers.txt file
  DO intCounter = 1, Object % m_intNumberOfTeamManagers
    READ (UNIT=FileUnit, FMT=*, IOSTAT=FileStatus) Object % mA_intTeamManagerImages99 (intCounter), &
      Object % mA_chrTeamMembersFiles99 (intCounter)
    !
                                                                IF (FileStatus /= 0) THEN
                                                                  ! FileStatus error
                                                                  CALL IIinma_ErrorHandler (Object, "File READ-Error 2", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  END DO
  !
  CLOSE (UNIT=FileUnit, IOSTAT=FileStatus, STATUS='KEEP')
                                                                IF (FileStatus /= 0) THEN
                                                                  CALL IIinma_ErrorHandler (Object, "File-Close-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
                                                                CALL OOOGglob_subResetProcedures
  !
END SUBROUTINE IIinma_LoadTeamManagers
!___________________________________________________________
!
SUBROUTINE IIinma_ActivateTeamManagerImage (Object, intTeamManagerImageNumber, chrTeamMembersFileName)
  !!!  synchronization counterpart routine  !!!!
  !!!  for IIimma_SYNC_CheckActivityFlag    !!!!
  ! activate a TeamManager on a remote image (as given by TeamManagers.txt)
  !
  USE OOOPimmc_admImageManager_CA ! access corresponding coarrays to
  USE OOOPimsc_admImageStatus_CA  ! communicate with remote or local PGAS memory
  !
  TYPE (OOOPinma_adtInitialManager), INTENT (IN) :: Object
  INTEGER(OOOGglob_kint), INTENT(IN) :: intTeamManagerImageNumber
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40), INTENT(IN) :: chrTeamMembersFileName
  !
                                                                CALL OOOGglob_subSetProcedures ("IIinma_ActivateTeamManagerImage")
  !
  ! firstly, set up the required data on the remote image:
  CALL OOOPimmcS_chrTeamMembersFileName_CA (OOOPimmcImageManager_CA_1, chrTeamMembersFileName, &
                                                   intTeamManagerImageNumber)
  ! set the activity flag on the remote image:
  CALL OOOPimsc_StructureConstructor_CA (OOOPimscImageStatus_CA_1)
  CALL OOOPimscS_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % TeamManager, &
                                          intTeamManagerImageNumber)
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE IIinma_ActivateTeamManagerImage
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________

!Private
SUBROUTINE IIinma_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  TYPE(OOOPinma_adtInitialManager), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IIinma_ErrorHandler
!__________________________________________________________



END MODULE OOOPinma_admInitialManager
