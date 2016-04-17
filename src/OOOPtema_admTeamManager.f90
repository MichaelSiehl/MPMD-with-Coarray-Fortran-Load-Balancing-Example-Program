! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

MODULE OOOPtema_admTeamManager
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPtema
!********************************************************
! Abstract Data Type (ADT):         OOOPtema_adtTeamManager
! Abstract Data Type Module (adm):  OOOPtema_admTeamManager.f90
!********************************************************
! Purpose:                    TeamManager-Object
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
USE OOOPtemc_admTeamManager_CA
USE OOOPimsc_admImageStatus_CA ! communicate with remote or local PGAS memory
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
PUBLIC :: OOOPtemaS_chrTeamMembersFileName, OOOPtemaG_chrTeamMembersFileName
PUBLIC :: OOOPtemaS_intNumberOfTeamMembers, OOOPtemaG_intNumberOfTeamMembers
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
PUBLIC :: OOOPtema_StructureConstructor
!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
PUBLIC :: OOOPtema_Start
PRIVATE :: IItema_LoadTeamMembers
PRIVATE :: IItema_ActivateTeamMemberImage
!___________________________________________________________
!
!*********************
!** Error Handling: **
!*********************
PRIVATE :: IItema_ErrorHandler
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
TYPE, PUBLIC :: OOOPtema_adtTeamManager
  PRIVATE
  !*****
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40) :: m_chrTeamMembersFileName = ""
  !****
  INTEGER(OOOGglob_kint) :: m_intNumberOfTeamMembers = 0
  !*****
  INTEGER(OOOGglob_kint), DIMENSION (1:OOOGglob_TeamMembers_UpperBound) :: mA_intTeamMemberImages99
  !*****
  TYPE (OOOEerroc_colError) :: m_UUerrocError ! error-Collection
  !
END TYPE OOOPtema_adtTeamManager
!___________________________________________________________
!
!****************************************************
!***  Corresponding Local Object Declaration:  ******
!****************************************************
!***
TYPE (OOOPtema_adtTeamManager), PUBLIC, SAVE :: OOOPtemaTeamManager_1
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
!
SUBROUTINE OOOPtemaS_chrTeamMembersFileName (Object, chrTeamMembersFileName)
  TYPE (OOOPtema_adtTeamManager), INTENT (INOUT) :: Object
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), INTENT (IN) :: chrTeamMembersFileName
                                                                CALL OOOGglob_subSetProcedures ("OOOPtemaS_chrTeamMembersFileName")
  Object % m_chrTeamMembersFileName = chrTeamMembersFileName
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemaS_chrTeamMembersFileName
!**********
SUBROUTINE OOOPtemaG_chrTeamMembersFileName (Object, chrTeamMembersFileName)
  TYPE (OOOPtema_adtTeamManager), INTENT (IN) :: Object
  CHARACTER(KIND=OOOGglob_kcha,LEN=*), INTENT (OUT) :: chrTeamMembersFileName
                                                                CALL OOOGglob_subSetProcedures ("OOOPtemaG_chrTeamMembersFileName")
  chrTeamMembersFileName = Object % m_chrTeamMembersFileName
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemaG_chrTeamMembersFileName
!__________________________________________________________
!
SUBROUTINE OOOPtemaS_intNumberOfTeamMembers (Object, intNumberOfTeamMembers)
  TYPE (OOOPtema_adtTeamManager), INTENT (INOUT) :: Object
  INTEGER(OOOGglob_kint), INTENT (IN) :: intNumberOfTeamMembers
  INTEGER(OOOGglob_kint) :: Status = 0 ! error status
                                                                CALL OOOGglob_subSetProcedures ("OOOPtemaS_intNumberOfTeamMembers")
                                                                !
                                                                IF (intNumberOfTeamMembers > OOOGglob_TeamMembers_UpperBound) THEN
                                                                  CALL IItema_ErrorHandler (Object, "to many elements", &
                                                                    OOOGglob_error, Status)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  Object % m_intNumberOfTeamMembers = intNumberOfTeamMembers
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemaS_intNumberOfTeamMembers
!**********
SUBROUTINE OOOPtemaG_intNumberOfTeamMembers (Object, intNumberOfTeamMembers)
  TYPE (OOOPtema_adtTeamManager), INTENT (IN) :: Object
  INTEGER(OOOGglob_kint), INTENT (OUT) :: intNumberOfTeamMembers
                                                                CALL OOOGglob_subSetProcedures ("OOOPtemaG_intNumberOfTeamMembers")
  intNumberOfTeamMembers = Object % m_intNumberOfTeamMembers
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtemaG_intNumberOfTeamMembers
!__________________________________________________________





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
SUBROUTINE OOOPtema_StructureConstructor (Object)
  ! structure constructor
  TYPE (OOOPtema_adtTeamManager), INTENT (INOUT) :: Object
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPtema_StructureConstructor")
  ! initialize something here
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtema_StructureConstructor
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________

SUBROUTINE OOOPtema_Start (Object, chrTeamMembersFileName)
  USE OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  TYPE (OOOPtema_adtTeamManager), INTENT (INOUT) :: Object
  CHARACTER(KIND=OOOGglob_kcha,LEN=OOOGglob_Len40), INTENT (IN) :: chrTeamMembersFileName
  INTEGER(OOOGglob_kint) :: intTeamMemberImageNumber ! 160414
  INTEGER(OOOGglob_kint) :: intCounter = 0
  !
                                                                CALL OOOGglob_subSetProcedures ("OOOPtema_Start")
  !
  CALL OOOPtema_StructureConstructor (Object)
  !
write(*,*) 'TeamManager started on Image: ', THIS_IMAGE()
  !
  ! 160414:
  ! we do an early setting of the ImageActivityFlag at this place
  ! to allow supersession of the ExecutionFinished value later on:
  CALL OOOPimscS_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                                                      ExecutionFinished, THIS_IMAGE())
  !
  CALL OOOPtemaS_chrTeamMembersFileName (Object, chrTeamMembersFileName)
  CALL IItema_LoadTeamMembers (Object) ! from TeamMembers.txt
  !
  ! activate the TeamMembers on their images as given by TeamMembers_x.txt:
  DO intCounter = 1, Object % m_intNumberOfTeamMembers
    ! (mA_intTeamMemberImages99(intCounter) gives the (remote) image number of the TeamMember): 160414
    intTeamMemberImageNumber = Object % mA_intTeamMemberImages99(intCounter) ! 160414
    CALL IItema_ActivateTeamMemberImage (Object, intTeamMemberImageNumber) ! 160414
  END DO
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE OOOPtema_Start
!___________________________________________________________
!
SUBROUTINE IItema_LoadTeamMembers (Object)
  ! method, loads the data from TeamMembers_x.txt
  TYPE (OOOPtema_adtTeamManager), INTENT (INOUT) :: Object
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
  INTEGER(OOOGglob_kint) :: intNumberOfTeamMembers
  !
                                                                CALL OOOGglob_subSetProcedures ("IItema_LoadTeamMembers")
  !
  FileUnit = OOOGglob_FileUnitA
  !
  ! get the path to the files directory:
  CALL OOOPstpa_LoadPath (UUStartPath1)
  CALL OOOPstpaG_chrPath (UUStartPath1, chrStartPath)
  !
  chrPathAndFileName = TRIM(chrStartPath) // Object % m_chrTeamMembersFileName
  !
  OPEN (UNIT=FileUnit, IOSTAT=FileStatus, FILE=TRIM(chrPathAndFileName), &
      STATUS=TRIM(OpenStatus), ACCESS=TRIM(OpenAccess), FORM=TRIM(OpenForm), &
      POSITION=TRIM(OpenPosition), ACTION=TRIM(OpenAction), &
      BLANK=TRIM(OpenBlank), DELIM='APOSTROPHE')
                                                                !
                                                                IF (FileStatus /= 0) THEN
                                                                  CALL IItema_ErrorHandler (Object, "File-Open-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  READ (UNIT=FileUnit, FMT=*, IOSTAT=FileStatus) intNumberOfTeamMembers
                                                                !
                                                                IF (FileStatus /= 0) THEN
                                                                  CALL IItema_ErrorHandler (Object, "File READ-Error 1", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  CALL OOOPtemaS_intNumberOfTeamMembers (Object, intNumberOfTeamMembers) ! contains error handling
  !
  ! read the TeamMember image numbers from file:
  READ (UNIT=FileUnit, FMT=*, IOSTAT=FileStatus) Object % mA_intTeamMemberImages99 (1 : Object % m_intNumberOfTeamMembers)
  !
                                                                IF (FileStatus /= 0) THEN
                                                                  ! FileStatus error
                                                                  CALL IItema_ErrorHandler (Object, "File READ-Error 2", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
  CLOSE (UNIT=FileUnit, IOSTAT=FileStatus, STATUS='KEEP')
                                                                IF (FileStatus /= 0) THEN
                                                                  CALL IItema_ErrorHandler (Object, "File-Close-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  CALL OOOGglob_subResetProcedures
                                                                  RETURN
                                                                END IF
  !
                                                                CALL OOOGglob_subResetProcedures
  !
END SUBROUTINE IItema_LoadTeamMembers
!___________________________________________________________
!
SUBROUTINE IItema_ActivateTeamMemberImage (Object, intTeamMemberImageNumber) ! 160414
  !!!  synchronization counterpart routine  !!!!
  !!!  for IIimma_SYNC_CheckActivityFlag    !!!!
  ! starts a single TeamMember on its image
  !
  USE OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  TYPE (OOOPtema_adtTeamManager), INTENT (INOUT) :: Object
  INTEGER(OOOGglob_kint), INTENT(IN) :: intTeamMemberImageNumber ! 160414
  !
                                                                CALL OOOGglob_subSetProcedures ("IItema_ActivateTeamMemberImage")
  ! communicate with remote or local PGAS memory to activate a TeamMember image: 160414
  CALL OOOPimscS_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % TeamMember, &
                                                         intTeamMemberImageNumber) ! 160414
  !
                                                                CALL OOOGglob_subResetProcedures
END SUBROUTINE IItema_ActivateTeamMemberImage
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
SUBROUTINE IItema_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  TYPE(OOOPtema_adtTeamManager), INTENT(INOUT) :: Object
  CHARACTER(KIND=1, LEN=*), INTENT(IN) :: chrErrorDescription
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorType ! 1=warning, 2=Severe System error
  INTEGER(OOOGglob_kint), INTENT(IN) :: intErrorNumber ! Run Time error Number (e.g. Status)
  CALL OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
END SUBROUTINE IItema_ErrorHandler
!__________________________________________________________





END MODULE OOOPtema_admTeamManager
