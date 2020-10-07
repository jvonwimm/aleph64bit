      SUBROUTINE GETEVT(FSEQ,LIST,ULIST,NRUN,NEVT,IER )
C----------------------------------------------------------------------
C - F.Ranjard - 911015
CKEY ALPHARD READ EVENT / USER
C
C!  read selected event from sequential or direct access file
C   Only event records are returned, all others  are skipped.
C
C   Inputs    :
C               FSEQ     .TRUE. when runs are in increasing order
C               LIST     list of bank names
C               ULIST    character string to steer unpacking
C                        Definition see in AUNPCK.
C               NRUN     run number   ( =0 means ignore run number   )
C               NEVT     event number ( =0 means ignore event number )
C                        ( NRUN=NEVT=0 means read next event )
C
C
C   Outputs   : IER =   ABRREC return code
C                             (1=event, 2=run, 3=unkown)
C                             (>4 event/run not found or eof)
C                             look at ABRREC for more details.
C
C   Calls:      ABRREC,BDROP,BGARB
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      LOGICAL FSEQ
      CHARACTER*(*) LIST,ULIST
      DATA NASEVT,NASRUN,NAEVEH /3*0/
C----------------------------------------------------------------------

      IF(NAEVEH.EQ.0) THEN
        NAEVEH=NAMIND('EVEH')
        NASEVT=NAMIND('SEVT')
        NASRUN=NAMIND('SRUN')
      ENDIF

      IER=0

C                      SELECT EVENT
C                     WITH RUN AND EVENT #

      IF(NEVT.NE.0) THEN
        IF(IW(NASRUN).NE.0) CALL BDROP(IW,'SRUN')
        KSEVT=IW(NASEVT)
        IF(KSEVT.LE.0) KSEVT=NBANK('SEVT',0,2)
        IF (NRUN.LE.0) THEN
           KEVEH = IW(NAEVEH)
           IF (KEVEH.NE.0) NRUN = IW(KEVEH+2)
        ENDIF
        IW(KSEVT+1)=NRUN
        IW(KSEVT+2)=NEVT
        LASRUN = NRUN
        LASEVT = NEVT
      ENDIF
C                SELECT RUN ONLY
      IF(NEVT.EQ.0.AND.NRUN.NE.0) THEN
        IF(IW(NASEVT).NE.0) CALL BDROP(IW,'SEVT')
        KSRUN=IW(NASRUN)
        IF(KSRUN.LE.0) KSRUN=NBANK('SRUN',0,1)
        IW(KSRUN+1)=NRUN
        LASRUN = NRUN
        LASEVT = 999999
      ENDIF
C                NO SELECTION, READ NEXT EVENT
      IF(NEVT.EQ.0.AND.NRUN.EQ.0) THEN
        IF(IW(NASEVT).NE.0) CALL BDROP(IW,'SEVT')
        IF(IW(NASRUN).NE.0) CALL BDROP(IW,'SRUN')
        LASRUN = 999999
        LASEVT = 999999
      ENDIF
      IF (.NOT.FSEQ) LASRUN = 999999
      CALL ABSMAX (LASRUN,LASEVT)
C
  1   CALL ABRREC (LIST,ULIST,IRET)
C                accept event records only
      IF(IRET.GE.4) GOTO 999
      IF(IRET.EQ.2) THEN
C      run record : it has been put on C-list by ABRREC
C                   look forward to the event required if any
         GOTO 1
      ENDIF
      IF(IRET.EQ.3) THEN
C      unknown record: skip it if an event is required
        IF (NEVT.GT.0 .OR. NRUN.GT.0) GOTO 1
      ENDIF
C
  999 IER= IRET
      RETURN
      END
#endif
