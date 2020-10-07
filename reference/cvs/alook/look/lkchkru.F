      SUBROUTINE LKCHKRU (FSEQ,NRUN,NOLD,IRET)
C -------------------------------------------------------------
CKEY LOOK CHECK RUN
C - F.Ranjard - 920121
C! check if run # NRUN is on the current file
C - Input    : FSEQ   / L  = .TRUE. if runs in increasing order
C              NRUN   / I  = new run number
C              NOLD   / I  = previous run number
C - Output   : IRET   / I  = return code
C                            = 17  run is present
C                            = 0   run is not present
C -------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
#include "lkunpk.h"
      CHARACTER TAPOLD*8, CHAINT*4
      LOGICAL FSEQ
      DATA NARUNL /0/
C -----------------------------------------------------------
      IF (NARUNL.EQ.0) NARUNL = NAMIND('RUNL')
      IRET = 0
      JRUNL = IW(NARUNL)
      IF (JRUNL.NE.0) THEN
         TAPOLD(1:4) = CHAINT(IW(JRUNL+1))
         TAPOLD(5:8) = CHAINT(IW(JRUNL+2))
         IF (INDEX(SEQNAM,TAPOLD(1:6)).EQ.0) GOTO 3
         DO 1 I=1,IW(JRUNL+3)
            IF (NRUN.EQ.IW(JRUNL+3+I)) GOTO 2
 1       CONTINUE
         GOTO 3
 2       CONTINUE
      ENDIF
      IRET = 17
      IF (FSEQ .AND. NRUN.LT.NOLD) THEN
         KSRUN = NBANK('SRUN',0,1)
         IW(KSRUN+1) = NRUN
         IF (LINSEL.NE.0) THEN
            CALL LKRWND('/EDIR')
         ELSE
            CALL LKRWND('/DATA')
         ENDIF
      ELSE
         IEVT=0
 10      CALL LKGTEVT(FSEQ,'E',ULIST,NRUN,IEVT,IER)
         IF(IER.EQ.4 ) THEN
C         EOF on EDIR : try to read a new FILM card if any
              CALL LKOPRD1 ('    ', '    ',LER)
              IF (LER.NE.0) THEN
                SEQFIL = .FALSE.
                WRITE(LOUT,*) ' [no more input file]'
                GOTO 3
              ELSE
                CALL ABRUEV (KRUN,KEVT)
                IF(NRUN.EQ.0.AND.IEVT.EQ.0) GOTO 3
                IF(IEVT.EQ.0) THEN
                  IF(NRUN.NE.KRUN) GOTO 10
                ELSE
                  IF(IEVT.NE.KEVT) GOTO 10
                ENDIF
                GOTO 3
              ENDIF
         ELSE IF(IER.EQ.2) THEN
           WRITE(LOUT,*) ' [End of Run]'
         ELSE IF(IER.EQ.7) THEN
           WRITE(LOUT,*) ' [End of Selected data] '
         ELSE IF(IER.EQ.8) THEN
           WRITE(LOUT,*) ' [Event not found]'
         ENDIF
C
         CALL LKSHOW (IER)
      ENDIF
      KSRUN = NDROP('SRUN',0)
C
 3    CONTINUE
      END
#endif
