      SUBROUTINE ABSEVT (IRUN, IEVT, IRET)
C-----------------------------------------------------------------------
CKEY ALPHARD READ BOS
C  AUTHOR :      H. Albrecht            Nov 89
C
C!    Event selection according to SEVT, SRUN, IRUN, NEVT, TIME cards.
C
C  Called from ABRREC
C  Calls : none.
C
C  Input : IRUN    run,
C          IEVT    event number
C  Output:
C        IRET =  0   event to be skipped
C             =  1   event to be accepted
C             =  7   end of selected data (NEVT)
C             =  8   end of selected data (SEVT/SRUN)
C             =-IRUN skip the end of this run
C
C  900920 - modified by F.Ranjard
C  ENTRY ABSMAX (LASRUN,LASEVT) sets MAXRUN=LASRUN , MAXEVT=LASEVT
C  ENTRY ABSLIM (N1EVT ,NLEVT ) sets NNMIN = N1EVT , NNMAX = NLEVT
C  ENTRY ABONER (ONE)           sets ONERUN= ONE
C                               ONERUN is true when there only one
C                               run per file.
C                               default is false (several runs).
C                               necessary in interactive mode
C  910627 - modified by F.Ranjard
C  set IRET=-IRUN to skip the end of run in ABRREC
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE NASEVT, NASRUN, NAIRUN, NANEVT, NAONER
#include "bcs.h"
      LOGICAL START, RUNS, ONERUN, ONE
      DATA IGNRUN /-1/, MAXRUN /-1/, MAXEVT /-1/, START /.TRUE./,
     +      NNEVT /0/, NNMIN /0/, NNMAX /99999999/, ONERUN/.FALSE./
C ------------------------------------------------------------------
C       init
C
      IRET = 0
C
      IF (START)  THEN
         START = .FALSE.
         NASEVT = NAMIND ('SEVT')
         NASRUN = NAMIND ('SRUN')
         NAIRUN = NAMIND ('IRUN')
         NANEVT = NAMIND ('NEVT')
         NAONER = NAMIND ('ONER')
         IF (IW(NAONER) .GT. 0) ONERUN = .TRUE.
      ENDIF
C
C =================   end of initialization.  =====================
C
C - next entry
C
      IF (IW(NASRUN) .EQ. 0 .AND. IW(NAIRUN) .EQ. 0 .AND.
     +    IW(NASEVT) .EQ. 0 .AND. IW(NANEVT) .EQ. 0)  GOTO 890
C
C       NEVT card
C
      IF (NNEVT .GE. NNMAX)  THEN
        IRET = 7
        GO TO 900
      ENDIF
C
      IF (IEVT .EQ. 0)  GO TO 300
      IF (IRUN - MAXRUN)  300, 210, 220
  210 IF (IEVT .LE. MAXEVT)  GO TO 300
  220 IRET = 8
      GO TO 900
C
  300 IF (IRUN .EQ. IGNRUN)  GO TO 550
C
C       SEVT cards
C
      INS = IW(NASEVT)
      IF (INS .EQ. 0)  GO TO 400
      RUNS = .FALSE.
      MEVT = 0
  310 IF (IW(INS+1) .NE. IRUN)  GO TO 330
      RUNS = .TRUE.
C       accept run records for runs specified on SEVT cards :
      IF (IEVT .EQ. 0)  GO TO 890
      DO  I=2,IW(INS)
        MEVT = MAX (MEVT,IABS(IW(INS+I)))
        IF (IEVT .EQ. IW(INS+I)) GOTO 800
        IF (IEVT.LE.-IW(INS+I) .AND. IEVT.GE.IW(INS+I-1)) GOTO 800
      ENDDO
  330 INS = IW(INS-1)
      IF (INS .NE. 0)  GO TO 310
C     SEVT selects the run but the event no. is > the greatest selected
C     event on this run  OR
C     SEVT does not select this run and there is no SRUN card   THEN
C     jump to the next run or to the next file if there is only one
C     run per file.
      IF ( (RUNS .AND. IEVT.GT.MEVT) .OR.
     &     (.NOT.RUNS .AND. IW(NASRUN).EQ.0) ) THEN
         IF (ONERUN) IRET = 5
         GOTO 550
      ENDIF
C     if SEVT selects the run but NOT the event : skip the event
      IF (RUNS)  GO TO 900
C
C       IRUN cards
C
  400 INS = IW(NAIRUN)
      IF (INS .EQ. 0)  GO TO 500
  410 DO  I=1,IW(INS)
        IF (IRUN .EQ. IW(INS+I))  GO TO 550
        IF (IRUN .LE. -IW(INS+I) .AND. IRUN .GE. IW(INS+I-1)) GOTO 550
      ENDDO
      INS = IW(INS-1)
      IF (INS .NE. 0)  GO TO 410
C
C       SRUN cards
C
  500 INS = IW(NASRUN)
      IF (INS .EQ. 0)  GO TO 800
  510 DO  I=1,IW(INS)
        IF (IRUN .EQ. IW(INS+I))  GO TO 800
        IF (IRUN .LE. -IW(INS+I) .AND. IRUN .GE. IW(INS+I-1)) GOTO 800
      ENDDO
      INS = IW(INS-1)
      IF (INS .NE. 0)  GO TO 510
C
C       skip the run
C
  550 IGNRUN = IRUN
      IF (IRET.EQ.0) IRET = -IRUN
      GO TO 900
C
C       NEVT card
C
  800 IF (IEVT .EQ. 0)  GO TO 890
      NNEVT = NNEVT + 1
      IF (NNEVT .LT. NNMIN)  GO TO 900
C
C       accept the event
C
  890 IRET = 1
  900 RETURN
C
C ===================================================================
C - set limits of SEVT and SRUN cards
      ENTRY ABSMAX (LASRUN,LASEVT)
      MAXRUN = LASRUN
      MAXEVT = LASEVT
      GOTO 900
C
C ====================================================================
C - set limits of NEVT cards
      ENTRY ABSLIM (N1EVT,NLEVT)
      NNMIN  = N1EVT
      NNMAX  = NLEVT
      GOTO 900
C
C ====================================================================
C - set ONERUN flag
      ENTRY ABONER (ONE)
      ONERUN = ONE
      GOTO 900
C
      END
#endif
