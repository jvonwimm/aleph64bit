      SUBROUTINE RNXMOD(MODUL,RMOD)
C----------------------------------------------------------------------
C!  - Analyse a module.-Called before JULIA modules
C!
C!  AUTHOR   : J.Knobloch  15-March-1989
C!  MODIFIED : J.Knobloch  21-March-1989
C!
C!    INPUT   : MODUL = Number of module
C!                      Special values:
C!                       MODUL=0   close previous module
C!                       MODUL=999 Print summary
C!                       MODUL=-1  End of Event
C!    OUTPUT  : RMOD    LOGICAL flag = .TRUE. if module to
C!                                     be executed
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rstatr.h"
#include "rflags.h"
#include "rparac.h"
#include "bcs.h"
#include "revhjj.h"
#include "rtimes.h"
      LOGICAL RMOD,FIRST,BTEST
      LOGICAL IFTIME, IFGARB
#include "bmacro.h"
C ---------------------------------
      DATA FIRST/.TRUE./
      DATA RNTIME/LASTMO*0./
      DATA RNCALL/LASTMO*0/
      DATA MPREV/0/TPREV/0./
      DATA TIMET/0./
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IFTIME = IW(NAMIND('MXTI')) .NE. 0
        IFGARB = IW(NAMIND('GARB')) .NE. 0
      ENDIF
C
      IF(MODUL.EQ.999) GOTO 500
      IF(MODUL.EQ.0.AND.MPREV.EQ.0)GOTO 999
C
C?   Check if module is valid or not
C
      IF ( MODUL.LT.-1 .OR. MODUL.GT.LASTMO ) THEN
        CALL RERROR ('RNXMOD',-1,'MODULE NUMBER OUT OF RANGE !!!')
        GOTO 999
      ENDIF
C
      JCMORF=MODUL
      IF ( IFGARB ) THEN
        CALL BGARB(IW)
      ENDIF
      IF( IFTIME ) THEN
        CALL ALTIME( TIME )
      ELSE
        TIME = TPREV
      ENDIF
      IF(MPREV.NE.0)THEN
        TMO=TIME-TPREV
        RNCALL(MPREV)=RNCALL(MPREV)+1
        RNTIME(MPREV)=RNTIME(MPREV)+TMO
        IF(FDEBRF.AND.JDBDRF(JULSK).GE.3) THEN
          WRITE(LOUTRL,'(A,A,A,F6.3,A,I6,A,I6,A,I6,A,I6)')
     *      ' RNXMOD - ',RNTXTS(MPREV),' -Time:',TMO
     *      ,' - WORKB:',I16-IW(16)-IW(18),' NAMED:'
     *      ,IW(15)-I15-IW(17),' Gap:',IW(16)-IW(15),
     *      ' Free:',IW(16)+IW(18)-IW(15)+IW(17)
        ENDIF
      ENDIF
      IF(FDEBRF.AND.JDBDRF(JULSK).GE.4) CALL RPNAMB
      TPREV=TIME
      MPREV=MODUL
      I15=IW(15)-IW(17)
      I16=IW(16)+IW(18)
C
C?   MODUL = -1 , End of Event Reconstruction
C
      IF (  MODUL.EQ.-1 ) THEN
        MPREV = 0
        TIMET = TIMET + TIME - AEVSRT
      ENDIF
      GOTO 999
C
C-     print summary at end of job
C
  500 CONTINUE
      WRITE(LOUTRL,'(A)')
     &  '0                 RNXMOD --- JOB SUMMARY --------------'
      WRITE(LOUTRL,'(2A)')
     &  '0     Module   Description......................',
     *  'Nb.CALLs Time/evt Percent. '
      DO 510 I=1,LASTMO
        NEVT=NRUNRS(4)
        TPEV=0.
        IF (NEVT.GT.0) TPEV = RNTIME(I)/NEVT
        FRPEV = 0.
        IF (TIMET.GT.0.) FRPEV = RNTIME(I) / TIMET
        WRITE(LOUTRL,'(5X,A,2X,A,(I8,F8.3,2X,F5.2,2x))')
     &    RNTXTS(I),RNTXTM(I),RNCALL(I),TPEV,100.*FRPEV
  510 CONTINUE
  999 RMOD=.TRUE.
      RETURN
      END
#endif
