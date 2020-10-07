      SUBROUTINE MUHITS
C--------------------------------------------------------------
C
C! Provides from MHIT bank cartesian coordinates of hits
C!
C! G.Capon,M.Dreucci       28-May-87
C!    input : bank MHIT
C!     Output_: Work bank index IWBANK :
C!
C!       RW(2+IWBANK+1)= X-hit..........Aleph-system
C!       RW(2+IWBANK+2)= Y-hit..........Aleph-system
C!       RW(2+IWBANK+3)= Z-hit..........Aleph-system
C!       IW(2+IWBANK+4)= IE (=1 for Int chambers, =2 for Ext chambers)
C!       IW(2+IWBANK+5)= ISUBC
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "munamc.h"
#include "mwbank.h"
#include "bmacro.h"
C
      JMHIT=IW(NAMHIT)
      NMHIT=IW(JMHIT+2)
      LWORK=2+5*NMHIT
      IWBANK=0
      CALL WBANK (IW,IWBANK,LWORK,*110)
      KIND = IWBANK+2
      IW(IWBANK+1)= 5
C
      DO 20 I = 1,NMHIT
C
      IW(KIND+5) = ITABL(JMHIT,I,1)
      NSLO       = ITABL(JMHIT,I,2)
C
      ISUBC = IW(KIND+5)
      IF(ISUBC.EQ.3)  THEN
           IF(NSLO.LE.12) IW(KIND+4)=1
           IF(NSLO.GT.12) IW(KIND+4)=2
           GO TO 10
           ENDIF
      IF(ISUBC.EQ.2)  THEN
           IF(NSLO.LE.34) IW(KIND+4)=1
           IF(NSLO.GE.35 .AND. NSLO.LE.43) IW(KIND+4)=2
           IF(NSLO.GE.44 .AND. NSLO.LE.53) IW(KIND+4)=1
           IF(NSLO.GE.54) IW(KIND+4)=2
           ELSE
           IF(NSLO.LE.66) IW(KIND+4)=1
           IF(NSLO.GE.67 .AND. NSLO.LE.70) IW(KIND+4)=2
           IF(NSLO.GE.71 .AND. NSLO.LE.74) IW(KIND+4)=1
           IF(NSLO.GE.75) IW(KIND+4)=2
           ENDIF
 10   CONTINUE
      RHO   = RTABL(JMHIT,I,6)
      THETA = RTABL(JMHIT,I,7)
      PHI   = RTABL(JMHIT,I,8)
C
      RW(KIND+1) =  RHO*SIN(THETA)*COS(PHI)
      RW(KIND+2) =  RHO*SIN(THETA)*SIN(PHI)
      RW(KIND+3) =  RHO*COS(THETA)
C
      KIND = KIND+5
 20   CONTINUE
C
 99   RETURN
  110 CALL RERROR('MUHITS',0,'NO SPACE FOR WORK BANK')
      RETURN
      END
#endif
