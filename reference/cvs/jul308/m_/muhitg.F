      SUBROUTINE MUHITG
C--------------------------------------------------------------
C
C! Provides from MHIT bank all useful hit info (New DB)
C!
C! P.Campana  170589
C!
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
      ISLOT = ITABL(JMHIT,I,2)
C Determination of layer type
      IF(IW(KIND+5)-2) 1,2,3
C  End Cap
 1    IF((ISLOT.GE.1.AND.ISLOT.LE.4).OR.
     +   (ISLOT.GE.9.AND.ISLOT.LE.12)) THEN
              IW(KIND+4)=1
         ELSE
              IW(KIND+4)=2
      ENDIF
      GOTO 4
C  Middle Angle
 2    IF((ISLOT.GE.1.AND.ISLOT.LE.10).OR.
     +   (ISLOT.GE.20.AND.ISLOT.LE.29))   THEN
              IW(KIND+4)=1
         ELSE
              IW(KIND+4)=2
      ENDIF
      GOTO 4
C  Barrel
 3    IF(ISLOT.GE.1.AND.ISLOT.LE.17) THEN
              IW(KIND+4)=1
         ELSE
              IW(KIND+4)=2
      ENDIF
C
 4    RHO   = RTABL(JMHIT,I,6)
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
 110  CALL RERROR('MUHITG',1,'INSUFF. SPACE TO CREATE WBANK')
      RETURN
      END
#endif
