      SUBROUTINE ECINIT(INEW,JNEW,KNEW,IORG,NDIRC)
C***********************************************************************
C!TRANSFORM STOREY ENCODING FOR CLUSTRIZATION
C!
C!      FUNCTIONAL DESCRIPTION:
C!       TRANSFORM NEW ECAL ENCODING INTO OLD ECAL ENCODING
C!       THEN COMPUTE POINTER INTO ARRAY MAPPING THE ECAL
C!       (IORG) AND STARTING ADDRESS FOR EACH STOREY_IROSEV(NDIRC).
C!
C***********************************************************************
#ifndef DOC
#include "eclcom.h"
C
      DIMENSION NRECL(5),NSTEP(5)
      DATA NRECL/1,9,25,41,45/
      DATA NSTEP/0,8,40,88,128/
C
      LI=INEW
      LJ=JNEW
      LK=KNEW
C? HERE INSERT TRANSFORMATION NEW ---->>OLD
      IF (JNEW .LT. 9) THEN
         NTA=1
      ELSEIF (JNEW .LT. 25) THEN
         NTA=2
      ELSEIF (JNEW .LT .41 ) THEN
         NTA=3
      ELSEIF (JNEW .LT. 51) THEN
         NTA=4
      ELSEIF (JNEW .LT. 179) THEN
         NTA=5
         LJ = JNEW-6
      ELSEIF (JNEW .LT. 189) THEN
         NTA=4
         LJ = JNEW-12
      ELSEIF (JNEW .LT. 205) THEN
         NTA=3
         LJ= JNEW-12
      ELSEIF (JNEW .LT. 221) THEN
         NTA=2
         LJ = JNEW-12
      ELSE
         NTA=1
         LJ = JNEW-12
      ENDIF
C
      MUL=NTA-INT(NTA/5)
      IF ((NTA.NE.5).AND.(LJ.GE.167)) THEN
         LJ=217-LJ
         IORG=(LK-1)*73728+768*(INT((LI+MUL-1)/MUL)-1)+NSTEP(NTA) +(LJ
     +   -NRECL(NTA))*MUL+MOD(LI-1,MUL)+ 1+ 640
C? choose right NDIR case for endcap 2(as in ECINRV)
      LJ=217-LJ
      ELSE
         IORG=(LK-1)*73728+768*(INT((LI+MUL-1)/MUL)-1)+NSTEP(NTA) +(LJ
     +   -NRECL(NTA))*MUL+MOD(LI-1,MUL)+1
      ENDIF
      NDIR=(LK-1)*228* 6+6*(LJ-1+INT(NTA/5)*6+INT(LJ/167)*12-INT(NTA/5)
     +*INT(LJ/167)*12) +MOD(LI-1,MUL)+1+INT((MUL*96+1-LI)/(MUL*96))*MUL
     ++INT(LI/(MUL*96))*2
C
      NDIRC=40*NINDEB(NDIR)
C? Verify that I,J,K STOREY address is not zero (geometry prot.)
      IF (LK .EQ. 0 .OR. LI .EQ. 0 .OR. LJ .EQ. 0) THEN
      IORG=0
      NDIRC=0
      ENDIF
C
      RETURN
      END
#endif
