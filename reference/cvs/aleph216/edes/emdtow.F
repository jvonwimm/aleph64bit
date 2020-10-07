          SUBROUTINE EMDTOW(RW,CL,SC,MD,RG)
C-------------------------------------------------------------
CKEY ECALDES ROW COLUMN SUBCOMPONENT MODULE/ USER
C     H.Videau      creation 23/03/89
C! Get subc and module from row and column
C   Input :
C           RW  row number         INTEGER
C           CL column number       INTEGER
C   Output: SC subcomponent number INTEGER
C           MD module number       INTEGER
C           RG region number       INTEGER
C   Called by USER
C-------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Input
          INTEGER RW,CL
C  Output
          INTEGER SC,MD,RG
C Execution
      RG = EXRGER(EROWEX(EXROER(RW)))
      SC = EXROES(RW)
      MD=FLOAT(CL-1)/FLOAT(EREGCN(RG))-FLOAT(ESCOCF(SC))*.5+1.
      IF(MD.EQ.13.AND.CL.LE.EREGCN(RG)*ECALMD)MD=1
          END
#endif
