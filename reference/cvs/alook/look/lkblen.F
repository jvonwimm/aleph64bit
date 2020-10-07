        SUBROUTINE LKBLEN(LIST)
C-----------------------------------------------------------------------
C! return the number of words in the banks of the list
C ---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
C
      CHARACTER*4 NAME,NLIST
      CHARACTER*1 LIST
      CHARACTER*4 NMEM(5)
      INTEGER LMEM(5),IMEM(5)
C ------------------------------------------------------------------
      M=0
      N=0
      LTOT=0
      WRITE(IW(6),*) '   Banks on list: ',LIST
    1 N=N+1
      NAME=NLIST(IW,N,LIST)
      IF (NAME.NE.' ') THEN
         IND=NAMIND(NAME)+1
    2    IND=IW(IND-1)
         IF (IND.NE.0) THEN
            M=M+1
            NMEM(M)=NAME
            IMEM(M)=IW(IND-2)
            IF(IMEM(M).GT.99999)IMEM(M)=99999
            IF(IMEM(M).LT.-9999)IMEM(M)=99999
            LMEM(M)=IW(IND)
            LTOT=LTOT+IW(IND)
            IF (M.EQ.5) THEN
                WRITE(IW(6),5000) (NMEM(L),IMEM(L),LMEM(L),L=1,M)
                M=0
            ENDIF
            GOTO 2
         ENDIF
         GOTO 1
      ELSEIF (M.GT.0) THEN
         WRITE(IW(6),5000) (NMEM(L),IMEM(L),LMEM(L),L=1,M)
      ENDIF
 5000 FORMAT(5(5X,A4,I5,' :',I8))
         WRITE(IW(6),5001)LTOT
 5001 FORMAT(' Total length=',I7,' words')
      RETURN
      END
#endif
