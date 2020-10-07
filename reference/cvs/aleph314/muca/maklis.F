      SUBROUTINE MAKLIS(IRLBN,JHACPA,JHACPB,INDOBJ,N,NVEC,IER)
C****************************************************************
C! Make list of objects for tabular relation banks              *
CKEY MUCAL MUON CALOBJ / INTERNAL
C  Author:    R.Tenchini     890310                             *
C                                                               *
C  Input : IRLBN = Relation bank index (the one from NLINK)     *
C          JHACPA= HAC parameter for object A                   *
C          JHACPB= HAC parameter for object B                   *
C          INDOBJ= Element of object A for which  you want      *
C                  the list of elements of objects B that       *
C                  are in relation                              *
C  Output:                                                      *
C          N     = Number of elements B related to INDOBJ       *
C          NVEC  = List of B elements related to INDOBJ         *
C          IER   = 0 -> OK                                      *
C                = 1 -> ERROR More than LENVEC relations        *
C                                                               *
C****************************************************************
#ifndef DOC
      SAVE
#include "bcs.h"
#include "mucapa.h"
      INTEGER NVEC(*),NSER(LENVEC)
      LOGICAL TROVAT
#include "bmacro.h"
      IER=0
      NRLBN=LROWS(IRLBN)
      N1=0
      N=0
      DO 10 I=1,NRLBN
         ISERV=ITABL(IRLBN,I,JHACPA)
         IF(ISERV.EQ.INDOBJ) THEN
            INOBJB=ITABL(IRLBN,I,JHACPB)
            IF(INOBJB.NE.0) THEN
               N1=N1+1
               IF(N1.GT.LENVEC) THEN
                  IER=1
                  GO TO 999
               ENDIF
               NSER(N1)=INOBJB
            ENDIF
         ENDIF
 10   CONTINUE
C
C   Here we suppress double counting
C
      DO 20 I=1,N1
         TROVAT=.FALSE.
         DO 30 J=1,N
            IF(NSER(I).EQ.NVEC(J)) THEN
               TROVAT=.TRUE.
            ENDIF
 30      CONTINUE
         IF(.NOT.TROVAT) THEN
            N=N+1
            NVEC(N)=NSER(I)
         ENDIF
 20   CONTINUE
 999  RETURN
      END
#endif
