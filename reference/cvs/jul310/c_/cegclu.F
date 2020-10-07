      SUBROUTINE CEGCLU(ITR,IECO,NECO,MXCLU,IER)
C***********************************************************************
C! Get EcOb assoc. with track itr (true association)                   *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETTD                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   ITR  : Geant track number                                         *
C!   Ieco : Table of EcObs                                             *
C!   Neco : Number ob Ecobs                                            *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rlunit.h"
#include "bcs.h"
      DIMENSION IECO(*)
#include "bmacro.h"
      KETD1=NLINK('ETTD',1)
      KETD2=NLINK('ETTD',2)
      KETD3=NLINK('ETTD',3)
      KETD4=NLINK('ETTD',4)
      LTD4A=IW(KETD4+LMHCOL)
C
C?  loop on bank ettd, to fin  digits for track itr
C
      NTRD=LROWS(KETD1)
      DO 10 I=1,NTRD
      IF(ITABL(KETD1,I,1).EQ.ITR)THEN
         ITK=I
C
C?  get  Number of towers hit   find storeys hit by track
C
         NTOW=ITABL(KETD2,ITK,1)
C
C?  get number of first tower hit
C
         IFTOW=ITABL(KETD3,ITK,1)
C
C?  loop on towers hit
C
         DO 110 ITOW=1,NTOW
         LABEL=IW(KETD4+IFTOW+(ITOW-1)*LTD4A+LMHLEN)
         JT=LABEL/2**16
         IFTW=(LABEL-JT*2**16)/4
C
C?   loop on stacks of the tower
C
         DO 111 K=1,3
C
C?    get  Ecobjects to which storey belongs
C?      and put them in table ieco
C
         CALL CESCLU(IFTW,JT,K,IECO,NECO,MXCLU,IER2)
         IF(IER2.NE.0)GO TO 998
  111    CONTINUE
  110    CONTINUE
         GO TO 999
      ENDIF
   10 CONTINUE
  999 CONTINUE
      IER=0
      RETURN
  998 CALL RERROR('CEGCLU',1,' ABORT')
      IER=1
      RETURN
      END
#endif
