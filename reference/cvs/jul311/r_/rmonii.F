      SUBROUTINE RMONII
C--------------------------------------------------
C!    Initialization routine for monitoring
C     J.Knobloch 31-7-89
C     Modified: J. Knobloch 18-9-89
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "bcs.h"
#include "monejj.h"
      CHARACTER*6 FTYPE
      CHARACTER*4 FDEVI
      CHARACTER FNAME*80
C
C     is MONI data card there ?
C
      KTYP=IW(NAMIND('MONI'))
      IF(KTYP.EQ.0)THEN
        LMONRL=0
      ELSE
C
C     yes -- analyse it
C
        CALL ACDARG('MONI','NATIVE','A',FNAME,FTYPE,FDEVI,IER)
#if defined(DEC) &&  ! defined(UNIX)
C
C     special VAX open for shared access
C
        OPEN  (UNIT=LMONRL,FILE=FNAME,STATUS='NEW',SHARED
     1     ,FORM='UNFORMATTED',ORGANIZATION='SEQUENTIAL',
     2     RECORDTYPE='VARIABLE',BLOCKSIZE=51*512,BUFFERCOUNT=4
     3     ,ERR=888)
#endif
      ENDIF
  888 RETURN
      END
#endif
