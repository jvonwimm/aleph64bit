      SUBROUTINE MINEGI
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill EGID bank from DGAM.
C
C     Author: Stephen Haywood      17-Jan-91
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dgamjj.h"
#include "egidjj.h"
#include "minfac.h"
#include "bmacro.h"
C
      KDGAM = NLINK('DGAM',100)
      IF (KDGAM.LE.0) THEN
         CALL MINUPD('DGAM')
         KDGAM = NLINK('DGAM',100)
         IF (KDGAM.LE.0) RETURN
      ENDIF
C
C++   Create EGID bank.
C
      NEGID = LROWS(KDGAM)
      IF(NEGID.LE.0) RETURN
      LEN = LMHLEN + LEGIDA * NEGID
      CALL AUBOS('EGID',0,LEN, KEGID,IGARB)
      CALL BLIST(IW,'S+','EGID')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDGAM = NLINK('DGAM',100)
      ENDIF
      IW(KEGID+LMHCOL) = LEGIDA
      IW(KEGID+LMHROW) = NEGID
C
C++   Fill EGID bank.
C
      DO 100 I=1,NEGID
         IWORD = ITABL(KDGAM,I,JDGANA)
         IW(KROW(KEGID,I)+JEGIIF) = MOD(IWORD,16)
         IW(KROW(KEGID,I)+JEGINA) = IWORD / 16
         RW(KROW(KEGID,I)+JEGIE1) = FLOAT(ITABL(KDGAM,I,JDGAE1))/10.
         RW(KROW(KEGID,I)+JEGIE2) = FLOAT(ITABL(KDGAM,I,JDGAE2))/10.
         RW(KROW(KEGID,I)+JEGIE3) = FLOAT(ITABL(KDGAM,I,JDGAE3))/1000.
         RW(KROW(KEGID,I)+JEGIE5) = FLOAT(ITABL(KDGAM,I,JDGAE5))/10.
         RW(KROW(KEGID,I)+JEGIE6) = FLOAT(ITABL(KDGAM,I,JDGAE6))/10.
         RW(KROW(KEGID,I)+JEGIE8) = FLOAT(ITABL(KDGAM,I,JDGAE8))/EFACTM
         RW(KROW(KEGID,I)+JEGICE) = FLOAT(ITABL(KDGAM,I,JDGAE0))/EFACTM
         RW(KROW(KEGID,I)+JEGITH) = FLOAT(ITABL(KDGAM,I,JDGATH))/AFACTM
         RW(KROW(KEGID,I)+JEGIPH) = FLOAT(ITABL(KDGAM,I,JDGAPH))/AFACTM
         IW(KROW(KEGID,I)+JEGIPE) = ITABL(KDGAM,I,JDGADE)
  100 CONTINUE
C
      RETURN
      END
#endif
