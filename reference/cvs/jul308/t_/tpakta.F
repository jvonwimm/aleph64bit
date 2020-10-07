      SUBROUTINE TPAKTA(IER)
C
C-----------------------------------------------------------------------
C! Pack TPC track-MC association for production output tape
C
C     Author:  R. Johnson   10-1-89
C
C     Output:   IER    /I    Error code is non-zero if BOS overflows
C
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tgmajj.h"
#include "tmtljj.h"
#include "ptmajj.h"
#include "ptmljj.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA MXMAT/4/
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTGMA=NAMIND('TGMA')
        NTMTL=NAMIND('TMTL')
        NPTMA=NAMIND('PTMA')
        NPTML=NAMIND('PTML')
      ENDIF
C
      IER=0
      KTGMA=IW(NTGMA)
      IF (KTGMA.EQ.0) GO TO 888
      IF (LROWS(KTGMA).EQ.0) GO TO 888
      KTMTL=IW(NTMTL)
      IF (KTMTL.EQ.0) GO TO 888
C
      IF (IW(NPTMA).NE.0) CALL BDROP(IW,'PTMA')
      IF (IW(NPTML).NE.0) CALL BDROP(IW,'PTML')
C
      LEN= LPTMAA*LROWS(KTGMA)+LMHLEN
      IW(1)=1
      CALL AUBOS('PTMA',0,LEN,KPTMA,IER)
      IF (IER.EQ.2) GO TO 999
      IW(KPTMA+LMHCOL)=LPTMAA
      IW(KPTMA+LMHROW)=LROWS(KTGMA)
C
      KTMTL=IW(NTMTL)
      LEN= LPTMLA*LROWS(KTMTL)+LMHLEN
      IW(1)=1
      CALL AUBOS('PTML',0,LEN,KPTML,IER)
      IF (IER.EQ.2) GO TO 999
      IW(KPTML+LMHCOL)=LPTMLA
      IW(KPTML+LMHROW)=0
C
      KTMTL=IW(NTMTL)
      KTGMA=IW(NTGMA)
      KPTMA=IW(NPTMA)
      DO 700 ITK=1,LROWS(KTGMA)
        NMAT=MIN(ITABL(KTGMA,ITK,JTGMNM),MXMAT)
        IW(KROW(KPTMA,ITK)+JPTMNM)=NMAT
        IW(KROW(KPTMA,ITK)+JPTMNA)=ITABL(KTGMA,ITK,JTGMNA)
        DO 500 II=1,NMAT
          IPT= ITABL(KTGMA,ITK,JTGMOM)+II
          IW(KPTML+LMHROW)=LROWS(KPTML)+1
          IPUT=LROWS(KPTML)
          IW(KROW(KPTML,IPUT)+JPTMMT)=ITABL(KTMTL,IPT,JTMTMT)
          IW(KROW(KPTML,IPUT)+JPTMNH)=ITABL(KTMTL,IPT,JTMTNH)
  500   CONTINUE
  700 CONTINUE
C
      CALL AUBPRS('PTML')
C
  888 CONTINUE
      RETURN
C
  999 CONTINUE
      CALL BDROP(IW,'PTMAPTML')
      RETURN
      END
#endif
