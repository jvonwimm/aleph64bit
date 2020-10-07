      SUBROUTINE QWRPTPX(ier)
C
C-----------------------------------------------------------------------
C! Transfer the TPXS banks to the PTPX banks
C!
C!    Author:   F. Palla 14-Nov-1998
C!              Called from QFPTPX
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C!
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "ptunjj.h"
#include "tpxsjj.h"
#include "ptpxjj.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTUN=NAMIND('PTUN')
        NTPXS=NAMIND('TPXS')
        NPTPX=NAMIND('PTPX')
      ENDIF
C
      KTPXS=IW(NTPXS)
      IF (KTPXS.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
C
      IF (IW(NPTPX).NE.0) CALL BDROP(IW,'PTPX')
      LEN=LPTPXA*LROWS(KTPXS)+LMHLEN
      IW(1)=1
      CALL AUBOS('PTPX',0,LEN,KPTPX,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=3
        GO TO 999
      ENDIF
      IW(KPTPX+LMHCOL)=LPTPXA
      IW(KPTPX+LMHROW)=LROWS(KTPXS)
      KTPXS=IW(NTPXS)
      KPTUN=IW(NPTUN)+LMHLEN
      DO 502 I=1,LROWS(KTPXS)
        IW(KROW(KPTPX,I)+JPTPSL)=ITABL(KTPXS,I,JTPXSI)
        IW(KROW(KPTPX,I)+JPTPTM)=
     &             INT(RTABL(KTPXS,I,JTPXTM)/RW(KPTUN+JPTUTM))
        IW(KROW(KPTPX,I)+JPTPUL)=
     &             INT(RTABL(KTPXS,I,JTPXTL)/RW(KPTUN+JPTUTL))
        IW(KROW(KPTPX,I)+JPTPNS)=INT(100.*RTABL(KTPXS,I,JTPXNS))
        IW(KROW(KPTPX,I)+JPTPAD)=
     &             INT(RTABL(KTPXS,I,JTPXAD)/RW(KPTUN+JPTUAD))
        IW(KROW(KPTPX,I)+JPTPTN)=ITABL(KTPXS,I,JTPXTN)
  502 CONTINUE
      IER=0
      CALL BLIST(IW,'E+','PTPX')
  999 CONTINUE
      RETURN
      END
#endif

