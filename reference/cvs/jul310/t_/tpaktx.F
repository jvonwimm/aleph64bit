      SUBROUTINE TPAKTX(IER)
C
C-----------------------------------------------------------------------
C! Transfer the TEXS,T2XS,TPXS banks to the PTEX,PT2X,PTPX banks
C!
C!    Author:   R. Johnson  16-06-88
C!    Modified: R. Johnson  21-01-91 add flag to the PTEX bank
C!              D. Casper   04-11-95 add pad dE/dx banks
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C!
C!    Called by TJTOP
C!
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "ptexjj.h"
#include "texsjj.h"
#include "ptunjj.h"
#include "t2xsjj.h"
#include "pt2xjj.h"
#include "tpxsjj.h"
#include "ptpxjj.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTEXS=NAMIND('TEXS')
        NPTEX=NAMIND('PTEX')
        NT2XS=NAMIND('T2XS')
        NPT2X=NAMIND('PT2X')
        NPTUN=NAMIND('PTUN')
        NTPXS=NAMIND('TPXS')
        NPTPX=NAMIND('PTPX')
      ENDIF
C
      KTEXS=IW(NTEXS)
      IF (KTEXS.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
      KT2XS = IW(NT2XS)
C
      IF (IW(NPTEX).NE.0) CALL BDROP(IW,'PTEX')
      LEN=LPTEXA*LROWS(KTEXS)+LMHLEN
      IW(1)=1
      CALL AUBOS('PTEX',0,LEN,KPTEX,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=3
        GO TO 999
      ENDIF
      IW(KPTEX+LMHCOL)=LPTEXA
      IW(KPTEX+LMHROW)=LROWS(KTEXS)
      KTEXS=IW(NTEXS)
      KPTUN=IW(NPTUN)+LMHLEN
      DO 500 I=1,LROWS(KTEXS)
        IW(KROW(KPTEX,I)+JPTESL)=ITABL(KTEXS,I,JTEXSI)
        IW(KROW(KPTEX,I)+JPTETM)=
     &             INT(RTABL(KTEXS,I,JTEXTM)/RW(KPTUN+JPTUTM))
        IW(KROW(KPTEX,I)+JPTEUL)=
     &             INT(RTABL(KTEXS,I,JTEXTL)/RW(KPTUN+JPTUTL))
        IW(KROW(KPTEX,I)+JPTENS)=ITABL(KTEXS,I,JTEXNS)
        IW(KROW(KPTEX,I)+JPTEAD)=
     &             INT(RTABL(KTEXS,I,JTEXAD)/RW(KPTUN+JPTUAD))
        IW(KROW(KPTEX,I)+JPTETN)=ITABL(KTEXS,I,JTEXTN)
        IW(KROW(KPTEX,I)+JPTESF)=ITABL(KTEXS,I,JTEXSF)
  500 CONTINUE
      IER=0
C
      IF (IW(NPT2X).NE.0) CALL BDROP(IW,'PT2X')
      IF (KT2XS.EQ.0) THEN
        IER = 1
        GOTO 999
      ENDIF
      LEN = LPT2XA*LROWS(KT2XS)+LMHLEN
      IW(1) = 1
      CALL AUBOS('PT2X',0,LEN,KPT2X,IGARB)
      IF (IGARB.EQ.2) THEN
        IER = 3
        GOTO 999
      ENDIF
      IW(KPT2X+LMHCOL)=LPT2XA
      IW(KPT2X+LMHROW)=LROWS(KT2XS)
      KT2XS=IW(NT2XS)
      KPTUN=IW(NPTUN)+LMHLEN
      DO 501 I=1,LROWS(KT2XS)
        IW(KROW(KPT2X,I)+JPT2SL)=ITABL(KT2XS,I,JT2XSI)
        IW(KROW(KPT2X,I)+JPT2TM)=
     &             INT(RTABL(KT2XS,I,JT2XTM)/RW(KPTUN+JPTUTM))
        IW(KROW(KPT2X,I)+JPT2UL)=
     &             INT(RTABL(KT2XS,I,JT2XTL)/RW(KPTUN+JPTUTL))
        IW(KROW(KPT2X,I)+JPT2NS)=ITABL(KT2XS,I,JT2XNS)
        IW(KROW(KPT2X,I)+JPT2AD)=
     &             INT(RTABL(KT2XS,I,JT2XAD)/RW(KPTUN+JPTUAD))
        IW(KROW(KPT2X,I)+JPT2T1)=ITABL(KT2XS,I,JT2XT1)
        IW(KROW(KPT2X,I)+JPT2T2)=ITABL(KT2XS,I,JT2XT2)
  501 CONTINUE
      IER = 0
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
  999 CONTINUE
      RETURN
      END
#endif
