      SUBROUTINE PTEXSJ(LIST,IER)
C
C-----------------------------------------------------------------------
C! Transfer the PTEX bank to the TEXS bank
C!
C!    Author:   R. Johnson  16-06-88
C!    Modified: D. Cinabro  07-04-91 Also transfer PT2X to T2XS
C!    Modified: D. Casper   16-11-95 Also transfer PTPX to TPXS
C!
C!    Input :   LIST      /C    BOS event list
C!                              if LIST(2:2).eq.'-' POT bank is dropped
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C                                            -1   OK but garbage coll.
C                                             2   not enough space
C                                            >2   TPC errors
C!
C!    Called by TPTOJ
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) LIST, PLIST*8, JLIST*8
#include "bcs.h"
#include "ptexjj.h"
#include "texsjj.h"
#include "pt2xjj.h"
#include "t2xsjj.h"
#include "ptunjj.h"
#include "ptpxjj.h"
#include "tpxsjj.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTEXS=NAMIND('TEXS')
        CALL BKFMT('TEXS','2I,(I,2F,I,F,2I)')
        NPTEX=NAMIND('PTEX')
        NT2XS=NAMIND('T2XS')
        CALL BKFMT('T2XS','2I,(I,2F,I,F,2I)')
        NPT2X=NAMIND('PT2X')
        NTPXS=NAMIND('TPXS')
        CALL BKFMT('TPXS','2I,(I,4F,I)')
        NPTPX=NAMIND('PTPX')
        NPTUN=NAMIND('PTUN')
      ENDIF
C
      IER = 0
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
C
      JPTEX=IW(NPTEX)
      IF (JPTEX.EQ.0) GOTO 900
      IF (LROWS(JPTEX).EQ.0) GOTO 900
C
      LEN=LTEXSA*LROWS(JPTEX)+LMHLEN
      IW(1)=1
      CALL AUBOS('TEXS',0,LEN,JTEXS,IER)
      IF (IER.EQ.2) GOTO 999
      IF (IER.EQ.1) JPTEX = IW(NPTEX)
      JLIST = 'TEXS'
      IW(JTEXS+LMHCOL)=LTEXSA
      IW(JTEXS+LMHROW)=LROWS(JPTEX)
C
      KPTUN=IW(NPTUN)+LMHLEN
      DO 500 I=1,LROWS(JPTEX)
        IW(KROW(JTEXS,I)+JTEXSI)=ITABL(JPTEX,I,JPTESL)
        RW(KROW(JTEXS,I)+JTEXTM)=
     &             FLOAT(ITABL(JPTEX,I,JPTETM))*RW(KPTUN+JPTUTM)
        RW(KROW(JTEXS,I)+JTEXTL)=
     &             FLOAT(ITABL(JPTEX,I,JPTEUL))*RW(KPTUN+JPTUTL)
        IW(KROW(JTEXS,I)+JTEXNS)=ITABL(JPTEX,I,JPTENS)
        RW(KROW(JTEXS,I)+JTEXAD)=
     &             FLOAT(ITABL(JPTEX,I,JPTEAD))*RW(KPTUN+JPTUAD)
        IW(KROW(JTEXS,I)+JTEXTN)=ITABL(JPTEX,I,JPTETN)
C
C++     Old POT versions do not have the saturation flag stored
C
        IF (LCOLS(JPTEX).GE.JPTESF) THEN
          IW(KROW(JTEXS,I)+JTEXSF)=ITABL(JPTEX,I,JPTESF)
        ELSE
          IW(KROW(JTEXS,I)+JTEXSF)=0
        ENDIF
  500 CONTINUE
C
  900 CONTINUE
      JPT2X=IW(NPT2X)
      IF (JPT2X.EQ.0) GOTO 950
      IF (LROWS(JPT2X).EQ.0) GOTO 950
      LEN=LT2XSA*LROWS(JPT2X)+LMHLEN
      IW(1)=1
      CALL AUBOS('T2XS',0,LEN,JT2XS,IER)
      IF (IER.EQ.2) GOTO 998
      IF (IER.EQ.1) JPT2X = IW(NPT2X)
      JLIST = 'TEXST2XS'
      IW(JT2XS+LMHCOL)=LT2XSA
      IW(JT2XS+LMHROW)=LROWS(JPT2X)
C
      KPTUN=IW(NPTUN)+LMHLEN
      DO I=1,LROWS(JPT2X)
        IW(KROW(JT2XS,I)+JT2XSI)=ITABL(JPT2X,I,JPT2SL)
        RW(KROW(JT2XS,I)+JT2XTM)=
     &             FLOAT(ITABL(JPT2X,I,JPT2TM))*RW(KPTUN+JPTUTM)
        RW(KROW(JT2XS,I)+JT2XTL)=
     &             FLOAT(ITABL(JPT2X,I,JPT2UL))*RW(KPTUN+JPTUTL)
        IW(KROW(JT2XS,I)+JT2XNS)=ITABL(JPT2X,I,JPT2NS)
        RW(KROW(JT2XS,I)+JT2XAD)=
     &             FLOAT(ITABL(JPT2X,I,JPT2AD))*RW(KPTUN+JPTUAD)
        IW(KROW(JT2XS,I)+JT2XT1)=ITABL(JPT2X,I,JPT2T1)
        IW(KROW(JT2XS,I)+JT2XT2)=ITABL(JPT2X,I,JPT2T2)
      ENDDO
C
  950 CONTINUE
      JPTPX=IW(NPTPX)
      IF (JPTPX.EQ.0) GOTO 998
      IF (LROWS(JPTPX).EQ.0) GOTO 998
C
      LEN=LTPXSA*LROWS(JPTPX)+LMHLEN
      IW(1)=1
      CALL AUBOS('TPXS',0,LEN,JTPXS,IER)
      IF (IER.EQ.2) GOTO 999
      IF (IER.EQ.1) JPTPX = IW(NPTPX)
      JLIST = 'TEXST2XSTPXS'
      IW(JTPXS+LMHCOL)=LTPXSA
      IW(JTPXS+LMHROW)=LROWS(JPTPX)
C
      KPTUN=IW(NPTUN)+LMHLEN
      DO I=1,LROWS(JPTPX)
        IW(KROW(JTPXS,I)+JTPXSI)=ITABL(JPTPX,I,JPTPSL)
        RW(KROW(JTPXS,I)+JTPXTM)=
     &             FLOAT(ITABL(JPTPX,I,JPTPTM))*RW(KPTUN+JPTUTM)
        RW(KROW(JTPXS,I)+JTPXTL)=
     &             FLOAT(ITABL(JPTPX,I,JPTPUL))*RW(KPTUN+JPTUTL)
        RW(KROW(JTPXS,I)+JTPXNS)=FLOAT(ITABL(JPTPX,I,JPTPNS))/100.
        RW(KROW(JTPXS,I)+JTPXAD)=
     &             FLOAT(ITABL(JPTPX,I,JPTPAD))*RW(KPTUN+JPTUAD)
        IW(KROW(JTPXS,I)+JTPXTN)=ITABL(JPTPX,I,JPTPTN)
      ENDDO
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
        PLIST = 'PTEXPT2XPTPX'
#include "jplist.h"
C
      IF (IER .EQ.1) IER = -1
C
  999 CONTINUE
      RETURN
      END
#endif
