      SUBROUTINE PCPATQ(IER)
C----------------------------------------------------------------------
CKEY PTOJ PCPA PCQA / USER
C----------------------------------------------------------------------
C!  - prepare PARLOC array ready to fill PCQA bank from PCPA bank.
C!
C!     IER =  0 OK
C!         = -1 banks missing
C!   Author   :-J. Carr    8 May 1991
C!    modified:-J. Carr    20 June 1991
C!
C!======================================================================
#ifndef DOC
      SAVE
#include "bcs.h"
#include "pcqajj.h"
#include "pcpajj.h"
C
      PARAMETER (NMLINE=50)
      DIMENSION LINEPC(NMLINE)
      PARAMETER ( MXNEUT=300 )
      DIMENSION PARLOC(MXNEUT,5)
      DIMENSION IPEPC(MXNEUT),IPCOB(MXNEUT),ITYPE(MXNEUT)
      LOGICAL PARGON(MXNEUT)
C
      LOGICAL FIRST
      LOGICAL HADRO,GAMMA,GAMEX,GARBA,LCALO
      DATA FIRST/.TRUE./
#include "bmacro.h"
      HADRO(IY) = IY.GE.13.AND.IY.LE.20
      GAMMA(IY) = IY.LT.13.AND.IY.NE.5
      GAMEX(IY) = IY.EQ.5
      GARBA(IY) = IY.EQ.21
      LCALO(IY) = IY.EQ.22
C ------------------------------------------------------------------
C
      IF(FIRST) THEN
        MPCPA = NAMIND ('PCPA')
        MPECO = NAMIND ('PECO')
        MPCOB = NAMIND ('PCOB')
        MNEMO = NAMIND ('NEMO')
        FIRST=.FALSE.
      ENDIF
C
      JPCPA = IW(MPCPA)
      IF(JPCPA.EQ.0)GOTO 999
      LPCPA = IW(JPCPA-1)
      NMAXP = MIN(IW(JPCPA+LMHLEN),MXNEUT)
C
      DO 30 I=1,NMAXP
        EN = RTABL(JPCPA,I,JPCPEN)
        TH = RTABL(JPCPA,I,JPCPTE)
        PH = RTABL(JPCPA,I,JPCPFI)
        IY = ITABL(JPCPA,I,JPCPNA)
        ITYPE(I)=IY
        IPEPC(I)=0
        IPCOB(I)=0
        IF(IY.EQ.19.OR.IY.EQ.20)THEN
           PSUM = RTABL(JPCPA,I,JPCPR2)
           PN = RTABL(JPCPA,I,JPCPR1)
        ELSE
           PSUM=0.
           PN = EN
        ENDIF
        CP = COS (PH)
        SP = SIN (PH)
        CT = COS (TH)
        ST = SIN (TH)
        PT = PN * ST
        PARLOC(I,1) = PT * CP
        PARLOC(I,2) = PT * SP
        PARLOC(I,3) = PN * CT
        PARLOC(I,4) = EN
        PARLOC(I,5) = PSUM
        PARGON(I)=.FALSE.
        IF(GARBA(IY)) PARGON(I)=.TRUE.
  30    CONTINUE
C
C   store PECO and PCOB if want to merge PCPA particles later in PCMECU
C       ( no merge if CARD NEMO included )
C
      IF(IW(MNEMO).NE.0) GO TO 42
C
C   Now get rid of some particles
C
      KPECO  = IW (MPECO)
      IF (KPECO .EQ. 0) goto 999
C
C Find which PCPA objects are from same PECO object
      LPECO = LROWS (KPECO)
      DO 40 IPECO=1,LPECO
C
         CALL NVPECO(IPECO, LINEPC, NLINE, NMLINE, IER)
         IF(IER.LT.0) GO TO 999
C
         DO 21 I=1,NLINE
           IF(LINEPC(I).LE.MXNEUT) IPEPC(LINEPC(I))=IPECO
  21     CONTINUE
  40  CONTINUE
  42  CONTINUE
C
C Find which PCPA objects are from same PCOB object
      KPCOB  = IW (MPCOB)
      IF (KPCOB .EQ. 0) goto 999
      LPCOB = LROWS (KPCOB)
      IMERGE=0
      DO 41 IPCB=1,LPCOB
C
         CALL NVPCOB(IPCB, LINEPC, NLINE, NMLINE, IER)
         IF(IER.LT.0) GO TO 999
C
         DO 23 I=1,NLINE
            IF(LINEPC(I).LE.MXNEUT) IPCOB(LINEPC(I))=IPCB
  23     CONTINUE
  41  CONTINUE
C
      CALL PCMECU(NMAXP,MXNEUT,PARLOC,IPEPC,IPCOB,ITYPE,PARGON)
C
      IER=0
      RETURN
  999 CONTINUE
      IER=-1
      RETURN
      END
#endif
