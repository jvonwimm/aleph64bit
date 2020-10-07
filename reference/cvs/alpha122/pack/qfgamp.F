      SUBROUTINE QFGAMP
CKEY FILL PCPA EFLOW /INTERNAL
C----------------------------------------------------------------------
C!  - Fill GAT section from PGAC bank ( or old PGPC or older EGPC )
C!
C!   Author   :- E. Blucher     02-Feb-1992
C!   Modified J. Boucrot  02-Apr-1993  for new GAMPEX 'PGPC' bank
C!   Modified J. Boucrot  28-Oct-1994  for new GAMPEX 'PGAC' bank
C!     kfgat = first GAMPEC  track
C!     klgat = last GAMPEC track
C!     kngat = number of GAMPEC tracks
C!     Objects from EGPC or PGPC bank stored as 'GAMP'
C!
C! Called from QFILL
C!======================================================================
#ifndef DOC
      SAVE INIT,IERR,IGAMP,NGAMP
#include "qcde.h"
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      PARAMETER ( ITSMB = 216 , IRFL1 = 1700 , IRFL2 = 1799 )
      DATA INIT /0/, IERR /0/
#include "qmacro.h"
C
      JR(I) = KOQVEC + ITK * KCQVEC + I
      JQDET(I,N) = KOQDET + (KFFRD+I-1) * KCQDET + N
C----------------------------------------------------------------------
      IF (INIT .EQ. 0) THEN
        INIT = 1
        IGAMP = KPCOMP ('GAMP')
        NGAMP = KFPADR(IGAMP)
      ENDIF
C
      JEGPC = IW(NAEGPC)
      JPGPC = IW(NAPGPC)
      JPGID = IW(NAPGID)
      JPGAC = IW(NAPGAC)
C
C Special patch to avoid crashes when processing some MC Florida tapes
C written with TPCSIM 216
C
      IF (KRUN.LE.2000.AND.INDATA.LT.5) THEN
         IREGM = 0
         JKRUN = IW(NAKRUN)
         JTSIM = IW(NAMIND('TSIM'))
         IF (JKRUN.EQ.0.OR.JTSIM.EQ.0) GO TO 3
         IVTPC = ITABL(JTSIM,1,1)
         IF (IVTPC.NE.ITSMB) GO TO 3
         IRAN1 = ITABL(JKRUN,1,JKRUFS)
         IF (IRAN1.LT.IRFL1.OR.IRAN1.GT.IRFL2) GO TO 3
C Bad data : one has to rerun GAMPEX :
         CALL BKFMT('PGPC',' 2I,(8F,I,8F,I)')
         CALL BDROP(IW,'PGPC')
         CALL GAPGPC
         NAPGPC = NAMIND('PGPC')
         JPGPC = IW(NAPGPC)
         CALL BLIST(IW,'S+','PGPC')
         CALL BKFMT('PGID','2I,(I,8F,I)')
         CALL BDROP(IW,'PGID')
         CALL GAPGID
         NAPGID = NAMIND('PGID')
         JPGID = IW(NAPGID)
         CALL BLIST(IW,'S+','PGID')
         GO TO 4
 3       CONTINUE
      ENDIF
C
C If data card 'REGA' provided by the user AND IF one reads a POT/DST
C AND IF 'PGPC' is not present , build it using the ALEPHLIB routine
C GAPGPC , and build 'PGID' using GAPGID
C
      IF (IW(NAMIND('REGA')).GT.0.AND.JPGPC.EQ.0.AND.INDATA.LT.5) THEN
          CALL BDROP(IW,'EGPC')
          JEGPC=0
          NAEGPC=NAMIND('EGPC')
          CALL GAPGPC
          NAPGPC=NAMIND('PGPC')
          JPGPC = IW(NAPGPC)
          CALL BLIST(IW,'S+','PGPC')
      ENDIF
C Same for 'PGID' bank :
      IF (IW(NAMIND('REGA')).GT.0.AND.JPGID.EQ.0.AND.INDATA.LT.5) THEN
          CALL BDROP(IW,'EGID')
          NAEGID=NAMIND('EGID')
          CALL GAPGID
          NAPGID=NAMIND('PGID')
          JPGID = IW(NAPGID)
          CALL BLIST(IW,'S+','PGID')
      ENDIF
C
C If data card 'REPG' provided by the user AND IF one reads a POT/DST
C AND IF 'PGAC' is not present , build it using the ALEPHLIB routine
C GAPGAC
C
      IF (IW(NAMIND('REPG')).GT.0.AND.JPGAC.EQ.0.AND.INDATA.LT.5) THEN
          CALL BDROP(IW,'EGPC')
          JEGPC=0
          NAEGPC=NAMIND('EGPC')
          CALL BDROP(IW,'PGPC')
          JPGPC=0
          NAPGPC=NAMIND('PGPC')
          CALL GAPGAC
          NAPGAC=NAMIND('PGAC')
          JPGAC = IW(NAPGAC)
          CALL BLIST(IW,'S+','PGAC')
      ENDIF
 4    CONTINUE
      IF (JEGPC.EQ.0.AND.JPGPC.EQ.0.AND.JPGAC.EQ.0) GO TO 999
C PGAC takes always precedence over old 'PGPC' :
C PGPC takes always precedence over old 'EGPC ' :
      IPPG=0
      IF (JPGAC.GT.0) THEN
         KNGAT = IW(JPGAC+LMHLEN)
         IPPG=3
         GO TO 5
      ENDIF
      IF (JPGPC.GT.0) THEN
         KNGAT = IW(JPGPC+LMHLEN)
         IPPG=1
         GO TO 5
      ENDIF
      IF (JEGPC.GT.0) THEN
         KNGAT = IW(JEGPC+LMHLEN)
         IPPG=2
      ENDIF
 5    IF (IPPG.EQ.0) GO TO 999
C
      KFGAT = KFFRT
      KLGAT=KFGAT + KNGAT -1
C
      IF (KLGAT .GE. KLFRT)  CALL QSBANK ('QVEC', KLGAT+200)
      IF (KFFRD + KNGAT -1 .GE. IW(KOQDET))
     &   CALL QSBANK ('QDET', KFFRD + KNGAT -1 + 100)
C
      DO 10 I=1,KNGAT
        ITK=KFGAT+I-1
        IF (IPPG.EQ.2) THEN
           PX = RTABL(JEGPC,I,JEGPPX)
           PY = RTABL(JEGPC,I,JEGPPY)
           PZ = RTABL(JEGPC,I,JEGPPZ)
           PN = SQRT( PX**2 + PY**2 + PZ**2)
        ENDIF
        IF (IPPG.EQ.1) THEN
           PN = RTABL(JPGPC,I,JPGPEC)
           TC = RTABL(JPGPC,I,JPGPTC)
           PC = RTABL(JPGPC,I,JPGPPC)
           PX = PN * SIN(TC) * COS(PC)
           PY = PN * SIN(TC) * SIN(PC)
           PZ = PN * COS(TC)
        ENDIF
        IF (IPPG.EQ.3) THEN
           PN = RTABL(JPGAC,I,JPGAEC)
           TC = RTABL(JPGAC,I,JPGATC)
           PC = RTABL(JPGAC,I,JPGAPC)
           PX = PN * SIN(TC) * COS(PC)
           PY = PN * SIN(TC) * SIN(PC)
           PZ = PN * COS(TC)
        ENDIF
        RW(JR(JQVEQX)) = PX
        RW(JR(JQVEQY)) = PY
        RW(JR(JQVEQZ)) = PZ
        RW(JR(JQVEQP)) = PN
        RW(JR(JQVEQE)) = PN
        RW(JR(JQVEQM)) = 0.
        RW(JR(JQVECH)) = 0.
        IW(JR(JQVETN)) = I
        IW(JR(JQVEPA)) = IGAMP
        IW(JR(JQVENP)) = IW(KOQFPA+NGAMP*KCQFPA+1)
        IW(KOQFPA+NGAMP*KCQFPA+1) = ITK
        IW(JR(JQVESC)) = 1
        IW(JR(JQVEKS)) = 0
        IW(JR(JQVECL)) = 1
        IW(JR(JQVEQD)) = JQDET(I,0)
        IW(JR(JQVESP)) = ITK
        IW(JR(JQVEOV)) = 0
        IW(JR(JQVEEV)) = 0
        IW(JR(JQVEND)) = 0
        IW(JR(JQVENO)) = 0
        IW(JR(JQVENM)) = 0
          DO 9 IDUM=1,KLOCKM
            IW(JR(JQVEBM+IDUM-1)) = 0
9         CONTINUE
        IW(JR(JQVELK)) = 0
        RW(JR(JQVEEW)) = 0.
        RW(JR(JQVEDB)) = 0.
        RW(JR(JQVEZB)) = 0.
        RW(JR(JQVESD)) = 0.
        RW(JR(JQVESZ)) = 0.
        RW(JR(JQVEEM)) = -1.
        RW(JR(JQVECF)) = -1.
C---Fill QDET
          IW(JQDET(I,JQDEAF)) = KQZER
          IW(JQDET(I,JQDEAL)) = KQZER
          IW(JQDET(I,JQDENT)) = 0
          IW(JQDET(I,JQDEAT)) = KQZER
          IW(JQDET(I,JQDEAE)) = KQZER
          IW(JQDET(I,JQDEAH)) = KQZER
          IW(JQDET(I,JQDEAM)) = KQZER
          IW(JQDET(I,JQDECF)) = 0
          IW(JQDET(I,JQDEEC)) = KQZER
          IW(JQDET(I,JQDEHC)) = KQZER
          IW(JQDET(I,JQDEET)) = KQZER
          IW(JQDET(I,JQDEFI)) = KQZER
          IW(JQDET(I,JQDENF)) = 0
          IW(JQDET(I,JQDENE)) = 0
          IW(JQDET(I,JQDENH)) = 0
          IW(JQDET(I,JQDEEF)) = KQZER
          IW(JQDET(I,JQDEPC)) = KQZER
          IF (IPPG.EQ.2) THEN
             IW(JQDET(I,JQDEEG)) = JEGPC + LMHLEN + (I-1)*LCOLS(JEGPC)
             IW(JQDET(I,JQDEPG)) = KQZER
          ENDIF
          IF (IPPG.EQ.1) THEN
             IW(JQDET(I,JQDEPG)) = JPGPC + LMHLEN + (I-1)*LCOLS(JPGPC)
             IW(JQDET(I,JQDEEG)) = KQZER
          ENDIF
          IF (IPPG.EQ.3) THEN
             IW(JQDET(I,JQDEPG)) = JPGAC + LMHLEN + (I-1)*LCOLS(JPGAC)
             IW(JQDET(I,JQDEEG)) = KQZER
          ENDIF
          IW(JQDET(I,JQDEMU)) = KQZER
C--- set bit mask
        CALL QSBITM(ITK)
   10 CONTINUE
C   UPDATE KLOCUS POINTERS.
      DO 20 N = KSGAT+1,KSMCT
        KLOCUS(1,N) = KLOCUS(2,KSGAT) + 1
   20 KLOCUS(2,N) = KLOCUS(2,KSGAT)
      KFFRT = KLOCUS(2,KSGAT) + 1
      KFFRD=KFFRD+KNGAT
  999 RETURN
      END
#endif
