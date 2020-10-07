      SUBROUTINE QFCHGD
CKEY  FILL CHARGED /INTERNAL
C----------------------------------------------------------------------
C! Fill QVEC with reconstructed charged tracks from POT / DST
C Called from QFILL
C!                                                  H.Albrecht 20.09.88
C!                            Modified:             E.Blucher  19.04.90
C!              Modified for FRFT NR=2:             E.Blucher  04.04.91
C!                   Modified for MUID:             E.Blucher  18.08.92
C----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT,IER,IER2,ICHGD,NCHGD,AMASPI
#include "qcde.h"
      DATA INIT, IER, IER2 /0, 0, 0/
      DATA  TMX,TMX2 / 1000000. ,  1.E+10  /
C----------------------------------------------------------------------
C
      IF (QMFLDC .EQ. 0.)THEN
        IER=IER+1
        IF(IER.LE.5)CALL QWMESE
     +   ('_QFCHGD_ Mag. field = 0; CHT section not filled ')
        GOTO 999
      ENDIF
C
      IF (INIT .EQ. 0)  THEN
        INIT = 1
        N = KPCOMP ('PI+')
        AMASPI = RW(KOQPAR+N*KCQPAR+JQPAMA)
        ICHGD = KPCOMP ('CHARGED')
        NCHGD = KFPADR(ICHGD)
      ENDIF
      IF (XMINI.AND.IW(NAPMSC).GT.0) CALL QFPMSC
C
      JFRFT = IW(NAFRFT) + LMHLEN
C
C     IF(XFRF2)THEN
C        IFRF2=NLINK('FRFT',2)
C        IF(IFRF2.NE.0)THEN
C          JFRFT=IFRF2+LMHLEN
C        ELSE
C          IF(IER2.LT.5)CALL QWMESE
C    +    ('_QFCHGD_ FRFT NR=2 not available -- NR=0 used.')
C          IER2=IER2+1
C        ENDIF
C     ENDIF
C
      LFRFT = IW(JFRFT-1)
      KNCHT = IW(JFRFT)
C
      JFRTL = IW(NAFRTL) + LMHLEN
      LFRTL = IW(JFRTL-1)
      JFRID = IW(NAFRID) + LMHLEN
      LFRID = IW(JFRID-1)
C
      KLCHT = KLCHT + KNCHT
      IF (KLCHT .GE. KLFRT)  CALL QSBANK ('QVEC', KLCHT + 200)
      IF (KLCHT - KLUST .GE. IW(KOQDET))
     &   CALL QSBANK ('QDET', KLCHT - KLUST + 50)
      IF (KNQLIN + KNCHT .GE. IW(KOQLIN))
     &   CALL QSBANK ('QLIN', KNQLIN + KNCHT + 500)
C
      JQVEC = KOQVEC + KFCHT * KCQVEC
C       do not use the first row in KOQDET :
      JQDET = KOQDET + (KFCHT - KLUST) * KCQDET
      JQDET0 = JQDET - KCQDET
C----------------------------------------------------------------------
C  Loop on all tracks of bank FRFT :
C
C
      ITK = KFCHT
      DO 10 NJUL = 1, KNCHT
C
C         basic track attributes :
C
        PS = QMFLDC / RW(JFRFT+JFRFIR)
        IF (PS .GT. 0.)  THEN
          RW(JQVEC+JQVECH) = -1.
          PT = PS
        ELSE
          RW(JQVEC+JQVECH) = 1.
          PT = - PS
        ENDIF
C
        CP = COS (RW(JFRFT+JFRFP0))
        SP = SIN (RW(JFRFT+JFRFP0))
        TL = RW(JFRFT+JFRFTL)
C
        PX = PT * CP
        RW(JQVEC+JQVEQX) = PX
        PY = PT * SP
        RW(JQVEC+JQVEQY) = PY
        PZ = PT * TL
        RW(JQVEC+JQVEQZ) = PZ
        RW(JQVEC+JQVEQP) = SQRT (PT**2 + PZ**2)
C         set mass = mass (pi+)
        RW(JQVEC+JQVEQM) = AMASPI
        E = SQRT (RW(JQVEC+JQVEQM)**2 + RW(JQVEC+JQVEQP)**2)
        RW(JQVEC+JQVEQE) = E
        IOKMA=1
        IF (E.GT.TMX) IOKMA=0
        DO 1 IMAT=JFRFEM,JFRFC2-1
           IF (ABS(RW(JFRFT+IMAT)).GT.TMX2) IOKMA=0
 1      CONTINUE
C
C         error matrix
C
        IF(XFILEM)THEN
        IF (IOKMA.EQ.1) THEN
        DPDK = PS**2 / QMFLDC
        XY = 2. * PS * DPDK * CP * SP * RW(JFRFT+JFRFEM+3)
        SXX = (DPDK * CP)**2 * RW(JFRFT+JFRFEM) +
     &        (PS * SP)**2 * RW(JFRFT+JFRFEM+5) + XY
        RW(JQVEC+JQVEEM) = SXX
        SXY = CP * SP * (DPDK**2 * RW(JFRFT+JFRFEM) -
     &                   PS**2 * RW(JFRFT+JFRFEM+5)) +
     &        PS * DPDK * (SP**2 - CP**2) * RW(JFRFT+JFRFEM+3)
        RW(JQVEC+JQVEEM+1) = SXY
        SYY = (DPDK * SP)**2 * RW(JFRFT+JFRFEM) +
     &        (PS * CP)**2 * RW(JFRFT+JFRFEM+5) - XY
        RW(JQVEC+JQVEEM+2) = SYY
        SXZ = DPDK**2 * CP * TL * RW(JFRFT+JFRFEM) -
     &        PS * DPDK * (CP * RW(JFRFT+JFRFEM+1) -
     &                     SP * TL * RW(JFRFT+JFRFEM+3)) -
     &        PS**2 * SP * RW(JFRFT+JFRFEM+4)
        RW(JQVEC+JQVEEM+3) = SXZ
        SYZ = DPDK**2 * SP * TL * RW(JFRFT+JFRFEM) -
     &        PS * DPDK * (SP * RW(JFRFT+JFRFEM+1) +
     &                     CP * TL * RW(JFRFT+JFRFEM+3)) +
     &        PS**2 * CP * RW(JFRFT+JFRFEM+4)
        RW(JQVEC+JQVEEM+4) = SYZ
        SZZ = (DPDK * TL)**2 * RW(JFRFT+JFRFEM) +
     &        PS**2 * RW(JFRFT+JFRFEM+2) -
     &        2. * PS * DPDK * TL * RW(JFRFT+JFRFEM+1)
        RW(JQVEC+JQVEEM+5) = SZZ
C
        RW(JQVEC+JQVEEM+6) = (PX * SXX + PY * SXY + PZ * SXZ) / E
        RW(JQVEC+JQVEEM+7) = (PX * SXY + PY * SYY + PZ * SYZ) / E
        RW(JQVEC+JQVEEM+8) = (PX * SXZ + PY * SYZ + PZ * SZZ) / E
        RW(JQVEC+JQVEEM+9) = (PX**2 * SXX + PY**2 * SYY + PZ**2 * SZZ +
     &    2. * (PX * (PY * SXY + PZ * SXZ) + PY * PZ * SYZ)) / E**2
        ELSE
        CALL QWMESE
     +  ('_QFCHGD_ Unphysical momentum -- error matrix not filled')
        RW(JQVEC+JQVEEM) = -1.
        ENDIF
        ELSE
          RW(JQVEC+JQVEEM) = -1.
        ENDIF
C
        RW(JQVEC+JQVECF) = -1.
C
C         distance to interaction point :
C
        RW(JQVEC+JQVEDB) = RW(JFRFT+JFRFD0) +
     +    QVYNOM * CP - QVXNOM * SP
        RW(JQVEC+JQVEZB) = RW(JFRFT+JFRFZ0) - QVZNOM
     &   + RW(JFRFT+JFRFTL) * ( QVXNOM * CP + QVYNOM * SP )
        IF (IOKMA.EQ.0) GO TO 5
        RW(JQVEC+JQVESD) = RW(JFRFT+JFRFEM+9)
        RW(JQVEC+JQVESZ) = RW(JFRFT+JFRFEM+14)
        DET = RW(JQVEC+JQVESD) * RW(JQVEC+JQVESZ) -
     &        RW(JFRFT+JFRFEM+13)**2
        IF (DET .GT. 0.)  RW(JQVEC+JQVECB) =
     &   (RW(JQVEC+JQVEDB)**2 * RW(JQVEC+JQVESZ) +
     &    RW(JQVEC+JQVEZB)**2 * RW(JQVEC+JQVESD) -
     &    2. * RW(JQVEC+JQVEDB) * RW(JQVEC+JQVEZB) *
     &    RW(JFRFT+JFRFEM+13)) / DET
 5      CONTINUE
C
C         flags :
C
        IW(JQVEC+JQVETN) = NJUL
        IW(JQVEC+JQVESC) = 1
        IW(JQVEC+JQVEKS) = 0
        IW(JQVEC+JQVECL) = 1
        IW(JQVEC+JQVEPA) = ICHGD
        IW(JQVEC+JQVEQD) = JQDET
        IW(JQVEC+JQVENP) = ITK + 1
        IW(JQVEC+JQVESP) = ITK
        IW(JQVEC+JQVEOV) = 0
        IW(JQVEC+JQVEEV) = 0
        IW(JQVEC+JQVEND) = 0
        IW(JQVEC+JQVENO) = 0
        IW(JQVEC+JQVENM) = 0
        DO 9 IB=1,KLOCKM
          IW(JQVEC+JQVEBM+IB-1) = 0
 9      CONTINUE
        IW(JQVEC+JQVELK) = 0
        RW(JQVEC+JQVEEW) = 0.
C
C         bank QDET :
C
        IW(JQDET+JQDEAF) = JFRFT
        IW(JQDET+JQDEAL) = JFRTL
        IW(JQDET+JQDENT) = 0
        IW(JQDET+JQDEAT) = KQZER
        IW(JQDET+JQDEAE) = KQZER
        IW(JQDET+JQDEAH) = KQZER
        IW(JQDET+JQDEAM) = KQZER
        IW(JQDET+JQDECF) = 0
        IW(JQDET+JQDEEC) = KQZER
        IW(JQDET+JQDEHC) = KQZER
        IW(JQDET+JQDEET) = KQZER
        IW(JQDET+JQDEFI) = JFRID
        IW(JQDET+JQDENF) = 1
        IW(JQDET+JQDEFL) = KNQLIN
        KNQLIN = KNQLIN + 1
        IW(KOQLIN+KNQLIN) = ITK
        IW(JQDET+JQDENE) = 0
        IW(JQDET+JQDENH) = 0
        IW(JQDET+JQDEEF) = KQZER
        IW(JQDET+JQDEPC) = KQZER
        IW(JQDET+JQDEEG) = KQZER
        IW(JQDET+JQDEMU) = KQZER
        IW(JQDET+JQDEPD) = KQZER
        IW(JQDET+JQDEPM) = KQZER
C
        ITK = ITK + 1
        JQDET = JQDET + KCQDET
        JQVEC = JQVEC + KCQVEC
        JFRTL = JFRTL + LFRTL
        JFRID = JFRID + LFRID
   10 JFRFT = JFRFT + LFRFT
C----------------------------------------------------------------------
C       set first and last KPDIR / KFOLLO relation
C
      IW(KOQFPA+NCHGD*KCQFPA+1) = KFCHT
      IW(JQVEC+JQVENP-KCQVEC) = 0
C
C         bank TEXS
C
      JTEXS = IW(NATEXS)
      IF (JTEXS .NE. 0)  THEN
        LTEXS = IW(JTEXS+1)
        LAST = IW(JTEXS+2)
        JTEXS = JTEXS + LMHLEN
C
        DO 20 N=1,LAST
          JQDET = JQDET0 + IW(JTEXS+JTEXTN) * KCQDET
          IF (IW(JQDET+JQDENT) .LT. JQDELT - JQDEAT + 1)  THEN
            IW(JQDET+JQDEAT+IW(JQDET+JQDENT)) = JTEXS
            IW(JQDET+JQDENT) = IW(JQDET+JQDENT) + 1
          ENDIF
   20   JTEXS = JTEXS + LTEXS
      ENDIF
C
C         bank EIDT
C
      JEIDT = IW(NAEIDT)
      IF (JEIDT .NE. 0)  THEN
        LEIDT = IW(JEIDT+1)
        LAST = IW(JEIDT+2)
        JEIDT = JEIDT + LMHLEN
C
        DO 30 N=1,LAST
          IW(JQDET0+IW(JEIDT+JEIDFR)*KCQDET+JQDEAE) = JEIDT
   30   JEIDT = JEIDT + LEIDT
      ENDIF
C
C         bank HMAD
C
      JHMAD = IW(NAHMAD)
      IF (JHMAD .NE. 0)  THEN
        LHMAD = IW(JHMAD+1)
        LAST = IW(JHMAD+2)
        JHMAD = JHMAD + LMHLEN
C
        DO 40 N=1,LAST
          IW(JQDET0+IW(JHMAD+JHMATN)*KCQDET+JQDEAH) = JHMAD
   40   JHMAD = JHMAD + LHMAD
      ENDIF
C
C         bank MCAD
C
      JMCAD = IW(NAMCAD)
      IF (JMCAD .NE. 0)  THEN
        LMCAD = IW(JMCAD+1)
        LAST = IW(JMCAD+2)
        JMCAD = JMCAD + LMHLEN
C
        DO 50 N=1,LAST
          IW(JQDET0+IW(JMCAD+JMCATN)*KCQDET+JQDEAM) = JMCAD
   50   JMCAD = JMCAD + LMCAD
      ENDIF
C
C         bank MUID
C
      JMUID = IW(NAMUID)
      IF (JMUID .NE. 0)  THEN
        LMUID = IW(JMUID+1)
        LAST = IW(JMUID+2)
        JMUID = JMUID + LMHLEN
C
        DO 60 N=1,LAST
          IW(JQDET0+IW(JMUID+JMUITN)*KCQDET+JQDEMU) = JMUID
   60   JMUID = JMUID + LMUID
      ENDIF
C
C       loci :
C
      DO 80 N = KSCHT+1,KSMCT
        KLOCUS(1,N) = KLOCUS(2,KSCHT) + 1
   80 KLOCUS(2,N) = KLOCUS(2,KSCHT)
      KFFRT = KLOCUS(2,KSCHT) + 1
      KFFRD = KFFRT
C
999   END
#endif
