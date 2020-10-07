      SUBROUTINE EXTRHC(ICHT)
C--------------------------------------------------------------------
C! Extrapolates track ICHT up to the HCAL to prepare a re-link
C  with isolated HCAL clusters.
C
C P. Janot.   13-jun-1991
C--------------------------------------------------------------------
#ifndef DOC
#include "intval.h"
      DIMENSION pintr(6), poutr(6), pin(7)
      DATA fieldb/15./
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qhac.h"
#include "qmacro.h"
C--------------------------------------------------------------------
      qqq = qch(icht)
C
C  First extrapolate the track up to ECAL entrance
C
      rcyl = 185.
      zcyl = 245.
      pintr(1) = qfrfir(icht)
      pintr(2) = qfrftl(icht)
      pintr(3) = qfrfp0(icht)
      pintr(4) = qfrfd0(icht)
      pintr(5) = qfrfz0(icht)
      pintr(6) = qfrfal(icht)
      CALL expfrf( rcyl, zcyl, pintr, poutr, icode)
      IF ( icode .EQ. 0 ) GOTO 999
C
C  Determine the ECAL energy linked to the track in each stack
C
      distmin = 999999.
      iecobj = 0
      eneobj = 0.
      DO 2 iecal = 1 , knecal(icht)
        iec = kecal(icht,iecal)
        ttt = sin(qpecth(iec)) / cos(qpecth(iec))
        xec = rcyl * cos(qpecph(iec))
        yec = rcyl * sin(qpecph(iec))
        zec = rcyl / ttt
        IF ( zec .GT. zcyl ) THEN
          zec = zcyl
          xec = xec/rcyl * zcyl * ttt
          yec = yec/rcyl * zcyl * ttt
        ELSEIF ( zec .LT. -zcyl ) THEN
          zec = -zcyl
          xec = -xec/rcyl * zcyl * ttt
          yec = -yec/rcyl * zcyl * ttt
        ENDIF
        dist = SQRT ( (xec-poutr(1))**2
     .              + (yec-poutr(2))**2
     .              + (zec-poutr(3))**2 )
        IF ( dist .LT. distmin ) THEN
          distmin = dist
          iecobj  = iec
          eneobj = qe(iecobj)
        ENDIF
    2 CONTINUE
      ene1 = 0.
      ene2 = 0.
      ene3 = 0.
      IF ( iecobj .NE. 0 .AND. distmin .LT. 4. ) THEN
        IF ( .NOT.xlock(iecobj) ) THEN
          IF ( qe(icht)-qpecec(iecobj) .LT. qm(icht)+0.001 ) GOTO 999
          ene1 = qpecec(iecobj)*qpece1(iecobj)
          ene2 = qpecec(iecobj)*qpece2(iecobj)
          ene3 = qpecec(iecobj) - ene1 - ene2
          IF ( ene3 .LT. 0. ) THEN
            ene1 = qpecec(iecobj)/3.
            ene2 = qpecec(iecobj)/3.
            ene3 = qpecec(iecobj)/3.
          ENDIF
        ENDIF
      ENDIF
C
C  Then extrapolate the track through the ECAL (3 stacks) up to
C  the coil entrance.
C
C --> end of 1st stack
      CALL ucopy(poutr(1), pin(1), 3)
      pin(4) = qpt(icht) * poutr(4) / qp(icht)
      pin(5) = qpt(icht) * poutr(5) / qp(icht)
      pin(6) = qpt(icht) * poutr(6) / qp(icht)
      ene    = qe(icht)-ene1/2.
      pin(7) = SQRT ( ene**2 - qm(icht)**2 )
      CALL vzero(poutr(1), 6)
      rcyl   = 196.
      zcyl   = 256.5
      CALL auhcyl(rcyl, zcyl, fieldb, qqq, pin, poutr, icode)
      IF ( icode .EQ. 0 ) GOTO 999
C --> end of 2nd stack
      CALL ucopy(poutr(1), pin(1), 6)
      ene = ene-(ene1+ene2)/2.
      pin(7) = SQRT ( ene**2 - qm(icht)**2 )
      CALL vzero(poutr(1), 6)
      rcyl   = 221.
      zcyl   = 283.
      CALL auhcyl(rcyl, zcyl, fieldb, qqq, pin, poutr, icode)
      IF ( icode .EQ. 0 ) GOTO 999
C --> end of 3rd stack
      CALL ucopy(poutr(1), pin(1), 6)
      ene = ene-(ene2+ene3)/2.
      pin(7) = SQRT ( ene**2 - qm(icht)**2 )
      CALL vzero(poutr(1), 6)
      rcyl   = 234.
      zcyl   = 297.
      CALL auhcyl(rcyl, zcyl, fieldb, qqq, pin, poutr, icode)
      IF ( icode .EQ. 0 ) GOTO 999
C  --> coil entrance
      CALL ucopy(poutr(1), pin(1), 6)
      ene = ene-ene3/2.
      pin(7) = SQRT ( ene**2 - qm(icht)**2 )
      CALL vzero(poutr(1), 6)
      rcyl   = 248.
      zcyl   = 314.99
      CALL auhcyl(rcyl, zcyl, fieldb, qqq, pin, poutr, icode)
      IF ( icode .EQ. 0 ) GOTO 999
C
C  Extrapolation up to the end of the coil (approximation : B=0!)
C
      CALL ucopy(poutr(1), pintr(1), 6)
      CALL vzero(poutr(1), 6)
      rcyl   = 292.
      zcyl   = 315.
      CALL aulcyl(rcyl, zcyl, pintr, poutr, icode)
      IF ( icode .EQ. 0 ) GOTO 999
C
C  Extrapolation up to the 'middle' of HCAL.
C  Modif 20 june 1991 . Try 3cm penetretation/GeV
C
      CALL ucopy(poutr(1), pin(1), 6)
      pin(7) = SQRT ( ene**2 - qm(icht)**2 )
      CALL vzero(poutr(1), 6)
C     rcyl   = 305. + qp(icht) * 3.
C     zcyl   = 320. + qp(icht) * 3.
      rcyl   = 375.
      zcyl   = 390.
      CALL auhcyl(rcyl, zcyl, fieldb,-qqq, pin, poutr, icode)
      IF ( icode .EQ. 0 ) GOTO 999
C
C  Determine the momentum of the resulting HCAL object
C
      dis  = SQRT ( poutr(1)**2 + poutr(2)**2 + poutr(3)**2 )
C     pxh  = ene * poutr(1) / dis
C     pyh  = ene * poutr(2) / dis
C     pzh  = ene * poutr(3) / dis
C     eh = SQRT(pxh**2+pyh**2+pzh**2+qm(icht)**2)
      kchatt = kchatt + 1
      IF ( kchatt .GT. mxtrk ) GOTO 999
      ncht(kchatt) = icht
      delt(kchatt) = ene
C     pexhca(kchatt,1) = pxh
C     pexhca(kchatt,2) = pyh
C     pexhca(kchatt,3) = pzh
      pexhca(kchatt,1) = poutr(1)
      pexhca(kchatt,2) = poutr(2)
      pexhca(kchatt,3) = poutr(3)
C
  999 RETURN
      END
#endif
