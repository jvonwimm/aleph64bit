      SUBROUTINE X1PRNT
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/01/10  FOR ALEPHLIB 204

C.
C.
C! - Print Level 1 Trigger Information
C.
C.
C. - Called by      ASTRIG                        from .GALEPH
C ----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "x1tsto.h"
#include "alcons.h"
#include "bcs.h"
C.
C ----------------------------------------------------------------------
C.
      LOGICAL BTEST
      CHARACTER*90 PATRN
      DIMENSION IEQUI(2)
      EQUIVALENCE (IEQUI ,ITRG12)
C.
#include "bmacro.h"
C.
C.
C. - HcWireTriggerSignals
C.
C.
      WRITE(IW(6),6002) IHWSUM
 6002 FORMAT(//,' +++X1MIXN+++ HcWireTriggerSegments (Double Planes)'
     * //,
     * ' Endcap A        : ',I12,5I16,/,
     * ' Endcap A        : ',I12,5I16,/,
     * ' Overlap         : ',12I8,/,
     * ' Barrel          : ',12I8,/,
     * ' Barrel          : ',12I8,/,
     * ' Overlap         : ',12I8,/,
     * ' Endcap B        : ',I12,5I16,/,
     * ' Endcap B        : ',I12,5I16)
C.
C.
C. - EcWireTriggerSignals
C.
C.
      WRITE(IW(6),6004) (IEWSUM(2*J-1),J= 1,12),
     *                   (IEWSUM(2*J  ),J= 1,12),
     *                   (IEWSUM(2*J-1),J=13,24),
     *                   (IEWSUM(2*J  ),J=13,24),
     *                   (IEWSUM(2*J-1),J=25,36),
     *                   (IEWSUM(2*J  ),J=25,36)
 6004 FORMAT(//,' +++X1MIXN+++ EcWireTriggerSignals  (MeV)',//,
     * ' Endcap A (odd ) : ',12I8,/,
     * ' Endcap A (even) : ',12I8,/,
     * ' Barrel   (odd ) : ',12I8,/,
     * ' Barrel   (even) : ',12I8,/,
     * ' Endcap B (odd ) : ',12I8,/,
     * ' Endcap B (even) : ',12I8)
C.
      WRITE(IW(6),6104) IECWTE
 6104 FORMAT(//,' +++X1MIXN+++ EcWireTotalEnergy (MeV)',//,
     * '   Endcap A (odd) :',I9,
     * '   Endcap B (odd) :',I9,
     * '   Barrel (odd)   :',I9,
     * '   Total (odd)    :',I9,/,
     * '   Endcap A (even):',I9,
     * '   Endcap B (even):',I9,
     * '   Barrel (even)  :',I9,
     * '   Total (even)   :',I9)
C.
C.
C. - LcWireTriggerSignals
C.
C.
      WRITE(IW(6),6005) ILWSUM
 6005 FORMAT(//,' +++X1MIXN+++ LcWireTriggerSignals  (MeV)',//,
     * ' Endcap B    : ', 4I9,/,
     * ' Endcap A    : ', 4I9,/)
C.
C. - SICAL  TriggerSignals
C.
      CALL SIXAPR
C.
C.
C.
C. - ITcLevel1TrackSignals
C.
C.
      PATRN = '     Endcap A        Overlap              Ba'//
     *        'rrel               Overlap       Endcap B   '
      WRITE(IW(6),6006) PATRN
 6006 FORMAT(//,' +++X1MIXN+++ ItLevel1TrackSignals       ',//,A)
      PATRN = '  000000  000000  000000000000  000000000000'//
     *        '  000000000000  000000000000  000000  000000'
      J = 1
      K = 1
      DO 609 I=1,NSEGM
        IF (I.EQ.7.OR.I.EQ.67) J = J + 2
        IF (MOD(I,12).EQ.1)    J = J + 2
        IF (I.EQ.33.OR.I.EQ.65) K = K + 1
        II = I - 1
        IF (I.GT.32) II = II - 32
        IF (I.GT.64) II = II - 32
        IF (BTEST(IITSUM(K),II)) PATRN(J:J) = '1'
        J = J + 1
  609 CONTINUE
      WRITE(IW(6),6007) PATRN
 6007 FORMAT(/,A)
C.
C.
C. - Level 1 trigger pattern
C.
C.
      IF (NTRBIT.NE.0) THEN
        WRITE(IW(6),6015)
 6015   FORMAT(////' +++X1APTN+++ Event accepted by level 1 trigger',//)
C.
C.
C. - Coincidence pattern for the level 1 physics triggers
C.
C.
        PATRN = '  Trigger           Endcap A      Overlap   '//
     *          '     Barrel        Overlap       Endcap B   '
        WRITE(IW(6),7001) PATRN
 7001   FORMAT(//,' +++X1APTN+++ Trigger coincidence pattern',
     x            ' of accepted physics triggers  ',//,A)
C.
C ----------------------------------------------------------------------
C.
C.
C  - Fill accepted trigger segments for each trigger
C.
        DO 701 K=1,NPHTR
          IF(IEQUI(2*K).EQ.0.AND.IEQUI(2*K-1).EQ.0)  GOTO 701
          J = 15
          IF (K.EQ.1) THEN
            PATRN = '  Muon          000000  000000  000000000000'//
     *              '  000000000000  000000000000  000000  000000'
          ELSE IF (K.EQ.2) THEN
            PATRN = '  Charg.El.Mag. 000000  000000  000000000000'//
     *              '  000000000000  000000000000  000000  000000'
          ELSE IF (K.EQ.3) THEN
            PATRN = '  Neutr.El.Mag. 000000  000000  000000000000'//
     *              '  000000000000  000000000000  000000  000000'
          ELSE IF (K.EQ.4) THEN
            PATRN = '  Total Energy Barrel                       '//
     *              '                                            '
          ELSE IF (K.EQ.5) THEN
            PATRN = '  Total Energy Endcap_A                     '//
     *              '                                            '
          ELSE IF (K.EQ.6) THEN
            PATRN = '  Total Energy Endcap_B                     '//
     *              '                                            '
          ELSE IF (K.EQ.7) THEN
            PATRN = '  Total Energy Endcap_A*Endcap_B            '//
     *              '                                            '
          ELSE IF (K.EQ.8) THEN
            PATRN = '  Bhabha-Lcal A*B*(A+B)                     '//
     *              '                                            '
          ELSE IF (K.EQ.9) THEN
            PATRN = '  SICAL   coincidence AHIGH-BLOW '//
     *          'ALOW-BHIGH AVHIGH BVHIGH AVLOW BVLOW THERMOMETERS '
            WRITE(IW(6),6007) PATRN
            PATRN = '  SICAL Bhabha  000000  0000  0000  0000  0000  '//
     *              '                                            '
          ELSE
          GO TO 701
          ENDIF
          IF (K.GT.3.AND.K.LT.9) GOTO 7021
          DO 702 L=1,NFSEG
            IF (L.GT.NBITVW) THEN
              N  = 2*K - 1
              IS = L - NBITVW - 1
            ELSE
              N  = 2*K
              IS = L - 1
            ENDIF
            IF (K.LT.7) THEN
              IF (L.EQ.7.OR.L.EQ.55) J = J + 2
              IF (MOD(L,12).EQ.1)    J = J + 2
            ELSE IF (K.EQ.9) THEN
              IF (L.EQ.1)            J = J + 2
              IF (L.EQ.7)            J = J + 2
              IF (L.EQ.11)           J = J + 2
              IF (L.EQ.15)           J = J + 2
              IF (L.EQ.19)           J = J + 2
            ENDIF
            IF (BTEST(IEQUI(N),IS)) PATRN(J:J) = '1'
            J = J + 1
 702      CONTINUE
 7021     WRITE(IW(6),6007) PATRN
 701    CONTINUE
      ELSE
        WRITE(IW(6),6016)
 6016   FORMAT(////' +++X1APTN+++ Event NOT accepted by level 1 trigger'
     x         //)
      ENDIF
C.
      RETURN
      END
#endif
