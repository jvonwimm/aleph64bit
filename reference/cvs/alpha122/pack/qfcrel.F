      SUBROUTINE QFCREL
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! Check the relations between ECAL objects and HCAL objects.
C
C!called from QFCASC
C!                                                   H.Albrecht 27.2.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT,ICALO,NCALO
      LOGICAL FLAG
#include "qcdesh.h"
      DATA INIT /0/
      JDH(I) = JQDET + NTKH * KCQDET + I
      JDE(I) = JQDET + NTKE * KCQDET + I
      JDC(I) = JQDET + NTKC * KCQDET + I
      JVC(I) = KOQVEC + NTKC * KCQVEC + I
C----------------------------------------------------------------------
C
      JQDET = KOQDET - KLUST * KCQDET
C
      IF (INIT .EQ. 0) THEN
        INIT = 1
        ICALO = KPCOMP ('CALOBJ')
        NCALO = KFPADR (ICALO)
      ENDIF
C
C         loop over all associated ECAL objects assoc. to HCAL :
C
      DO 50 NTKE = KFEHT, KLEHT
        IF (IABS(IW(JDE(JQDECF))) .NE. 1)  GO TO 50
C
        NHCAL = 1
   10   IF (NHCAL .GT. IW(JDE(JQDENH)))  GO TO 40
        NTKH = IW(KOQLIN+IW(JDE(JQDEHL))+NHCAL)
C
C           Last chance to ignore the ECAL/HCAL relation :
C
        CALL QFCOMP (NTKE, NTKH, FLAG)
        IF (FLAG)  THEN
          NHCAL = NHCAL + 1
          GO TO 10
        ENDIF
C
C             remove relations
C
        IW(JDE(JQDENH)) = IW(JDE(JQDENH)) - 1
        NDIF = 0
        DO 20 N = 1, IW(JDE(JQDENH))
          IF (IW(KOQLIN+IW(JDE(JQDEHL))+N) .EQ. NTKH)  NDIF = 1
   20   IW(KOQLIN+IW(JDE(JQDEHL))+N) =
     &      IW(KOQLIN+IW(JDE(JQDEHL))+N+NDIF)
C
        IW(JDH(JQDENE)) = IW(JDH(JQDENE)) - 1
        NDIF = 0
        DO 30 N = 1, IW(JDH(JQDENE))
          IF (IW(KOQLIN+IW(JDH(JQDEEL))+N) .EQ. NTKE)  NDIF = 1
   30   IW(KOQLIN+IW(JDH(JQDEEL))+N) =
     &      IW(KOQLIN+IW(JDH(JQDEEL))+N+NDIF)
C
        GO TO 10
C
   40   IF (IW(JDE(JQDENH)) .NE. 0)  GO TO 50
C
C         ECAL not associated any more to an HCAL :
C
        IF (IW(JDE(JQDENF)) .EQ. 0)  THEN
C
C               ECAL is NOT ass. to a charged track. Copy into OV :
C               Object will be accessible in loops over reco. objects.
C
          IF (KFOVT - 1 .LE. KLUST)  THEN
            CALL QWMESE ('_QFCREL_ Not enough space')
          ELSE
            KFOVT = KFOVT - 1
            NTKC = KFOVT
            CALL QVCOPY (NTKC, NTKE)
            IW(JVC(JQVEPA)) = ICALO
            IW(JVC(JQVENP)) = IW(KOQFPA+NCALO*KCQFPA+1)
            IW(KOQFPA+NCALO*KCQFPA+1) = NTKC
          ENDIF
        ENDIF
C
C               Copy into AS :
C               Object will be accessible in loops over CAL objects.
C
        IF (KLAST + 1 .GE. KFEHT)  THEN
          CALL QWMESE ('_QFCREL_ Not enough space')
        ELSE
          KLAST = KLAST + 1
          NTKC = KLAST
          CALL QVCOPY (NTKC, NTKE)
          IW(JVC(JQVEPA)) = ICALO
          IW(JVC(JQVENP)) = IW(KOQFPA+NCALO*KCQFPA+1)
          IW(KOQFPA+NCALO*KCQFPA+1) = NTKC
        ENDIF
C
   50 CONTINUE
C
      END
#endif
