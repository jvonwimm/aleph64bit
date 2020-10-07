      SUBROUTINE QFCASC
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! Combine ECAL and HCAL if associated to each other
C
C! called from QFCALD
C!                                                   H.Albrecht 27.2.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT,ICALO,NCALO
#include "pcrljj.h"
#include "qcdesh.h"
      DATA INIT /0/
      JVH(I) = KOQVEC + NTKH * KCQVEC + I
      JVE(I) = KOQVEC + NTKE * KCQVEC + I
      JVR(I) = KOQVEC + NTKR * KCQVEC + I
      JVC(I) = KOQVEC + NTKC * KCQVEC + I
      JDH(I) = JQDET + NTKH * KCQDET + I
      JDE(I) = JQDET + NTKE * KCQDET + I
      JDR(I) = JQDET + NTKR * KCQDET + I
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
C       Loop over all HCAL objects with associated ECAL objects :
C
      DO 50 NTKH = KFEHT, KLEHT
        ICALFL = IW(JDH(JQDECF))
        IF (IABS(ICALFL) .EQ. 1)  GO TO 50
C
C         Associated to a charged track ?
C
        IF (ICALFL .GT. 0)  THEN
          KLIST = KLIST + 1
          NTKR = KLIST
        ELSE
          KFAST = KFAST - 1
          NTKR = KFAST
        ENDIF
C
C         Zero 4-vectors etc.
C
        RW(JVR(JQVEQX)) = 0.
        RW(JVR(JQVEQY)) = 0.
        RW(JVR(JQVEQZ)) = 0.
        RW(JVR(JQVEQE)) = 0.
        RW(JVR(JQVEQM)) = 0.
        RW(JVR(JQVEQP)) = 0.
        RW(JVR(JQVECH)) = 0.
C
C         error matrix
C
        RW(JVR(JQVEEM)) = 0.
        RW(JVR(JQVEEM+1)) = 0.
        RW(JVR(JQVEEM+2)) = 0.
        RW(JVR(JQVEEM+3)) = 0.
        RW(JVR(JQVEEM+4)) = 0.
        RW(JVR(JQVEEM+5)) = 0.
        RW(JVR(JQVEEM+6)) = 0.
        RW(JVR(JQVEEM+7)) = 0.
        RW(JVR(JQVEEM+8)) = 0.
        RW(JVR(JQVEEM+9)) = 0.
        RW(JVR(JQVECF)) = -1.
C
C         flags, pointers :
C
        IW(JVR(JQVETN)) = 0
        IW(JVR(JQVESC)) = 1
        IW(JVR(JQVEKS)) = 0
        IW(JVR(JQVECL)) = 1
        IW(JVR(JQVEPA)) = ICALO
        IW(JVR(JQVENP)) = IW(KOQFPA+NCALO*KCQFPA+1)
        IW(KOQFPA+NCALO*KCQFPA+1) = NTKR
        IW(JVR(JQVEQD)) = IW(JVH(JQVEQD))
        IW(JVR(JQVESP)) = NTKR
        IW(JVR(JQVEOV)) = 0
        IW(JVR(JQVEEV)) = 0
        IW(JVR(JQVEND)) = 0
        IW(JVR(JQVENO)) = 0
        IW(JVR(JQVENM)) = 0
        DO 9 IB=1,KLOCKM
          IW(JVR(JQVEBM+IB-1)) = 0
  9     CONTINUE
        IW(JVR(JQVELK)) = 0
        RW(JVR(JQVEDB)) = 0.
        RW(JVR(JQVEZB)) = 0.
        RW(JVR(JQVESD)) = 0.
        RW(JVR(JQVESZ)) = 0.
        RW(JVR(JQVECB)) = 0.
        RW(JVR(JQVEEW)) = 0.
C
C         add HCAL 4-vector
C
        CALL QFCADD (NTKR, NTKH)
        IW(KQWRK+1) = NTKH
C
C         loop over all associated ECAL objects :
C
        MECAL = IW(JDH(JQDENE))
        DO 40 NECAL = 1, MECAL
          NTKE = IW(KOQLIN+IW(JDH(JQDEEL))+NECAL)
C
C           add ECAL 4-momenta
C
          CALL QFCADD (NTKR, NTKE)
          IW(KQWRK+NECAL+1) = NTKE
C
   40   CONTINUE
C
C         now, set momentum
C
        RW(JVR(JQVEQP)) = SQRT (RW(JVR(JQVEQX))**2 +
     &    RW(JVR(JQVEQY))**2 + RW(JVR(JQVEQZ))**2)
C
C         daughter - mother relation
C
        CALL QADAU  (NTKR, MECAL+1, IW(KQWRK+1))
        CALL QAMOTH (NTKR, MECAL+1, IW(KQWRK+1))
C
   50 CONTINUE
C
      KNOVT = KLOVT - KFOVT + 1
      KNAST = KLAST - KFAST + 1
      KNIST = KLIST - KFIST + 1
C
      END
#endif
