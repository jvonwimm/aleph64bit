      SUBROUTINE QFBITM
CKEY FILL MASKS /INTERNAL
C-----------------------------------------------------------------------
C! Fill bit masks for reconstructed particles
C!                                                   H.Albrecht 27.11.88
C!                                                   E.Blucher  25.07.90
C!                                    Modified       E.Blucher  03.02.93
C! called from QFILL to set bit masks for all reconstructed objects.
C! Entry pointQSBITM added to set bit mask for
C! single tracks -- called from KVSAVC.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR,INIT,IECAL,IHCAL,NECAL,NHCAL,NWRD,NBIT
#include "qcde.h"
      DATA IERR /0/, INIT /0/
#include "qmacro.h"
      JQVEC(I,N) = KOQVEC + I * KCQVEC + N
C-----------------------------------------------------------------------
C
      IF (INIT .EQ. 0)  THEN
        INIT = 1
        IECAL = KPCOMP ('ECAL')
        NECAL = KFPADR (IECAL)
        IHCAL = KPCOMP ('HCAL')
        NHCAL = KFPADR (IHCAL)
      ENDIF
C
      NBIT = 0
      NWRD = 0
C
C       Charged tracks
C
      DO 60 ITK=KFCHT,KLCHT
        IF (NBIT .LT. 32)  THEN
          NBIT = NBIT + 1
        ELSE
          NBIT = 1
          NWRD = NWRD + 1
          IF (NWRD .GE. KLOCKM)  GO TO 800
        ENDIF
        IW(JQVEC(ITK,JQVEBM+NWRD)) = KBIT(NBIT)
C
        DO 30 I=1,KNECAL(ITK)
          ITKC = KECAL(ITK,I)
          IW(JQVEC(ITKC,JQVEBM+NWRD)) =
     &      IOR (IW(JQVEC(ITKC,JQVEBM+NWRD)), KBIT(NBIT))
          DO 20 IM=1,KNMOTH(ITKC)
   20     IW(JQVEC(KMOTH(ITKC,IM),JQVEBM+NWRD)) =
     &       IOR (IW(JQVEC(KMOTH(ITKC,IM),JQVEBM+NWRD)), KBIT(NBIT))
   30   CONTINUE
C
        DO 50 I=1,KNHCAL(ITK)
          ITKC = KHCAL(ITK,I)
          IW(JQVEC(ITKC,JQVEBM+NWRD)) =
     &      IOR (IW(JQVEC(ITKC,JQVEBM+NWRD)), KBIT(NBIT))
          DO 40 IM=1,KNMOTH(ITKC)
   40     IW(JQVEC(KMOTH(ITKC,IM),JQVEBM+NWRD)) =
     &       IOR (IW(JQVEC(KMOTH(ITKC,IM),JQVEBM+NWRD)), KBIT(NBIT))
   50   CONTINUE
   60 CONTINUE
C
C       Ecal objects
C
      ITK = IW(KOQFPA+NECAL*KCQFPA+1)
  110 IF (ITK .EQ. 0)  GO TO 200
      IF (NBIT .LT. 32)  THEN
        NBIT = NBIT + 1
      ELSE
        NBIT = 1
        NWRD = NWRD + 1
        IF (NWRD .GE. KLOCKM)  GO TO 800
      ENDIF
      IW(JQVEC(ITK,JQVEBM+NWRD)) =
     &     IOR (IW(JQVEC(ITK,JQVEBM+NWRD)), KBIT(NBIT))
C
C            go along KSAME pointer
C
      ITKS = KSAME (ITK)
  120 IF (ITKS .EQ. ITK)  GO TO 130
      IW(JQVEC(ITKS,JQVEBM+NWRD)) =
     &     IOR (IW(JQVEC(ITKS,JQVEBM+NWRD)), KBIT(NBIT))
      ITKS = KSAME (ITKS)
      GO TO 120
C
C            composite cal objects
C
 130  IF (KNMOTH(ITK).LE.0) GO TO 150
      DO 140 I=1,KNMOTH(ITK)
  140 IW(JQVEC(KMOTH(ITK,I),JQVEBM+NWRD)) =
     &     IOR (IW(JQVEC(KMOTH(ITK,I),JQVEBM+NWRD)), KBIT(NBIT))
 150  CONTINUE
C
      ITK = IW(KOQVEC+ITK*KCQVEC+JQVENP)
      GO TO 110
C
C       Hcal objects
C
  200 ITK = IW(KOQFPA+NHCAL*KCQFPA+1)
  210 IF (ITK .EQ. 0)  GO TO 900
      IF (NBIT .LT. 32)  THEN
        NBIT = NBIT + 1
      ELSE
        NBIT = 1
        NWRD = NWRD + 1
        IF (NWRD .GE. KLOCKM)  GO TO 800
      ENDIF
      IW(JQVEC(ITK,JQVEBM+NWRD)) =
     &     IOR (IW(JQVEC(ITK,JQVEBM+NWRD)), KBIT(NBIT))
C
C            go along KSAME pointer
C
      ITKS = KSAME (ITK)
  220 IF (ITKS .EQ. ITK)  GO TO 230
      IW(JQVEC(ITKS,JQVEBM+NWRD)) =
     &     IOR (IW(JQVEC(ITKS,JQVEBM+NWRD)), KBIT(NBIT))
      ITKS = KSAME (ITKS)
      GO TO 220
C
C            composite cal objects
C
 230  IF (KNMOTH(ITK).LE.0) GO TO 250
      DO 240 I=1,KNMOTH(ITK)
  240 IW(JQVEC(KMOTH(ITK,I),JQVEBM+NWRD)) =
     &    IOR (IW(JQVEC(KMOTH(ITK,I),JQVEBM+NWRD)), KBIT(NBIT))
C
 250  CONTINUE
      ITK = IW(KOQVEC+ITK*KCQVEC+JQVENP)
      GO TO 210
C
C   entry point for single tracks
C
      ENTRY QSBITM(IDUM)
      IF (NBIT.GE.NBITW-1.AND.NWRD.GE.KLOCKM-1) GO TO 800
        IF (NBIT .LT. 32)  THEN
          NBIT = NBIT + 1
        ELSE
          NBIT = 1
          NWRD = NWRD + 1
          IF (NWRD .GE. KLOCKM)  GO TO 800
        ENDIF
        IW(JQVEC(IDUM,JQVEBM+NWRD)) = KBIT(NBIT)
      GOTO 900
C
C       more than 448 reconstructed objects
C
  800 IERR = IERR + 1
      IF (IERR .LE. 10)  CALL QWMESE
     &  ('_QFBITM_ More than 448 objects. Lock will not work')
C
  900 RETURN
      END
#endif
