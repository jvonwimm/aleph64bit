      SUBROUTINE QFMCST
CKEY FILL MC /INTERNAL
C-----------------------------------------------------------------------
C! Fill stability code for MC particles
C!                                                  H.Albrecht 18.10.88
C!called from QFILMC
C-----------------------------------------------------------------------
#ifndef DOC
      PARAMETER (RITC=28.8,ZITC=100.,RTPC=180.,ZTPC=200.)
#include "qcdesh.h"
      DIMENSION MT(30),MD(30),MF(30)
C
      JQVEC(I,N) = KOQVEC + I * KCQVEC + N
      IQVEC(I,N) = IW (KOQVEC + I * KCQVEC + N)
      RQVEC(I,N) = RW (KOQVEC + I * KCQVEC + N)
      IDAUG(I,N) = IW (KOQLIN + IQVEC(I,JQVEDL) + N)
      VR2(I) = RW(KOQVRT+I*KCQVRT+JQVRVX)**2 +
     &         RW(KOQVRT+I*KCQVRT+JQVRVY)**2
      VZA(I) = ABS (RW(KOQVRT+I*KCQVRT+JQVRVZ))
C-----------------------------------------------------------------------
C
      DO 60 ITK=KFMCT,KLMCT
        IF (IQVEC(ITK,JQVENO) .NE. 0)  GO TO 60
C
        NF = 0
        NG = 0
        NT = ITK
C
   10   NG = NG + 1
        IF (NG .GT. 30)  THEN
          CALL QWMESE ('_QFMCST_ more than 30 generations')
          GO TO 60
        ENDIF
C
        IEND = IQVEC(NT,JQVEEV)
C not decaying ...
        IF (IEND .EQ. 0)  THEN
          IW(JQVEC(NT,JQVESC)) = 1
          IF (NF .NE. 0)  IW(JQVEC(NT,JQVESC)) = 3
          GO TO 50
        ENDIF
C
C treat charged and neutral particles in a different way :
        IF (RQVEC(NT,JQVECH) .NE. 0.)  THEN
C charged particle decaying outside ITC
          IF (VZA(IEND) .GT. ZITC .OR. VR2(IEND) .GT. RITC**2)
     &      GO TO 40
        ELSE
C neutral particle decaying outside TPC
          IF (VZA(IEND) .GT. ZTPC .OR. VR2(IEND) .GT. RTPC**2)
     &      GO TO 40
        ENDIF
C
        ND = 1
C unstable particle : -1 = resonance; -2 = finite decay length
        IW(JQVEC(NT,JQVESC)) = -1
C decaying at origin vertex :
        IF (IEND .EQ. IQVEC(NT,JQVEOV))  GO TO 30
        IW(JQVEC(NT,JQVESC)) = -2
C check energy conservation
        E = 0.
        DO 20 ID = 1, IQVEC(NT,JQVEND)
   20   E = E + RQVEC (IDAUG(NT,ID),JQVEQE)
C tolerance 20 MeV :
        IF (ABS (E - RQVEC(NT,JQVEQE)) .GT. 0.020)  THEN
          IF (NF .EQ. 0)  IW(JQVEC(NT,JQVESC)) = -3
          NF = 1
        ENDIF
C
   30   IF (ND .GT. IQVEC(NT,JQVEND))  GO TO 50
        MF(NG) = NF
        MT(NG) = NT
        MD(NG) = ND
        NT = IDAUG(NT,ND)
        GO TO 10
C
   40   IW(JQVEC(NT,JQVESC)) = 2
        IF (NF .NE. 0)  IW(JQVEC(NT,JQVESC)) = 3
C
   50   IF (NG .EQ. 1)  GO TO 60
        NG = NG - 1
        NF = MF(NG)
        NT = MT(NG)
        ND = MD(NG) + 1
        GO TO 30
C
   60 CONTINUE
C
      END
#endif
