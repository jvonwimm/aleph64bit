      SUBROUTINE ALCLUS(NJET,TGEN,DMIN)
C
C----------------------------------------------------------------------
C! ALEPHLIB copy of the LUND algorithm LUCLUS.
CKEY PHYSICS LUND JETS MINI MDST /USER
C
C     Author:    T.Sjostrand
C     Modified:  S.Haywood
C
C     Called by RMIMJT in Mini-DST code.
C
C     Input:     by commons contained in comdeck LUNDCOM (and LUNDEQU).
C                These are filled partially by the initialisation
C                routine AINJET and partially by the user.
C     Output:    NJET   = number of jets found
C                TGEN, DMIN see LUND writeup.
C
C  Description
C  -----------
C     See LUND Monte-Carlo write-up: CERN Pool program W5035.
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "lundcom.h"
#include "lundequ.h"
C
C      SUBROUTINE LUCLUS(NJET,TGEN,DMIN)
C+CDE,LUJETS.
C+CDE,LUDAT1.
C+CDE,LUDATE.
C...MOMENTA AND SUM OF MOMENTA FOR PARTICLES
C...(P(I,4) IS TEMPORARILY USED TO REPRESENT ABSOLUTE MOMENTA)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C ---------------------------------------------------------------
      IF(FIRST)THEN
         CALL AINJET
         FIRST=.FALSE.
      ENDIF
      NP=0
      PS=0.
      DO 100 I=1,N
      IF(K(I,1).GE.20000) GOTO 100
      NP=NP+1
      P(I,4)=SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2)
      PS=PS+P(I,4)
  100 CONTINUE
      IF(NP.LE.2*IABS(MSTE(22))) THEN
C...VERY LOW MULTIPLICITIES NOT CONSIDERED
        NJET=-1
        TGEN=-1.
        DMIN=-1.
        RETURN
      ENDIF
      NL=0
      IF(MSTE(22).GE.0) THEN
C...FIND INITIAL JET CONFIGURATION. IF TOO FEW JETS, MAKE HARDER CUTS
        DINIT=1.25*PARE(32)
  110   DINIT=0.8*DINIT
C...SUM UP SMALL MOMENTUM REGION, JET IF ENOUGH ABSOLUTE MOMENTUM
        NJET=0
        NA=0
        DO 120 J=1,3
  120   P(N+1,J)=0.
        DO 140 I=1,N
        IF(K(I,1).GE.20000) GOTO 140
        K(I,1)=0
        IF(P(I,4).GT.2.*DINIT) GOTO 140
        NA=NA+1
        K(I,1)=1
        DO 130 J=1,3
  130   P(N+1,J)=P(N+1,J)+P(I,J)
  140   CONTINUE
        P(N+1,4)=SQRT(P(N+1,1)**2+P(N+1,2)**2+P(N+1,3)**2)
        IF(P(N+1,4).GT.2.*DINIT) NJET=1
        IF(DINIT.GE.0.2*PARE(32).AND.NJET+NP-NA.LT.2*IABS(MSTE(22)))
     &  GOTO 110
C...FIND FASTEST PARTICLE, SUM UP JET AROUND IT. ITERATE UNTIL ALL
C...PARTICLES USED UP
  150   NJET=NJET+1
        IF(MST(23).GE.1.AND.N+2*NJET.GE.MST(30)-5-MST(31)) THEN
          NJET=-2
          TGEN=-2.
          DMIN=-2.
          RETURN
        ENDIF
        PMAX=0.
        DO 160 I=1,N
        IF(K(I,1).NE.0.OR.P(I,4).LE.PMAX) GOTO 160
        IM=I
        PMAX=P(I,4)
  160   CONTINUE
        DO 170 J=1,3
  170   P(N+NJET,J)=0.
        DO 190 I=1,N
        IF(K(I,1).NE.0) GOTO 190
        D2=(P(I,4)*P(IM,4)-P(I,1)*P(IM,1)-P(I,2)*P(IM,2)-
     &  P(I,3)*P(IM,3))*2.*P(I,4)*P(IM,4)/(P(I,4)+P(IM,4))**2
        IF(D2.GT.DINIT**2) GOTO 190
        NA=NA+1
        K(I,1)=NJET
        DO 180 J=1,3
  180   P(N+NJET,J)=P(N+NJET,J)+P(I,J)
  190   CONTINUE
        P(N+NJET,4)=SQRT(P(N+NJET,1)**2+P(N+NJET,2)**2+P(N+NJET,3)**2)
        IF(DINIT.GE.0.2*PARE(32).AND.NJET+NP-NA.LT.2*IABS(MSTE(22)))
     &  GOTO 110
        IF(NA.LT.NP) GOTO 150
      ELSE
C...USE GIVEN INITIAL JET CONFIGURATION
        DO 200 IT=N+1,N+NJET
  200   P(IT,4)=SQRT(P(IT,1)**2+P(IT,2)**2+P(IT,3)**2)
      ENDIF
C...ASSIGN ALL PARTICLES TO NEAREST JET, SUM UP NEW JET MOMENTA
  210 TSAV=0.
  220 DO 230 IT=N+NJET+1,N+2*NJET
      DO 230 J=1,3
  230 P(IT,J)=0.
      DO 270 I=1,N
      IF(K(I,1).GE.20000) GOTO 270
      IF(MSTE(23).EQ.1) THEN
C...SYMMETRIC DISTANCE MEASURE BETWEEN PARTICLE AND JET
        D2MIN=1E10
        DO 240 IT=N+1,N+NJET
        IF(P(IT,4).LT.DINIT) GOTO 240
        D2=(P(I,4)*P(IT,4)-P(I,1)*P(IT,1)-P(I,2)*P(IT,2)-
     &  P(I,3)*P(IT,3))*2.*P(I,4)*P(IT,4)/(P(I,4)+P(IT,4))**2
        IF(D2.GE.D2MIN) GOTO 240
        IM=IT
        D2MIN=D2
  240   CONTINUE
      ELSE
C..."MULTICITY" DISTANCE MEASURE BETWEEN PARTICLE AND JET
        PMAX=-1E10
        DO 250 IT=N+1,N+NJET
        IF(P(IT,4).LT.DINIT) GOTO 250
        PROD=(P(I,1)*P(IT,1)+P(I,2)*P(IT,2)+P(I,3)*P(IT,3))/P(IT,4)
        IF(PROD.LE.PMAX) GOTO 250
        IM=IT
        PMAX=PROD
  250   CONTINUE
      ENDIF
      K(I,1)=IM-N
      DO 260 J=1,3
  260 P(IM+NJET,J)=P(IM+NJET,J)+P(I,J)
  270 CONTINUE
C...ABSOLUTE VALUE AND SUM OF JET MOMENTA, FIND TWO CLOSEST JETS
      PSJT=0.
      DO 280 IT=N+NJET+1,N+2*NJET
      P(IT,4)=SQRT(P(IT,1)**2+P(IT,2)**2+P(IT,3)**2)
  280 PSJT=PSJT+P(IT,4)
      D2MIN=1E10
      DO 290 IT1=N+NJET+1,N+2*NJET-1
      DO 290 IT2=IT1+1,N+2*NJET
      D2=(P(IT1,4)*P(IT2,4)-P(IT1,1)*P(IT2,1)-P(IT1,2)*P(IT2,2)-
     &P(IT1,3)*P(IT2,3))*2.*P(IT1,4)*P(IT2,4)/
     &MAX(0.01,P(IT1,4)+P(IT2,4))**2
      IF(D2.GE.D2MIN) GOTO 290
      IM1=IT1
      IM2=IT2
      D2MIN=D2
  290 CONTINUE
C...IF ALLOWED, JOIN TWO CLOSEST JETS AND START OVER
      IF(NJET.GT.IABS(MSTE(22)).AND.D2MIN.LT.PARE(33)**2) THEN
        NR=1
        DO 300 J=1,3
  300   P(N+NR,J)=P(IM1,J)+P(IM2,J)
        P(N+NR,4)=SQRT(P(N+NR,1)**2+P(N+NR,2)**2+P(N+NR,3)**2)
        DO 320 IT=N+NJET+1,N+2*NJET
        IF(IT.EQ.IM1.OR.IT.EQ.IM2) GOTO 320
        NR=NR+1
        DO 310 J=1,5
  310   P(N+NR,J)=P(IT,J)
  320   CONTINUE
        NJET=NJET-1
        GOTO 210
C...DIVIDE UP BROAD JET IF EMPTY CLUSTER IN LIST OF FINAL ONES
      ELSEIF(NJET.EQ.IABS(MSTE(22)).AND.NL.LE.2) THEN
        DO 330 IT=N+1,N+NJET
  330   K(IT,2)=0
        DO 340 I=1,N
  340   IF(K(I,1).LT.20000) K(N+K(I,1),2)=K(N+K(I,1),2)+1
        IM=0
        DO 350 IT=N+1,N+NJET
  350   IF(K(IT,2).EQ.0) IM=IT
        IF(IM.NE.0) THEN
          NL=NL+1
          IR=0
          D2MAX=0.
          DO 360 I=1,N
          IF(K(I,1).GE.20000) GOTO 360
          IF(K(N+K(I,1),2).LE.1.OR.P(I,4).LT.DINIT) GOTO 360
          IT=N+NJET+K(I,1)
          D2=(P(I,4)*P(IT,4)-P(I,1)*P(IT,1)-P(I,2)*P(IT,2)-
     &    P(I,3)*P(IT,3))*2.*P(I,4)*P(IT,4)/(P(I,4)+P(IT,4))**2
          IF(D2.LE.D2MAX) GOTO 360
          IR=I
          D2MAX=D2
  360     CONTINUE
          IF(IR.EQ.0) GOTO 390
          IT=N+NJET+K(IR,1)
          DO 370 J=1,3
          P(IM+NJET,J)=P(IR,J)
  370     P(IT,J)=P(IT,J)-P(IR,J)
          P(IM+NJET,4)=P(IR,4)
          P(IT,4)=SQRT(P(IT,1)**2+P(IT,2)**2+P(IT,3)**2)
          DO 380 IT=N+1,N+NJET
          DO 380 J=1,5
  380     P(IT,J)=P(IT+NJET,J)
          IF(NL.LE.2) GOTO 210
        ENDIF
      ENDIF
C...IF GENERALIZED THRUST HAS NOT YET CONVERGED, CONTINUE ITERATION
  390 TGEN=PSJT/PS
      IF(TGEN.GT.TSAV+PARE(34).AND.NL.LE.2) THEN
        TSAV=TGEN
        DO 400 IT=N+1,N+NJET
        DO 400 J=1,5
  400   P(IT,J)=P(IT+NJET,J)
        GOTO 220
      ENDIF
C...REORDER JETS AFTER MOMENTUM, SUM UP JET ENERGIES AND MULTIPLICITIES
      DO 420 IT=N+1,N+NJET
      PMAX=0.
      DO 410 IR=N+NJET+1,N+2*NJET
      IF(P(IR,4).LE.PMAX) GOTO 410
      IM=IR
      PMAX=P(IR,4)
  410 CONTINUE
      K(IM,1)=IT-N
      P(IM,4)=-1.
      K(IT,1)=IT-N
      K(IT,2)=0
      P(IT,4)=0.
      DO 420 J=1,3
  420 P(IT,J)=P(IM,J)
      DO 430 I=1,N
      IF(K(I,1).GE.20000) GOTO 430
      K(I,1)=K(N+NJET+K(I,1),1)
      P(I,4)=SQRT(P(I,5)**2+P(I,1)**2+P(I,2)**2+P(I,3)**2)
      K(N+K(I,1),2)=K(N+K(I,1),2)+1
      P(N+K(I,1),4)=P(N+K(I,1),4)+P(I,4)
  430 CONTINUE
      IM=0
      DO 440 IT=N+1,N+NJET
      IF(K(IT,2).EQ.0) IM=IT
  440 P(IT,5)=SQRT(MAX(P(IT,4)**2-P(IT,1)**2-P(IT,2)**2-P(IT,3)**2,0.))
C...VALUES AT RETURN (NEGATIVE FOR FAILURE FIXED NUMBER OF CLUSTERS)
      DMIN=SQRT(D2MIN)
      IF(NJET.EQ.1) DMIN=0.
      MST(3)=NJET
      IF(IM.NE.0) THEN
        NJET=-1
        TGEN=-1.
        DMIN=-1.
      ENDIF
      RETURN
      END
#endif
