      SUBROUTINE QGLUCL(MINCLU,DMAX1,DMAX2,MULSYM
     &                 ,NJET,TGEN,DMIN)
CKEY JETS /INTERNAL
C----------------------------------------------------------------------
C   Author   : P. Perez       20-APR-1989
C
C   Description
C   ===========
C!   Evaluate jet multiplicities based on the LUCLUS algorithm (JETSET)
C
C input :
C         MINCLU min. # of clusters to be reconstructed
C                 (if <0, work space momenta are used as a start)
C                 (usually=1)
C         DMAX1 max. distance to form starting clusters(usually=0.25GeV)
C         DMAX2  "      "     to join 2 clusters       (usually=2.5 GeV)
C         MULSYM = 1 for symmetric distance criterion (usual)
C                = 2  "  multicity    "
C         KTBI            # of particles           (from QCTBUF)
C         QTBIX,Y,Z,E(i)  four momentum (i=1,KTBI) ( "     "   )
C output: NJET number of reconstructed jets
C              = -1 if not enough particles
C              = -2 not enough working space (MXWORK)
C         TGEN generalized thrust
C         DMIN minimum distance between 2 jets
C              = 0  when only 1 jet
C              = -1 , -2 as for NJET
C         QTBOX,Y,Z,E(j)  four  momentum of jet j (j=1,NJET)(in QCTBUF)
C         KTBOF(i)        jet # of particle i     (i=1,KTBI)( "   "   )
C======================================================================
#ifndef DOC
      SAVE CONVRG
C
#include "qctbuf.h"
      DIMENSION P(KTBIMX+2*KTBOMX,5)
      DIMENSION K(KTBIMX+2*KTBOMX,2)
      DIMENSION MST(40), MSTE(40), PARE(40)
C
      DATA CONVRG /0.0001/
C ---------------------------------------------------------------
C
      MST (23) = 1
      MST (30) = KTBIMX + KTBOMX
      MST (31) = 0
      MSTE(22) = MINCLU
      MSTE(23) = MULSYM
      PARE(32) = DMAX1
      PARE(33) = DMAX2
      PARE(34) = CONVRG
C
      N = KTBI
C
C...momenta and sum of momenta for particles
C...(P(i,4) is temporarily used to represent absolute momenta)
      NP=0
      PS=0.
      DO 100 I=1,N
      KTBOF(I) = 0
      P(I,1) = QTBIX(I)
      P(I,2) = QTBIY(I)
      P(I,3) = QTBIZ(I)
      P(I,4) = SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2)
      P(I,5) = SQRT(MAX((QTBIE(I)**2 - P(I,4)**2),0.))
      IF(K(I,1).GE.20000) GOTO 100
      NP=NP+1
      PS=PS+P(I,4)
  100 CONTINUE
      IF(NP.LE.2*IABS(MSTE(22))) THEN
C...very low multiplicities not considered
        NJET=-1
        TGEN=-1.
        DMIN=-1.
        GOTO 999
      ENDIF
      NL=0
      IF(MSTE(22).GE.0) THEN
C...find initial jet configuration. if too few jets, make harder cuts
        DINIT=1.25*PARE(32)
  110   DINIT=0.8*DINIT
C...sum up small momentum region, jet if enough absolute momentum
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
C...find fastest particle, sum up jet around it. iterate until all
C...particles used up
  150   NJET=NJET+1
        IF(MST(23).GE.1.AND.N+2*NJET.GE.MST(30)-5-MST(31)) THEN
          NJET=-2
          TGEN=-2.
          DMIN=-2.
          GOTO 999
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
C...use given initial jet configuration
        DO 200 IT=N+1,N+NJET
  200   P(IT,4)=SQRT(P(IT,1)**2+P(IT,2)**2+P(IT,3)**2)
      ENDIF
C...assign all particles to nearest jet, sum up new jet momenta
  210 TSAV=0.
  220 DO 230 IT=N+NJET+1,N+2*NJET
      DO 230 J=1,3
  230 P(IT,J)=0.
      DO 270 I=1,N
      IF(K(I,1).GE.20000) GOTO 270
      IF(MSTE(23).EQ.1) THEN
C...symmetric distance measure between particle and jet
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
C..."multicity" distance measure between particle and jet
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
C...absolute value and sum of jet momenta, find two closest jets
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
C...if allowed, join two closest jets and start over
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
C...divide up broad jet if empty cluster in list of final ones
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
C...if generalized thrust has not yet converged, continue iteration
  390 TGEN=PSJT/PS
      IF(TGEN.GT.TSAV+PARE(34).AND.NL.LE.2) THEN
        TSAV=TGEN
        DO 400 IT=N+1,N+NJET
        DO 400 J=1,5
  400   P(IT,J)=P(IT+NJET,J)
        GOTO 220
      ENDIF
C...reorder jets after momentum, sum up jet energies and multiplicities
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
      KTBOF(I) = K(I,1)
  430 CONTINUE
      IM=0
      DO 440 IT=N+1,N+NJET
        QTBOX(IT-N) = P(IT,1)
        QTBOY(IT-N) = P(IT,2)
        QTBOZ(IT-N) = P(IT,3)
        QTBOE(IT-N) = P(IT,4)
        IF(K(IT,2).EQ.0) IM=IT
  440 P(IT,5)=SQRT(MAX(P(IT,4)**2-P(IT,1)**2-P(IT,2)**2-P(IT,3)**2,0.))
C...values at return (negative for failure fixed number of clusters)
      DMIN=SQRT(D2MIN)
      IF(NJET.EQ.1) DMIN=0.
      MST(3)=NJET
      IF(IM.NE.0) THEN
        NJET=-1
        TGEN=-1.
        DMIN=-1.
      ENDIF
  999 RETURN
      END
#endif
