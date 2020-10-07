      SUBROUTINE ALMMCL(YCUT,EVIST,ETHR,KTBO)
C----------------------------------------------------------------------
C   Author   : M.N Minard     31-JAN-1990
C              transformed from QGMMCL from ALPHA
C
C   Description
C   ===========
C!   Evaluate jet multiplicities based on the scaled invariant mass
C!   squared algorithm
C    from momentum vectors stored iN QCTBUF.INC
C
C  input   : YCUT            ycut value ( (M/EVIST)**2 )
C            EVIST           visible energy
C                            (if EVIST=0., it is computed from the
C                             input particle energies)
C            N               # of particles           (from LUJETS)
C            P (I,(1,2,3,4)) four momentum (i=1,N)    ( "     "   )
C  output  :
C            P (N+J,(1,2,3,4)four  momentum of jet j (j=1,NJET)(LUJETS)
C            K(N+I,1)       jet # of particle i     (i=1,N)
C            KTBO        -1  input  error (N/EVIST) (in   LUJETS)
C                        -2  error from individ. track
C                        -3  too many jets
C                       NJET # of jets (normal return)
C?
C!======================================================================
#ifndef DOC
#include "jetset.h"
C
      PARAMETER ( KTBIMX = 2000 , KTBOMX = 10)
      DIMENSION  PP(5,KTBIMX) , KP(2,KTBIMX)
      REAL MASS , MINMAS
      DATA CMIN / 0.30 /
C
C------------------------------------------------------------------
C   set initial values
C------------------------------------------------------------------
      NJET = 0
      EVIS=EVIST
      IF(N.GT.KTBIMX) THEN
C        WRITE(LOUTIO,*) ' ***MMCLUS ktbi=',KTBI,'>ktbimx'
        IRET = -1
        GOTO 999
      ENDIF
C
      IRET = 1
C
C-    Initialise array for jets
C
C
C-    Initialise jets pointer
C
      CALL VZERO(KP(1,1),2*KTBIMX)
C
      DO 11 I =1, KTBIMX
        K(I,1) = 0
   11 CONTINUE
C
C-    Fill working array PP(5,LNJPAR)
C
      EVISI = 0.
      IE = 0
      DO 1 I = 1, N
        IF (P(I,4).LT.CMIN) GO TO 1
        IE = IE + 1
        KP(2,IE) = I
        PP(1,IE)  = P(I,1)
        PP(2,IE)  = P(I,2)
        PP(3,IE)  = P(I,3)
        PP(4,IE)  = P(I,4)
        PP(5,IE)  = SQRT(PP(1,IE)**2+PP(2,IE)**2+PP(3,IE)**2)
        EVISI    = EVISI + P(I,4)
    1 CONTINUE
C
C-    Initialise end of common
C
      DO 10 I = IE+1,KTBIMX
        PP(1,I) = 0.
        PP(2,I) = 0.
        PP(3,I) = 0.
        PP(4,I) = 0.
        PP(5,I) = 0.
   10 CONTINUE
C
      IF(EVIS.EQ.0.) EVIS = EVISI
      EVIS2 = EVIS**2
C
      IF(EVIS .LE. 0.) THEN
        IRET = -1
        GOTO 999
      ENDIF
C
  100 MINMAS = 5.E8
C
      DO 2000 I = 1, IE-1
C
C skip if particle is already merged in a jet
        IF (PP(4,I).LT.0) GO TO 2000
        PI = PP(5,I)
C
        DO 2100 J = I+1, IE
C skip if particle is already merged in a jet
C
          IF ( PP(4,J).LT.0) GO TO 2100
          PJ = PP(5,J)
          IF (PI .LE. 0. .OR. PJ .LE. 0.) THEN
            IRET = -2
            GO TO 2100
          ENDIF
          COSANG = (PP(1,I)*PP(1,J)+PP(2,I)*PP(2,J)+PP(3,I)*PP(3,J))
          COSANG = COSANG / (PI*PJ)
C
          ACOSAN = ABS(COSANG)
          IF(ACOSAN .GT. 1.0001) THEN
            IRET = -2
            GOTO 999
          ELSEIF(ACOSAN.GT.1..AND.ACOSAN.LE.1.0001) THEN
            COSANG = SIGN(1.,COSANG)
          ENDIF
C
          MASS = 2. * PP(4,I) * PP(4,J) * (1.-COSANG)
C
          IF(MASS.LT.MINMAS) THEN
            MINMAS = MASS
            MINPA1 = I
            MINPA2 = J
          ENDIF
C
 2100   CONTINUE
 2000 CONTINUE
C
      KTBMAX = KTBOMX
      IF ( KTBMAX.GT.(KTBIMX-N-1)) KTBMAX = KTBIMX-N-1
      YIJMIN = MINMAS / EVIS2
      IF(YIJMIN .GT. YCUT) THEN
C no more jets, fill Jet array arrays
        NJET = 0
        DO 3000 I = 1, IE
          IF(PP(4,I).GT.ETHR) THEN
            NJET = NJET + 1
            IF(NJET.GT.KTBMAX) THEN
              IRET = -3
              GOTO 999
            ENDIF
            P(NJET+N,1) = PP(1,I)
            P(NJET+N,2) = PP(2,I)
            P(NJET+N,3) = PP(3,I)
            P(NJET+N,4) = PP(4,I)
C KTBOF(ipart) = associated jet #, (even when only 1 part. in "jet")
            KP(1,I) = NJET
            DO 3002 J = 1, N
              IF(KP(1,J).EQ.I) THEN
                KP(1,J) = NJET
                IKR = KP(2,J)
                K(IKR,1) = NJET
              ENDIF
 3002       CONTINUE
          ENDIF
 3000   CONTINUE
C
        GOTO 999
C
      ELSE
C update pairing,
C merge particles of the pair and "zero" the 2nd part. of the pair
        KP(1,MINPA1) = MINPA1
        KP(1,MINPA2) = MINPA1
        DO 5000 I = 1, IE
          IF(KP(1,I).EQ.MINPA2) KP(1,I) = MINPA1
 5000   CONTINUE
C
        PP(1,MINPA1) = PP(1,MINPA1) + PP(1,MINPA2)
        PP(2,MINPA1) = PP(2,MINPA1) + PP(2,MINPA2)
        PP(3,MINPA1) = PP(3,MINPA1) + PP(3,MINPA2)
        PP(4,MINPA1) = PP(4,MINPA1) + PP(4,MINPA2)
        PP(5,MINPA1) = SQRT(PP(1,MINPA1)**2+PP(2,MINPA1)**2+
     &                      PP(3,MINPA1)**2)
C
        PP(1,MINPA2) = -1.
        PP(2,MINPA2) = -1.
        PP(3,MINPA2) = -1.
        PP(4,MINPA2) = -1.
        PP(5,MINPA2) = -1.
      ENDIF
      GOTO 100
C
  999 KTBO = IRET
      IF(IRET.GE.0.AND.IRET.NE.-3) KTBO = NJET
C
      RETURN
      END
#endif
