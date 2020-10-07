      SUBROUTINE QGMDCL(ALPHA,DELTA,ETA,EVIST)
CKEY JETS /INTERNAL
C----------------------------------------------------------------------
C   Author   : P. Perez       28-MAR-1989
C
C   Description
C   ===========
C!   Evaluate jet multiplicities based on the scaled min. distance
C!   method (suggested by Hagiwara)
C    from momentum vectors stored in QCTBUF.INC
C
C   Distance between particles i and j is defined by :
C                                   2. * (Ei*Ej)**alpha * (1-cos(i,j))
C
C  input   : ALPHA           weight
C            DELTA           half opening angle (degrees)
C            ETA             energy cut-off for jets (in un. of Evis)
C            EVIST           visible energy
C                            (if EVIST=0., it is computed from the
C                             input particle energies)
C            KTBI            # of particles           (from QCTBUF)
C            QTBIX,Y,Z,E(i)  four momentum (i=1,KTBI) ( "     "   )
C  output  :
C            QTBOX,Y,Z,E(j)  four  momentum of jet j (j=1,NJET)
C            KTBOF(i)        jet # of particle i     (i=1,KTBI)
C            KTBO        -1  input  error (KTBI/EVIST) (in   QCTBUF)
C                        -2  error from individ. track
C                        -3  too many jets
C                       NJET # of jets (normal return)
C======================================================================
#ifndef DOC
#include "qctbuf.h"
C
      DIMENSION  PP(5,KTBIMX)
      REAL MASS , MINMAS
C
C------------------------------------------------------------------
C   set initial values
C------------------------------------------------------------------
      EVIS=EVIST
      IF(KTBI.GT.KTBIMX) THEN
C        WRITE(LOUTIO,*) ' ***MDCLUS ntk=',KTBI,'>ktbimx'
        IRET = -1
        GOTO 999
      ENDIF
C
      IRET = 1
      DO 10 I = 1, KTBOMX
      QTBOX(I) = 0.
      QTBOY(I) = 0.
      QTBOZ(I) = 0.
      QTBOE(I) = 0.
   10 CONTINUE
      DO 11 I = 1, KTBIMX
      KTBOF(I) = 0
   11 CONTINUE
      EVISI = 0.
      DO 1 I = 1, KTBI
        KTBOF(I) = 0
        PP(1,I)  = QTBIX(I)
        PP(2,I)  = QTBIY(I)
        PP(3,I)  = QTBIZ(I)
        PP(4,I)  = QTBIE(I)
        PP(5,I)  = SQRT(PP(1,I)**2+PP(2,I)**2+PP(3,I)**2)
        EVISI    = EVISI + QTBIE(I)
    1 CONTINUE
C
      IF(EVIS.EQ.0.) EVIS = EVISI
      EVIS2 = EVIS**2
C
      ACUT = 2. * (1. - COS(DELTA * 3.1415927 / 90.) )
      ECUT = 2. * ETA * EVIS
C
  100 MINMAS = 5.E8
C
      DO 2000 I = 1, KTBI-1
C
C skip if particle is already merged in a jet
        IF(PP(4,I) .LE. 0.) GO TO 2000
        PI = PP(5,I)
C
        DO 2100 J = I+1, KTBI
C skip if particle is already merged in a jet
          IF(PP(4,J) .LE. 0.) GO TO 2100
C
          PJ = PP(5,J)
          IF (PI .LE. 0. .OR. PJ .LE. 0.) THEN
C            WRITE(LOUTIO,*) ' ***MDCLUS  : momentum is 0'
            IRET = -2
            GO TO 2100
          ENDIF
          COSANG = (PP(1,I)*PP(1,J)+PP(2,I)*PP(2,J)+PP(3,I)*PP(3,J))
          COSANG = COSANG / (PI*PJ)
C
          ACOSAN = ABS(COSANG)
          IF(ACOSAN .GT. 1.0001) THEN
C            WRITE(LOUTIO,*) '***MDCLUS cosang= ',COSANG
            IRET = -2
            GOTO 999
          ELSEIF(ACOSAN.GT.1..AND.ACOSAN.LE.1.0001) THEN
            COSANG = SIGN(1.,COSANG)
          ENDIF
C
          MASS = 2. * (PP(4,I)*PP(4,J))**ALPHA * (1.-COSANG)
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
      AMIN = MINMAS / EVIS2**ALPHA
      IF(AMIN .GT. ACUT) THEN
C no more jets, fill QTBOX,Y,Z,E and KTBOF arrays
        NJET = 0
        DO 3000 I = 1, KTBI
          IF(PP(4,I).GT.ECUT) THEN
            NJET = NJET + 1
            IF(NJET.GT.KTBOMX) THEN
              IRET = -3
              GOTO 999
            ENDIF
            QTBOX(NJET) = PP(1,I)
            QTBOY(NJET) = PP(2,I)
            QTBOZ(NJET) = PP(3,I)
            QTBOE(NJET) = PP(4,I)
C KTBOF(ipart) = associated jet #, (even when only 1 part. in "jet")
            KTBOF(I) = NJET
            DO 3002 J = 1, KTBI
              IF(KTBOF(J).EQ.I) KTBOF(J) = NJET
 3002       CONTINUE
          ENDIF
 3000   CONTINUE
C
        GOTO 999
C
      ELSE
C update pairing,
C merge particles of the pair and "zero" the 2nd part. of the pair
        KTBOF(MINPA1) = MINPA1
        KTBOF(MINPA2) = MINPA1
        DO 5000 I = 1, KTBI
          IF(KTBOF(I).EQ.MINPA2) KTBOF(I) = MINPA1
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
      IF(IRET.GE.0) KTBO = NJET
C
      RETURN
      END
#endif
