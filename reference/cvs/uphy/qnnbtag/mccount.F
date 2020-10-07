*****************************************************************
      SUBROUTINE MC_COUNT(ITK0,NTOT,NCHT,PTOT,PCHT,CHARGE)
*****************************************************
*  Subroutine to count the number of MC particles coming from
*  the decay of another MC particle.    Andy Greene   .
*  counts only the number of stable MC particles.
*****************************************************
#include "qcde.h"
      REAL MOMCH(3),MOMTOT(3)
      REAL EDIF,MDIF
#include "qmacro.h"
C
C-- Count decay products of charmed hadron
C
      NCHT = 0
      NTOT = 0
      CHARGE = 0
      DO ICOR=1,3
        MOMTOT(ICOR) = 0.0
        MOMCH(ICOR) = 0.0
      END DO
      DO ITK1 = KFMCT,KLMCT
        ITK2 = ITK1
 80     IF (KNMOTH(ITK2).NE.0) THEN
          EDIF = QE(KMOTH(ITK2,1))
          MDIF = QM(KMOTH(ITK2,1))
C
C     /\ Energy : E(mum) - sum(E(dau)) ; idem for the mass.
C
          DO IDAU=1,KNDAU(KMOTH(ITK2,1))
            EDIF = EDIF-QE(KDAU(KMOTH(ITK2,1),IDAU))
            MDIF = MDIF-QM(KDAU(KMOTH(ITK2,1),IDAU))
          END DO
C
C      If  E = 0 and M > 0 good recognititon
C
          IF (ABS(EDIF).LE.0.005.AND.MDIF.GE.0.) THEN
            ITK2 = KMOTH(ITK2,1)     ! ITK2 becomes the mother(meson)
C
C      Work on the mother ITK0  = ITK2
C
            IF (ITK2.EQ.ITK0) THEN
              EDIF = QE(ITK1)    ! Energy of daughter
              MDIF = QM(ITK1)
C
C      Work on daughters of daughter
C
              DO IDAU=1,KNDAU(ITK1)
                EDIF = EDIF-QE(KDAU(ITK1,IDAU))
                MDIF = MDIF-QM(KDAU(ITK1,IDAU))
              END DO
C
C      If the particle is stable , compute the sum of P
C
              IF (.NOT.(ABS(EDIF).LE.0.005.AND.MDIF.GE.0.).OR.
     &              KNDAU(ITK1).EQ.0) THEN
                NTOT = NTOT+1
                MOMTOT(1) = MOMTOT(1)+QX(ITK1)
                MOMTOT(2) = MOMTOT(2)+QY(ITK1)
                MOMTOT(3) = MOMTOT(3)+QZ(ITK1)
C
C       Counts for charged tracks
C
                IF (QCH(ITK1).NE.0.)THEN
                  NCHT = NCHT+1
                  MOMCH(1) = MOMCH(1)+ QX(ITK1)
                  MOMCH(2) = MOMCH(2)+ QY(ITK1)
                  MOMCH(3) = MOMCH(3)+ QZ(ITK1)
                  CHARGE   = CHARGE  + QCH(ITK1)
                ENDIF
C
              ENDIF     ! stable track
            ELSE
              GOTO 80
            ENDIF       ! the ITK1 is the Meson
          ENDIF         ! Test of mass and energy
        ENDIF           ! If the mother exists
      END DO
C
C  Get momentum sum magnitude
C
      PCHT = 0.0
      PTOT = 0.0
      DO ICOR=1,3
        PTOT = PTOT + MOMTOT(ICOR)**2
        PCHT = PCHT + MOMCH(ICOR)**2
      END DO
      PTOT = SQRT(PTOT)
      PCHT = SQRT(PCHT)
C
      RETURN
      END
