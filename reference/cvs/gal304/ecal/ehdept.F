*DK ehdept
      SUBROUTINE EHDEPT (DELT,NRJ)
C.----------------------------------------------------------------
C       J.Badier - 25/3/86
C! Tr. elem energy deposit
C       Depot d'energie pour un segment de trace
C
C           Input :  DELT Longueur du segment (for hadrons..)
C           Output:  NRJ  Energie deposee en KEV
C  - Called by EHCHAR
C  - calls      IUCOMP                              KERNLIB
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ehpash.h"
#include "ehcond.h"
#include "trkcom.h"
#include "gcking.h"
C   14 milieux.
      EXTERNAL RNDM,IUCOMP
      PARAMETER (NMIL=14)
C
      CHARACTER*4 CHAINT,CHAHOL
      CHARACTER*4 TEVOL(NMIL)
C
      DATA TEVOL / 'EBS2', 'EBS1', 'EBS3',
     +             'ECS2', 'ECS1', 'ECS3', 'ECS0',
     +             'EDS2', 'EDS1', 'EDS3', 'EDS0',
     +             'EB12', 'EC12', 'ED12' /
C
C
C ----- Recherche du milieu
      DO 1 MILI = 1 , NMIL
         IF( TRKVOL .EQ. TEVOL(MILI) ) GO TO 2
    1 CONTINUE
      MILI = 0
    2 CONTINUE
C
C  Pour e+,e-,photons
C
C     Traitement  'VERY FAST' provisoirement abandonne.
C     Utilise RHOVIT et FASTNR
C     avec fraction annulee pour ajuster la fluctuation de l'energie.
      IF(ITRKEL(4) .LE. 3)          THEN
C ----- Fluctuation de l'energie perdue
         ALEA = RNDM(ALEA)
         IF ( ALEA .LT. RHODEP ) THEN
            ENRJ = ALEA * FLUCT1(MILI)
         ELSE
            ENRJ = RNDM(ENRJ) * FLUCT2(MILI)
         ENDIF
         NRJ = NINT ( TRKELE(12) *  ENRJ )
      ELSE
C
C  Traitement des Hadrons charges ( Snow ).
C
C
C switch off soft delta rays produced by hadrons and muons
C  they will be taken into account by the landau fluctuations .
C
          IF(CHAHOL(KGCASE).EQ.'DRAY'.AND.GKIN(4,1).LT.0.1)NGKINE = 0
C
C allow some fraction of the nuclear fragments to deposit energy
C
          IF(TRKELE(13).GT.1.0)THEN
            IDEPO = 0
            RR = RNDM(ENRJ)
            IF(RR.LT.FNFRAG)IDEPO = 1
          ELSE
            IDEPO = 1
          ENDIF

          IF (TRKELE(7) .GT. 0.
     &       .AND. IDEPO .EQ.  1
     &       .AND. TRKELE(14) .NE. 0.)    THEN
             GABE = 0.5 * ( TRKELE(7) + TRKNXT(7) ) / TRKELE(13)
             CALL EBEBLO( GABE , TRKELE(14) , TRKELE(13) , EP1 )
             IF( EP1 .GT. DESATU ) EP1 = DESATU
             CALL ELANDA( DELT , RLAN )
             ENRJ = DELT * EP1 * RLAN
             NRJ = NINT ( ENRJ * EPICUT(MILI) )
         ELSE
             NRJ = 0
         ENDIF
      ENDIF
C
      RETURN
C
      END
#endif
