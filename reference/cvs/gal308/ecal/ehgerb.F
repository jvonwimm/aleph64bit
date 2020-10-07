*DK ehgerb
      SUBROUTINE EHGERB(ENER,IFIN,XGERB,NRJDP)
C ----------------------------------------------------------------
C       J.Badier - 11/12/85
C       J.Badier - 16/03/87 : Adaptation aux gerbes hadroniques.
C! Computes shower points
C    Input : ENER   Energie totale de la gerbe (Gev)
C
C    Le premier appel est une initialisation selon la condition
C           TIRAGE = 'FLUCTUATED' methode standard
C                    'VERY FAST' methode rapide (Defaut)
C       Output  IFIN :  Nombre de points a tirer dans la gerbe
C
C    Aux appels suivants  (IFIN NE 0)
C       Output  XGERB:  Coordonnees du i eme point i=1,IFIN
C               NRJDP   Signal exprime en kev pour ce point.
C   - Called by EHSHOW
C   - Calls POISSN,EHLOTI,EHTRTI,EHDEPS
C ------------------------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION XGERB(3)
C
#include "alcons.h"
#include "iocom.h"
#include "ehpash.h"
#include "ecdbug.h"
#include "ehcond.h"
#include "gcmate.h"
#include "capano.h"
#include "trkcom.h"
C
      EXTERNAL RNDM
C
      IF ( IFIN .EQ.  0)           THEN
C ----- Initialisation.
         ICOND = 0
C ----- Gerbe electromagnetique.
         EMGNRJ = ENER
         HADNRJ = 0.
         NATGER = 1
C ----- Nombre IEM de points electromagnetiques a tirer.
C ----- PARGV# : Nombre de tirages par gev.
            PISNU = EMGNRJ * PARGV1
         CALL POISSN(PISNU,IEM,IER)
         IF( IEM .GT.  0) THEN
C ----- Initialisation du tirage electromagnetique.
            CALL CAEPAR
C ----- Composante longitudinale.
            CALL EHLOTI( ICOND , XGERB(1) )
         ENDIF
C ----- Nombre de points a tirer.
         IFIN = IEM
      ELSE
C
C ----- Tirage des points.
         IF(ICOND .EQ. 0) DREST = ELIMST( 2 , 'SUP' )
         ICOND = 1
C
C ----- Tirage electromagnetique.
C ----- Longitudinal.
         CALL EHLOTI( ICOND , XGERB(1) )
         XGERB(1) = XGERB(1) * GRADL
         IF( XGERB(1). GT. DREST ) XGERB(1) =
     +   .648 * XGERB(1) + .352 * DREST
C ----- Radial.
         CALL EHTRTI( XGERB(1) , RAD )
         IF (RAD.GT.0.) THEN
            PHI = TWOPI * RNDM(RAD)
            XGERB(2) = RAD * COS(PHI)
            XGERB(3) = RAD * SIN(PHI)
         ELSE
            XGERB(2)=0.
            XGERB(3)=0.
         ENDIF
C ----- Energie deposee.
         CALL EHDEPS(NRJDP)
C
      ENDIF
C
   10 CONTINUE
      RETURN
  800 FORMAT(5X,'CONDITION DE TIRAGE INCONNUE',5X,A16)
      END
#endif
