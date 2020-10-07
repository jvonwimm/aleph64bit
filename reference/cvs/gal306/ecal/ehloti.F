*DK ehloti
      SUBROUTINE EHLOTI( ICOND , CLOTI )
C.----------------------------------------------------------------
C       J.Badier - 27/ 2/86
C                  Modifie le 13/03/87.
C! Shower longitud. profile
C ----- Tirage longitudinal d'une gerbe .
C   Input:
C       ICOND = 0 phase initialisation
C               1 Tirage d'un point
C   Output:
C       CLOTI  tirage logitudinal en longueurs de radiation.
C
C       METH a ete defini dans la phase d'initialisation
C   Calls EHPALO , RNDM , GAUSS ,GAMMA
C      Apelle EHPALO,RANNOR,RNDM,GAUSS,GAMMA
C       Appele par EHGERB
C ----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ehpash.h"
#include "capano.h"
#include "ehcond.h"
C
      EXTERNAL EHPALO , RNDM ,GAUSS ,GAMMA
C
C ----- Initialisation du tirage longitudinal
C
      IF(ICOND .EQ. 0)                  THEN
C
      IPASS = 0
      IF( EMBETA .LT. BETMAX )          THEN
         IF(EMALFA.GT.1.) THEN
C ----- Parametrisation standard
            METH=3
C ----- Preparation des 3 zones de tirage
C ----- REDS = parametrisation reduite integree
            REDS1 = GAUSS( EHPALO , .0 , ZONSH1 , .01 )
            REDS2 = GAUSS( EHPALO , ZONSH1 , ZONSH2 , .01) + REDS1
            REDS3=EXP(EMALM1)*GAMMA(EMALFA)/EMALM1**EMALFA
            PINT1 = REDS1 / REDS3
            PINT2 = REDS2 / REDS3
C
C ----- Preparation des tirages zone 1
            PNU1=REDS1/(EHPALO(ZONSH1)*ZONSH1)
            PS1=ZONSH1*EMALM1/(EMBETA*PINT1**PNU1)
C ----- Preparation des tirages zone 3
            PS3=EMALM1/EMBETA
            PNU3=PS3*(REDS3-REDS2)/EHPALO(ZONSH2)
            PS3=ZONSH2*PS3
C ----- Pour electron primaire.
            ELPR = DEPMIN / EMBETA
            IELPR = 0
            IF ( TIRAGE .EQ. 'VERY FAST' )  IELPR=NINT(ELPR*PARGV1)
            IF ( TIRAGE .EQ. 'FLUCTUATED' ) IELPR=NINT(ELPR*PARGV2)
C
         ELSE
C ----- Tirage exponentiel.
            METH = 2
         ENDIF
      ELSE
C ----- Depot ponctuel.
         METH = 1
      ENDIF
C
C ----- Tirages
C
      ELSE
        IPASS = IPASS + 1
C ----- METH=1 : Depot ponctuel
        IF(METH.EQ.1)                    THEN
          CLOTI = 1.
        ELSE
C ----- METH=2 : Tirage selon une exponentielle
        IF( METH .EQ. 2 .OR. IPASS .LE. IELPR ) THEN
          ALEA=RNDM(METH)
          IF(ALEA.LE..00001) ALEA=.000005
          CLOTI=-ALOG(ALEA)/EMBETA
        ENDIF
C ----- METH=3 : Tirage selon la parametrisation standard
        IF(METH.EQ.3)                    THEN
   10     CONTINUE
          ALEA=RNDM(METH)
C ----- Trois zones .
          IF(ALEA.LT.PINT1)              THEN
C ----- ZONE 1
            IF(ALEA.LE..00001) ALEA=.000005
            CLOTI=PS1*ALEA**PNU1
          ELSE
            IF(ALEA.LT.PINT2)            THEN
C ----- ZONE 2
   20          CLOTI = ZONSH1 + (ZONSH2-ZONSH1) * RNDM(ALEA)
               YVAL = EHPALO(CLOTI)
               YTIR = RNDM(YVAL)
               IF(YTIR.GT.YVAL) GO TO 20
               CLOTI = CLOTI*EMALM1/EMBETA
            ELSE
C ----- ZONE 3
               ALEA = RNDM(ALEA)
               IF(ALEA.LE..00001) ALEA = .000005
               CLOTI = PS3 - PNU3 * ALOG(ALEA)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      ENDIF
C
      RETURN
      END
#endif
