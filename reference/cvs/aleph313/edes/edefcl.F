      SUBROUTINE EDEFCL
C.----------------------------------------------------------------------
CKEY ECALDES COLUMNS INITIALISATION / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 30/01/88
C                                              M.Rumpf   03/19/87
C! Initialises column system
C  Defines the column geometry in a module for the module-type, then
C  applies the module displacement to get it in its slot.
C  First generates points situated on the planes delimiting the columns
C  in the module type frame. These contain ~ the z axis and are at
C  deltaphi =  pi/(48*NRG) in region NRG
C   Calls: EROTPL
C   Called by EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "alcons.h"
#include "ecccom.h"
C
C  variables locales
      INTEGER SC,MD,CL,PN,RG,BPCL(ECALLP)
      REAL VDOT
C   B est l'angle embrasse par une colonne.
      REAL B
C Plans temporaires
      REAL PTEC(ECALLS),PTSC(ECALLS),PTMD(ECALLS)
C Point definissant le plan limite de colonne
      REAL PTCL(ECALLS)
C
C
C Generation des points definissant les colonnes par region
C Puis definition des coord locales (au faisceau) des plans
      DO 1 RG=1,ECALRG
         B=TWOPI/FLOAT(ECALMD*EREGCN(RG))
         DO 2 CL=EREGFR(RG),EREGLS(RG)
            PTCL(1)=TAN(FLOAT(CL-(EREGFR(RG)+EREGLS(RG))/2)*B)
            PTCL(2)=-1.
            PTCL(3)=0.
            PTCL(4)=1.
          ECOLPC(1,CL)=VDOT(PTCL(1),EBPLLF(1,ECALC1),ECALLS)
          ECOLPC(2,CL)=VDOT(PTCL(1),EBPLLF(1,ECALC2),ECALLS)
 2      CONTINUE
 1    CONTINUE
C
C
C Passage dans le systeme general des plans de reference pour chaque
C module:   rotations theoriques et alignement.
C   Faire  ECALDM, ETSCDM, puis EMODDM, puis EALIAM.
      BPCL(1)=ECALC1
      BPCL(2)=ECALC2
C
      DO 12 PN=1,ECALLP
        CALL EROTPL(PTEC,EBPLLF(1,BPCL(PN)),ECALDM)
        DO 11 SC=1,ECALSC
          CALL EROTPL(PTSC,PTEC,ETSCDM(1,1,ESCOET(SC)))
          DO 10 MD=1,ECALMD
            CALL EROTPL(PTMD,PTSC,EMODDM(1,1,MD))
            CALL EROTPL(EALICL(1,PN,MD,SC),PTMD,EALIAM(1,1,MD,SC))
 10       CONTINUE
 11     CONTINUE
 12   CONTINUE
      END
#endif
