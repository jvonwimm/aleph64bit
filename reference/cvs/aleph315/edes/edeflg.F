      SUBROUTINE EDEFLG
C.----------------------------------------------------------------------
CKEY ECALDES ROW INITIALISATION / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 11/03/90
C! Initialises ROWS system:
C  Define row geometry in module_type then applies displacements
C  for each module.
C  Define region geometry
C   Calls: EGPTLG
C   Called by EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
      INTEGER SC,MD,RG,EC,SS,RW,PN
      REAL VDOT
C Points definissant les plans limites de lignes
      REAL PTLG(ECALLS,ECALXW+3)
C Plans temporaires. Variables locales
      REAL PTEC(ECALLS),PTSC(ECALLS),PTMD(ECALLS),R
C
C
C  Generation des points definissant les lignes
C
      CALL EGPTLG(PTLG)
C
      DO 1 PN=1,ECALLP
C  Definition des coord locales (faisceau) des plans
        DO 2 RW=1,ECALXW+3
          SC=2
          IF(RW.LE.51)SC=1
          IF(RW.GE.181)SC=3
          EXROPC(PN,RW)=
     &      VDOT(PTLG(1,RW),EBPLLF(1,ESCORR(PN,SC)),ECALLS)
 2      CONTINUE
 1    CONTINUE
C  normalisation des EXROPC
      DO 11 RW=1,ECALXW+3
        R=SQRT(EXROPC(1,RW)**2+EXROPC(2,RW)**2)
        EXROPC(1,RW)=EXROPC(1,RW)/R
        EXROPC(2,RW)=EXROPC(2,RW)/R
 11   CONTINUE
      DO 10 PN=1,ECALLP
C
C Definition des coordonnees locales dans le faisceau de regions
C
        DO 3 RG=1,ECALXG
          RW=EXRGFR(RG)
          SC=EXROES(RW)
          EXRGPC(PN,RG)=EXROPC(PN,RW+SC-1)
 3      CONTINUE
        EXRGPC(PN,ECALXG+1)=EXROPC(PN,ECALXW+3)
C
C Passage dans le systeme general des plans de reference pour chaque
C module:   rotations theoriques et alignement.
C   Faire  ECALDM, ROTSC, puis ROTMD, puis EALIAM.
        SS=1
        DO 4 SC=1,ECALSC
          CALL EROTPL(PTEC,EBPLLF(1,ESCORR(PN,SC)),ECALDM)
          CALL EROTPL(PTSC,PTEC,ETSCDM(1,1,ESCOET(SC)))
          DO 5 MD=1,ECALMD
            CALL EROTPL(PTMD,PTSC,EMODDM(1,1,MD))
            CALL EROTPL(EALIRW(1,PN,SS,MD,SC),PTMD,EALIAM(1,1,MD,SC))
            IF(ESCOET(SC).EQ.2) THEN
C   Dans les bouchons nous faisons aussi la rotation de sous-secteur
              CALL EROTPL(EALIRW(1,PN,2,MD,SC),EALIRW(1,PN,1,MD,SC),
     &                     ESECDM(1,1,2))
              CALL EROTPL(EALIRW(1,PN,1,MD,SC),EALIRW(1,PN,1,MD,SC),
     &                     ESECDM(1,1,1))
            END IF
 5        CONTINUE
 4      CONTINUE
 10   CONTINUE
      END
#endif
