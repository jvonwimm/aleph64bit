      SUBROUTINE ELECID
C-------------------------------------------------------
C!  STEERING ROUTINE FOR ELECTRON IDENTIFICATION
C!
C!  AUTHOR   : D. PALLIN
C!  MODIFIED :
C!
C!  BANKS :
C!    INPUT   : ECLU,ECT1,ERL3
C!    OUTPUT  : EIDT
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "eidtbk.h"
#include "rflags.h"
#include "rparac.h"
#include "eclujj.h"
#include "erl3jj.h"
#include "ect1jj.h"
#include "ecnamc.h"
#include "rlunit.h"
#include "bmacro.h"
C
      ITOTE4=0
C
C?   Do Nothing of no tracks
C
      NAFRFT=NAMIND('FRFT')
      KFRFT=IW(NAFRFT)
      NTRKS=LROWS(KFRFT)
      IF (NTRKS.LE.0) GOTO 999
C
C?   CREATE EIDT BANK
      CALL ECREIB(IER)
      IF (IER.NE.0) GO TO 997
C?   CREATE E4DE BANK
      CALL ECRE4D(IER)
      IF (IER.NE.0) GO TO 998
C
      KECLU = IW(NAECLU)
      KECT1 = IW(NAECT1)
      KERL3 = IW(NAERL3)
C
      IF ( KECLU.EQ.0) GO TO 999
      NECLU = IW(KECLU + LMHROW)
      IF (NECLU.LT.1) GO TO 999
C
C?   LOOP ON CLUSTERS
C
C
      DO 1  ICL=1,NECLU
C
C?   SKIP CLUSTER WITH NO STOREYS
        IF (ITABL(KECLU,ICL,JECLES).EQ.0) GOTO 1
C
C?   CHECK IF A CHARGED TRACK IS ASSOCIATED
        CALL ECHARG(ICL,ICHR)
        IF(ICHR.EQ.0) GOTO 1
C
C?   GET ASSOCIATED TRACKS AND LOOP OVER TRACKS OF THIS CLUSTER
C
C
C
        IREL=ITABL(KECT1,ICL,JECTER)
        ICT=0
    2   IF (IREL.NE.0.AND.ICT.LT.5)  THEN
          ITK=ITABL(KERL3,IREL,JERLET)
          CALL ESLAS(ICL,ITK,IKEPT)
          IF(IKEPT.EQ.1)THEN
            ICT=ICT+1
C           DO IDENTIFICATION OF TRACK ITK BELONGING TO CLUSTER ICL
            CALL EIDENT(ICL,ITK,ICT)
          ENDIF
C
          IREL=ITABL(KERL3,IREL,JERLER)
          GO TO 2
        ENDIF
C
C? LOOP OVER TRACKS ENDING
C
        KECLU = IW (NAECLU)
        KERL3 = IW (NAERL3)
        KECT1 = IW (NAECT1)
    1 CONTINUE
C
C? LOOP OVER CLUSTER ENDING
C
C      PRINT RESULTS OF ELECTRON IDENTIFICATION
      IF(JDBDRF(JULEC).GT.0)
     &WRITE(LDEBRL,*)ITOTE4,' PARTICLES HAVE AN ELECTRON HYPOTHESIS ',
     & 'IN THIS EVENT'
C
C
C?   CLOSE EIDT  BANK
      CALL ECLEIB
C? CREATE POT BANK
C     CALL ECRPEI
      GOTO 999
C
  997 CALL RERROR(' ELECID',1,' Unable to create EIDT Bank')
      GOTO 999
C
  998 CALL RERROR(' ELECID',2,' Unable to create E4DE Bank')
      GOTO 999
C
  999 RETURN
      END
#endif
