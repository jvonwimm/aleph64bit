      SUBROUTINE EPATRN(IFRF,I1,I2,I3,I4,I5,I6)
C---------------------------------------------------------------------
C!  GIVE ECAL PATTERN FOR FRID BANK
C
C  AUTHOR : D. PALLIN
C
C  INPUT :  IFRF INDEX OF TRACK IN FRFT BANK
C  OUTPUT:  I1-I3   = BIT PATTERN FOR STACK1-3. =1 IF HIT
C           I4-I6   = BIT PATTERN FOR STACK1-3 DEAD CHANNELS. =0 IF DEAD
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "eidtjj.h"
#include "bmacro.h"
C
      I1=0
      I2=0
      I3=0
      I4=1
      I5=1
      I6=1
      NEIDT=NAMIND('EIDT')
      KEIDT=IW(NEIDT)
      IF(KEIDT.EQ.0)RETURN
      LEIDT=LROWS(KEIDT)
      IF(LEIDT.EQ.0)RETURN
      DO 1 I=1,LEIDT
      IF(IW(KROW(KEIDT,I)+JEIDFR).EQ.IFRF)THEN
           IF(RW(KROW(KEIDT,I)+JEIDE1).GT.0.)I1=1
           IF(RW(KROW(KEIDT,I)+JEIDE2).GT.0.)I2=1
           IF(RW(KROW(KEIDT,I)+JEIDE3).GT.0.)I3=1
      ENDIF
  1   CONTINUE
      NE4DE=NAMIND('E4DE')
      KE4DE=IW(NE4DE)
      LE4DE=LROWS(KE4DE)
      IF(LE4DE.EQ.0)RETURN
            I4=IW(KROW(KE4DE,IFRF)+1)
            I5=IW(KROW(KE4DE,IFRF)+2)
            I6=IW(KROW(KE4DE,IFRF)+3)
      RETURN
      END
#endif
