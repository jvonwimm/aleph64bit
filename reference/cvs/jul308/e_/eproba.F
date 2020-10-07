      SUBROUTINE EPROBA(IFRF,IFLAG)
C---------------------------------------------------------------------
C!  GIVE ELECTRON HYPOTHESIS FROM ECAL ANALYSIS
C
C  AUTHOR : D. PALLIN
C
C  INPUT :  IFRF INDEX OF TRACK IN FRFT BANK
C  OUTPUT:  IFLAG   =1 IF ELECTRON , 0 IF NOT
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "eidtjj.h"
#include "bmacro.h"
C
      IFLAG=0
      NEIDT=NAMIND('EIDT')
      KEIDT=IW(NEIDT)
      IF(KEIDT.EQ.0)RETURN
      LEIDT=LROWS(KEIDT)
      IF(LEIDT.EQ.0)RETURN
      DO 1 I=1,LEIDT
      IF(IW(KROW(KEIDT,I)+JEIDFR).EQ.IFRF)THEN
            IFLAG=IW(KROW(KEIDT,I)+JEIDIP)
      ENDIF
  1   CONTINUE
      RETURN
      END
#endif
