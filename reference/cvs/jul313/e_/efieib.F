      SUBROUTINE EFIEIB(NTR,ICLN)
C----------------------------------------------------------------------
C!  - FILL EIDT BANK
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "eidtbk.h"
#include "ecnamc.h"
#include "eidtjj.h"
#include "rlunit.h"
#include "bmacro.h"
C
      KEIDT=IW(NAEIDT)
C
C    FILL EIDT ATTRIBUTES
C
      NEID=IW(KEIDT+LMHROW)
      NEID=NEID+1
      IW(KEIDT+LMHROW)=NEID
      IW(KROW(KEIDT,NEID)+JEIDIF)=IEFLAG
      RW(KROW(KEIDT,NEID)+JEIDR1)=R1
      RW(KROW(KEIDT,NEID)+JEIDR2)=R2
      RW(KROW(KEIDT,NEID)+JEIDR3)=R3
      RW(KROW(KEIDT,NEID)+JEIDR4)=R4
      RW(KROW(KEIDT,NEID)+JEIDR5)=R5
      RW(KROW(KEIDT,NEID)+JEIDR6)=R6
      RW(KROW(KEIDT,NEID)+JEIDR7)=R7
      RW(KROW(KEIDT,NEID)+JEIDEC)=ECORR
      IW(KROW(KEIDT,NEID)+JEIDIP)=IPP
      RW(KROW(KEIDT,NEID)+JEIDE1)=E41
      RW(KROW(KEIDT,NEID)+JEIDE2)=E42
      RW(KROW(KEIDT,NEID)+JEIDE3)=E43
      IW(KROW(KEIDT,NEID)+JEIDFR)=NTR
C!!!!!! WE CONSIDER THAT FOR ONE ECLU ROW ,THERE IS ONE AND ONLY ONE
C!!!!!! ECOB ROW, AND WITH THE SAME ROW INDEX
C!!!!!! SAME THING FOR PEOB
      IW(KROW(KEIDT,NEID)+JEIDPE)=ICLN
      RETURN
      END
#endif
