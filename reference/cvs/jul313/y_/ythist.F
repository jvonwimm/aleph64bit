      SUBROUTINE YTHIST
C
C----------------------------------------------------------*
C!    histogram filling for the TOPOLOGY reconstruction
C!
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!
C!
C!    Description
C!    ===========
C!    This routine fills histograms
C!    for the TOPOLOGY reconstruction program
C!
C!---------------------------------------------------------*
#ifndef DOC
#include "bcs.h"
#include "pyerjj.h"
#include "rflags.h"
#include "rparac.h"
C!---------------------------------------------------------*
      EXTERNAL NAMIND
#include "bmacro.h"
C!---------------------------------------------------------*
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      KPYER=IW(NAMIND('PYER'))
      IF(KPYER.EQ.0) GOTO 999
      JOFF= JULYT * 1000
      NBT = (ITABL(KPYER,1,JPYEDF)+3)/2
      CALL HF1(JOFF,FLOAT(NBT),1.)
      CALL HF1(JOFF+1,RTABL(KPYER,1,JPYEVX),1.)
      CALL HF1(JOFF+2,RTABL(KPYER,1,JPYEVY),1.)
      CALL HF1(JOFF+3,RTABL(KPYER,1,JPYEVZ),1.)
      IF(ITABL(KPYER,1,JPYEDF).GT.0)
     +  CALL HF1(JOFF+4,
     +  RTABL(KPYER,1,JPYEC2)/FLOAT(ITABL(KPYER,1,JPYEDF)),1.)
C
      RETURN
 999  CONTINUE
      RETURN
      END
#endif
