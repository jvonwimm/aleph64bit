      SUBROUTINE YFHIST
C----------------------------------------------------------------
C! Fill histograms for main vertex (Mermekides)
C!
C!    Author:   M. Mermikides  11 Oct 1987
C!    Modified: M.A. Ciocci,L. Rolandi 4/5/88
C!    Modified: M.Bosman 12/04/89
C----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "rparac.h"
#include "yvxljj.h"
#include "yynamc.h"
#include "bmacro.h"
C
      KYVXL=IW(NAYVXL)
      IF (KYVXL.EQ.0) RETURN
      IF (JHISRF(JULYR).LE.0)RETURN
C
C Offset for VERTEX histograms
C
      JOFF = JULYR * 1000
      DO 10 I = 1,IW(KYVXL+LMHLEN)
         X= RW(KROW(KYVXL,I)+JYVXXP)
         Y= RW(KROW(KYVXL,I)+JYVXYP)
         Z= RW(KROW(KYVXL,I)+JYVXZP)
         CALL HF2(JOFF+1,X,Y,1.)
         CALL HF1(JOFF+2,Z,1.)
         DX = 1./SQRT(RW(KROW(KYVXL,I)+JYVXEM))
         DY = 1./SQRT(RW(KROW(KYVXL,I)+JYVXEM+2))
         DZ = 1./SQRT(RW(KROW(KYVXL,I)+JYVXEM+5))
         CALL HF1(JOFF+3,DX,1.)
         CALL HF1(JOFF+4,DY,1.)
         CALL HF1(JOFF+5,DZ,1.)
         NT= IW(KROW(KYVXL,I)+JYVXNT)
         CALL HF1(JOFF+6,FLOAT(NT)+.1,1.)
         PRB = PROB(RW(KROW(KYVXL,I)+JYVXCH),IW(KROW(KYVXL,I)+JYVXDF))
         CALL HF1(JOFF+7,PRB,1.)
   10 CONTINUE
C
         RETURN
         END
#endif
