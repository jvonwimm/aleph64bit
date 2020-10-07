      SUBROUTINE BLDPECO(ICLU,ERAW)
C----------------------------------------------------------------------
C! Rescue routine to rebuild from ETDI the banks PECO and PEST if the
C  cluster energy is crazy (namely < 0 )!
C
C  Patrick Janot - 19 mai 1989
C----------------------------------------------------------------------
#ifndef DOC
#include "parabank.h"
#include "parcut.h"
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qhac.h"
#include "qmacro.h"
C----------------------------------------------------------------------
      eraw = 0.
      DO 12 ipads = 1 , npest
        IF ( itabl(ipest, ipads, jpespe) .NE. iclu ) GOTO 12
        IF ( itabl(ipest, ipads, jpesed) .NE. 0) GOTO 12
        kstak = itabl(ipest, ipads, jpesks)
        ntow  = itabl(ipest, ipads, jpeset)
        ensto = itabl(ietdi, ntow,  kstak+1) * 1.E-6
        IF ( ensto .LE. .030 ) ensto = 0.
        eraw = eraw + ensto
        RW(ipest+lmhlen+(ipads-1)*IW(ipest+1)+jpeser) = ensto
   12 CONTINUE
      RW(ipeco+lmhlen+(iclu-1)*IW(ipeco+1)+jpecer) = eraw
      RW(ipeco+lmhlen+(iclu-1)*IW(ipeco+1)+jpecec) = eraw
C
      RETURN
      END
#endif
