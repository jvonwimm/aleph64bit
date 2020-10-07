      SUBROUTINE IDMUON(ICH,NEXP,NFIR,N10,N03,NMU,IDF,EHC,LIDT,
     .                  xmult,rapp,ang,ishad,sudnt,imcf)
C-----------------------------------------------------------------
C! Return muon identication information for the ENFLW track ich
C
C  Input:  -- ich:   The ENFLW track
C
C  Output: -- nexp: number of expected planes in the HCAL
C          -- nfir: number of fired    planes in the HCAL
C          -- n10 : number of fired    planes in the last 10
C          -- n03 : number of fired    planes in the last 3
C          -- nmu : number of fired    muon chamber layers
C          -- idf : official ALEPH muon flag
C          -- ehc : energy of he PHCO associated to ich
C          -- lidt: local identification flag
C          -- xmult --> imcf : same definition as in QMUIDO
C
C  Patrick Janot - 27 June 1991
C-----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "qcde.h"
#include "qmacro.h"
C
      jch = ichefw(ich)
C
      IF ( jch .GE. kfrtot(3) .OR. jch .LT. kfrtot(1) ) THEN
        nexp = 0
        nfir = 0
        n10  = 0
        n03  = 0
        nmu  = 0
        idf  = 0
        ehc  = 0.
        lidt = 5
        xmult = 0.
        rapp  = 0.
        ang   = 0.
        ishad = 0
        sudnt = 0.
        imcf  = 0
      ELSE
        jch = jch - kfrtot(1) + 1
        nexp = muflg(jch)/20
        nfir = muwrd(jch)/2000
        n10  = (muwrd(jch) - nfir*2000)/100
        n03  = (muwrd(jch) - nfir*2000 - n10*100)/10
        nmu  = (muwrd(jch) - nfir*2000 - n10*100-n03*10)/1
        idf  = muflg(jch) - nexp*20
        IF ( ( idf .GE.  5 .AND. idf .LE.  9 ) .OR.
     .       ( idf .GE. 16 .AND. idf .LE. 19 ) ) THEN
          idf = idf - 20
          IF ( idf .EQ. -4 ) idf = -10
          nexp = nexp + 1
        ENDIF
        ehc  = ehcal(jch)
        lidt = idtflg(jch)
        xmult = yxmult(jch)
        rapp  = yrapp (jch)
        ang   = yang  (jch)
        ishad = jishad(jch)
        sudnt = ysudnt(jch)
        imcf  = jimcf (jch)
      ENDIF
C
      RETURN
      END
#endif
