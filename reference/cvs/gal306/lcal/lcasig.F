*DK lcasig
      SUBROUTINE LCASIG
C--------------------------------------------------------------
C! Controls the analog signal processing in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C. - Called by  ASASIG                           from this .HLB
C. - Calls      LCSATU                           from this .HLB
C -----------------------------------------------
#ifndef DOC
#include "iocom.h"
#include "jobcom.h"
#include "lccomc.h"
#include "bcs.h"
#include "lcnamc.h"
#include "lshijj.h"
#include "lwhijj.h"
      INTEGER CAFIHT
      LOGICAL FDEB
#include "bmacro.h"
#include "lcmacr.h"
C -------------------------------------------------------------
      FDEB = FDEBJO .AND. IPRIJO(5).NE.0
C
C - Drop banks which exist already
      IF (IW(NALTHT) .NE. 0) CALL BDROP (IW,'LTHTLWHT')
C
C - Reduce bank size to the real one
      CALL AUBPRS ('LSHILWHI')
      IF (FDEB) THEN
         CALL PRTABL('LSHI',0)
         CALL PRTABL('LWHI',0)
      ENDIF
C
C - Fill hit banks
      JLTHT = CAFIHT (NALSHI,'LTHT')
      IF (JLTHT.EQ.0) GOTO 998
      IF (FDEB) CALL PRTABL ('LTHT',0)
      JLWHT = CAFIHT (NALWHI,'LWHT')
      IF (JLWHT.EQ.0) GOTO 998
      IF (FDEB) CALL PRTABL ('LWHT',0)
C
C Is there any tower hits ?
      KINDX = IW(NALTHT)
      IF (KINDX .EQ. 0) RETURN
      IF (LROWS(KINDX) .EQ. 0) RETURN
C
      IF (LCPRNT.GE.2) THEN
         WRITE (LOUTIO,*)' +++LCASIG+++'
         WRITE (LOUTIO,*)' LTHT bank : nrows/ncols ',
     &          LROWS(KINDX),LCOLS(KINDX)
         WRITE (LOUTIO,*)' Tower hits bank LTHT'
         WRITE (LOUTIO,*)'   Module    icol    irow       Hits'
        DO 130 J = 1,LROWS(KINDX)
          KR = KROW(KINDX,J)
          MODU=LCMOD(IW(KR+1))
          IROW=LCROW(IW(KR+1))
          ICOL=LCCOL(IW(KR+1))
          WRITE(LOUTIO,5300) MODU,ICOL,IROW,(IW(KR+K),K=2,4)
  130   CONTINUE
 5300 FORMAT(6I8)
       ENDIF
C
C - Saturate Analog signals
      CALL LCSATU
C
      RETURN
C
C - not enough space
C
 998  CONTINUE
      CALL ALTELL ('LCASIG: not enough space for HIT banks ',1,'NEXT')
      END
#endif
