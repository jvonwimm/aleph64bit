      SUBROUTINE TAGVAR5
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "comnnt.h"
#include "qmacro.h"
C
         IFLAG5 = 0
         XJFM = -1
C
         IF(NJET.LT.1) THEN
         IFLAG5 = 1
         GOTO 1000
         ENDIF
C
      ENERGY = 0.
      XMASS  = 0.
      JET    = 0
      ENERGY = QE(JJ1)
C
      EKKM  = 0.
      PXKKM = 0.
      PYKKM = 0.
      PZKKM = 0.
      DO L = IG , ID
      IF (XSAME(L,J)) THEN
      EKKM = EKKM + QE(L)
      PXKKM= PXKKM+ QX(L)
      PYKKM= PYKKM+ QY(L)
      PZKKM= PZKKM+ QZ(L)
      ENDIF
      ENDDO
C
      XMKKM  = EKKM**2 - PXKKM**2 - PYKKM**2 - PZKKM**2
      IF (XMKKM.LE.0.) THEN
      XMKKM  = 0.
      ELSE
      XMKKM  = SQRT (XMKKM)
      ENDIF
      IF (SCHEME.EQ.'E') THEN
      XMASS = QM(JJ1)
      ELSE
      XMASS =XMKKM
      ENDIF
C
      JET    = J1
C
      IF(XMASS.LT.0.001) THEN
        IFLAG5 = 1
        GOTO 1000
      ENDIF
C
      CALL JETFOR (KRECO,JET,GAMMANAT,ENERGY,XMASS,IFLAG,XJFM)
C
         IF (IFLAG.EQ.0) THEN
         IFLAG5 = 1
         GOTO 1000
         ENDIF
C
 1000 CONTINUE
C
      RETURN
      END
*AF
#endif
