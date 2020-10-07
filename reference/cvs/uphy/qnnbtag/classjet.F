      SUBROUTINE CLASSJET(ICLASS)
C-----------------------------------------------------
C     Select the two most energetic jets : J1 and J2
#include "qcde.h"
#include "comnnt.h"
      REAL       E1, E2
#include "qmacro.h"
C
      E1  = 0.0
      E2  = 0.0
      J1  = 0
      J2  = 0
      JJ1 = 0
      JJ2 = 0
C
      IF( NJET .GT. 0 ) THEN
      DO  210  J = 1, NJET
C
      IF( J .EQ. 1 ) THEN
      JJ = KPDIR(CNAM, ICLASS )
      ELSE
      JJ = KFOLLO( JJ )
      ENDIF
C
      IF( QE(JJ) .GT. E1 ) THEN
      E2  = E1
      J2  = J1
      JJ2 = JJ1
      E1  = QE(JJ)
      J1  = J
      JJ1 = JJ
      ELSEIF( QE(JJ) .GT. E2 ) THEN
      E2  = QE(JJ)
      J2  = J
      JJ2 = JJ
      ENDIF
C
  210 CONTINUE
C
      ENDIF
      RETURN
      END
C
*AF
