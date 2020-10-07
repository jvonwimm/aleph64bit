      INTEGER FUNCTION MECRFC(LST33,KSTAG)
C.----------------------------------------------------------------------
CKEY GAMPACK CRACK / INTERNAL
C   J.C.Brient      Creation  1/10/91
C!  Test ECAL crack for photon
C   Input :
C           LST33   central matrix of the photon INTEGER
C           KSTAG   stack number                 INTEGER
C   Output:
C           Function = 1 if the photon is near a ECAL crack
C   Calls: ESRBC,EBCDRG
C   Called by GAMPEK
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
C
C--for INPUT DIMENSION
      DIMENSION LST33(3,6,3)
C
C--for EBCDRG
      DIMENSION IKOD(4) , NREG(3)
      DIMENSION IDEST(3) , ICLST(3) , POINT(3)
#include "bmacro.h"
C
      MECRFC = 0
C
      DO 150 I = 1 , 3
        DO 100 J = 1, 6
          IT = LST33(I,J,1)
          JF = LST33(I,J,2)
          KS = LST33(I,J,3)
          IF(KS .NE. KSTAG ) GO TO 100
          IF(IT .LE. 0 .OR. JF .LE. 0 ) GO TO 100
          CALL ESRBC('ALEPH',IT,JF,KS,POINT)
          PM= VMOD ( POINT , 3 )
          TE = ACOS ( POINT(3) / PM )
          FE = ATAN2( POINT(2) , POINT(1)  )
          CALL EBCDRG(TE,FE,ITT,JFF,IKOD,NREG,IER)
          IF(IKOD(3) .NE. 0 ) THEN
            MECRFC = 1
            RETURN
          ENDIF
          IF(IKOD(2) .EQ. 1 ) THEN
            MECRFC = 1
            RETURN
          ENDIF
          IF(IER .EQ. 1 ) THEN
            MECRFC = 1
            RETURN
          ENDIF
  100   CONTINUE
  150 CONTINUE
C
      RETURN
      END
#endif
