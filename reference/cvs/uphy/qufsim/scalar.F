      REAL FUNCTION SCALAR(IT1,IT2,IOP,NDOT)
C--------------------------------------------------------------
C! IOP= 1  scalar IT1/IT2
C! IOP= 2  scalar IT1/total momentum
C--------------------------------------------------------------
#include "lundcom.h"
      SCALAR= 0.
      IF(NDOT.LT.1.OR.NDOT.GT.3) RETURN
      XT1= 0.
      XT2= 0.
      X12= 0.
      IF(IOP.EQ.1) THEN
       DO L1=1,NDOT
        XT1= p7lu(IT1,L1) * p7lu(IT1,L1) + XT1
        XT2= p7lu(IT2,L1) * p7lu(IT2,L1) + XT2
        X12= p7lu(IT1,L1) * p7lu(IT2,L1) + X12
       END DO
      ELSE IF(IOP.EQ.2) THEN
       DO L1=1,NDOT
        XT1= p7lu(IT1,L1) * p7lu(IT1,L1) + XT1
        XT2= PLU(0,L1)      * PLU(0,L1)      + XT2
        X12= p7lu(IT1,L1) * PLU(0,L1)      + X12
       END DO
      END IF
      IF(XT1.GT.(.0).AND.XT2.GT.(.0)) THEN
       SCALAR= X12/SQRT(XT1)/SQRT(XT2)
      END IF
C
      RETURN
      END
