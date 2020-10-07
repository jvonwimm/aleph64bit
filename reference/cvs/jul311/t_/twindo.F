      SUBROUTINE TWINDO(ISEC,R01,R02,IT1,IT2,NW,R0,BK,MAXS,SIG)
C-------------------------------------------------------------------
C!   Pick wire pulses within wire range R01 to R02 and time
C   window IT1 to IT2 and return the number of pulses found in NW
C   and their positions and bucket numbers in the arrays R0 and BK.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeow.h"
#include "twirct.h"
C
      DIMENSION R0(*),BK(*),SIG(*)
#include "bmacro.h"
C
      B1 = FLOAT(IT1)
      B2 = FLOAT(IT2)
      IF (B1.GT.B2) THEN
        BS = B1
        B1 = B2
        B2 = BS
      ENDIF
C
      ITYPE = ITPTYP(ISEC)
      KTWRR = NLINK('TWRR',ISEC)
      IWIR1 = (R01 - TWIRE1(ITYPE))/TWSTEP(ITYPE) + 1
      IWIR2 = (R02 - TWIRE1(ITYPE))/TWSTEP(ITYPE) + 1
      NW = 0
      IF (KTWRR.EQ.0) GO TO 999
C
      DO 20 I=1,LROWS(KTWRR)
         IWPK = ITABL(KTWRR,I,1)
         IQUAL = IBITS(IWPK,13,1)
         IF (IQUAL.NE.1) GO TO 20
         IWIRN = IBITS(IWPK,24,8)
         IF (IWIRN.LT.IWIR1.OR.IWIRN.GT.IWIR2) GO TO 20
         ITIM  = IBITS(IWPK,0,13)
         BUCK = FLOAT(ITIM)/TPACKW
         IF (BUCK.LT.B1.OR.BUCK.GT.B2) GO TO 20
         NW = NW + 1
         R0(NW) = TWIRE1(ITYPE) + FLOAT(IWIRN-1)*TWSTEP(ITYPE)
         BK(NW) = BUCK
C Use nominal error (in bucket units)
         SIG(NW) = 0.5
         IF (NW.EQ.MAXS) THEN
            CALL RERROR('TWINDO',1,'Truncate NW to 50')
            GO TO 999
         ENDIF
   20 CONTINUE
  999 RETURN
      END
#endif
