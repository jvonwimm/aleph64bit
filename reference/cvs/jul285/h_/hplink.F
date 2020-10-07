      LOGICAL FUNCTION HPLINK(IS10,IM10,L10,X10,W10,
     +                        IS20,IM20,L20,X20,W20)
C*************************************************************
C! Function to link cluster                                  *
C!                                                           *
C! Author  : G. BAGLIESI  880128                             *
C!                                                           *
C! VERSION 1.0                                               *
C!                                                           *
C! HPLINK=.TRUE.  ->   CLUSTER LINKED                        *
C! HPLINK=.FALSE. ->   CLUSTER NOT LINKED                    *
C!
C!
C! Two CLUSTERS are linked if their distance is less than 10.5 CM
C! on the same LAYER or less than 3.5 on different LAYERS
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "hdpcut.h"
#include "hcgeom.h"
#include "bmacro.h"
C
C ---- The width of barrel layers is calculated
C
      HPLINK = .FALSE.
      IS1=IS10
      IM1=IM10
      L1=L10
      X1=X10
      W1=W10
      IS2=IS20
      IM2=IM20
      L2=L20
      X2=X20
      W2=W20
C
C ---- If they are not in the same SUBC  or if the dist.
C ---- is greater than 2 layers
C
      IF(IS1.NE.IS2) THEN
         GOTO 20
      ENDIF
      IF(ABS(L1-L2).GT.2) THEN
         GOTO 20
      ENDIF
C
C The CLUSTER are reshuffled in the natural order
C
      WW1  =   W1/2.
      WW2  =   W2/2.
      X1MAX=X1+WW1
      X1MIN=X1-WW1
      X2MAX=X2+WW2
      X2MIN=X2-WW2
C
C ---- We are in the same module
C
      IF(IM1.EQ.IM2) THEN
         IF(L1.EQ.L2) THEN
            IF(ABS(X2-X1).LT.DMIXHD+WW1+WW2) HPLINK=.TRUE.
            GOTO 20
         ENDIF
         IF(ABS(L1-L2).LE.2) THEN
            IF(ABS(X2-X1).LT.DMIYHD+WW1+WW2) HPLINK=.TRUE.
            GOTO 20
         ENDIF
      ENDIF
C
C The minimum and maximum module number is calculated
C
      IMMIN=MIN(IM1,IM2)
      IMMAX=MAX(IM1,IM2)
C
C ---- We are in contiguous barrel modules
C
      IF(IS1.EQ.2.AND.(ABS(IM1-IM2).EQ.1.OR.ABS(IM1-IM2).EQ.23)) THEN
C
         IF(IMMIN.EQ.1.AND.IMMAX.EQ.24) IMMIN=24
C
C ---- There are two possible configurations
C
         IF(MOD(IMMIN,2).EQ.0) THEN
            IF(L1.EQ.L2) THEN
               IF(X1MIN+X2MIN.LT.BMIXHD) HPLINK=.TRUE.
               GOTO 20
            ENDIF
            IF(ABS(L1-L2).LE.2) THEN
               IF(X1MIN+X2MIN.LT.BMIYHD) HPLINK=.TRUE.
               GOTO 20
            ENDIF
C
         ELSE
C
            XBMA1 = HCLAWI(L1) - X1MAX
            XBMA2 = HCLAWI(L2) - X2MAX
            IF (L1.EQ.L2) THEN
               IF(XBMA1+XBMA2.LT.CMIXHD) HPLINK=.TRUE.
               GOTO 20
            ENDIF
            IF(ABS(L1-L2).LE.2) THEN
               IF(XBMA1+XBMA2.LT.CMIYHD) HPLINK=.TRUE.
               GOTO 20
            ENDIF
C
         ENDIF
      ENDIF
C
C   Here we are in contiguous modules
C   in the 2nd endcap zone (X STRIPS )
C
      IF((IS1.EQ.1.OR.IS1.EQ.3).AND.((IMMIN.EQ.3.AND.IMMAX.EQ.4).OR.
     +(IMMIN.EQ.1.AND.IMMAX.EQ.6))) THEN
         IF(L1.EQ.L2) THEN
            IF(X1MIN+X2MIN.LT.BMIXHD) HPLINK=.TRUE.
            GOTO 20
         ENDIF
         IF(ABS(L1-L2).LE.2) THEN
            IF(X1MIN+X2MIN.LT.BMIYHD) HPLINK=.TRUE.
            GOTO 20
         ENDIF
      ENDIF
   20 CONTINUE
      RETURN
      END
#endif
