               SUBROUTINE HDEADM
C-------------------------------------------------------------------
CKEY HCALDES HCAL DEAD TUBE  / INTERNAL
C
C!   Create and fill Look_up Tables for Hcal dead Tubes
C!
C!                            Author:G.Catanesi 1/08/89
C!                            Modify:L.Silvestris 07/07/90
C!
C - Banks : read HTXD
C         : read HRDT
C-------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "hcgeom.h"
#include "hjjpar.h"
#include "hcdetu.h"
C
#include "bmacro.h"
C? Set to zero the Barrel table
      DO 1 I=1,LPHCBM
         DO 2 J=1,LHCNL
            MH8FDB(I,J)=0
            LSH8DB(I,J)=0
 2       CONTINUE
 1    CONTINUE
C
C  Set to zero the EndCap table
C
      DO 3 K=1,LPHCES*2
         DO 4 N1= 1,LHCNL-1
            DO 5 N2=1,2
               MH8FDE(K,N1,N2) = 0
               LSH8DE(K,N1,N2) = 0
 5          CONTINUE
 4       CONTINUE
 3    CONTINUE
C
C     Look the unrecoverable dead tubes
C
      JHTXD = IW(NAMIND( 'HTXD'))
      IF(JHTXD.EQ.0) GOTO 90
C
      NHTXD = LROWS(JHTXD)
      IF(NHTXD.EQ.0)GOTO 90
C
C? Loop on bank HTDX
      DO 6 K=1,NHTXD
C
         IADR  =  ITABL(JHTXD,K,JHTXEA)
C
C? Decode the Adress: IADR ==> ITUB, ILAY, IMOD, IPOR
#include "htdadd.h"
C
C? Fill the Look_up Table
      IF(IPOR.EQ.LPBAR)THEN
C?     Barrel Case
         IF(ITUB.GT.NEITHC(ILAY))THEN
C           even module
            ITUB = 2*NEITHC(ILAY)+1 - ITUB
            IMOD = IMOD*2
         ELSE
C           odd module
            IMOD = IMOD*2 -1
         ENDIF
         MH8FDB(IMOD,ILAY) = MH8FDB(IMOD,ILAY)+2**ITUB
      ELSE
C?     EndCap
         IF(IPOR.EQ.LPECB)IMOD = IMOD + LPHCES
         IF(MOD(ITUB,2).EQ.0)THEN
            ITUB = ITUB/2
            IDTU = 2
         ELSE
            ITUB = ITUB/2 + 1
            IDTU = 1
         ENDIF
         MH8FDE(IMOD,ILAY,IDTU) = MH8FDE(IMOD,ILAY,IDTU) + 2**ITUB
      ENDIF
C
 6    CONTINUE
C
90    CONTINUE
C
C     Look the recoverable dead tubes
C
      JHRDT = IW(NAMIND( 'HRDT'))
      IF(JHRDT.EQ.0) GOTO 99
C
      NHRDT = LROWS(JHRDT)
      IF(NHRDT.EQ.0)GOTO 99
C
C? Loop on bank HRDT
      DO 16 K=1,NHRDT
C
         IADR  =  ITABL(JHRDT,K,JHRDEA)
C
C? Decode the Adress: IADR ==> ITUB, ILAY, IMOD, IPOR
#include "htdadd.h"
C
C? Fill the Look_up Table
      IF(IPOR.EQ.LPBAR)THEN
C?     Barrel Case
         IF(ITUB.GT.NEITHC(ILAY))THEN
C           even module
            ITUB = 2*NEITHC(ILAY)+1 - ITUB
            IMOD = IMOD*2
         ELSE
C           odd module
            IMOD = IMOD*2 -1
         ENDIF
         LSH8DB(IMOD,ILAY) = LSH8DB(IMOD,ILAY)+2**ITUB
      ELSE
C?     EndCap
         IF(IPOR.EQ.LPECB)IMOD = IMOD + LPHCES
         IF(MOD(ITUB,2).EQ.0)THEN
            ITUB = ITUB/2
            IDTU = 2
         ELSE
            ITUB = ITUB/2 + 1
            IDTU = 1
         ENDIF
         LSH8DE(IMOD,ILAY,IDTU) = LSH8DE(IMOD,ILAY,IDTU) + 2**ITUB
      ENDIF
C
16    CONTINUE
C
99    CONTINUE
      RETURN
      END
#endif
