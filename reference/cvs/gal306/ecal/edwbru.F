*DK edwbru
      SUBROUTINE EDWBRU
C-----------------------------------------------------------------
C!  SIMULATE NOISE ON WIRE READOUT
C
C   AUTHOR   : B. MICHEL, D. PALLIN 05/89
C   MODIFIED :
C   Called by : ECDIGI
C   Calls     : EPRIMI  from this HLB
C             : RANNOR       Genlib
C----------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "ehpash.h"
#include "ecnamc.h"
#include "bcs.h"
#include "ecsize.h"
#include "ewdijj.h"
#include "ewtmco.h"
C
C NOISE PER PLANE AND FOR THE SUM IN KEV
C ST3EC1 , ST3BA1 : SAMPLING IN STACK 3 FOR END-CAPS AND BARREL
      PARAMETER (NTMODU = LCOMP * LMODU)
      PARAMETER(PLCOHE=300,PLUNCO=1000,SMCOHE=17100,SMUNCO=7550)
      DIMENSION JSUM(NTMODU),SAMTIM(8),SAMSUM(8)
      EXTERNAL EPRIMI
      DATA(SAMTIM(I),I=1,8)/0.,512.,1024.,1536.,2048.,2560.,3072.,3584./
#include "bmacro.h"
C
      DO 11 I=1,8
   11 SAMSUM(I) = 0.
      DO 12 I=1,NTMODU
      DO 12 J=1,LEWDIA
   12 IWIRES(J,I) = 0
      DO 5 J=1,NTMODU
      JSUM(J) = 0
 5    IWIRES(1,J)=J
C
      KEWHT=IW(NAEWHT)
      IF(KEWHT.EQ.0)GO TO  13
      NEWHT=LROWS(KEWHT)
C
      DO 1 J=1,NEWHT
      KLINE=KROW(KEWHT,J)
      JJ=IW(KLINE+1)
      DO 1 I=2,LWPLA + 1
      IWIRES(I,JJ)=IW(KLINE+I)
      JSUM(JJ)=JSUM(JJ)+IWIRES(I,JJ)
C Stack 3
      IF(I.GE.35)                   THEN
         IF(JJ.LT.13.OR.JJ.GT.24)THEN
C            End caps
             IWIRES(I,JJ)=NINT(REAL(IWIRES(I,JJ))/ST3EC1)
         ELSE
C            Barrel
             IWIRES(I,JJ)=NINT(REAL(IWIRES(I,JJ))/ST3BA1)
         ENDIF
C
       ENDIF
 1    CONTINUE
 13   CONTINUE
C
C   Add analog sum sampling
C
      OFFSET=EPRIMI(SAMTIM(1))
      DO 2 J=1,NTMODU
        DO 3 K=2,8
        SAMSUM(K)=FLOAT(JSUM(J))*(EPRIMI(SAMTIM(K))-OFFSET)
 3      IWIRES(K+46,J)=IFIX(SAMSUM(K))
C
C   Noise simulation
C
      CALL RANNOR(SIG1,SIG2)
      PLCO=PLCOHE*SIG1
      SMCO=SMCOHE*SIG1
      DO 4 K=2,LEWDIA
      CALL RANNOR(SIG1,SIG2)
      IF(K.LT.JEWDSS)        THEN
                PLUN=PLUNCO*SIG1
                BRUIT=PLCO+PLUN
      ELSE
                SMUN=SMUNCO*SIG2
                BRUIT=SMCO+SMUN
      ENDIF
 4    IWIRES(K,J)=IWIRES(K,J)+IFIX(BRUIT)
 2    CONTINUE
C
      RETURN
      END
#endif
