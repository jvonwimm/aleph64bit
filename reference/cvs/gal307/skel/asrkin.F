*DK asrkin
      SUBROUTINE ASRKIN
C ----------------------------------------------------------
C - F.Ranjard - 851023
C - Transfer contents of 'VERT' banks to Geant3 'VERT' banks
C   and do the same for 'KINE' banks
C - Drop 'VERT' and 'KINE' BOS banks
C   release space.
C - Called from    ASRETP                    from this .HLB
C - Calls          MZBOOK                    from ZEBRA.lib
C                  BDROP                     from BOS77.hlb
C ----------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
#include "jobcom.h"
#include "gcbank.h"
#include "gcnum.h"
#include "kincom.h"
      DATA IFI /0/
#include "bmacro.h"
#include "kmacro.h"
C --------------------------------------------------------------
C
C - Get the ALEPHLIB version # used in KINGAL or GALEPH
      IF (IFI .EQ. 0) THEN
         IFI = 1
         CALL KIAVER (AVER,IPROG)
      ENDIF
C - Get PART bank index
      JPART = IW(NAPART)
C
C - Transfer 'VERT' banks
C
      CALL MZBOOK (IGXDIV,JGVERT,JGVERT,1,'VERT',NIVXKI,NIVXKI,1,2,0)
      IGB(JGVERT-5) = 0
      KV = NLINK ('VERT',1)
 6    CONTINUE
      IF (KV.NE.0) THEN
         NVX  = IW(KV-2)
         IF (NVX .GT. NIVXKI) GOTO 69
         KV1 = KPARVK(KV)
         KV2 = KLISVK(KV)
         CALL MZBOOK (IGXDIV,JV,JGVERT,-NVX,'VERT',0,0,IW(KV+3)+7,3,0)
         GB(JV+1) = RW(KV1+1)
         GB(JV+2) = RW(KV1+2)
         GB(JV+3) = RW(KV1+3)
         GB(JV+4) = RW(KV1+4)
         GB(JV+5) = IW(KV1+5)
         GB(JV+6) = 0.
         GB(JV+7) = NOFVK(KV)
         DO 61 N=1,NOFVK(KV)
            GB(JV+7+N) = IW(KV2+N)
 61      CONTINUE
         NGVERT = MAX (NVX,NGVERT)
         IGB(JGVERT+1) = NGVERT
         KV = IW(KV-1)
         GOTO 6
      ENDIF
 69   CONTINUE
C
C - Transfer 'KINE' banks
C
      CALL MZBOOK (IGXDIV,JGKINE,JGKINE,1,'KINE',NITRKI,NITRKI,1,2,0)
      IGB(JGKINE-5) = 0
      KK = NLINK ('KINE',1)
7     CONTINUE
      IF (KK.NE.0) THEN
         NKI  = IW(KK-2)
         IF (NKI .GT. NITRKI) GOTO 79
         KK1  = KPARVK(KK)
         KK2  = KLISVK(KK)
         CALL MZBOOK (IGXDIV,JK,JGKINE,-NKI,'KINE',0,0,IW(KK+3)+6,3,0)
         GB(JK+1) = RW(KK1+1)
         GB(JK+2) = RW(KK1+2)
         GB(JK+3) = RW(KK1+3)
C        In the ALEPHLIB 9.0 the KINE and FKIN banks have been modified
C        word(4) has been changed to contain mass instead of energy.
C        to be backward compatible the following test is necessary.
C        when files created with an ALEPHLIB earlier than 9.0 will have
C        disappeared only the statment GB(JK+4)=ENERVK(KK) will be necessary.
         IF (AVER .LT. 9.0) THEN
            GB(JK+4) = RW(KK1+4)
         ELSE
            GB(JK+4) = ENERVK(KK)
         ENDIF
         GB(JK+5) = ITABL (JPART,IW(KK2),1)
         GB(JK+6) = IW(KK2+1)
         GB(JK+7) = NOFVK(KK)-1
         DO 71 N=2,NOFVK(KK)
            GB(JK+6+N) = IW(KK2+N)
 71      CONTINUE
         NGTRAC = MAX (NKI,NGTRAC)
         IGB(JGKINE+1) = NGTRAC
         KK = IW(KK-1)
         GOTO 7
      ENDIF
 79   CONTINUE
C
C ----------- end -------------------------------------
 999  CONTINUE
      RETURN
      END
#endif
