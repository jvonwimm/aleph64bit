*DK hchlwd
      SUBROUTINE HCHLWD
C --------------------------------------------
C
C!    Fill HLWD bank to remember the hit position along the tube
C!
C!       Author      : F.Ranjard - 911121
C!
C!       Input bank  :       HWHT and HCSE
C!       Output bank :       HLWD
C!
C!      -Called by : HCASIG
C!      -Calls     : MVBITS  from CERNLIB
C-------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "bcs.h"
#include "iocom.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hcsejj.h"
#include "hwhtjj.h"
#include "hlwdjj.h"
#include "hctrig.h"
#include "hcgega.h"
      PARAMETER (LHIT=3)
      DIMENSION ITUPR(LHIT),LCLUP(LHIT),ILAPR(LHIT),MODPR(LHIT),
     +          YHIT(LHIT)
#include "bmacro.h"
C ----------------------------------------------
C
C - fill the word(2) with the hit position along the tube (Y-coord)
C
      IF (JDHCHI.EQ.0) RETURN
      JHLWD = IW(NAHLWD)
      IF (JHLWD.EQ.0) RETURN
      NHCHI = LROWS(JDHCHI)
      DO 16 I=1,NHCHI
         IHCSE = ITABL(JDHCHI,I,1)
         KHLWD = KROW(JHLWD,I)
         IW(KHLWD+JHLWWA) = ITABL(JDHCSE,IHCSE,JHCSTA)
         RW(KHLWD+JHLWHP) = RTABL(JDHCSE,IHCSE,JHCSTY)
 16   CONTINUE
      IW(JHLWD+LMHROW) = LROWS(JDHCHI)
C
      NHLWD = LROWS (JHLWD)
      KHLWD = JHLWD + LMHLEN
C
C          Loop on tubes
C
      LCLU = 1
C
C - loop over hits
C
      DO 10 I = 1, NHLWD
         N = IW(KHLWD+JHLWWA)
         ITU = MOD(N,1000)
         N = N/1000
         ILA = MOD(N,100)
         N = N/100
         IMO = MOD(N,100)
         N = N/100
         IPO = N
C
C          new tube
            ILN = ILA - 1
            CALL MVBITS(ILN,0,8,IW(KHLWD+1),16)
            CALL MVBITS(LCLU,0,8,IW(KHLWD+1),8)
            IMN = IMO
C
            IF(IPO.EQ.LPBAR) THEN
C          Barrel
               IMN = (IMO-1)/ 2+ 1
               IMN = IMN + LPHCTR
               IF (MOD(IMO,2) .EQ. 0) THEN
                  ITN = NHCTU1(ILA)*2 - ITU
               ELSE
                  ITN = ITU - 1
               ENDIF
            ELSE
C          End-cap
               IMOD = IMO
               IF(IPO.EQ.LPECB) IMOD = IMO + LPHCTR/2
               IF(ITU.GT.NHCTRE) THEN
                  IMN = IXHCSE(IMOD)
                  ITN = ITU - NHCTRE - 1
               ELSE
                  IMN = IXHCFI(IMOD)
                  ITN = ITU - 1
               ENDIF
C
            ENDIF
C
C -      fill tube and module parts of the digit
            CALL MVBITS(ITN,0,8,IW(KHLWD+1),0)
            CALL MVBITS(IMN,0,8,IW(KHLWD+1),24)
C
         KHLWD = KHLWD + LHLWDA
   10 CONTINUE
C
      IF(FHCDEB)THEN
         WRITE(LOUTIO,500) LROWS(JHLWD)
         KHLWD = JHLWD + LMHLEN
         DO 30 I=1,LROWS(JHLWD),LHIT
            JMAX = MIN (LHIT,NHLWD-I+1)
            DO 20 J=1,JMAX
               N       = IW(KHLWD+1)
               ITUPR(J)= IBITS (N,0,8)
               ILAPR(J)= IBITS (N,16,8)
               MODPR(J)= IBITS (N,24,8)
               YHIT (J)= RW(KHLWD+2)
               IHCSE   = ITABL(JDHCHI,I+J-1,1)
               LCLUP(J)= ITABL(JDHCSE,IHCSE,JHCSTA)
   20       KHLWD = KHLWD + LHLWDA
            WRITE (LOUTIO,510) I,I+2,(LCLUP(J),MODPR(J),ILAPR(J),
     +      ITUPR(J),YHIT(J),J=1,JMAX)
   30    CONTINUE
      ENDIF
      RETURN
  500 FORMAT (/1X,'+++HCHLWD+++ HLWD hit position along the tube ',
     +         I5/ 8X,3(4X,'  Address   Mo  La  Tu  position '))
  510 FORMAT (1X,I3,'-',I3,3(4X,I10,3I4,F10.2,1X))
      END
#endif
