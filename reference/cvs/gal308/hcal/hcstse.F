*DK hcstse
      SUBROUTINE HCSTSE(VTUB,IPOR,IMOD,ILAY,XTUB,YTUB,DYTUB,ITUB,NTUB)
C ----------------------------------------------------
C - F.Ranjard - 860529
C-  mod by G.Catanesi 871021
C - mod by F.Ranjard  890523 : do not keep negative tube #
C!  mod by A.Venturi 980701 : allow one set of absolute coordinates
C!  for each track segment: VTUB(100,3)
C! - Fill the bank JDHCSE with the NTUB fired tubes with
C!   numbers listed in ITUB from plane # IPLAN
C
C ------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "bcs.h"
#include "hcnamc.h"
#include "hcsejj.h"
#include "trkcom.h"
      DIMENSION VTUB(3,*),XTUB(*),YTUB(*),DYTUB(*),ITUB(*)
C
#include "bmacro.h"
C --------------------------------------------------------
C
      ND = NTUB*LHCSEA
      IF (LFRWRD(JDHCSE) .LT. ND) THEN
         ND = MAX (ND,IW(JDHCSE)/2)
         CALL WBANK (IW,JDHCSE,IW(JDHCSE)+ND,*20)
      ENDIF
      KHCSE = KNEXT (JDHCSE)
      DO 10 J = 1, NTUB
         IF (ITUB(J) .LT. 0) GOTO 10
         IW(KHCSE+1) = ITUB(J)
         IW(KHCSE+2) = ILAY
         IW(KHCSE+3) = IMOD
         IW(KHCSE+4) = IPOR
         ITADD = ITUB(J) + ILAY*1000+IMOD*100000+IPOR*10000000
         IW(KHCSE+5) = ITADD
         RW(KHCSE+6) = XTUB(J)
         RW(KHCSE+7) = YTUB(J)
         RW(KHCSE+8) = DYTUB(J)
         RW(KHCSE+9) = VTUB(1,J)
         RW(KHCSE+10) = VTUB(2,J)
         RW(KHCSE+11) = VTUB(3,J)
         IW(KHCSE+12) = ITRKEL(1)
C
         IW(JDHCSE+LMHROW) = IW(JDHCSE+LMHROW) + 1
         KHCSE = KHCSE + LHCSEA
 10   CONTINUE
      RETURN
C
C       not enough space to extend JDHCSE
C
   20 CONTINUE
      CALL WDROP (IW,JDHCSE)
      CALL ALTELL ('HCSTSE: not enough space to enlarge JDHCSE ',1,
     &'NEXT')
      RETURN
      END
#endif
