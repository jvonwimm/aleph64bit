*DK hcieve
      SUBROUTINE HCIEVE
C-----------------------------------------------------
C! Initilize or upgrade variables and counters at the beginning of the e
C!
C!       Author       :G.Catanesi   86/05/22
C!       Modified by  :G.Catanesi   87/07/10
C!
C!       Called by : HCHIT
C!       Calls     : VZERO from CERNLIB
C!                 : BDROP,WBANK from BOS77
C!                 : ALTELL from ALEPHLIB
C------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "jobcom.h"
#include "trkcom.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hcsejj.h"
#include "bmacro.h"
C.
C           Check for old hit banks and drop them
C
      IDRP = IW(NAHTHT)
      IF (IDRP.NE.0) CALL BDROP(IW,
     +  'HTHTHWHTHTTRHWTRHTTDHWTDHTDTHWDTHPDIHPHT')
C
C - Book work bank JDHCSE
C
      CALL WBANK (IW,JDHCSE,MHCSE*LHCSEA+LMHLEN,*10)
      IW(JDHCSE+1) = LHCSEA
      IW(JDHCSE+2) = 0
      IW(JDHCSE-3) = INTCHA ('HCSE')
C
C     INITIALIZE COUNTERS AT THE BEGINNING OF THE EVENT
C
      FHCDEB = FDEBJO .AND. IPRIJO(7).NE.0
      FHCDB2 = FHCDEB .AND. ICHCJO(1).EQ.2
      FHCDB1 = FHCDEB .AND. ICHCJO(1).GT.0
      HCEPOR(1) = 0.
      HCEPOR(2) = 0.
      HCEPOR(3) = 0.
C
      GOTO 20
   10 CONTINUE
      CALL ALTELL ('HCIEVE: not enough space to book JDHCSE ',1,'NEXT')
C
   20 CONTINUE
      RETURN
      END
#endif
