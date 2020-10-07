*DK hchise
      SUBROUTINE HCHISE
C --------------------------------------------------------------
C - F.RANJARD - 860619            modified - 890801
C! order HTHT in increasing order of storey address
C  modify HCSE accordingly
C - Called by   HCASIG                                from this .HLB
C --------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "iocom.h"
#include "bcs.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hthtjj.h"
#include "hcsejj.h"
      INTEGER JDCKEY
      DATA JDCKEY /0/
#include "bmacro.h"
C -------------------------------------------------------------------
      JHTHT = IW(NAHTHT)
      IF (JHTHT.EQ.0) GOTO 999
      NHTHT = LROWS(JHTHT)
      IF (NHTHT.EQ.0) GOTO 999
      IF (FHCDB1) THEN
         KHCSE = JDHCSE + LMHLEN
         WRITE (IW(6),802) (IW(K+JHCSFS),K=KHCSE,KHCSE+LROWS(JDHCSE)*
     &                      LHCSEA-1,LHCSEA)
      ENDIF
C
C - get the positions in HTHT of storey addresses
      CALL WBANK (IW,JDCKEY,NHTHT,*998)
      DO 1 I=1,NHTHT
         IW(JDCKEY+I) = (I-1)*LHTHTA + JHTHSA
 1    CONTINUE
C
C - get the position in HTHT of storey addresses when they are in
C   increasing order
      CALL SORTZV (IW(JHTHT+LMHLEN+1),IW(JDCKEY+1),NHTHT,-1,0,1)
C
C - get the new row # instead of the new position
      DO 2 I=1,NHTHT
         IW(JDCKEY+I) = (IW(JDCKEY+I) - JHTHSA)/LHTHTA + 1
 2    CONTINUE
C
C - update HCSE
      KHCSE = JDHCSE + LMHLEN
      NHCSE = LROWS(JDHCSE)
      DO 3 I=1,LROWS(JDHCSE)
         IF (IW(KHCSE+JHCSFS).EQ.0) GOTO 3
         IW(KHCSE+JHCSFS) = IUCOMP(IW(KHCSE+JHCSFS),IW(JDCKEY+1),NHTHT)
 3    KHCSE = KHCSE + LHCSEA
      KHCSE = JDHCSE + LMHLEN
C
C - reoder HTHT in increasing order of storey address
      CALL SORTIQ (IW(JHTHT+LMHLEN+1),LHTHTA,NHTHT,JHTHSA)
C
C - debug
      IF (FHCDB1) THEN
         WRITE (IW(6),'(/1X,''+++HCHISE+++ HTHT in increasing order'')')
         KHTHT = JHTHT + LMHLEN
         WRITE (IW(6),801) ((IW(K+I),I=1,3),RW(K+4),K=KHTHT,KHTHT+
     &                      NHTHT*LHTHTA-1,LHTHTA)
         KHCSE = JDHCSE + LMHLEN
         WRITE (IW(6),802) (IW(K+JHCSFS),K=KHCSE,KHCSE+LROWS(JDHCSE)*
     &                      LHCSEA-1,LHCSEA)
      ENDIF
 999  CONTINUE
      CALL WDROP (IW,JDCKEY)
      RETURN
 801  FORMAT (3(I10,I4,I4,F10.5,5X))
 802  FORMAT (/1X,'+++HCHISE+++ HCSE word(15): HTHT row n0.'/(5X,25I4))
C - NOT enough space to enlarge work bank
 998  CALL WDROP (IW,JDCKEY)
      CALL ALTELL ('HCHISE: BOS array too small',1,'NEXT')
      END
#endif
