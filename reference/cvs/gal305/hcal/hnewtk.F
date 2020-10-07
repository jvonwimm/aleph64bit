*DK hnewtk
      SUBROUTINE HNEWTK
C-----------------------------------------------------------
C
C!Update same Statistical informations when a new track enter in HCAL
C!
C!             Author    :G.Catanesi  87/05/25
C!
C!             Called by  : HCHIT
C!             Calls      : none
C!
C --------------------------------------------
#ifndef DOC
C
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hcthjj.h"
#include "trkcom.h"
#include "bmacro.h"
#include "kmacro.h"
C ----------------------------------------------------------
C   CREATE A TEMPORARY BANK 'HCTH' at the beginning of the event
C
      IF(FBEGJO(7))THEN
         CALL WBANK(IW,JDHCTH,MHCTH*LHCTHA+LMHLEN,*10)
         IW(JDHCTH+1) = LHCTHA
         IW(JDHCTH-3) = INTCHA('HCTH')
         IW(JDHCTH+2) = 0
         IOLDT = 0
      ENDIF
C
      IF (ITRKEL(1) .EQ. IOLDT) RETURN
      IOLDT = ITRKEL(1)
C
C Check if exist enough space to fill a row
C
      IF (LFRWRD(JDHCTH) .LT. LHCTHA) THEN
         CALL WBANK (IW,JDHCTH,IW(JDHCTH)*2,*10)
      ENDIF
C
      JKINE = NLINK ('KINE',ITRKEL(1))
C
C   FILL THE NEXT ROW OF THE HCTH BANK
C
      KHCTH = KNEXT(JDHCTH)
      IW(KHCTH+1) = ITRKEL(1)
      IF (ITRKEL(2) .EQ. 0) RW(KHCTH+3) = TRKELE(8)
C
      IW(JDHCTH +LMHROW) = IW(JDHCTH +LMHROW) + 1
C
      GOTO 20
C
C Not enough space  to extend JDHCTH
C
   10 CONTINUE
      CALL WDROP(IW,JDHCTH)
      WRITE (LOUTIO,*)' +++HNEWTK+++ no space to enlarge JDHCTH ',
     &                '- NO printout'
      FHCDEB = .FALSE.
C
   20 CONTINUE
C
      RETURN
      END
#endif
