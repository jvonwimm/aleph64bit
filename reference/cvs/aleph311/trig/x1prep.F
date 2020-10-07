      SUBROUTINE X1PREP(IERR)
C----------------------------------------------------------------------
C!  - Prepare the bank XTDI used for the trigger analysis
C.
C.   Author   :- Alois Putzer           4-AUG-1989
C.               Martin Wunsch          4-AUG-1989
C.               Yves A. Maumary       20-DEC-1989 Adapted for ALEPHLIB
C!   Modified :- Yves A. Maumary       16-MAR-1990 for '90 run
C.
C.   Inputs:
C.        - none
C.
C.   Outputs:
C.        - IERR : INTEGER = 0 : creation and preparation of the bank
C.                               successful
C.                         = 1 : creation or preparation of the bank
C.                               failed
C.
C.   Libraries required: ALEPHLIB, BOS77, CERNLIB
C.
C.   Calls: ALTELL, AUBOS, BKFMT
C.
C?   Description
C?   ===========
C?   Creates the XTDI bank and prepares it (fill in names and set
C?   'filled' tag = 0)
C?
C.======================================================================
#ifndef DOC
      SAVE
      EXTERNAL INTCHA,NAMIND
#include "bcs.h"
#include "x1namc.h"
#include "x1para.h"
#include "xtdijj.h"
      CHARACTER*4 RNAME
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      IERR = 0
      IF(FIRST)THEN
C - fill strings CHXTD and CHX1S from PARAMETER statement
C   CHXTDI and CHX1SC and then use these strings to please
C   IBM compiler
        CHXTD = CHXTDI
        CHX1S = CHX1SC
        FIRST = .FALSE.
      ENDIF
C
C - Create bank XTDI
      LTDI = LXTDIA*LROWDI + LMHLEN
      CALL AUBOS('XTDI',1,LTDI,INDXS,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL ALTELL('X1PREP: no space to book XTDI',0,'RETURN')
        IERR = 1
        RETURN
      ENDIF
      CALL BKFMT('XTDI','2I,(A,4I)')
C - Prepare bank XTDI
      NAXTDI = NAMIND('XTDI')
      INDXS = IW(NAXTDI)
      IF(INDXS.NE.0)THEN
        IW(INDXS+LMHCOL) = LXTDIA
        IW(INDXS+LMHROW) = LROWDI
        IPOIN = INDXS + LMHLEN
C -- Fill row names for bank XTDI and reset fill tag
        DO 10 J = 1,LROWDI
          RNAME = CHXTD((J-1)*4+1:J*4)
          IW(IPOIN+JXTDNA) = INTCHA(RNAME)
          IW(IPOIN+JXTDFI) = 0
          IPOIN = IPOIN + LXTDIA
   10   CONTINUE
      ELSE
        IERR = 1
      ENDIF
      RETURN
      END
#endif
