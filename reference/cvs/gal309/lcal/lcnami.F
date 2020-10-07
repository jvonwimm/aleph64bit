*DK lcnami
      SUBROUTINE LCNAMI
C--------------------------------------------------------------
C! Prepare BOS banks
C! Author     : P.Hansen & J. Dines Hansen 860801
C! Modified   : P.Hansen 870920
C! Desciption :
C! ============
C! Set name-indices for LCal BOS banks
C! Set printing formats
C!
C! - called from LCIRUN                          from this .HLB
C! - calls       NAMIND,BKFMT                    from BOS
C!
C -------------------------------------------------
#ifndef DOC
      EXTERNAL NAMIND
#include "iocom.h"
#include "bcs.h"
#include "lcnamc.h"
#include "lccomc.h"
C -------------------------------------------------------------
C
      NALALI = NAMIND('LALI')
      NALCAL = NAMIND('LCAL')
      NALLAY = NAMIND('LLAY')
      NALMTY = NAMIND('LMTY')
      NALSCO = NAMIND('LSCO')
      NALSLO = NAMIND('LSLO')
      NALWRG = NAMIND('LWRG')
      NALDRE = NAMIND('LDRE')
      NALCCA = NAMIND('LCCA')
      NALCPG = NAMIND('LCPG')
      NALCEL = NAMIND('LCEL')
      NALSHO = NAMIND('LSHO')
C
      NALSHI = NAMIND('LSHI')
      NALWHI = NAMIND('LWHI')
      CALL BKFMT('LSHI','I')
      CALL BKFMT('LWHI','I')
C
      NALWHT = NAMIND('LWHT')
      CALL BKFMT('LWHT','(I)')
C
      NALWDI = NAMIND('LWDI')
      CALL BKFMT('LWDI','(I)')
C
      NALTHT = NAMIND('LTHT')
      CALL BKFMT('LTHT','(I)')
C
      NALTDI = NAMIND('LTDI')
      CALL BKFMT('LTDI','(I)')
C
      NALTTR = NAMIND('LTTR')
      CALL BKFMT('LTTR','(I)')
C
      NALWTR = NAMIND('LWTR')
      CALL BKFMT('LWTR','(I)')
C
      RETURN
C
      END
#endif
