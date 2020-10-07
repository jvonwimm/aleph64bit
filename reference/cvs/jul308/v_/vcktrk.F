      SUBROUTINE  VCKTRK(ITRK,ITCO,IHIT)
C-----------------------------------------------------
C!    returns number of expected VDET hits for a track ITRK
C!
C!
C!    by Hans-Guenther Moser
C!
C!    created:           01/06/89
C!
C!    last modification: 01/06/89
C!
C!    -----------------------------------------
C!
C!    can be called after the VDET track-hit association has been done
C!    (after call to VDLINK)
C!    uses the information in the VPRT banks (track extrapolation banks)
C!
C!    ITRK   (in) : track number
C!    ITC0   (in) : = 1 if ITC only track (0 if TPC used)
C!    IHIT   (out): number of hits expected (-1 if error occurred)
C!
C!
C!    if ITCO = 0 it is assumed that ITRK is a track number in
C!    TGFT #1
C!    if ITCO = 1 it is assumed that ITRK is a track number in
C!    ITFT #2
C!
C!
C     =========================================
#ifndef DOC
C
#include "bcs.h"
#include "bmacro.h"
C
      IF(ITCO.EQ.0) ICODE =  1000 + ITRK
      IF(ITCO.EQ.1) ICODE = 12000 + ITRK
C
      NPRT = NLINK('VPRT',ICODE)
      IF(NPRT.LE.0) THEN
         IHIT = -1
         RETURN
      ENDIF
C
      IHIT = IW(NPRT + 2)
      RETURN
      END
#endif
