      SUBROUTINE TPADPO(ISEC,IROW,IPADT,RPAD,PPAD,PWID)
C-------------------------------------------------------------
C!  Returns position of pad IPAD in row IROW of sector  ISEC
C
C       RPAD = radius at centre of pad
C       PPAD = phi at middle of pad in LOCAL ref. sys.
C       PWID = Angular pad width
C M.Mermikides 7/1/87
C?
C!======================================================================
#ifndef DOC
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
C
      ITYPE = ITPTYP(ISEC)
      NPADS  = NTPDPR(IROW,ITYPE)
      RPAD = TPDRBG(ITYPE) + (IROW-1)*TPDRST(ITYPE)
      DP = TPDPHS(IROW,ITYPE)
C
      IPAD = IPADT
C
C  XP is in full pad width units.
C  Pads are counted from right to left, facing the active surface
C  of the sector. (This means that numbering is in increasing global
C  phi for the -ve z endplate (sectors 1-18), and in decreasing phi
C  on the opposite endplate.)
C
      IF (IPAD.LE.2) THEN
         XP = FLOAT(IPAD-1)
         PWID = TPDHWD(ITYPE)/RPAD
         PPAD = -TPDPHW(IROW,ITYPE)  + XP*DP/2. + DP/4.
      ELSEIF (IPAD.GT.NPADS) THEN
         XP = FLOAT(NPADS +IPAD - 2)/2.
         PWID = TPDHWD(ITYPE)/RPAD
         PPAD = -TPDPHW(IROW,ITYPE)  + XP*DP  - DP/4.
      ELSE
         XP = FLOAT(IPAD - 1)
         PWID = TPDWID(ITYPE)/RPAD
         PPAD = -TPDPHW(IROW,ITYPE)  + (XP - 0.5)*DP
      ENDIF
C
      RETURN
      END
#endif
