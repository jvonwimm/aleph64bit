*DK ehbari
      SUBROUTINE EHBARI(XMAX1,XMAX2,YMAX,ZMAX1,ZMAX2)
C.--------------------------------------------------------------------
C M.R/B.B Special interface for BARREL
C Calls EVOLPL,EVOLCR from ALEPHLIB
C Called by EHGEOB
C.---------------------------------------------------------------------
#ifndef DOC
      SAVE
C
      DIMENSION PLANES (4,8),CORNER(3,12)
      ISC = 2
      IMD = 0
C
      CALL EVOLPL('B sensitive     ',ISC,IMD,LEPLAN,PLANES)
      CALL EVOLCR('B sensitive     ',LEPLAN,PLANES,LEPOIN,CORNER)
C
      YMAX = CORNER(3,1)
      XMAX2 = CORNER(1,12)
      XMAX1 = CORNER(1,7)
      ZMAX1 = .5 * (CORNER(2,1)-CORNER(2,5))
      ZMAX2 = .5 * (CORNER(2,5)-CORNER(2,9))
C
      END
#endif
