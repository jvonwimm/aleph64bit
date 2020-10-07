*DK ehpeta
      SUBROUTINE EHPETA(XMAX,YMAX1,YMAX2,ZMAX)
C.--------------------------------------------------------------------
C M.R/B.B Special interface for END CAPS
C Calls EVOLPL,EVOLCR from ALEPHLIB
C Called by EHGEOE
C.---------------------------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION PLANES (4,8),CORNER(3,12)
      ISC = 1
      IMD = 0
C
      CALL EVOLPL('E sensitive     ',ISC,IMD,LEPLAN,PLANES)
      CALL EVOLCR('E sensitive     ',LEPLAN,PLANES,LEPOIN,CORNER)
C
      YSUP = CORNER(3,1)
      YINF = CORNER(3,2)
      ZMAX = .5 * (CORNER(2,1)-CORNER(2,5))
      XMAX = .5 * (CORNER(1,7)-CORNER(1,5))
      CALL EVOLPL('E stack3        ',ISC,IMD,LEPLAN,PLANES)
      YMED = - PLANES(4,6)
      YMAX2 = .5 * (YSUP - YMED)
      YMAX1 = .5 * (YMED - YINF)
C
      END
#endif
