      SUBROUTINE LCSATU
C--------------------------------------------------------------
C. - J.Dines Hansen & P.Hansen - 860417
C. - Saturation in luminosity calorimeter
C. - Modifies banks LTHT and LWHT
C. - Called by  LCASIG                           from this .HLB
C -----------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      RETURN
      END
