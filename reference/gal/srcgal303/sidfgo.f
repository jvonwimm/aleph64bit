      SUBROUTINE SIDFGO
C--------------------------------------------------------------
C! Store geometry quantities in common
C. - B.Bloch-Devaux        920110
C. - Called by  SIIIRUN            from this .HLB
C. - Calls      SIGTGO             from Alephlib
C-------------------------------------------------------------
      COMMON/SIGACO/NMODSI,NRBNSI,NPBNSI,NZBNSI,Z0SNSI(2),
     $              ZWIDSI,ZWFRSI,ZWFLSI,ZWLASI,ZWRFSI,ZWRLSI,OVLPSI
      DIMENSION ICONS(5),RCONS(20)
C ----------------------------------------------------------
      CALL SIGTGO(ICONS,RCONS)
C - Fill array of geometrical constants
      NMODSI = ICONS(1)
      NZBNSI = ICONS(2)
      NRBNSI = ICONS(3)
      NPBNSI = ICONS(4)
      Z0SNSI(1) = RCONS(1)
      Z0SNSI(2) = RCONS(2)
      ZWIDSI    = RCONS(3)
      ZWRFSI    = RCONS(11)
      ZWRLSI    = RCONS(12)
      ZWFRSI    = RCONS(13)
      ZWLASI    = RCONS(15)
      ZWFLSI    = RCONS(16)
      OVLPSI    = RCONS(17)
      RETURN
      END
