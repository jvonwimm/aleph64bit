      SUBROUTINE X2WSUM
C ----------------------------------------------------------------------
CKEY TRIG LEVEL2 SUMMARY / INTERNAL
C.   Modified : T. Medcalf - 89/10/03
C! - Print Level2 Trigger Summary
C.
C. - Called by      XTWSUM                        from this .HLB
C.
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/XTRCOM/NACCTR,NACCL1,NACCL2,NACCL3
C ----------------------------------------------------------------------
C.
C. - Print trigger level2 summary
C.
      WRITE(IW(6),100) NACCL2
  100 FORMAT(/// 22X,' Level 2     : ',I10,' events with good tracks')
C.
      RETURN
      END
