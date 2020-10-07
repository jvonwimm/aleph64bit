      SUBROUTINE ALVERS (ALEFV)
C --------------------------------------------------------------------
C - F.Ranjard - 870401
C! Return the Alephlib version # in ALEFV
C  Print the version # on IW(6)
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C - ALEPHLIB   21.1   951208  17:30:39
      REAL ALEVER
      PARAMETER (ALEVER = 21.1)
C
C ------------------------------------------------------------------
      ALEFV = ALEVER
C
      LOUT = IW(6)
      IF (LOUT.NE.0) THEN
         WRITE (LOUT,'(/1X,''+++ALVERS+++ Alephlib version '',F6.2)')
     &          ALEFV
      ENDIF
C
      END
