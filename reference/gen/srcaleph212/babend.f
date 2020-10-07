      SUBROUTINE BABEND(TEXT)
C ---------------------------------------------------------------
C - V.Blobel - from BOS77                        F.Ranjard - 870203
C! Call trace back in case of BOS fatal error
C - Print 'TEXT' then call traceback routine
C - called by several BOS77 routines
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*(*) TEXT
C -----------------------------------------------------------------
      LOUT = IW(6)
      IF (LOUT.GT.0) WRITE(LOUT,'(1X,''+++BABEND+++ '',A)') TEXT
C
      CALL BOSBK(IW)
      CALL EXIT
      END
