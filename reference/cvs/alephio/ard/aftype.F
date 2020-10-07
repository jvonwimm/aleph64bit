      SUBROUTINE AFTYPE(ATYP,N)
C----------------------------------------------------------------------
C - D.Schlatter - 880811
C!     returns  list of valid ALEPH file-types
C
C     output:      ATYP(N)   strings of valid filetypes
C                  N         number of different filetypes
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER*6 ATYP(*)
C
      N=6
C             native fortran files
      ATYP(1)='NATIVE'
C             EPIO files
      ATYP(2)='EPIO'
C             event directory text files
      ATYP(3)='EDIR'
C             BOS direct access files
      ATYP(4)='DAF'
C             CARD-format ( fixed , LRECL=80 )
      ATYP(5)='CARDS'
C             HBOOK histograms (direct access files)
      ATYP(6)='HIS'
C
      RETURN
      END
#endif
