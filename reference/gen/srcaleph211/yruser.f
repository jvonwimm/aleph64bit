      SUBROUTINE YRUSER(IFAIL)
C----------------------------------------------------------*
C!    user routine called at the end of the package
CKEY YTOP USER
C!    Author :     M. Bosman      16/07/91
C!
C!    Description
C!    ===========
C!
C!    user personnal  code
C!
C!---------------------------------------------------------*
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C!---------------------------------------------------------*
C
      IFAIL = 0
C
      RETURN
      END
