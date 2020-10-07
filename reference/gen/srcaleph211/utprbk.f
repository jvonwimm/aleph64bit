      SUBROUTINE UTPRBK(IBKNA)
C ---------------------------------------------------------------------
C.
C.
C! - Print the content of tables in memory
C.
C. - Author   : A. Putzer  - 89/03/07
C.
C.
C.   Arguments: -  IBKNA (input) Name of the bank wanted (or 'ALL ')
C.              -                (or 'X   ' for all banks starting
C.              -                with X)
C.
C ---------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      IBSNR = 0
      IRWNR = 0
      LDBAS = JUNIDB(0)
      CALL UTBKPR(IBKNA,IBSNR,IRWNR,LDBAS,IW(6),IFLAG)
      RETURN
      END
