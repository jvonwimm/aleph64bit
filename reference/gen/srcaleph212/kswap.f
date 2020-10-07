      SUBROUTINE KSWAP
C -----------------------------------------------------------------
C -  B.Bloch September 92
C! Swap cards read from data base to temporary ones
CKEY KINE KINGAL INIT  /   INTERNAL
C
C
C ------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C----------------------------------------------------------------------
      WRITE (IW(6),100)
      CALL BSWAP(IW,'GMOB','KMOB')
      CALL BSWAP(IW,'GADM','KADM')
      CALL BSWAP(IW,'GRPL','KRPL')
      WRITE (IW(6),200)
 100  FORMAT(1X,/,'============= This section handle user input supersed
     $ing  KREF defaults ==============')
 200  FORMAT
     $(/1X,'++++KSWAP swapped GMOB/GADM/GRPL from KREF to KMOB/KADM/KRPL
     $')
      RETURN
      END
