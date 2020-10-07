      SUBROUTINE FMPRNT
C-----------------------------------------------------------------
C!  Print out properties of volumes for fast muon tracking
      SAVE
      PARAMETER (NRGION = 11)
      COMMON/FMGEOM/FMZMIN(NRGION),FMZMAX(NRGION),FMRMIN(NRGION),
     &              FMRMAX(NRGION)
      COMMON/FMSMUL/FMRADL(NRGION),SINMUL(101),COSMUL(101),SQRMUL(101)
      COMMON/FMELSS/FRATIO(NRGION),CONBB1(NRGION),CONBB2(NRGION),XMUMAS,
     &              XMUMS2
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOUT = IW(6)
      WRITE (LOUT, 1000)
 1000 FORMAT(' +++ FMPRNT +++ Volumes for fast muon tracking ',
     &   //,' NR    RMIN    RMAX    ZMIN   ZMAX   X0     C1     C2 ',/)
      DO 20 J=1,NRGION
         WRITE (LOUT,1001) J,FMRMIN(J),FMRMAX(J),FMZMIN(J),
     &                       FMZMAX(J),FMRADL(J),CONBB1(J),CONBB2(J)
 1001 FORMAT (1X,I2,2X,7F8.3)
   20 CONTINUE
      RETURN
      END
