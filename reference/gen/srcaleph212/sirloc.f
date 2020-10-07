      REAL FUNCTION SIRLOC(RAD,Z)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE POSITION/ USER
C     B.BLOCH       February 92
C! Find local position inside R bin
C   Input :
C          RAD  Radius (cm)
C          Z    Z position (cm)
C   Output:
C          SIRLOC Position ( cm) in R bin (0.,1.)
C                -1.means outside detector or error
C   Called by USER program
C.---------------------------------------------------------------------
      COMMON/SIGECO/NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI(2),RMAXSI(2),
     $              Z0SNSI(2),ZWIDSI,ZWRFSI,ZWRLSI,ZWFRSI,ZWFLSI,
     $              ZWBKSI,ZWLASI,OVLPSI,DPOSSI(3,2),GAPXSI(2),
     $              PHSHFT(3,2),RADSTP,PHISTP,ISINUM(12,2)
      SIRLOC = -1.
C GET module
      IMD = 1
      IF (Z.LT.0.) IMD = 2
C GET RADIUS  bin
      IF ( RAD.LT.RMINSI(IMD)) GO TO 999
      IRD = INT((RAD-RMINSI(IMD))/RADSTP)+1
      IF ( IRD.LE.0 .OR. IRD.GT.NRBNSI) GO TO 999
      SIRLOC =(RAD - RMINSI(IMD) - (IRD-1)*RADSTP)/RADSTP
 999  RETURN
      END
