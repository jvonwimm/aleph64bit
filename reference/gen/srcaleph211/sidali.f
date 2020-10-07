       REAL FUNCTION SIDALI(IFL,IBD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE ADDRESS / USER
C     B.BLOCH       May 1992
C! Address Decoding function SICAL into Rho,Phi,z
C   Input :
C          IFL   FLAG to define return value(1= rho,2=phi,3=z)
C          IBD   ADDRESS encoded (14 bits)
C   Output:
C          Rho    Radial position ( cm)
C    OR    Phi    Azimuth ( degrees fom 0. to 360.)
C    OR    z      Z position ( cm)
C          0..    means error
C   Called by USER program  : NO FINE INTERNAL ALIGNEMENT PROVIDED !!!!!
C.---------------------------------------------------------------------
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      COMMON/SIGECO/NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI(2),RMAXSI(2),
     $              Z0SNSI(2),ZWIDSI,ZWRFSI,ZWRLSI,ZWFRSI,ZWFLSI,
     $              ZWBKSI,ZWLASI,OVLPSI,DPOSSI(3,2),GAPXSI(2),
     $              PHSHFT(3,2),RADSTP,PHISTP,ISINUM(12,2)
C! Sical masks to decode address
      INTEGER  SIBIMD , SIBIPH , SIBIRD , SIBITP ,SIBIOR
      INTEGER  SIMD , SIPH , SIRD , SIST ,SIOR
      PARAMETER (SIBIMD = 2048, SIBIPH = 64 , SIBIRD =  4, SIBITP = 1)
      PARAMETER (SIBIOR = 4096 )
      PARAMETER (MSSIMD = 2048 ,MSSIPH = 1984 ,MSSIRD = 60,MSSITP = 3)
      PARAMETER (MSSIOR = 12288)
C  Maximum address from JULIA has 14 bits
      PARAMETER ( IADMX = 16383)
      DATA IOLD /-1/
C! Sical statement functions to access subcomponents
      SIMD(IXX) = ( IAND(IXX,MSSIMD))/SIBIMD +1
      SIST(IXX,IYY) =(( IAND(IXX,MSSITP))/SIBITP )*3 + IYY +1
      SIPH(IXX) = ( IAND(IXX,MSSIPH))/SIBIPH +1
      SIRD(IXX) = ( IAND(IXX,MSSIRD))/SIBIRD +1
      SIOR(IXX) = ( IAND(IXX,MSSIOR))/SIBIOR -1
C
      IF ( IOLD.NE.IBD) THEN
         IAD = IBD+4095
         IF ( IAD.LT.0 .OR. IAD.GT.IADMX) GO TO 998
         IMD = SIMD(IAD)
         JOR = SIOR(IAD)
         IST = SIST(IAD,JOR)
         IPH = SIPH(IAD)
         IRD = SIRD(IAD)
         IOLD = IBD
         RED = RMINSI(IMD)+ RADSTP*(IRD-1)+0.5*RADSTP
         ITP = MOD(IST-1,3)+1
         PHU = PHISTP*(IPH-1) + PHSHFT(ITP,IMD)+0.5*PHISTP
         X = RED*COS(PHU) + DPOSSI(1,IMD)
         Y = RED*SIN(PHU) + DPOSSI(2,IMD)
      ENDIF
      IF ( IFL.EQ.1) THEN
C GET RADIUS
         RAD = SQRT(X*X+Y*Y)
         SIDALI = RAD
      ELSE IF ( IFL.EQ.2) THEN
C GET PHI
         PHI = ATG(Y,X)
         SIDALI = PHI*RADEG
      ELSE IF ( IFL.EQ.3) THEN
C GET z position
         ZZ = Z0SNSI(IMD)+ ZWIDSI*(IST-1) +DPOSSI(3,IMD)
         IF (IMD.EQ.2) ZZ = -ZZ
         SIDALI = ZZ
      ENDIF
      RETURN
 998  CONTINUE
      SIDALI = 0.
      RETURN
      END
