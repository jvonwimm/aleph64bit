      SUBROUTINE SIDCOD(IAD,IOR,IMD,IST,IPH,IRD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address Decoding routine  SICAL
C   Input :
C          IAD   ADDRESS encoded (16 bits)
C          IOR   Bin number in triplet (0-2)
C   Output:
C          IMD    Module number1-2
C          IST    Stack  number 1-12
C          IPH    PHibin number 1-32
C          IRD    Radialbin number 1-16
C      any Ixx = -1 means error
C   Called by USER program
C.---------------------------------------------------------------------
C  Maximum address from data has 12 bits and possible 3 overflow bits =
      PARAMETER ( IADMX = 32767)
C! Sical masks to decode address
      INTEGER  SIBIMD , SIBIPH , SIBIRD , SIBITP ,SIBIOR
      INTEGER  SIMD , SIPH , SIRD , SIST ,SIOR
      PARAMETER (SIBIMD = 2048, SIBIPH = 64 , SIBIRD =  4, SIBITP = 1)
      PARAMETER (SIBIOR = 4096 )
      PARAMETER (MSSIMD = 2048 ,MSSIPH = 1984 ,MSSIRD = 60,MSSITP = 3)
      PARAMETER (MSSIOR = 12288)
C! Sical statement functions to access subcomponents
      SIMD(IXX) = ( IAND(IXX,MSSIMD))/SIBIMD +1
      SIST(IXX,IYY) =(( IAND(IXX,MSSITP))/SIBITP )*3 + IYY +1
      SIPH(IXX) = ( IAND(IXX,MSSIPH))/SIBIPH +1
      SIRD(IXX) = ( IAND(IXX,MSSIRD))/SIBIRD +1
      SIOR(IXX) = ( IAND(IXX,MSSIOR))/SIBIOR -1
      IF ( IAD.LT.0 .OR. IAD.GT.IADMX) GO TO 998
      IMD = SIMD(IAD)
      IST = SIST(IAD,IOR)
      IPH = SIPH(IAD)
      IRD = SIRD(IAD)
      RETURN
 998  CONTINUE
      IMD = -1
      IST = -1
      IPH = -1
      IRD = -1
      RETURN
      END
