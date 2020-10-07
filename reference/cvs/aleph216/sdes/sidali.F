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
#ifndef DOC
      SAVE
#include "alcons.h"
#include "sigeco.h"
#include "simask.h"
C  Maximum address from JULIA has 14 bits
      PARAMETER ( IADMX = 16383)
      DATA IOLD /-1/
#include "simacr.h"
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
#endif
