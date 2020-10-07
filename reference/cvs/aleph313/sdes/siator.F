      SUBROUTINE SIATOR(IAD,IOR,RAD,TETA,PHI,IOK)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address Decoding routine  into R,THETA,PHI Radial coordinates
C   Input :
C          IAD   ADDRESS encoded (16 bits)
C          IOR   Bin number in triplet (0-2)
C   Output:
C          RAD ,TETA,PHI Radial coordinates of pad center
C          IOK = -1 means error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sigeco.h"
#include "sinali.h"
#include "sintjj.h"
      PARAMETER ( RDEF = 16. ,YTDEF = 24.5 )
#include "bmacro.h"
C  First decode address into elements
      IOK = 0
      CALL SIDCOD(IAD,IOR,IMD,IST,IPH,IRD)
      IF ( IMD.EQ.-1) GO TO 998
C get local coordinates
#include "siloca.h"
C transform to ARS cylindrical coordinates
      RAD = SQRT(X*X + Y* Y )
      TETA = ATG (RAD,Z)
      PHI  = ATG ( Y,X)
      RETURN
 998  IOK = -1
      RETURN
      END
#endif
