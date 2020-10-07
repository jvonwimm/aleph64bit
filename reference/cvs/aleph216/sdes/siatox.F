      SUBROUTINE SIATOX(IAD,IOR,POS,IOK)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address Decoding routine  into X,Y,Z position
C   Input :
C          IAD   ADDRESS encoded (16 bits)
C          IOR   Bin number in triplet (0-2)
C   Output:
C          POS(3) x,y,z of corresponding pad barycenter
C          IOK = -1 means error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sigeco.h"
#include "sinali.h"
#include "sintjj.h"
      DIMENSION POS(3)
      PARAMETER ( RDEF = 16. ,YTDEF = 24.5 )
#include "bmacro.h"
C  First decode address into elements
      IOK = 0
      CALL SIDCOD(IAD,IOR,IMD,IST,IPH,IRD)
      IF ( IMD.EQ.-1) GO TO 998
C local coordinates
#include "siloca.h"
C GET X,Y ,Z
      POS(1) = X
      POS(2) = Y
      POS(3) = Z
      RETURN
 998  IOK = -1
      RETURN
      END
#endif
