      SUBROUTINE EKNEWE (IBOX,ISC)
C----------------------------------------------------------------------
C!  - Kill ECAL  Endcap box
C                 Author M.N Minard
C                        5/06/90
C
C     Input IBOX : Box number
C           ISC  : Subcomponent #
C     Bank created EDKB
C     Bank used EBOK
C     Called from EBOXKI
C?
C!======================================================================
#ifndef DOC
#include "edkbjj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      DIMENSION ITET(4) , NTET(4) , NPHI(4)
      DIMENSION ITET0(4),NTET0(4 )
      DIMENSION IPHI(2,4)
      DATA NTET0 / 221 , 205 , 189 , 179 /
      DATA ITET / 1 , 9 , 25 , 41 /
      DATA NTET / 8 , 16 , 16 , 10 /
      DATA NPHI / 4 , 8 , 12 , 16 /
#include "bmacro.h"
      IKILL = 0
      NKILL = 3 * 512
      IF ( ISC . EQ. 1 )CALL UCOPY ( ITET(1),ITET0(1),4)
      IF ( ISC.EQ.3) THEN
         DO 10 IL = 1, 4
         ITET0(IL) = NTET0(IL)
 10      CONTINUE
      ENDIF
C
C-    Update EDKB bank
C
      NEDKB = 0
      NAEDKB = NAMIND('EDKB')
      KEDKB = IW(NAEDKB)
      IF ( KEDKB.NE.0) NEDKB = LROWS(KEDKB)
      LENG = (NEDKB+NKILL)*LEDKBA + LMHLEN
      CALL AUBOS('EDKB',0,LENG,KEDKB,IGARB)
      IF ( IGARB.EQ.2 ) GO TO 200
      IW ( KEDKB + LMHCOL ) = LEDKBA
      DO 100 IL =1,3
      DO 100 IK = 1,4
      IPHI0 = IBOX * NPHI(IK)
      DO 100 IPI = IPHI0+1,IPHI0+NPHI(IK)
      DO 100 ITE = ITET0(IK),ITET0(IK)+NTET(IK)-1
      IKILL = IKILL + 1
      JEDKB = KROW(KEDKB,NEDKB+IKILL)
      IW(JEDKB + 1  ) = IL*2**26 + (ITE)*2**16 + (IPI)*2**2
 100  CONTINUE
      IW ( KEDKB + LMHROW ) = NEDKB + IKILL
  200  CONTINUE
 900  RETURN
      END
#endif
