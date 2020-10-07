      SUBROUTINE EKNEWB (IBOX)
C----------------------------------------------------------------------
C!  - Kill ECAL  barrel box
C                 Author M.N Minard
C                        5/06/90
C
C     Input IBOX : Box number
C     Bank created EDKB
C     Bank used EBOK
C     Called from EBOXKI
C?
C!======================================================================
#ifndef DOC
#include "edkbjj.h"
#include "bcs.h"
      EXTERNAL NAMIND
#include "bmacro.h"
C
C-    Decode MOdule & box
C
      IMOD =((IBOX-1)/4) + 1
      ISET = MOD (IBOX-1,4) + 1
      IP = 1
      IF ( ISET.EQ.1.OR.ISET.EQ.4) IP =2
      IT = 1
      IF ( ISET.EQ.4.OR.ISET.EQ.3) IT = 2
      IKILL = 0
      NKILL = 3*64*16
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
      IW ( KEDKB + LMHROW ) = NEDKB + NKILL
      IW ( KEDKB + LMHCOL ) = LEDKBA

C
C-    GENERATE DEAD ADRESS FOR BARREL
C
      IPI0 = (IMOD-1)*32 + (IP-1)*16
      IT0 = (IT-1) * 64 + 50
      DO 100 IL =1,3
      DO 100 IK = 1,64
      DO 100 IJ = 1,16
      IKILL = IKILL + 1
      JEDKB = KROW(KEDKB,NEDKB+IKILL)
      IW(JEDKB + 1  ) = IL*2**26 + (IK+IT0)*2**16 + (IJ+IPI0)*2**2
 100  CONTINUE
 200  CONTINUE
 900  CONTINUE
      END
#endif
