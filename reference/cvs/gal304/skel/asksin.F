*DK asksin
      SUBROUTINE ASKSIN (IDEV,ISTA,NITR,NIVX,ECMS,WEIT)
C ----------------------------------------------------------------------
C. - R.BEUSELINCK - 830623   for GALEPH 3.2
C.                                       modified by F.RANJARD - 871029
C! Generate a single track particle of a given type in a given range
C.   of momentum and cosine(theta).
C - Output arguments:
C          IDEV     = particle type
C          ISTA     = event status word  ( 0 means OK )
C          NITR     = # of generated tracks
C          NIVX     = # of generated vertices
C          ECMS     = particle momentum
C          WEIT     = event weight
C.   BKINJO(4) = particle type
C.          5    minimum value of the momentum
C.          6    maximum value of the momentum
C.          7    minimum value of cos(theta)
C.          8    maximum value of cos(theta)
C. - called by    ASKINE when TKINJO = PART             from this .HLB
C. - calls        RANNOR , RNDM                         from KERNLIB
C.                KBVERT, KBKINE                        from this .HLB
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "kincom.h"
#include "alcons.h"
#include "jqcom.h"
      EXTERNAL RNDM
      REAL VRTX(4),PLAB(4)
#include "bmacro.h"
C -----------------------------------------------------------------------------
C
C - smear vertex position
C
      CALL RANNOR(RN1,RN2)
      CALL RANNOR(RN3,DUM)
      VRTX(1)=RN1*BKINJO(1)
      VRTX(2)=RN2*BKINJO(2)
      VRTX(3)=RN3*BKINJO(3)
      VRTX(4)=0.
      NIVX = 1
      JKVOL = IW(NAKVOL)
      IW(JKVOL+LMHROW) = NIVX
      KKVOL = KROW(JKVOL,NIVX)
      IW(KKVOL+1) = INTCHA ('BPVA')
C
C - store vertex coordinates
C
      JVERT = KBVERT (NIVX,VRTX,0)
      IF (JVERT .LE. 0) THEN
         CALL ALTELL ('ASKSIN: not enough space for VERT',1,'NEXT')
      ENDIF
C
C--  generate a single track of type BKINJO(4) with suitable
C--  momentum and direction.
C--
      PMOD = BKINJO(5) + (BKINJO(6)-BKINJO(5))*RNDM(0)
      PHI  = TWOPI*RNDM(0)
      COST = BKINJO(7) + (BKINJO(8)-BKINJO(7))*RNDM(0)
      IF (RNDM(0).GT.0.5) COST = -COST
      SINT = SQRT(1. - COST**2)
      PLAB(1)=PMOD*SINT*COS(PHI)
      PLAB(2)=PMOD*SINT*SIN(PHI)
      PLAB(3)=PMOD*COST
      PLAB(4) = 0.
C
      IDEV = NINT(BKINJO(4))
      NITR = 1
      JKINE = KBKINE (NITR,PLAB,IDEV,NIVX)
      IF ( JKINE .LE. 0) THEN
         CALL ALTELL ('ASKSIN: not enough space for VERT or KINE',1,
     &                'NEXT')
      ENDIF
      WEIT = 1.
      ISTA = 0
      ECMS = PMOD
C
C - END
C
999   RETURN
      END
#endif
