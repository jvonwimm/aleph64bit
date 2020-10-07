        SUBROUTINE MUCOMP
C-------------------------------------------------------------------
C!
C!  compare reconstructed muon-hits with truth
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C!  called by MPREDA
C!
C! this routine uses straight tracks to check Galeph and Julia
C! muon geometry routines. called only if muon hist flag is ge 2
C!
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "munamc.h"
#include "alcons.h"
#include "mhitjj.h"
#include "fkinjj.h"
#include "bmacro.h"
C
      JFKIN=NLINK('FKIN',0)
      IF(JFKIN.EQ.0) RETURN
C Return if more than one particle
      IF (LROWS(JFKIN).GT.1) RETURN
      PX=RTABL(JFKIN,1,JFKIPX)
      PY=RTABL(JFKIN,1,JFKIPY)
      PZ=RTABL(JFKIN,1,JFKIPZ)
      JFKIN=NLINK('FKIN',0)
      IF(JFKIN.EQ.0) RETURN
C Return if more than one particle
      IF (LROWS(JFKIN).GT.1) RETURN
      PX=RTABL(JFKIN,1,JFKIPX)
      PY=RTABL(JFKIN,1,JFKIPY)
      PZ=RTABL(JFKIN,1,JFKIPZ)
      PHIMU=ATAN2(PY,PX)
      IF(PHIMU.LT.0.)PHIMU=PHIMU+TWOPI
      PTOT=SQRT(PX*PX+PY*PY+PZ*PZ)
      THEMU=ACOS(PZ/PTOT)
C
C?        compare reconstructed and true Mc values (angles and
C?              coordinates )
C
      JMHIT=IW(NAMHIT)
      NMHIT=IW(JMHIT+2)
C
      DO 10 JHIT=1,NMHIT
         ISUBC=ITABL(JMHIT,JHIT,JMHISC)
         ISLOT=ITABL(JMHIT,JHIT,JMHISN)
         LASL1=2*ITABL(JMHIT,JHIT,JMHISN)+ITABL(JMHIT,JHIT,JMHILN)
         RHO=RTABL(JMHIT,JHIT,JMHIRH)
         THETA=RTABL(JMHIT,JHIT,JMHITH)
         PHI=RTABL(JMHIT,JHIT,JMHIPH)
C
C?        skip if more than one hit in a single layer
C
         DO 5 IHIT=1,NMHIT
            IF(IHIT.EQ.JHIT) GO TO 5
            LASL2=2*ITABL(JMHIT,IHIT,JMHISN)+ITABL(JMHIT,IHIT,JMHILN)
            IF(LASL1.EQ.LASL2) GO TO 10
 5       CONTINUE
         DTHET=THEMU-THETA
         DPHI=PHIMU-PHI
         IF(DPHI.GT.PI) DPHI=DPHI-TWOPI
         IF(DPHI.LT.-PI) DPHI=DPHI+TWOPI
         CALL HFILL(8101,DPHI,0.,1.)
         CALL HFILL(8102,DTHET,0.,1.)
C
         IF(ISUBC.EQ.1) THEN
            DX=RHO*(SIN(THETA)*COS(PHI)-SIN(THEMU)*COS(PHIMU))
            DY=RHO*(SIN(THETA)*SIN(PHI)-SIN(THEMU)*SIN(PHIMU))
            CALL HFILL(8110+ISUBC,DX,0.,1.)
            CALL HFILL(8120+ISUBC,DY,0.,1.)
            CALL HFILL(8200+ISUBC,DX,FLOAT(ISLOT),1.)
            CALL HFILL(8210+ISUBC,DY,FLOAT(ISLOT),1.)
         ELSE
            RDPHI=RHO*SIN(THETA)*DPHI
            DZ=RHO*(COS(THETA)-COS(THEMU))
            CALL HFILL(8110+ISUBC,RDPHI,0.,1.)
            CALL HFILL(8120+ISUBC,DZ,0.,1.)
            CALL HFILL(8200+ISUBC,RDPHI,FLOAT(ISLOT),1.)
            CALL HFILL(8210+ISUBC,DZ,FLOAT(ISLOT),1.)
         ENDIF
 10   CONTINUE
C
      RETURN
      END
#endif
