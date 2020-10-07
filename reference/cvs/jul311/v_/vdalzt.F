      SUBROUTINE VDALZT
C ----------------------------------------------------------------------
C!  VDALZT smears the VDZT in accordance with
C!         the errors in the alignment quantities.
C!
C-  Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C!
C!  Description
C!  ===========
C!  Called by VDALIN to do half the job
C
C
C  Output : smeard VDZT bank
C
C -----------------------------------------------------------------------
#ifndef DOC
      REAL VUW(3), XYZ(3)
#include "bcs.h"
#include "vdztjj.h"
      INTEGER KVDZT, IZ
      INTEGER IV, NAMIND, IERR, JWAF, NREAL, VJWABR, NR, NH, IROW,
     $   VVUWXY, VXYZVU
      REAL VDGAUS, SIGW, TNG
C   bit 29 now means that smearing has been added to a cluster
      INTEGER IMCSMR
      PARAMETER (IMCSMR=268435456)
#include "bmacro.h"
      IV = 1
      NVDZT=NAMIND('VDZT')
      KVDZT=NVDZT+1
 30   KVDZT=IW(KVDZT-1)
      IF(KVDZT.EQ.0)GO TO 31
      NR=IW(KVDZT-2)
      NH=LROWS(KVDZT)
      DO 60 IZ=1,NH
        IF(IAND(ITABL(KVDZT,IZ,JVDZQF),IMCSMR).NE.0)
     $     GOTO 60
C-- get the global coordinate of the hit from the VDHT bank
C-- also get the tangent of the angle (slope of the track, projected
C-- to the view.
        CALL VDGTXL(IV,NR,IZ,XYZ,TNG,IERR)
        IF(IERR.NE.0)GO TO 60
C-- from the bank number and global XYZ, get wafer number JWAF
C-- also get the real VALC bank to use
        IERR = VJWABR(NR,XYZ(3),JWAF,NREAL)
C-- get the alignment error to be applied to the RPHI and Z coordinates
        CALL VDALSG(NREAL,JWAF,IV,XYZ,TNG,SIGW)
C-- go to the local coordinates VUW
        IERR = VXYZVU(XYZ,JWAF,VUW)
C-- smear the W coordinate
        VUW(3) = RTABL(KVDZT,IZ,JVDZWC)
        VUW(3) = VDGAUS(VUW(3),SIGW)
C-- go back to the global coordinates
        IERR = VVUWXY(VUW,JWAF,XYZ)
C-- put the smeared quantities back in VDZT
        IROW = KROW(KVDZT,IZ)
C-- note that VUW(1) and VUW(2) are true positions from VDHT.
C-- does that make a difference?????
        RW(IROW+JVDZZC) = XYZ(3)
        RW(IROW+JVDZWC) = VUW(3)
        RW(IROW+JVDZSZ) = SQRT(RW(IROW+JVDZSZ)**2+SIGW**2)
        RW(IROW+JVDZSW) = SQRT(RW(IROW+JVDZSW)**2+SIGW**2)
C  Flag the hit
        IW(IROW+JVDZQF) = IOR(IW(IROW+JVDZQF),IMCSMR)
 60   CONTINUE
      GO TO 30
 31   CONTINUE
      RETURN
      END
#endif
