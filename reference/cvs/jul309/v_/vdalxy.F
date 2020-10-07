      SUBROUTINE VDALXY
C ----------------------------------------------------------------------
C!  Smears the VDXY in accordance with
C!         the errors in the alignment quantities.
C!
C -  Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C
C!   Description
C!  ===========
C!  Called by VDALIN to do half the job
C!
C
C  Output : smeard VDXY bank
C
C -----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "vdxyjj.h"
      INTEGER KVDXY, IXY
      REAL VUW(3), XYZ(3), SIGU, TNG
      INTEGER IV, NAMIND, IERR, JWAF, NREAL, VJWABR, NR, NH, IROW,
     $   VVUWXY, VXYZVU
      REAL VDGAUS, COSTH, SAV
C   bit 29 now means that smearing has been added to a cluster
      INTEGER IMCSMR
      PARAMETER (IMCSMR=268435456)
#include "bmacro.h"
      IV = 2
      NVDXY=NAMIND('VDXY')
      KVDXY=NVDXY+1
 30   KVDXY=IW(KVDXY-1)
      IF(KVDXY.EQ.0)GO TO 31
      NR=IW(KVDXY-2)
      NH=LROWS(KVDXY)
      DO 60 IXY=1,NH
        IF(IAND(ITABL(KVDXY,IXY,JVDXQF),IMCSMR).NE.0)
     $     GOTO 60
C-- get the global coordinate of the hit from the VDHT bank
C-- also get the tangent of the angle (slope of the track, projected
C-- to the view.
        CALL VDGTXL(IV,NR,IXY,XYZ,TNG,IERR)
        IF(IERR.NE.0)GO TO 60
C-- from the bank number and global XYZ, get wafer number JWAF
C-- also get the real VALC bank to use
        IERR = VJWABR(NR,XYZ(3),JWAF,NREAL)
C-- get the alignment error to be applied to the RPHI coordinate
        CALL VDALSG(NREAL,JWAF,IV,XYZ,TNG,SIGU)
C-- go to the local coordinates VUW
        IERR = VXYZVU(XYZ,JWAF,VUW)
C-- smear the U coordinate
        SAV = RTABL(KVDXY,IXY,JVDXUC)
        VUW(2) = SAV
        VUW(2) = VDGAUS(VUW(2),SIGU)
C-- go back to the global coordinates
        IERR = VVUWXY(VUW,JWAF,XYZ)
C-- put the smeared quantities back in VDXY and VDXY
        IROW = KROW(KVDXY,IXY)
C-- note that VUW(1) and VUW(3) are true positions from VDHT.
C-- does that make a difference?????
        IROW = KROW(KVDXY,IXY)
        RW(IROW+JVDXXC) = XYZ(1)
        RW(IROW+JVDXYC) = XYZ(2)
        RW(IROW+JVDXUC) = VUW(2)
        RW(IROW+JVDXSU) = SQRT(RW(IROW+JVDXSU)**2+SIGU**2)
C  Flag the hit
        IW(IROW+JVDXQF) = IOR(IW(IROW+JVDXQF),IMCSMR)
 60   CONTINUE
      GO TO 30
 31   CONTINUE
      RETURN
      END
#endif
