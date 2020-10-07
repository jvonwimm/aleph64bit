      SUBROUTINE VDOSMR
C----------------------------------------------------------------------
C!  VDOSMR smears the VDXY and VDZT coordinates with averages
C!         stored in VDSM bank.
C!         Origanally, it was a part of VDMCEF as introduced
C!         by Dave brown.  Expected to be obselete in view of
C!         the new routine VDWSMR, dealigning the MC detector
C!         wafer by wafer and event by event.
C -   Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C!
C!   Description
C!   ===========
C!   The alignment procedure for the VDET essentially measures the
C!   position of each wafer with respect to the rest of the detector.
C!   The results are given in the form of 6 quantities - 3
C!   translations and 3 rotations to go from the nominal position to
C!   the measure postion.  The statistical accuracy of these
C!   measurements is given in the 6x6 (symmetric) error matrix (21
C!   numbers) for each wafer.  There is no mathematically rigorous way
C!   of treating the uncertainty in the real position of the wafers -
C!   especially because it is measured with respect to the rest of the
C!   detector which is, in turn, subject to the same uncertainties.
C!   and the global correlations are not stored.
C!  Many possible treatments can be thought about:
C!
C!  This subroutine does treatment A.
C!  (Treatment B => VDALIN   and   Treatment C => VDWSMR)
C!
C!  A: Take some kind of an average error for each hit, smear the
C!  position with this error.  This is in effect done in the current
C!  version of the Monte Carlo (Jul274, in VDMCEF). The VDSM bank
C!  that holds these average smears reads: SIGU = 8.8mu and
C!  SIGW=9.7mu.
C!
C!  This method has the drawback of ignoring the position/angular
C!  dependence of the propagated errors.
C
C  - Input banks : VDXY, VDZT, VDSM
C  - Output banks: VDXY, VDZT smeared and quality flag, 29th bit
C                  changed.
C       27-8-93 to include hit position smearing      D. Brown
C       This uses the VDSM bank.
C
C ======================================================================
#ifndef DOC
#include "bcs.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vdsmjj.h"

C  Local variables
C
      INTEGER IVDXY,IVDZT,IVNEX
      INTEGER KVDXY,KVDZT,KEVEH
      INTEGER NVDXY,NVDZT
      INTEGER MVDXY,MVDZT
      INTEGER IVDSM,MVDSM
      INTEGER NHXY,NHZT,IHXY,IHZT
      REAL UW
      REAL DU,DW,SIGU,SIGW,VUW(3),XYZ(3),DU1, DW1
      LOGICAL FIRST
C   bit 29 now means that smearing has been added to a cluster
      INTEGER IMCSMR
      PARAMETER (IMCSMR=268435456)
C
C  BOS function
C
      INTEGER NAMIND
      INTEGER AGETDB, NLINK, LDBAS, JUNIDB
C
C  Variables for inline functions
C
C
C  Inline functions
C
      INTEGER IOR
C
C  Define variables used in BMACRO
C
#include "bmacrod.h"
C
      DATA FIRST/.TRUE./
      SAVE  SIGU,SIGW, MVDZT, MVDXY
#include "bmacro.h"
C -------------------------------------------------------------
C  initialization
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        MVDXY = NAMIND('VDXY')
        MVDZT = NAMIND('VDZT')
        IVDSM = AGETDB('VDSM',2)
        MVDSM = NAMIND('VDSM')
        IF (IVDSM.LE.0) THEN
          CALL RERROR('VDOSMR',1,'No VDSM  matrix!')
          RETURN
        ENDIF
        IF(IVDSM.GT.0)THEN
          SIGU = RTABL(IVDSM,1,JVDSUS)
          SIGW = RTABL(IVDSM,1,JVDSWS)
        ELSE
          SIGU = 0.0
          SIGW = 0.0
        END IF
C
C This is printed only once per job
C
        WRITE (6,*) ' VOWSMR: SIGU =', SIGU, '   SIGW =',SIGW
      ENDIF
C
C     If we had an error in initialization just return
C
      IVDSM = IW(MVDSM)
C
C     Normal operation start
C
C
C  Set seed for position smearing random number
C
      KEVEH = IW(NAMIND('EVEH'))
      CALL NORRIN(FLOAT(IW(KEVEH+2)),FLOAT(IW(KEVEH+6)))
C
C  Loop over all VDXY banks and flag hits
C
      IVDXY  = IW(MVDXY)
      IF(IVDXY .LE. 0) GO TO 3
 2    CONTINUE
C  loop over all the hits of the VDXY bank
      NHXY  = LROWS(IVDXY)
      NVDXY = IW(IVDXY-2)
      IVNEX = IW(IVDXY-1)
      DO 20 IHXY=1,NHXY
        KVDXY=KROW(IVDXY,IHXY)
        UW=RW(KVDXY+JVDXUC)
C
C  Add smearing on the U position, but ONLY if it hasn't already been
C  done before
C
        IF(SIGU.GT.0.0.AND.IAND(IW(KVDXY+JVDXQF),IMCSMR).EQ.0)THEN
          CALL RANNOR (DU,DU1)
          RW(KVDXY+JVDXUC) = UW + DU*SIGU
C
C  Update the global position
C
          VUW(1) = 0.0
          VUW(2) = UW + DU*SIGU
          VUW(3) = 0.0
C
C-- Call to VDWFXY changed to VGWFXY, because the correction
C-- to read-out direction should not be applied twice.
          CALL VGWFXY(NVDXY,VUW,XYZ)
          RW(KVDXY+JVDXXC) = XYZ(1)
          RW(KVDXY+JVDXYC) = XYZ(2)
C
C  Flag the hit
C
          IW(KVDXY+JVDXQF)=IOR(IW(KVDXY+JVDXQF),IMCSMR)
        END IF
C
 20   CONTINUE
C  Update pointer, and return for next bank
      IVDXY=IVNEX
      IF(IVDXY .GT. 0) GO TO 2
C
C  all done with VDXY, now do VDZT
C
 3    CONTINUE
C
C  Now loop over all VDZT banks and flag hits
C
      IVDZT = IW(MVDZT)
      IF(IVDZT .LE. 0) RETURN
C  Loop over all VDZT banks
 5    CONTINUE
C  loop over all the hits of the VDZT bank
      NHZT  = LROWS(IVDZT)
      NVDZT = IW(IVDZT-2)
      IVNEX = IW(IVDZT-1)
      DO 50 IHZT=1,NHZT
        KVDZT=KROW(IVDZT,IHZT)
        UW=RW(KVDZT+JVDZWC)
C
C  Add smearing on the W position
C
        IF(SIGW.GT.0.0.AND.IAND(IW(KVDZT+JVDZQF),IMCSMR).EQ.0)THEN
          CALL RANNOR (DW,DW1)
          RW(KVDZT+JVDZWC) = UW + DW*SIGW
C
C  Update the global position
C
          VUW(1) = 0.0
          VUW(2) = 0.0
          VUW(3) = UW + DW*SIGW
C
C-- Call to VDWFXY changed to VGWFXY, because the correction
C-- to read-out direction should not be applied twice.
          CALL VGWFXY(NVDZT,VUW,XYZ)
          RW(KVDZT+JVDZZC) = XYZ(3)
C
C  Flag the hit
C
          IW(KVDZT+JVDZQF)=IOR(IW(KVDZT+JVDZQF),IMCSMR)
        END IF

 50   CONTINUE
C  Update pointer, and return for next bank
      IVDZT = IVNEX
      IF(IVDZT .GT. 0) GO TO 5
C
      RETURN
C
      END
#endif
