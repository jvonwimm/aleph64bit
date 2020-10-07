      SUBROUTINE TCSHF1( DELIN, DELOUT ,IBAD)
C
C----------------------------------------------------------------C
C! Shifts coordinates derived from sub-clusters with one half-   C
C! pad by the empirically known mean-offset. It is assumed       C
C! that the calibration of the half-pad pulseheights has been    C
C! applied before this.                                          C
C                                                                C
C---           SUBROUTINE TCSHF1                                 C
C              =================                                 C
C                                                                C
C           AUTHOR: T.LOHSE        9.Mar 1989                    C
C                   D.CASPER       6.Aug 1997 Create from TCSHFT C
C                                                                C
C     INPUT:    DELIN  distance of the r-phi coordinate with two C
C                      half-pads to the boundary between the     C
C                      half-pads. DELIN must be positive in the  C
C                      coordinate lies towards the INSIDE of the C
C                      sector, otherwise negative.               C
C                                                                C
C     OUTPUT:   DELOUT as DELIN after the correction for the     C
C                      known coordinate shift (same sign         C
C                      convention).                              C
C               IBAD is not equal to zero if the coordinate      C
C                    is too far out to be shifted.               C
C                                                                C
C----------------------------------------------------------------C
#ifndef DOC
C
#include "bcs.h"
#include "tnrnjj.h"
C
C
      DATA NTNRN/0/,FIRST/0/
C
      IBAD = 0
      IF (NTNRN.EQ.0) NTNRN=NAMIND('TNRN')
      KTNRN=IW(NTNRN)
      IF (KTNRN.EQ.0) THEN
        IF (FIRST.EQ.0)CALL RERROR('TCSHF1',1,'No TNRN bank.'//
     +               '  TPC one half-pad coordinates NOT corrected')
        DELOUT = DELIN
        FIRST = 1
        RETURN
      ENDIF
      KTNRN=KTNRN+LMHLEN
C
C--->  see if correction is necessary (is coordinate close enough to the
C--->  half-pad?)
C
      IF (DELIN .GT. RW(KTNRN+JTNRHH)) THEN
        DELOUT = DELIN
        RETURN
      ENDIF

C
C--->  check for unreasonable input coordinates
C
      DEL = DELIN
      CUT = RW(KTNRN+JTNRHL)
      IF  ( DEL.LT.CUT ) THEN
        DEL = CUT
        IBAD =  1
      ENDIF
C
      SHIFT  = RW(KTNRN+JTNRH0) + DEL*RW(KTNRN+JTNRH1)
     +         + RW(KTNRN+JTNRH2)*DEL*DEL
      DELOUT = DELIN - SHIFT
C
      RETURN
      END
#endif
