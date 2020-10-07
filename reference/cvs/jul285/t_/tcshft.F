      SUBROUTINE TCSHFT( DELIN, DELOUT ,IBAD)
C
C----------------------------------------------------------------C
C! Shifts coordinates derived from sub-clusters with two half-   C
C! pads by the empirically known mean-offset. It is assumed      C
C! that the calibration of the half-pad pulseheights has been    C
C! applied before this.                                          C
C                                                                C
C---           SUBROUTINE TCSHFT                                 C
C              =================                                 C
C                                                                C
C           AUTHOR: T.LOHSE        9.Mar 1989                    C
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
#include "ternjj.h"
C
C
      DATA NTERN/0/
C
      IBAD = 0
      IF (NTERN.EQ.0) NTERN=NAMIND('TERN')
      KTERN=IW(NTERN)
      IF (KTERN.EQ.0) THEN
        CALL RERROR('TCSHFT',1,'Cannot find the TERN bank.'//
     +               '  TPC half-pad coordinates NOT corrected')
        DELOUT = DELIN
        RETURN
      ENDIF
      KTERN=KTERN+LMHLEN
C
C--->  check for unreasonable input coordinates
C
      DEL = DELIN
      CUT = RW(KTERN+JTERHC)
      IF  ( DEL.LT.CUT ) THEN
        DEL = CUT
        IBAD =  1
      ENDIF
C
      SHIFT  = RW(KTERN+JTERH0) + DEL*RW(KTERN+JTERH1)
     +         + RW(KTERN+JTERHM)*EXP(RW(KTERN+JTERHP)*DEL)
C
      DELOUT = DELIN - SHIFT
C
      RETURN
      END
#endif
