      SUBROUTINE VRMWF(IWAF,IV,IROM)
C----------------------------------------------------------------------
C!  Transform wafer number to appropriate address for VDXY/VDZT bank
C   Get relevant readout module for a given wafer/view
C!  For old Vdet Wafer/view -> R/O module
C!  For new Vdet Wafer -> Wafer
CKEY VDET TRACK
C!
C!   Author   :- A. Bonissent March 1995
C!   Inputs:
C!
C!        IWAF       - encoded wafer number (see sbank VDZT/VDXY)
C!        IV         - view
C!
C!   Outputs:
C!
C!        IROM       - encoded readout moduule number
C!
C!   What it does :
C!
C!    (IWAF has)       (IROM contains)
C!                -------------------------
C!      WAF#        VDET91           VDET95
C!                IV=1   IV=2        ANY IV
C!        1        1      1            1
C!        2        2      1            2
C!        3        3      4            3
C!        4        4      4            4
C!        5        -      -            5
C!        6        -      -            6
C?
C!======================================================================
#ifndef DOC
      INTEGER VDYEAR
      IROM=IWAF
      IF(VDYEAR().NE.95)THEN
        IF(IV.EQ.2)THEN
          IZ = MOD(IWAF/1000,10)
          IF(IZ.EQ.2)IROM=IWAF+1000
          IF(IZ.EQ.1)IROM=IWAF-1000
        ENDIF
      ENDIF
      RETURN
      END
#endif
