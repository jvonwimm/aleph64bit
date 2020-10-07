      SUBROUTINE VTEREX(VTERRAW,NVTXT,LAYVTXT,VTERMAT)
C ---------------------------------------------------------------------
C! Expand the VTER error matrix into 8X8 form
CKEY VGLOB VDET TRACK
C
C      D.Brown - 7-11-94
C
C  INPUT;  VTERRAW(36)  = raw upper-diagonal VTER-bank track error matrix
C          NVTXT        = # of rows in VTXT bank
C          LAYVTXT(4)   = VDET Layers corresponding to the VTXT rows
C OUTPUT;  VTERMAT(8,8) = Expanded 8X8 matrix.  Missing rows in
C          VTERRAW are expanded to give 1.0 on the diagonal and 0.0
C           off-diagonal
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  IO variables
C
      INTEGER NVTXT,LAYVTXT(4)
      REAL VTERRAW(36),VTERMAT(8,8)
C
C  Local variables
C
      INTEGER ILAY,IOVER,IVIEW,IVTXT,OLDLAY
      INTEGER IROW,JROW,IPOINT,JPOINT
      INTEGER ROWFLAG(8)
      INTEGER IBART(8,8)
C
C  Data statements
C
      DATA IBART/    1, 2, 4, 7,11,16,22,29,
     &               2, 3, 5, 8,12,17,23,30,
     &               4, 5, 6, 9,13,18,24,31,
     &               7, 8, 9,10,14,19,25,32,
     &              11,12,13,14,15,20,26,33,
     &              16,17,18,19,20,21,27,34,
     &              22,23,24,25,26,27,28,35,
     &              29,30,31,32,33,34,35,36/
C
C  Build the row flag; 2 Layers, 2 overlaps, 2 views
C
      DO IROW = 1, 8
         ROWFLAG(IROW) = 0
      END DO
C
      OLDLAY = 0          
      DO IVTXT = 1, NVTXT
        ILAY = LAYVTXT(IVTXT)
        IF (ILAY .EQ. OLDLAY) THEN
          IOVER = 2
        ELSE
          IOVER = 1
        END IF
        OLDLAY = ILAY
        DO IVIEW = 1, 2
          IROW = 4*(ILAY-1) + 2*(IOVER-1) + IVIEW
          ROWFLAG(IROW) = IVTXT
        END DO
      END DO
C
C  Loop over the VTER elements
C
      IPOINT = 0
      DO IROW = 1, 8
        IF (ROWFLAG(IROW) .GT. 0) IPOINT = IPOINT + 1
        JPOINT = 0
        DO JROW = 1, 8
          IF (ROWFLAG(JROW) .GT. 0) JPOINT = JPOINT + 1
          IF (ROWFLAG(IROW) .GT. 0 .AND. ROWFLAG(JROW) .GT. 0) THEN
C
C  This element exists;  Take the entry from the upper-diagonal bank
C
            VTERMAT(JROW,IROW) = VTERRAW(IBART(JPOINT,IPOINT))
          ELSE
C
C  Phony element
C
            IF (IROW .EQ. JROW) THEN
              VTERMAT(JROW,IROW) = 1.0
            ELSE
              VTERMAT(JROW,IROW) = 0.0
            END IF
          END IF
        END DO
      END DO
      RETURN
      END
#endif
