      SUBROUTINE TWPEAK(ARR,NTOT, THRSH,MAXLEN,
     &                  IOFF1,NLEN,PSUM,NISOL,MXISOL)
C--------------------------------------------------------------------
C!  Find isolated peaks in array.
C
C  INPUTS:   PASSED:  --ARR,    Array
C                     --NTOT,   Length of array
C                     --THRSH   Threshold above which we take take peak
C                     --MAXLEN  Max no of successive bins above THRSH
C                     --MXISOL, MAX number of peaks allowed
C
C  OUTPUTS:  PASSED:  --NISOL,  number of peaks
C                     --IOFF1,  offset from ARR(1) of 1ST BIN OF
C                               each peak
C                     --NLEN,   No of bins in each peak
C                     --PSUM,   Sum of bin contents under peak
C?
C!======================================================================
#ifndef DOC
C
      DIMENSION ARR(NTOT)
      DIMENSION IOFF1(*),NLEN(*),PSUM(*)
      LOGICAL LOUT
C
C?    loop over bins in array. When the ordinate rises above THRSH
C?    we open a new peak starting from that point. When it falls again
C?    below THRSH, then we close off the peak by calculating the
C?    total number of bins up to that point, or if the number of bins
C?    over threshold was more than MAXLEN, then we just drop that pulse
C?    and go to the next.
C
      LOUT=.TRUE.
      NISOL=0
      DO 80 IS=1,NTOT
        IF (LOUT) THEN
          IF (ARR(IS).GE.THRSH) THEN
            LOUT=.FALSE.
            NISOL=NISOL+1
            IOFF1(NISOL)=IS-1
            NLEN(NISOL)=IS-IOFF1(NISOL)
            PSUM(NISOL) = ARR(IS)
            MXLEN=NTOT-IOFF1(NISOL)
            NOVER=1
          END IF
        ELSE
          IF (ARR(IS).LT.THRSH .OR. IS.EQ.NTOT) THEN
            LOUT=.TRUE.
            IF (NOVER.GT.MAXLEN) THEN
              NISOL=NISOL-1
            ELSE
              NLEN(NISOL)=MIN(MXLEN,NLEN(NISOL))
              IF (NISOL.EQ.MXISOL) GO TO 90
            END IF
          ELSE
            NLEN(NISOL)=NLEN(NISOL)+1
            PSUM(NISOL) = PSUM(NISOL) + ARR(IS)
            NOVER=NOVER+1
          END IF
        END IF
   80 CONTINUE
   90 CONTINUE
      RETURN
      END
#endif
