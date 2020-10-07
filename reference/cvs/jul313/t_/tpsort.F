      SUBROUTINE TPSORT(IPDNR,IT0NR,IDXSR,NP)
C
C-----------------------------------------------------------------------
C! Sort pulses by pad number and time
C!
C!    Author:    R. Johnson   15-12-86
C!    Modified:  R. Johnson   27-08-89
C!
C!    Input:
C!       - IPDNR(np)   /I        Pad numbers of the pulses
C!       - IT0NR(np)   /I        T0 of each pulse
C!       - IDXSR(np)   /I        Ordering of the pulses
C!       - NP          /I        Number of pulses
C!    Output:
C!       - IDXSR(np)   /I        Ordering of the pulses
C!
C!    Called by TCLUST
C!
C!    Description
C!    ===========
C!       This routine sorts the pulses of a cluster by pad number by
C!    a simple ripple sort.  They are sorted in either descending or
C!    ascending order, depending on the initial order of the first
C!    and last pulses.  This is appropriate because most clusters are
C!    initially either in ascending or descending order, and those
C!    which are not so generally are close to being so.  Pulses with
C!    the same pad number are sorted by increasing time.
C!--------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION IPDNR(NP),IDXSR(NP),IT0NR(NP)
      LOGICAL LSORT
C
      IF (IPDNR(1).LT.IPDNR(NP)) THEN
        DO 29 I=1,NP-1,1
          LSORT=.TRUE.
          DO 27 K=1,NP-I,1
            IP=IDXSR(K)
            IPNXT=IDXSR(K+1)
            IF (IPDNR(IPNXT) .LT. IPDNR(IP)) THEN
              LSORT=.FALSE.
              IDXSR(K)=IPNXT
              IDXSR(K+1)=IP
            ELSEIF (IPDNR(IPNXT).EQ.IPDNR(IP)) THEN
              IF (IT0NR(IPNXT).LT.IT0NR(IP)) THEN
                LSORT=.FALSE.
                IDXSR(K)=IPNXT
                IDXSR(K+1)=IP
              ENDIF
            ENDIF
   27     CONTINUE
          IF (LSORT) GO TO 31
   29   CONTINUE
   31   CONTINUE
      ELSE
        DO 36 I=2,NP,1
          LSORT=.TRUE.
          DO 34 K=NP,I,-1
            IP=IDXSR(K)
            IPNXT=IDXSR(K-1)
            IF (IPDNR(IPNXT) .LT. IPDNR(IP)) THEN
              LSORT=.FALSE.
              IDXSR(K)=IPNXT
              IDXSR(K-1)=IP
            ELSEIF (IPDNR(IPNXT).EQ.IPDNR(IP)) THEN
              IF (IT0NR(IPNXT).GT.IT0NR(IP)) THEN
                LSORT=.FALSE.
                IDXSR(K)=IPNXT
                IDXSR(K-1)=IP
              ENDIF
            ENDIF
   34     CONTINUE
          IF (LSORT) GO TO 38
   36   CONTINUE
   38   CONTINUE
      ENDIF
      RETURN
      END
#endif
