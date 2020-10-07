      SUBROUTINE CHIV0(IFRFT,IROW,MOM,D,M2,SM2,
     &  CHISQ,POINT,DERR,V0MOM,TMOM,IERROR)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Compute the chisquared for a V0 candidate, given an initial
C  estimate of the decay point.
C  Author                                                D. Brown 6-9-93
C  Called from QFNDV0
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Global includes
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C
C  Input variables
C
      INTEGER IFRFT,IROW(2),KTRK
      REAL D(3),M2,SM2(2),MOM(2)
C
C  Output variables
C
      INTEGER IERROR
      REAL CHISQ,POINT(3),DERR(3,3),V0MOM(3),TMOM(3,2)
C
C  Local variables
C
      INTEGER ICOR,JCOR,IERR,JERR,ITRK
      INTEGER IPT,JROW
      INTEGER ITER,MAXITER/5/
      REAL RADIUS(2),TARC(2),TRKPT(3,2),T1(3,2),AMAT(3,3,2)
      REAL TERR(3,3,2)
      REAL DELTA,BVEC(3),OMAT(3,3),KERR2
      REAL ALPHA,BETA(3),GAMMA(3,3)
      REAL DD(3)
C
      REAL DIST,DIST_ERR,DISTCUT/1.0/
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+LMHCOL)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+LMHROW)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C ----------------------------------------------------------------------
C  Copy the point; it may get overwritten
C
      DO ICOR=1,3
        POINT(ICOR) = D(ICOR)
      END DO
      DIST = 100.
      ITER = 0
C
C  Compute some things that don't change
C
      DO ITRK=1,2
        RADIUS(ITRK) = 1./RTABL(IFRFT,IROW(ITRK),1)
C
C  Get the upper corner of the error matrix
C
        IPT = 7
        DO IERR=1,3
          DO JERR =1,IERR
            TERR(IERR,JERR,ITRK) = RTABL(IFRFT,IROW(ITRK),IPT)
            IPT = IPT + 1
          END DO
        END DO
        DO IERR=1,3
          DO JERR=IERR+1,3
            TERR(IERR,JERR,ITRK) = TERR(JERR,IERR,ITRK)
          END DO
        END DO
      END DO
C
C  Iterate
C
      DO WHILE(DIST .GT. DISTCUT .AND. ITER .LT. MAXITER)
C
C  Move the tracks to the initial point; this also computes
C  some vectors and matricies
C
        DO ITRK=1,2
          JROW = KROW(IFRFT,IROW(ITRK))
          CALL TMOVE(POINT,RW(JROW+1),RW(JROW+7),
     &          TARC(ITRK),TRKPT(1,ITRK),T1(1,ITRK),AMAT(1,1,ITRK))
        END DO
C
C  Get the mass constraint kinematic information
C
        CALL V0KINE(M2,SM2,MOM,T1,RADIUS,TERR,
     &        DELTA,BVEC,OMAT,KERR2)
C
C  Compute tensors
C
        ALPHA = DELTA**2/KERR2
        DO ICOR=1,3
          BETA(ICOR) = BVEC(ICOR)/KERR2
          DO JCOR=1,3
            GAMMA(JCOR,ICOR) = OMAT(JCOR,ICOR)/KERR2
            DO KTRK=1,2
              ALPHA = ALPHA + AMAT(JCOR,ICOR,KTRK)*
     &              TRKPT(ICOR,KTRK)*TRKPT(JCOR,KTRK)
              BETA(ICOR) = BETA(ICOR) + AMAT(JCOR,ICOR,KTRK)*
     &              TRKPT(JCOR,KTRK)
              GAMMA(JCOR,ICOR) = GAMMA(JCOR,ICOR) +
     &              AMAT(JCOR,ICOR,KTRK)
            END DO
          END DO
        END DO
C
C  Invert the matrix
C
        CALL RSINV(3,GAMMA,3,IERROR)
        IF(IERROR.NE.0)RETURN
C
C  Solve for the change in D and the chisquared
C
        CHISQ = ALPHA
        DIST = 0.0
        DIST_ERR = 0.0
        DO ICOR=1,3
          DD(ICOR) = 0.0
          DO JCOR = 1,3
            DD(ICOR) = DD(ICOR) + GAMMA(JCOR,ICOR)*BETA(JCOR)
            DERR(JCOR,ICOR) = GAMMA(JCOR,ICOR)
            CHISQ = CHISQ - GAMMA(JCOR,ICOR)*BETA(ICOR)*BETA(JCOR)
          END DO
          DIST = DIST + DD(ICOR)**2
          DIST_ERR = MAX(DIST_ERR,DERR(ICOR,ICOR))
        END DO
        DIST = SQRT(DIST/DIST_ERR)
C
C  Update the point
C
        DO ICOR=1,3
          POINT(ICOR) = POINT(ICOR) + DD(ICOR)
        END DO
        ITER = ITER + 1
      END DO
C
C  Move the tracks to this new point, and find the momentum
C
      DO ICOR=1,3
        V0MOM(ICOR) = 0.0
      END DO
      DO ITRK=1,2
        JROW = KROW(IFRFT,IROW(ITRK))
        CALL TMOVE(POINT,RW(JROW+1),RW(JROW+7),
     &      TARC(ITRK),TRKPT(1,ITRK),T1(1,ITRK),AMAT(1,1,ITRK))
        DO ICOR=1,3
          TMOM(ICOR,ITRK) = MOM(ITRK)*T1(ICOR,ITRK)
          V0MOM(ICOR) = V0MOM(ICOR) + TMOM(ICOR,ITRK)
        END DO
      END DO
      RETURN
      END
