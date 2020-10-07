       SUBROUTINE ENOF4(F4,ECLU,THET,ESF4N)
C=============================================================
C!     Calculate normalised estimator for photon
C!     Author                             M.N Minard 10/02/92
CKEY PHOTONS
C!
C!     Input    F4 : transverse estimator
C!             ECLU : cluster energy
C!             Thet : Theta of cluster
C!     Output  ESF4N: Normalised  estimator
C!     Banks used EF4N
C---------------------------------------------------
       SAVE
      PARAMETER(JEF4MT=1,JEF4XT=2,JEF4P1=3,JEF4P2=4,JEF4P3=5,JEF4P4=6,
     +          JEF4P5=7,JEF4P6=8,JEF4S1=9,JEF4S2=10,JEF4S3=11,
     +          JEF4S4=12,JEF4S5=13,JEF4S6=14,LEF4NA=14)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
       DIMENSION XPARM(4,2)
       CHARACTER DET*2, LIST*4
       INTEGER ALGTDB,GTSTUP
       DATA NAEF4N /0/ , IROLD/0/, DET/'EC'/, LIST/'EF4N'/
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
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
C
C
C- 1st entry
       IF (NAEF4N.EQ.0) THEN
          NAEF4N = NAMIND('EF4N')
       ENDIF
C
C-     Access the normalisation bank
C
C! Get banks from DB depending on run and setup code
C
      CALL ABRUEV (IRUN,IEVT)
      IRET = 0
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IF (IRUN.LE.2000) THEN
           ITP = GTSTUP (DET,IRUN)
        ELSE
           ITP = IRUN
        ENDIF
        IRET= ALGTDB(JUNIDB(0),LIST,-ITP)
      ENDIF
C
       ESF4N = -99.
       KEF4N = IW(NAEF4N)
       IF ( KEF4N.EQ.0) GO TO 999
C
C      Choose the parameters according theta range
C
       NEF4N = 0
       IF (KEF4N.NE.0) NEF4N = LROWS(KEF4N)
       DO IEF4N = 1,NEF4N
          JEF4N = KROW(KEF4N,IEF4N)
          IF (THET.GE.RW(JEF4N+JEF4MT).AND.THET.LT.RW(JEF4N+JEF4XT))THEN
             CALL UCOPY(RW(JEF4N+JEF4P1),XPARM(1,1),4)
             CALL UCOPY(RW(JEF4N+JEF4S1),XPARM(1,2),4)
          ENDIF
       ENDDO
C
C      Calculate expected value and sigma
C
       EXP = XPARM(3,1)*((XPARM(1,1)+XPARM(2,1)/SQRT(ECLU))/XPARM(4,1))
       SEXP= XPARM(3,2)*((XPARM(1,2)+XPARM(2,2)/SQRT(ECLU))/XPARM(4,2))
       ESF4N = (F4-EXP)/SEXP
 999   RETURN
       END
