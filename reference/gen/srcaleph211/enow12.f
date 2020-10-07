       SUBROUTINE ENOW12(XW1,XW2,ECLU,THET,ESW1N,ESW2N)
C=============================================================
C!     Calculate normalised estimator for photon
C!     Author                             M.N Minard 10/02/92
CKEY PHOTONS
C!
C!     Input   XW1 : 1st longitudinal estimator
C!             XW2 : 2nd longitudinal estimator
C!             ECLU : cluster energy
C!             Thet : Theta of cluster
C!     Output  ESW1N: Normalised 1st estimator
C!             ESW2N: Normalised 2nd estimator
C!     Banks used EW1N , EW2N
C---------------------------------------------------
      SAVE
      PARAMETER(JEW1MT=1,JEW1XT=2,JEW1P1=3,JEW1P2=4,JEW1P3=5,JEW1P4=6,
     +          JEW1P5=7,JEW1P6=8,JEW1S1=9,JEW1S2=10,JEW1S3=11,
     +          JEW1S4=12,JEW1S5=13,JEW1S6=14,LEW1NA=14)
      PARAMETER(JEW2MT=1,JEW2XT=2,JEW2P1=3,JEW2P2=4,JEW2P3=5,JEW2P4=6,
     +          JEW2P5=7,JEW2P6=8,JEW2S1=9,JEW2S2=10,JEW2S3=11,
     +          JEW2S4=12,JEW2S5=13,JEW2S6=14,LEW2NA=14)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
       DIMENSION XPARM(3,2)
       INTEGER ALGTDB,GTSTUP
       CHARACTER DET*2, LIST*8
       DATA NAEW1N,NAEW2N /2*0/, IROLD/0/, DET/'EC'/, LIST/'EW1NEW2N'/
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
      IF (NAEW1N.EQ.0) THEN
         NAEW1N = NAMIND('EW1N')
         NAEW2N = NAMIND('EW2N')
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
       ESW1N = -99.
       ESW2N = -99.
       KEW1N = IW(NAEW1N)
       IF ( KEW1N.EQ.0) GO TO 999
       KEW2N = IW(NAEW2N)
C
C      Choose the parameters according theta range
C
       NEW1N = 0
       IF (KEW1N.NE.0) NEW1N = LROWS(KEW1N)
       DO IEW1N = 1,NEW1N
          JEW1N = KROW(KEW1N,IEW1N)
          IF (THET.GE.RW(JEW1N+JEW1MT).AND.THET.LT.RW(JEW1N+JEW1XT))THEN
             CALL UCOPY(RW(JEW1N+JEW1P1),XPARM(1,1),3)
             CALL UCOPY(RW(JEW1N+JEW1S1),XPARM(1,2),3)
          ENDIF
       ENDDO
       CALL ENOLWF(XW1,ECLU,XPARM,ESW1N)
       NEW2N = 0
       IF (KEW2N.NE.0) NEW2N = LROWS(KEW2N)
       DO IEW2N = 1,NEW2N
          JEW2N = KROW(KEW2N,IEW2N)
          IF (THET.GE.RW(JEW2N+JEW2MT).AND.THET.LT.RW(JEW2N+JEW2XT))THEN
             CALL UCOPY(RW(JEW2N+JEW2P1),XPARM(1,1),3)
             CALL UCOPY(RW(JEW2N+JEW2S1),XPARM(1,2),3)
          ENDIF
       ENDDO
       CALL ENOLWF(XW2,ECLU,XPARM,ESW2N)
 999   CONTINUE
       RETURN
       END
