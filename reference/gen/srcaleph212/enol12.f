       SUBROUTINE ENOL12(XL1,XL2,ECLU,THET,ESL1N,ESL2N)
C=============================================================
C!     Calculate normalised estimator for photon
C!     Author                             M.N Minard 10/02/92
CKEY PHOTONS
C!
C!     Input   XL1 : 1st longitudinal estimator
C!             XL2 : 2nd longitudinal estimator
C!             ECLU : cluster energy
C!             Thet : Theta of cluster
C!     Output  ESL1N: Normalised 1st estimator
C!             ESL2N: Normalised 2nd estimator
C!     Banks used EL1N , EL2N
C---------------------------------------------------
      SAVE
      PARAMETER(JEL1MT=1,JEL1XT=2,JEL1P1=3,JEL1P2=4,JEL1P3=5,JEL1P4=6,
     +          JEL1P5=7,JEL1P6=8,JEL1S1=9,JEL1S2=10,JEL1S3=11,
     +          JEL1S4=12,JEL1S5=13,JEL1S6=14,LEL1NA=14)
      PARAMETER(JEL2MT=1,JEL2XT=2,JEL2P1=3,JEL2P2=4,JEL2P3=5,JEL2P4=6,
     +          JEL2P5=7,JEL2P6=8,JEL2S1=9,JEL2S2=10,JEL2S3=11,
     +          JEL2S4=12,JEL2S5=13,JEL2S6=14,LEL2NA=14)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
       DIMENSION XPARM(3,2)
       CHARACTER DET*2, LIST*8
       INTEGER ALGTDB,GTSTUP
       DATA NAEL1N,NAEL2N /2*0/, IROLD/0/, DET/'EC'/, LIST/'EL1NEL2N'/
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
C - 1st entry
      IF (NAEL1N.EQ.0) THEN
         NAEL1N = NAMIND('EL1N')
         NAEL2N = NAMIND('EL2N')
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
       ESL1N = -99.
       ESL2N = -99.
       KEL1N = IW(NAEL1N)
       IF ( KEL1N.EQ.0) GO TO 999
       KEL2N = IW(NAEL2N)
C
C      Choose the parameters according theta range
C
       NEL1N = 0
       IF (KEL1N.NE.0) NEL1N = LROWS(KEL1N)
       DO IEL1N = 1,NEL1N
          JEL1N = KROW(KEL1N,IEL1N)
          IF (THET.GE.RW(JEL1N+JEL1MT).AND.THET.LT.RW(JEL1N+JEL1XT))THEN
             CALL UCOPY(RW(JEL1N+JEL1P1),XPARM(1,1),3)
             CALL UCOPY(RW(JEL1N+JEL1S1),XPARM(1,2),3)
          ENDIF
       ENDDO
       CALL ENOLWF(XL1,ECLU,XPARM,ESL1N)
       NEL2N = 0
       IF (KEL2N.NE.0) NEL2N = LROWS(KEL2N)
       DO IEL2N = 1,NEL2N
          JEL2N = KROW(KEL2N,IEL2N)
          IF (THET.GE.RW(JEL2N+JEL2MT).AND.THET.LT.RW(JEL2N+JEL2XT))THEN
             CALL UCOPY(RW(JEL2N+JEL2P1),XPARM(1,1),3)
             CALL UCOPY(RW(JEL2N+JEL2S1),XPARM(1,2),3)
          ENDIF
       ENDDO
       CALL ENOLWF(XL2,ECLU,XPARM,ESL2N)
 999   CONTINUE
       RETURN
       END
