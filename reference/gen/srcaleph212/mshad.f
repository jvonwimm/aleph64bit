      FUNCTION MSHAD(I1,I2)
C----------------------------------------------------------------------
C
CKEY MUONID MUON SHADOW / INTERNAL
C
C!  test if two muon candidate tracks are in shadow (uses HROA bank)
C!
C!  author : G. Capon         23-mar-1991
C!           G. Taylor        23-apr-1992
C!
C!  input : I1,I2 track numbers (in JULIA)
C!  output: MSHAD  = flag (=1/0 if tracks are/are not in shadow)
C=======================================================================
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JHRODX=1,JHROSG=2,JHROSF=3,JHROHI=4,LHROAA=4)
      DIMENSION IHCLO(23,2),DINV2(23)
C correction 25/2/92 from A Venturi for overlap region
C put ROADWD value in here since it never(?) changes
      PARAMETER (LAY1SU=1,LAY1CO=1,ROADWD=3.)
      LOGICAL BTEST
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
C-----------------------------------------------------------------------
      MSHAD=0
      NCOM=0
      CALL VZERO (IHCLO,23*2)
      DO 24 N=1,2
        IF(N.EQ.1) IMU=I1
        IF(N.EQ.2) IMU=I2
        ITR=IMU
        JHROA=NLINK('HROA',ITR)
        IF (JHROA.LE.0) GOTO 55
        CALL VZERO(DINV2,23)
        DO 41 L=1,LROWS(JHROA)
          IM=ITABL(JHROA,L,JHROHI)
          IF(IM.LT.0) GO TO 41
          KFLAG=ITABL(JHROA,L,JHROSF)
          LAY=KFLAG/256
          IF(.NOT.BTEST(KFLAG,0)) GO TO 41
          IF(LAY.LT.1 .OR. LAY.GT.23) GO TO 41
          DIFX=RTABL(JHROA,L,JHRODX)
          SIG =RTABL(JHROA,L,JHROSG)
          DIFN=DIFX/(ROADWD*SIG+3.)
          IF (DIFN.EQ.0) DIFN = 0.001
          DINVS=1./(DIFN*DIFN)
C
C correction 25/2/92 from A Venturi for overlap region
C
          IF(DINVS.GT.DINV2(LAY).AND.BTEST(KFLAG,2)) THEN
            DINV2(LAY)=DINVS
            IHCLO(LAY,N)=ITABL(JHROA,L,JHROHI)
          ENDIF
   41   CONTINUE
   24 CONTINUE
C
C compute common hits in last ten layers
C
      DO 53 LAY=LAY1CO,23
        IF (IHCLO(LAY,1)*IHCLO(LAY,2).EQ.0) GO TO 53
        IF (IHCLO(LAY,1) .EQ. IHCLO(LAY,2)) NCOM=NCOM+1
   53 CONTINUE
   55 CONTINUE
C
C                check much hits in common to 2 tracks
C
      ISAME=0
      JMTHR=IW(NAMIND('MTHR'))
      IF (JMTHR.LE.0) GO TO 39
      ITR1=I1
      ITR2=I2
      DO 31 K=1,LROWS(JMTHR)
        ITRAC=ITABL(JMTHR,K,3)
        IF (ITRAC.EQ.ITR1) THEN
          IHIT=ITABL(JMTHR,K,1)
          DO 32 J=1,LROWS(JMTHR)
            NHIT=ITABL(JMTHR,J,1)
            NTRAC=ITABL(JMTHR,J,3)
            IF (NHIT.EQ.IHIT .AND. NTRAC.EQ.ITR2) THEN
              ISAME=1
              GO TO 39
            ENDIF
   32     CONTINUE
        ENDIF
   31 CONTINUE
   39 CONTINUE
C
C            set shadow flag
C
      IF (NCOM.GT.4 .OR. ISAME.GT.0) MSHAD=1
      RETURN
      END
