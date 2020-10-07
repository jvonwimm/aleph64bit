      LOGICAL FUNCTION MSOLMC(ITR1)
C----------------------------------------------------------------------
C
CKEY MUONID MUON SHADOW / INTERNAL
C
C!  Find out if track ITR1 has a mu chamber hit which it shares with
C!  no other track
C!  author : G. TAYLOR        7-APR-1992
C!
C!  input : ITR1 track numbers (in JULIA)
C!  output: MSOLMC = .TRUE. if track has a non shared mu ch hit
C=======================================================================
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL SHARED
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
C
C                check much hits in common to 2 tracks
C
      MSOLMC=.FALSE.
      JMTHR=IW(NAMIND('MTHR'))
      IF (JMTHR.LE.0) GO TO 39
      DO 31 K=1,LROWS(JMTHR)
        ITRAC=ITABL(JMTHR,K,3)
        IF (ITRAC.EQ.ITR1) THEN
          IHIT=ITABL(JMTHR,K,1)
          SHARED=.FALSE.
          DO 32 J=1,LROWS(JMTHR)
            NHIT=ITABL(JMTHR,J,1)
            NTRAC=ITABL(JMTHR,J,3)
            IF (NHIT.EQ.IHIT .AND. NTRAC.NE.ITRAC) SHARED=.TRUE.
   32     CONTINUE
          IF(.NOT.SHARED) MSOLMC=.TRUE.
        ENDIF
   31 CONTINUE
   39 CONTINUE
      RETURN
      END
