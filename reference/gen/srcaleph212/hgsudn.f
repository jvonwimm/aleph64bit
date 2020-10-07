      FUNCTION HGSUDN(ITR)
C----------------------------------------------------------------------
C
CKEY MUONID HCAL SHADOW / INTERNAL
C
C!  calculate the normalised sum of square of residuals (uses HROA bank)
C!
C!  author : G. Taylor         15-May-1991
C!
C!  input : ITR (in JULIA)
C!  output: HGSUDN /R= normalised sum of square of residuals :
C!                   =1/nfired*sum (dx.i/roadwidth.i)**2
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
      DIMENSION DINV2(23)
C
C correction 25/2/92 from A Venturi for overlap region
C put ROADWD value in here since it never(?) changes
C
      PARAMETER (LAY1SU=1,ROADWD=3.)
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
C
C
      HGSUDN=999.
      JHROA=NLINK('HROA',ITR)
      IF (JHROA.LE.0) RETURN
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
C correction 25/2/92 from A Venturi for overlap region
        IF(DINVS.GT.DINV2(LAY).AND.BTEST(KFLAG,2)) THEN
          DINV2(LAY)=DINVS
        ENDIF
   41 CONTINUE
      SUMD=0.
      SLAY=0.
      DO 42 LAY=LAY1SU,23
        IF (DINV2(LAY).GT.0.) THEN
          SLAY=SLAY+1.
          SUMD=SUMD+1./DINV2(LAY)
        ENDIF
   42 CONTINUE
      IF (SLAY.GT.0.) HGSUDN=SUMD/SLAY
      RETURN
      END
