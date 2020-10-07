      SUBROUTINE ITRSB(NHIT,IMSK,ISPBT)
C.
C...ITRSB  1.00  890707                                  J.Ratcliffe
C.
C!  Evaluate ITC special trigger bits.
C.
C.  Calling arguments:
C.  NHIT  - Array of hit flag for each wire (=1 if hit).       (INPUT)
C.  IMSK  - Array of 192 R-Phi masks (=1 if positive).         (INPUT)
C.  ISPBT - 1 word containing the 24 'special' trigger bits.  (OUTPUT)
C.
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      COMMON /ITSPEC/ITB2BS,ITHCLU,ITTCLU,ITRTHR,ITHTHR(8),ITBTHR(8)
      INTEGER ITB2BS,ITRTHR,ITHTHR,ITBTHR
      LOGICAL ITHCLU,ITTCLU
C
      PARAMETER (LHB=0, LBV=8, LBB=16, LTR=17, LMS=18, LTC=19, LTCO=23)
      INTEGER ISPBT
      INTEGER NHIT(8,0:145),IMSK(0:192),IMSK2(96),NW(8)
      LOGICAL BTEST
      DATA NW/4*96, 4*144/
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
      ISPBT = 0
C
C--  Set Mulitiplexer Select bit
C--
      IF (ICITJO(3).EQ.0) ISPBT = IBSET(ISPBT,LMS)
C
C--  Set Back to Back bit
C--
      K = 0
      CALL VZERO(IMSK2,96)
      DO 10 I = 1,191,2
        K = K + 1
        IF (IMSK(I)+IMSK(I+1).GT.0) IMSK2(K) = 1
   10 CONTINUE
C
      DO 20 I = 1,96
        IF (IMSK2(I).EQ.1) THEN
          DO 25 J = -ITB2BS,+ITB2BS
            ISEC = MOD((48+I+J),96)
            IF (ISEC.EQ.0) ISEC = 96
            IF (IMSK2(ISEC).EQ.1) THEN
              ISPBT = IBSET(ISPBT,LBB)
              GOTO 26
            ENDIF
   25     CONTINUE
        ENDIF
   20 CONTINUE
   26 CONTINUE
C
C--  Set Track bit and 5 Track Count bits
C--
      NMSK = 0
      DO 30 I = 1,192
        IF (IMSK(I).EQ.1) THEN
          IF (ITTCLU.OR.IMSK(I-1).EQ.0) NMSK = NMSK + 1
        ENDIF
   30 CONTINUE
C
      IF (NMSK.GT.ITRTHR) ISPBT = IBSET(ISPBT,LTR)
      IBT = IBITS(NMSK,0,4)
      CALL MVBITS(IBT,0,4,ISPBT,LTC)
      IBT = IBITS(NMSK,4,4)
      IF (IBT.GT.0) ISPBT = IBSET(ISPBT,LTCO)
C
C--  Set 8 Hit bits and 8 Background Veto bits
C--
      DO 50 I = 1,8
        NH = 0
        DO 55 J = 1,NW(I)
          IF (NHIT(I,J).EQ.1) THEN
            IF (ITHCLU.OR.NHIT(I,J-1).EQ.0) NH = NH + 1
          ENDIF
   55   CONTINUE
        IF (NH.GT.ITHTHR(I)) ISPBT = IBSET(ISPBT,I-1+LHB)
        IF (NH.GT.ITBTHR(I)) ISPBT = IBSET(ISPBT,I-1+LBV)
   50 CONTINUE
C
      END
