       SUBROUTINE RPECO( ETOWE, TGV1)
C----------------------------------------------------------------------
CKEY EDIR CLUSTER PARAMETERS
C! Calculate parameters of the 2 most energetic cluster.
C-
C   Input   : None
C   Output  : ETOWE  = Total tower energy
C             TGV1(1-2) = Energy of the 2 most energetic cluster
C             TGV1(3-4) = Polar angleof the 2 most energetic cluster
C             TGV1(5)   = Angle between the 2 most energetic cluster
C             TGV1(6-7) = PECO # of the 2 most energetic cluster
C-
C   Called by   : ECAGET
C   Calls  : None
C   Input banks : PECO
C-
C                                    Author: S.Dugey  - 910400
C----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
C --
      DIMENSION TGV1(7)
C --
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
C --
C   Initialise variables
C --
      ETOWE= 0.
      CALL VZERO (TGV1,7)
C --
      NAPECO = NAMIND ('PECO')
      KPECO = IW (NAPECO)
      IF ( KPECO .EQ . 0) GOTO 999
      NPECO = LROWS (KPECO)
C --
C   Loop on PECO
C --
      IPEH1 = 0
      EH1 = 0.
      DO 100 IPECO = 1,NPECO
C --
C   Skip LCAL PECO
C --
        IF ( ITABL (KPECO,IPECO,JPECKD).EQ.192) GO TO 100
        EPEC = RTABL( KPECO,IPECO,JPECER)
        ETOWE = ETOWE + EPEC
        IF(EPEC.GT.EH1) THEN
          IPEH1 = IPECO
          EH1 = EPEC
        ENDIF
 100  CONTINUE
C --
      IPEH2 = 0
      EH2 = 0.
      DO 101 IPECO = 1,NPECO
        IF ( ITABL (KPECO,IPECO,JPECKD).EQ.192) GO TO 101
        EPEC = RTABL( KPECO,IPECO,JPECER)
        IF(EPEC.GT.EH2.AND.IPECO.NE.IPEH1) THEN
          IPEH2 = IPECO
          EH2 = EPEC
        ENDIF
 101  CONTINUE
C --
C   Fill TGV1
C --
      IF(EH1.NE.0.) THEN
        TGV1(1) = EH1
        TGV1(3) = RTABL(KPECO,IPEH1,JPECTH)
        TGV1(6) = IPEH1
        IF( EH2.NE.0.) THEN
          TGV1(2) = EH2
          TGV1(4) = RTABL(KPECO,IPEH2,JPECTH)
          PH1 = RTABL(KPECO,IPEH1,JPECPH)
          PH2 = RTABL(KPECO,IPEH2,JPECPH)
          SIN1 = SIN (TGV1(3))
          SIN2 = SIN (TGV1(4))
          COTH12 = SIN1*SIN2 * (COS(PH1)*COS(PH2) + SIN(PH1)*SIN(PH2))
     +       + COS(TGV1(3)) * COS(TGV1(4))
          IF (ABS(COTH12).GT.1.) COTH12=SIGN(1.,COTH12)
          TGV1(5) = ACOS ( COTH12 )
          TGV1(7) = IPEH2
        ENDIF
      ENDIF
C --
 999  CONTINUE
      RETURN
      END
