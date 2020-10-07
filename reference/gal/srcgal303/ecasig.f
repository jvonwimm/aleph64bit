      SUBROUTINE ECASIG
C----------------------------------------------------------------------
C    Y. Karyotakis 11-3-86   End of Tracking
C! Reduce Hits banks
C - Calls : WDROP                                       from BOSlib
C           AUBPRS                                      from ALEPHLIB
C           CAFIHT                                      from this .HLB
C ----------------------------------------------------
      SAVE
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)
      INTEGER CAFIHT
      LOGICAL FDEB
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
C ------------------------------------------------------------
      FDEB = FDEBJO .AND. IPRIJO(4).EQ.1
C
C - drop working bank IDPSIG and reduce size to real one
C
      CALL WDROP (IW,IDPSIG)
      CALL AUBPRS('ESHIEWHI')
      IF(FDEB) THEN
        CALL PRTABL('ESHI',0)
        CALL PRTABL('EWHI',0)
      ENDIF
C
C     Create wires bank
C
      JETHT = CAFIHT (NAESHI,'ETHT')
      IF (JETHT.EQ.0) GOTO 998
      IF (FDEB) CALL PRTABL ('ETHT',0)
C
C - Create tower bank
C
      JEWHT = CAFIHT (NAEWHI,'EWHT')
      IF (JEWHT.EQ.0) GOTO 998
      IF (FDEB) CALL PRTABL ('EWHT',0)
C
C Statistics
C
C    if FHISJO(4)=.TRUE, then fill histograms
C
       IF(FHISJO(4)) THEN
           IDSKI=439
           WSKI=1.
           SKIZ=0.
           DO 123 I=21,30,1
               ECCONT(I)=ECCONT(I)/1000
123        CONTINUE
           CALL HFILL(IDSKI+2,ECCONT(21),SKIZ,WSKI)
           CALL HFILL(IDSKI+3,ECCONT(22),SKIZ,WSKI)
           CALL HFILL(IDSKI+4,ECCONT(23),SKIZ,WSKI)
           CALL HFILL(IDSKI+5,ECCONT(24),SKIZ,WSKI)
           CALL HFILL(IDSKI+6,ECCONT(26),ECCONT(25),WSKI)
           CALL HFILL(IDSKI+7,ECCONT(27),SKIZ,WSKI)
           CALL HFILL(IDSKI+10,ECCONT(27),SKIZ,WSKI)
           CALL HFILL(IDSKI+11,ECCONT(28),SKIZ,WSKI)
           CALL HFILL(IDSKI+12,ECCONT(29),SKIZ,WSKI)
           CALL HFILL(IDSKI+13,ECCONT(30),SKIZ,WSKI)
       ENDIF
C
      KETHT = IW(NAETHT)
      NTOWR = LROWS (KETHT)
      NECONT(4) = NECONT(4) + NTOWR
      IF (NTOWR.GT.0)   NECONT(1) = NECONT(1) + 1
      EEVT = VSUM(ECCONT,3)/1000000.
      ECCONT(7) = ECCONT(7) + EEVT
      ECCONT(8) = ECCONT(8) + EEVT**2
      DO 10 I=1,3
   10 ECCONT(I+3) = ECCONT(I+3) + ECCONT(I)/1000000.
      DO 11 I=1,3
   11 ECCONT(I) = 0.
 980  RETURN
C
C - not enough space
C
 998  CONTINUE
      CALL ALTELL ('ECASIG: not enough space for HIT banks ',1,'NEXT')
      END
