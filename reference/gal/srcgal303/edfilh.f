       SUBROUTINE EDFILH
C.----------------------------------------------------------------------
C   D.Zwierski  (X)                                    October 87
C! Fill standard histog. for digits
C  Si FHISJO(4)=.TRUE., then fills histograms with digital
C  values taken from EWDI and ETDI BOS banks
C
C  Called by ECDIGI
C.----------------------------------------------------------------------
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
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
C    initialisation of ECCONT(I) I=25,30, used for filling
C
       IF(FHISJO(4)) THEN
           WSKI=1.
           SKIZ=0.
           IDSKI=439
           DO 121 I=25,30,1
               ECCONT(I)=0.
121        CONTINUE
C
C    take digital signals on wires from EWDI bank
C
           KDSKI=NLINK('EWDI',0)
           IF(KDSKI.EQ.0) GOTO 125
           DO 123 I=1,LROWS(KDSKI),1
               DO 124 J=1,45,1
                   IESKI=ITABL(KDSKI,I,J+1)
                   IF(MOD(J,2).EQ.0) THEN
                       ECCONT(26)=ECCONT(26)+IESKI
                   ELSE
                       ECCONT(25)=ECCONT(25)+IESKI
                   ENDIF
                   ZPSKI=J-0.5
                   ESKI=FLOAT(IESKI)/1000
                   CALL HFILL(IDSKI+18,ZPSKI,SKIZ,ESKI)
                   IF(IESKI.NE.0) THEN
                       CALL HFILL(IDSKI+19,ZPSKI,SKIZ,WSKI)
                   ENDIF
124            CONTINUE
123        CONTINUE
           ECCONT(25)=ECCONT(25)/1000.
           ECCONT(26)=ECCONT(26)/1000.
           CALL HFILL(IDSKI+16,ECCONT(26),ECCONT(25),WSKI)
           ECCONT(26)=ECCONT(26)+ECCONT(25)
           CALL HFILL(IDSKI+17,ECCONT(26),SKIZ,WSKI)
C
C    take digital signals on towers from ETDI bank
C
125        KDSKI=NLINK('ETDI',0)
           IF(KDSKI.EQ.0) GOTO 225
       ROWSKI=FLOAT(LROWS(KDSKI))
       CALL HFILL(IDSKI+24,ROWSKI,SKIZ,WSKI)
           DO 223 I=1,LROWS(KDSKI),1
               DO 224 J=2,4,1
                   IESKI=ITABL(KDSKI,I,J)
                   ECCONT(25+J)=ECCONT(25+J)+IESKI
                   ECCONT(30)=ECCONT(30)+IESKI
224            CONTINUE
223        CONTINUE
           ECCONT(27)=ECCONT(27)/1000.
           ECCONT(28)=ECCONT(28)/1000.
           ECCONT(29)=ECCONT(29)/1000.
           ECCONT(30)=ECCONT(30)/1000.
           CALL HFILL(IDSKI+20,ECCONT(30),SKIZ,WSKI)
           CALL HFILL(IDSKI+21,ECCONT(27),SKIZ,WSKI)
           CALL HFILL(IDSKI+22,ECCONT(28),SKIZ,WSKI)
           CALL HFILL(IDSKI+23,ECCONT(29),SKIZ,WSKI)
       ENDIF
225    CONTINUE
       RETURN
       END
