      SUBROUTINE EHCHAR(PDEP,TRLEN,IPLAN,NGTR)
C.----------------------------------------------------------------------
C  M.Rumpf      Jan 86
C! Track elem. energy
C  Input : PDEP  (NGTR)     Energy deposition point
C          TRLEN (NGTR)     Track segment length
C          IPLAN (NGTR)     Wire plane number
C          NGTR             number of points
C  Store results in temporary bank PSIG
C  UNIT for signal is 1 Kev integer
C  - Called by EHTRKE
C  - Calls     EHDEPT,WBANK
C.----------------------------------------------------------------------
      SAVE
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)
C - user stop particle flag
      PARAMETER (NOMOR=3)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      LOGICAL FTINO, FMUON, FELEC, FHADC
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
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
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
      REAL PDEP(3,NGTR), TRLEN(NGTR)
      INTEGER IPLAN(NGTR)
       DIMENSION MIL(4)
       DATA MIL/4HEB12,4HEBS3,4HEC12,4HECS3/
       DATA NEVSKI/0/
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
C --------------------------------------------------------------
C
C    Book working bank PSIG if does not exist or extend it if too small
C
      LEN = LPS1*NGTR+LMHLEN
      IF (IDPSIG.EQ.0) THEN
         CALL WBANK (IW,IDPSIG,LEN,*999)
         IW(IDPSIG-3) = INTCHA ('PSIG')
         IW(IDPSIG+1) = LPS1
      ELSE
         IF (LEN .GT. IW(IDPSIG)) THEN
            CALL WBANK (IW,IDPSIG,LEN,*999)
         ENDIF
      ENDIF
      IW(IDPSIG+2) = 0
C
C    if FHISJO(4)=.TRUE. and if new event, then initialisation
C    of ECCONT(I) I=21,30, used for filling histograms
C
       IF(FHISJO(4)) THEN
           IDSKI=439
           SKIZ=0.
           WSKI=1
           IF(NEVTJO.NE.NEVSKI) THEN
               DO 57 J=21,30,1
                   ECCONT(J)=0.
57             CONTINUE
           ENDIF
       ENDIF
C
C
C    Loop over all points
C
      NECSG = LROWS (IDPSIG)
      IPX   = KNEXT (IDPSIG)
C
      DO 1 I=1,NGTR
C
      CALL EHDEPT (TRLEN(I),IECDE)
C
      IF (TSMEAR.EQ.'SMEARING')             THEN
C           Do something
      ENDIF
C
C   Store point in bank PSIG
C
      IF(IECDE.EQ.0)                        GOTO 1
          NECSG = NECSG + 1
C
C   Add a new entry
C
      RW(IPX+1) = PDEP(1,I)
      RW(IPX+2) = PDEP(2,I)
      RW(IPX+3) = PDEP(3,I)
          IW(IPX + 4) = IECDE
          IW(IPX + 5) = IECDE
          IW(IPX + 6) = IPLAN(I)
C
C    if FHISJO(4)=.TRUE.,then get data for filling
C
       IF(FHISJO(4)) THEN
C
C analog signal per evt ans sensible volume
C
           NUMSKI=IUCOMP(ITRKEL(5),MIL,4)
           NUMSKI=NUMSKI+20
           ECCONT(NUMSKI)=ECCONT(NUMSKI)+IECDE
C
C signal of odd planes vs signal of even planes (analog)
C
           IF(MOD(IPLAN(I),2).EQ.0)  THEN
               ECCONT(26)=ECCONT(26)+IECDE
           ELSE
               ECCONT(25)=ECCONT(25)+IECDE
           ENDIF
C
C analog signal for all the wire planes
C
           ECCONT(27)=ECCONT(27)+IECDE
C
C analog signal per wire planes
C
           ZPSKI=IPLAN(I)-0.5
           ESKI=FLOAT(IECDE)/1000.
           CALL HFILL(IDSKI+8,ZPSKI,SKIZ,ESKI)
C
C number of hits per wire planes
C
           CALL HFILL(IDSKI+9,ZPSKI,SKIZ,WSKI)
C
C analog signal deposited in towers in all stacks and each stacks
C
           IF(IPLAN(I).LE.10) THEN
               ECCONT(28)=ECCONT(28)+IECDE
           ELSE IF (IPLAN(I).LE.33) THEN
               ECCONT(29)=ECCONT(29)+IECDE
           ELSE IF (IPLAN(I).LE.45) THEN
               ECCONT(30)=ECCONT(30)+IECDE
            ENDIF
           NEVSKI=NEVTJO
       ENDIF
C
C
         IPX = IPX + LCOLS(IDPSIG)
C
 1    CONTINUE
      IW(IDPSIG+2) = NECSG
C
 999  CONTINUE
      RETURN
      END
