      SUBROUTINE HCFORA
C--------------------------------------------------------------------
C
C!  Implements the module FORMAT TOWER SIGNAL
C!
C!          Author : G.Zito  86/05/21
C!          mod. by: G.Catanesi, F.Ranjard - 890522
C!                   1 stack or 2 stacks
C!          Input bank : HTHT  McHcStoreys
C!          Output bank: HTDI  McHcTowerDigitising
C!
C!
C!        -Called by : HCDIGI
C!        -Calls     : SORTMI,MVBITS from CERNLIB
C-------------------------------------------------------
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
      PARAMETER(JHTHSA=1,JHTHPT=2,JHTHTT=3,JHTHED=4,LHTHTA=4)
      PARAMETER(JHTDTA=1,JHTDED=2,LHTDIA=2)
      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      COMMON/HCGEGA/ HCSMTH,HCIRTH,HCLSLA,HCTUTH, NHCINL,NHCOUL,NHCTRE,
     +HCPHOF,NHCTU1(LHCNL), HCLARA(LHCNL),HCLAWI(LHCNL),IHCREG(LHCTR),
     +HCZMIN(LPHC),HCZMAX(LPHC),HCRMIN(LPHC), HCRMAX(LPHC),HCTIRF(LPHC),
     +NHCPLA(LPHC), HCWINO(LPHC),HCLTNO(LPHC)
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
C
      JHTHT = IW(NAHTHT)
      IF (JHTHT.EQ.0) RETURN
      NHTHT = LROWS (JHTHT)
      IF (NHTHT.EQ.0) RETURN
C
C      Loop on Storeys
C
      KHTHT = JHTHT +LMHLEN
C
      JHTDI = IW(NAHTDI)
      KHTDI = JHTDI + LMHLEN
C
      NHTDI = 0
      ITH1 = 0
      IPH1 = 0
      DO 10 I = 1, NHTHT
         N = IW(KHTHT+1)
         IST = MOD(N,100)
         N = N / 100
         ITH = MOD(N,100)
         N = N / 100
         IPH = N
         NED = INT(RW(KHTHT+4)*1000.)
         IF(ITH.NE.ITH1.OR.IPH.NE.IPH1) THEN
            ITH1 = ITH
            IPH1 = IPH
            NHTDI = NHTDI + 1
            KHTDI = KROW(JHTDI,NHTDI)
            IPHM = IPH
            CALL MVBITS(IPHM,0,16,IW(KHTDI+1),0)
            ITHM = ITH
            CALL MVBITS(ITHM,0,8,IW(KHTDI+1),16)
            IRE = IHCREG(ITH)
            CALL MVBITS(IRE,0,8,IW(KHTDI+1),24)
         ENDIF
         IST = MIN(IST,LHTDIA-1)
         IW(KHTDI+1+IST) = IW(KHTDI+1+IST) + NED
         KHTHT = KHTHT + LHTHTA
   10 CONTINUE
C
      IW(JHTDI+LMHROW) = NHTDI
C
      IF(FHCDEB)THEN
         WRITE(LOUTIO,500) NHTDI
         KHTDI = JHTDI + LMHLEN
         DO 20 I = 1, NHTDI
            IPH = IBITS(IW(KHTDI+1),0,16)
            ITH = IBITS(IW(KHTDI+1),16,8)
            IRE = IBITS(IW(KHTDI+1),24,8)
            WRITE(LOUTIO,510)I,IPH,ITH,IRE,(IW(KHTDI+J),J=JHTDED,LHTDIA)
            KHTDI = KHTDI + LHTDIA
   20    CONTINUE
      ENDIF
      RETURN
  500 FORMAT (/1X,'+++HCFORA+++ HTDI McHcTowerDigitising ',I5/
     +'      row#       Phi     Theta    Region    Estack')
  510 FORMAT(10I10)
      END
