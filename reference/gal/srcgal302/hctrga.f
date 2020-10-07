      SUBROUTINE HCTRGA
C --------------------------------------------
C
C! Implements the module FORM TOWER TRIGGER SIGNAL
C!
C!
C!      Author     : G.Zito 86/05/21
C!      Modified by: F.Ranjard 89/02/10
C!
C!      Input bank  : HTHT  McHcStoreys
C!      Output bank : HTTR  McHcTowerTrigSignal
C!
C!
C!   -Called by : HCDIGI
C!   -Calls     : none
C -----------------------------------------
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
      PARAMETER(JHTTED=1,LHTTRA=1)
      PARAMETER(JHTHSA=1,JHTHPT=2,JHTHTT=3,JHTHED=4,LHTHTA=4)
      PARAMETER (LPHCTR= 12,LPHCTF=24,LHCRT=62)
      COMMON /HCTRIG/ NHCEPR,NHCBPR,NHCETR,NHCBTR , IHCTRG(LHCRT),NHCBTS
     +(LHCRT),NHCETS(LHCRT) , IYHCFI(LPHCTF),IXHCFI(LPHCTR),IXHCSE
     +(LPHCTR) , MHCETR,MHCBTR

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
      JHTHT = IW(NAHTHT)
      NHTHT = LROWS (JHTHT)
      IF (NHTHT.EQ.0) RETURN
      JHTTR = IW(NAHTTR)
C
C          Scan the table McHcStoreys adding to trigger signal
C
      KHTHT = JHTHT + LMHLEN
      N1 = NHCETR
      N2 = N1 + NHCBTR
      N3 = N1 + NHCBTR/2
      DO 10 I = 1, NHTHT
         NTHS = IW(KHTHT+3)
         NFIS = IW(KHTHT+2)
C
C          Trigger signals stored following Putzer description
C
         IF (NTHS.LE.N1) THEN
            IPOS = (NFIS-1)*NHCETR + NTHS
         ELSE IF (NTHS.LE.N3) THEN
            IPOS = MHCETR + (NFIS-1)*(NHCBTR/2) + NTHS
         ELSE IF (NTHS.LE.N2) THEN
            IPOS = MHCETR + (NHCBPR+NFIS-2)*(NHCBTR/2)+NTHS
         ELSE
            IPOS = MHCETR + MHCBTR + (NFIS-1)*NHCETR + NTHS
         ENDIF
         KHTTR = KROW (JHTTR,IPOS)
         IW(KHTTR+1) = IW(KHTTR+1) + NINT (RW(KHTHT+4)*1000.)
         KHTHT = KHTHT + LHTHTA
   10 CONTINUE
C
C
      IF(FHCDB1)THEN
         WRITE(LOUTIO,500)
         KHTTR = JHTTR + LMHLEN
         DO 20 I= 1,IW(JHTTR)-LMHLEN,12
            WRITE(LOUTIO,510) I,I+11,(IW(KHTTR+K),K=1,12)
   20    KHTTR = KHTTR + 12
      ENDIF
C
      RETURN
  500 FORMAT (/1X,'+++HCTRGA+++ HTTR  McHcTrigSignal (Mev)')
  510 FORMAT (1X,I3,'-',I3,12I9)
      END
