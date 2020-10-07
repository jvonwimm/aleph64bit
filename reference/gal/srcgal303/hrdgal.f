      SUBROUTINE HRDGAL
C------------------------------------------------------------------
C! Get HCAL bank used only in Galeph and store the content in the
C                     HCCONG common
C- F.Ranjard - 880202
C- Modified by L.Silvestris     31/08/90
C              F.Ranjard        11/10/91
C              get HCOS and HGEA depending on the GEANT version number
C              use MDARD to get banks from data base
C- Called by : HCIRUN
C!          Calls     : ALTELL from Alephlib
C!                      UCOPY,HISPRE from Cernlib
C!
C -------------------------------------------------
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C
      PARAMETER(JHTDTA=1,JHTDED=2,LHTDIA=2)
      PARAMETER(JHTHSA=1,JHTHPT=2,JHTHTT=3,JHTHED=4,LHTHTA=4)
      PARAMETER(JHWHTA=1,LHWHTA=1)
      PARAMETER(JHWDCA=1,LHWDIA=1)
      PARAMETER(JHTTED=1,LHTTRA=1)
      PARAMETER(JHWTNO=1,LHWTRA=1)
      PARAMETER(JHCTID=1,JHCTVR=2,JHCTEP=4,JHCTBP=5,JHCTET=6,JHCTBT=7,
     +          JHCTTM=8,LHCTGA=193)
      PARAMETER(JHSBID=1,JHSBVR=2,JHSBEN=4,JHSBSN=5,JHSBMT=6,JHSBXS=7,
     +          JHSBRS=10,JHSBPM=13,JHSBHB=14,LHSBAA=14)
      PARAMETER(LHSBXS=3,LHSBRS=3)
      PARAMETER(JHSEID=1,JHSEVR=2,JHSEFN=4,JHSESN=5,JHSESL=6,JHSEMT=7,
     +          JHSEXS=8,JHSERS=11,JHSEPM=14,JHSEHE=15,LHSECA=15)
      PARAMETER(LHSEXS=3,LHSERS=3)
      PARAMETER(JHTRID=1,JHTRVR=2,JHTRNB=4,JHTRLE=5,JHTRUE=6,JHTRSR=7,
     +          LHTREA=106)
      PARAMETER(JHCOTA=1,JHCOTS=2,JHCOOL=3,JHCOMT=4,JHCOEA=5,JHCOEF=6,
     +          JHCOEB=7,JHCOCF=8,JHCOIF=9,JHCOMA=10,LHCOSA=10)
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /HCCONG/ HCTUAC,HCSTDT,HCADCE,HADCMX,HCPFAC,
     &                HCTINS,RHBAMN,ZHECMN,ZHBAMX,HSTREA,HSTUST,
     +                NHCFSS,HCFSS1,HCFSS2,HCFLSS(100)
     &               ,HTLEMX,HCTEFF(3),HPINDU
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
C ----------------------------------------------------------------------
C - get GEANT version number
       IGEANT = INT (GVERSN*100.)
C - get machine row number : CRAY=2, .NOT.CRAY=1
       MAC = 1
C
       JHCOS = MDARD (IW,LRDBIO,'HCOS',IGEANT)
       IF (JHCOS.EQ.0) THEN
C      try to get HCOS,NR=1 (GEANT 313 version)
          JHCOS = MDARD (IW,LRDBIO,'HCOS',1)
       ENDIF
       JHGEA = MDARD (IW,LRDBIO,'HGEA',IGEANT)
       IF (JHGEA.NE.0) THEN
          IF (MAC.GT.LROWS(JHGEA)) MAC = LROWS(JHGEA)
C      in case the row corresponding to the machine is not the 1st one
C      copy the row # MAC into the 1st one
          IF (MAC.GT.1) THEN
             KHGEA = KROW(JHGEA,MAC)
             CALL UCOPY (RW(KHGEA+1),RW(JHGEA+LMHLEN+1),IW(JHGEA+1))
          ENDIF
       ENDIF
C
       JHTRE = MDARD (IW,LRDBIO,'HTRE',1)
C
       IF (JHCOS.NE.0) THEN
          IF (MAC.GT.LROWS(JHCOS)) MAC = LROWS(JHCOS)
          HCTUAC   = RTABL(JHCOS,MAC,JHCOTA)
          HSTREA   = RTABL(JHCOS,MAC,JHCOTS)
          HCSTDT   = RTABL(JHCOS,MAC,JHCOOL)
          HTLEMX   = RTABL(JHCOS,MAC,JHCOMT)
          HCTEFF(1)= RTABL(JHCOS,MAC,JHCOEA)
          HCTEFF(2)= RTABL(JHCOS,MAC,JHCOEA+1)
          HCTEFF(3)= RTABL(JHCOS,MAC,JHCOEA+2)
          HCADCE   = RTABL(JHCOS,MAC,JHCOCF)
          HPINDU   = RTABL(JHCOS,MAC,JHCOIF)
          HADCMX   = RTABL(JHCOS,MAC,JHCOMA)
      ELSE
         GOTO 998
      ENDIF
C
      IF(JHTRE.NE.0) THEN
         NHCFSS = ITABL(JHTRE,1,JHTRNB)
         HCFSS1 = RTABL(JHTRE,1,JHTRLE)
         HCFSS2 = RTABL(JHTRE,1,JHTRUE)
C
         CALL UCOPY(RW(JHTRE+LMHLEN+JHTRSR),HCFLSS(1),NHCFSS)
         CALL HISPRE(HCFLSS,NHCFSS)
C
      ELSE
         GOTO 998
      ENDIF
C
C  Get geometry D.B. variables used in Galeph
C
      CALL HGETDB
      RETURN
998   CONTINUE
      CALL ALTELL('HRDGAL: missing DB banks ', 0, 'STOP')
      END
