      SUBROUTINE AGLCAL
C-----------------------------------------------------
C!    Implement Luminosity Calorimeter geometry
C!
C   Author : B. Bloch-Devaux                   4 April 1985
C   Modified : P. H. Hansen  (Data Base use)    29 Novem 1987
C     Modified:   H. Meinhard       26-Jul-1990  (2)
C        for the implementation of SATR dowels
C   Modified : B.Bloch-Devaux 28 January,1991  for
C        -inox screws in front plate
C        -lead cutoff from inner part of stacks
C   Modified : B.Bloch-Devaux 15 October,1991  for
C        -implement SATR passive parts only if SATR there
C        -take alignment into account to take care of new 92 positions
C
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSMIXT,GSTMED,GSVOLU,GSPOS,GSPOSP      from  GEANT3
C.  -Calls AGMIX,AGDIMP                           from this HLB
C.  -Calls ALTELL                                 from ALEPHLIB
C.
C. -Stores extra material needed
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below 'ECEA'('ECEB') level for LCAL part
C.
C-----------------------------------------------------------
      EXTERNAL GHSIGM,JHOCHA
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)
      PARAMETER(LSENV=30)
      PARAMETER (LIMVOL=17)
C
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)
     2      , NAGIMP,LAGIMP(3,LIMVOL)
C
       COMMON /WRKSPC/ WSPACE(88320)
      PARAMETER (LPTAB=50)
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))
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
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
      COMMON /LCNAMC/ NALTHT, NALTDI, NALWHT, NALWDI, NALTTR, NALWTR,
     *                NALWHI, NALSHI, NALCWS, NALCAL, NALLAY, NALMTY,
     *                NALALI, NALSCO, NALSLO, NALWRG, NALCEL, NALCSH,
     *                NALDRE, NALCCA, NALCPG, NALSHO
      PARAMETER(JLALID=1,JLALVR=2,JLALDX=4,JLALDR=7,JLALDM=10,
     +          JLALDT=13,JLALDP=14,JLALLS=15,LLALIA=15)
      PARAMETER(JLCEID=1,JLCEVR=2,JLCEAM=4,JLCETH=5,JLCESN=8,JLCETN=11,
     +          JLCEWN=14,JLCECN=17,JLCEGV=18,JLCETR=19,LLCELA=19)
      PARAMETER(JLSHID=1,JLSHVR=2,JLSHP1=4,JLSHP2=5,JLSHP3=6,JLSHP4=7,
     +          JLSHP5=8,JLSHP6=9,JLSHP7=10,JLSHP8=11,JLSHD1=12,
     +          JLSHD2=13,JLSHD3=14,JLSHD4=15,JLSHD5=16,JLSHEC=17,
     +          JLSHEM=18,JLSHET=19,JLSHSC=20,JLSHSS=21,
     +          JLSHZ1=22,JLSHZ2=23,JLSHR1=24,JLSHR2=25,LLCSHA=25)
      PARAMETER(JLCAID=1,JLCAVR=2,JLCACN=4,JLCANS=5,JLCAZD=6,JLCARI=7,
     +          JLCARO=8,JLCAZL=9,JLCATI=10,JLCATO=11,JLCAGI=12,
     +          JLCAGO=13,JLCAGB=14,JLCASD=15,JLCASL=16,JLCASP=17,
     +          JLCASR=18,JLCASM=19,JLCASX=20,JLCABI=21,JLCABS=22,
     +          JLCABX=23,JLCAB1=24,JLCAH1=25,JLCAM1=26,JLCAB2=27,
     +          JLCAH2=28,JLCAM2=29,LLCALA=29)
      PARAMETER(JLDRID=1,JLDRVR=2,JLDRDT=4,JLDRD2=5,JLDRDS=6,JLDRXL=7,
     +          JLDRXH=11,JLDRYS=15,JLDRY0=19,JLDRDY=20,JLDRY5=21,
     +          JLDRDW=22,LLDREA=22)
      PARAMETER(JLLAID=1,JLLAVR=2,JLLALP=4,JLLAPS=5,JLLAPO=6,JLLALM=7)
      PARAMETER( LLLAYA=7 )
      PARAMETER(JLMTID=1,JLMTVR=2,JLMTMT=4,JLMTNS=5,JLMTNL=6,JLMTNP=9,
     +          JLMTNW=10,JLMTN2=11,JLMTNR=12,JLMTNT=13,JLMTFL=14,
     +          JLMTFR=15,JLMTBL=16,JLMTBR=17,JLMTST=18,JLMTRA=21,
     +          JLMTDT=24,JLMTCC=25,JLMTBA=26,JLMTBS=27,JLMTRD=28,
     +          JLMTXD=29,JLMTYD=30,LLMTYA=30)
      PARAMETER(JLRWID=1,JLRWVR=2,JLRWLR=4,JLRWLC=5,JLRWNC=6,JLRWLA=7,
     +          JLRWLM=23,LLRWGA=23)
      PARAMETER(JLSCID=1,JLSCVR=2,JLSCCN=4,JLSCNS=5,JLSCRP=6,JLSCRR=9,
     +          JLSCSG=12,JLSCLC=13,LLSCOA=13)
      PARAMETER(JLSLID=1,JLSLVR=2,JLSLSN=4,JLSLXS=5,JLSLRS=8,JLSLXM=11,
     +          JLSLTM=14,JLSLPM=15,JLSLLM=16,JLSLLS=17,LLSLOA=17)
      PARAMETER(JLWRID=1,JLWRVR=2,JLWRLW=4,JLWRTW=5,JLWRD2=6,JLWRXL=7,
     +          JLWRYL=8,JLWRYH=9,JLWRLM=10,LLWRGA=10)
      PARAMETER(JLCCID=1,JLCCVR=2,JLCCDI=4,LLCCAA=19)
      PARAMETER(JLCPID=1,JLCPVR=2,JLCPLP=4,JLCPPS=5,JLCPPO=6,LLCPGA=6)
      PARAMETER(JSATID=1,JSATVR=2,JSATCN=4,JSATSU=5,JSATLA=6,JSATSC=7,
     +          JSATWI=8,JSATIR=9,JSATOR=10,JSATIM=11,JSATOM=12,
     +          JSATWD=13,JSATLD=14,JSATLO=15,JSATGL=16,JSATET=17,
     +          JSATBT=18,JSATOT=19,JSATGD=20,JSATMT=21,JSATDL=29,
     +          JSATDR=32,JSATCR=35,JSATCA=36,JSATTD=37,JSATPR=38,
     +          JSATPP=39,JSATPD=40,JSATPA=41,JSATBN=45,JSATBX=46,
     +          JSATBY=47,JSATBZ=48,JSATBR=49,JSATMP=50,JSATFR=56,
     +          JSATFD=57,LSATRA=57)
      CHARACTER*4 ECL(2),ECN(2)
      REAL AXCO(3),ZXCO(3),WXCO(3)
      PARAMETER (LTAB1=5  ,LTAB2=4 ,LMED=7 )
      DIMENSION TAB1(LTAB1,LMED)
      DIMENSION PHDOW(6)
      DATA ECL/'LCEA','LCEB'/
      DATA ECN/'ECEA','ECEB'/
C
C ===================================================================
C     Medium are defined as:
C    1-LC calo volume                      material: IMLRG
C    2-LC wall volume                      material: IMLFR
C    3-LC passive gas mixture              material: IMAXC
C    4-LC back plate volume                material: IMLBP
C    5-LC  stack 1+2 volume                material: light average
C    6-LC stack3 volume                    material: dense average
C    7-SATR dowel feet, brass              material: IMBRA (=22)
C
C   For each medium the two following rows are filled:
C     TMXFD , DMXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     2 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     3 20.    ,  0.5  , 0.1  , 0.02 , 0.05,
     4 20.    ,  0.5  , 0.1  , 0.1  , 0.80 ,
     5 20.    ,  0.5  , 0.1  , 0.1  , 0.80 ,
     6 20.    ,  0.5  , 0.1  , 0.1  , 0.50 ,
     7 20.    ,  0.5  , 0.1  , 0.1  , 0.3   /
C
C        A    , Z    , DENS  , RADL
      DATA NXCO,AXCO,ZXCO,WXCO,DXCO
     1   /  3, 12.011 , 15.999 , 131.3,
     2          6.0   ,  8.0   ,  54.0,
     3          0.0211,  0.0562,   0.9227,  0.005107 /
C   LCAL MODULE
      DATA ELAY ,EPB1 ,EPB2
     1   / .585 ,.280 ,.560  /
C Thicknesses of wire layer,lead1,lead2
C number of SATR dowels (if not already in data base)
      DATA NDOW/6/
C phi position of SATR dowels (if not already in data base)
      DATA PHDOW/67.5,112.5,172.5,247.5,292.5,352.5/
C radius,depth of SATR dowel feet (if not already in data base)
      DATA RFDOW/1.8/, DFDOW/1.0/
C ---------------------------------------------------------------------
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
      IMLFR=9
      IMLRG=15
      IMLBP=10
      IMMYL=26
      IMCOP=11
      IMGRA=6
      IMLEA=13
      IMG10=27
      IMBRA=22
      IAGMAT=IAGMAT+1
      IMAXC=IAGMAT
      CALL GSMIXT(IAGMAT,'LCAL XE-CO2$',AXCO,ZXCO,DXCO,NXCO,WXCO)
C
C   To compute the stack properties , we use the elementary components:
C    aluminum:#IMLFR                                    Mylar:#IMMYL
C    Copper  :#IMCOP         Graphite:#IMGRA
C    Gas mix :#IMAXC from Xenon and CO2              G10 PCB :#IMG10
C
      JTAB(1)=IMMYL
      JTAB(2)=IMCOP
      JTAB(3)=IMG10
      JTAB(4)=IMGRA
      JTAB(5)=IMLFR
      JTAB(6)=IMAXC
C
C The thicknesses defined below correspond to the canonical
C pile up of a layer in the LCAL ( see tranparencies from J.D.Hansen
C in ALEPH Plenary Meeting 6-7 march 1986)
C last mod 15-01-1988 after pile-up of a module
      PTAB(11)=0.036
      PTAB(12)=0.0050
      PTAB(13)=0.112
      PTAB(14)=0.002
      PTAB(15)=0.142
      PTAB(16)=0.288
      NAG=6
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DLAY)
      IAGMAT=IAGMAT+1
      IMCAY=IAGMAT
      CALL GSMIXT(IMCAY,'LCAL MODU WIRE LAYER$',PTAB(21),PTAB(31),DLAY,
     1  NAG,     PTAB(41))
      IMINX = 20
      JTAB(1) = IMLFR
      JTAB(2) = IMINX
C  Region on circumference of Front plate has a SCREW (inox)
C  each 3.2 cm ( 6mm in diam, 3.7 cm deep) surrounded by Al
C  Following numbers are respectively the volumes corresponding
C  to aluminum and stainless steel (that is one inox screw in a 3.2cm
C  Aluminum cell)
      PTAB(11) = 6.058
      PTAB(12) = 1.046
      NAG = 2
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      IAGMAT = IAGMAT +1
      IMATI = IAGMAT
      CALL GSMIXT(IAGMAT,'LCAL screws front plate',PTAB(21),PTAB(31),
     $            DX,NAG,PTAB(41))
C----------------------------------------------------------------------
C
C Get geometry bank pointers
      KLCAL = IW(NALCAL)
      KLMTY = IW(NALMTY)
      KLSCO = IW(NALSCO)
      KLALI = IW(NALALI)
      IF(KLCAL.EQ.0 .OR. KLMTY.EQ.0 .OR. KLSCO.EQ.0 .OR. KLALI.EQ.0)THEN
        CALL ALTELL('MISSING GEOMETRY BANKS',0,'RETURN')
        GOTO 999
      ENDIF
C  -- this applies only if SATR is there
      IF (IGEOJO(6).EQ.1) THEN
         JSATR = IW(NAMIND('SATR'))
      IF(JSATR.EQ.0) CALL ALTELL('MISSING SA GEOMETRY BANKS',0,'RETURN')
      ENDIF
C
      ISV=IDETJO(5)
C
C     Define 'LCEA' ( 'LCEB' ) as Tube and place in ECEA ( ECEB)
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'LC CALO VOLUME$',IMLRG,0,IAGFLD,ALFIEL,
     1         TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
C Look for maximum extension of the 4 modules
      PTAB(1) = AGLIMR(9)
      RMAX0 = RTABL(KLCAL,1,JLCARO)
      RMAX  = RMAX0
      DO 9 IX = 1,4
         RMAX = MAX ( RMAX,RMAX0+0.5*RTABL(KLCAL,1,JLCAGB)+ABS(
     $                RTABL(KLALI,IX,JLALDX)))
         RMAX = MAX ( RMAX,RMAX0+ABS(RTABL(KLALI,IX,JLALDX+1)))
  9   CONTINUE
      PTAB(2) = RMAX
C   Take into account that the 2 halves are not contiguous
      PTAB(3) = 0.5*RTABL(KLCAL,1,JLCAZL)
      IMED=IAGMED
      CALL GSVOLU('LCEA','TUBE',IMED,PTAB,3,IVOL)
      CALL GSVOLU('LCEB','TUBE',IMED,PTAB,3,IVOL)
C
      NSUBC = ITABL(KLCAL,1,JLCANS)
      DO 10 I=1,NSUBC
        X=RTABL(KLSCO,I,JLSCRP)
        Y=RTABL(KLSCO,I,JLSCRP+1)
        Z=ABS(RTABL(KLSCO,I,JLSCRP+2))+PTAB(3)
        CALL GSPOS(ECL(I),1,ECN(I),X,Y,Z,0,'ONLY')
 10   CONTINUE
C
C     Define 'LCMO' as tubs of aluminum and place in LCEA (LCEB)
C
      IAGMED = IAGMED+1
      IAGAL=IAGMED
      CALL GSTMED(IAGAL,'LC WALLS VOLUME $',IMLFR,0,IAGFLD,ALFIEL,
     1         TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
C
      IAGMED=IAGMED+1
      IAGXC=IAGMED
      CALL GSTMED(IAGXC,'LC PASSIVE GAS MIX$',IMAXC,0,IAGFLD,ALFIEL,
     1         TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      PTAB(1) = RTABL(KLCAL,1,JLCARI)
      PTAB(2) = RTABL(KLCAL,1,JLCARO)
      PTAB(3) = 0.5*RTABL(KLCAL,1,JLCAZL)
      PTAB(4)=-D90
      PTAB(5)=D90
      CALL GSVOLU('LCMO','TUBS',IAGAL,PTAB,5,IVOL)
      X = 0.5*RTABL(KLCAL,1,JLCAGB)+RTABL(KLALI,4,JLALDX)
      Y = RTABL(KLALI,4,JLALDX+1)
      Z=0.
      CALL GSPOS('LCMO',1,'LCEA',X,Y,Z,0,'ONLY')
      ICR=3
      X = 0.5*RTABL(KLCAL,1,JLCAGB)-RTABL(KLALI,3,JLALDX)
      Y = RTABL(KLALI,3,JLALDX+1)
      CALL GSPOS('LCMO',2,'LCEA',-X,Y,Z,ICR,'ONLY')
      X = 0.5*RTABL(KLCAL,1,JLCAGB)-RTABL(KLALI,1,JLALDX)
      Y = RTABL(KLALI,1,JLALDX+1)
      CALL GSPOS('LCMO',1,'LCEB',-X,Y,Z,ICR,'ONLY')
      X = 0.5*RTABL(KLCAL,1,JLCAGB)+RTABL(KLALI,2,JLALDX)
      Y = RTABL(KLALI,2,JLALDX+1)
      CALL GSPOS('LCMO',2,'LCEB',X,Y,Z,0,'ONLY')
C
C   Inox screws region on inner circumference and vertical sides
C   inner circumference: small area of aluminum (LSFR) with 9 inox
C                        screws (LSCR)
C   vertical sides: 2 bands with average inox+alu (LSBR)
C
      IF (LCOLS(KLCAL).GE.LLCALA) THEN
         IAGMED = IAGMED +1
         CALL GSTMED(IAGMED,'LCAL INOX SCREWS AREA',IMATI,0,IAGFLD,
     $   ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
         PTAB(1) = 0.5*RTABL(KLCAL,1,JLCASD)
         PTAB(2) = 0.5*(RTABL(KLCAL,1,JLCABS)-RTABL(KLCAL,1,JLCABI))
         PTAB(3) = 0.5 *RTABL(KLCAL,1,JLCASL)
         CALL GSVOLU('LSBR','BOX ',IAGMED,PTAB,3,IVOL)
         X = RTABL(KLCAL,1,JLCABX)
         Y = 0.5*(RTABL(KLCAL,1,JLCABI)+RTABL(KLCAL,1,JLCABS))
         Z = -0.5*RTABL(KLCAL,1,JLCAZL)+PTAB(3)
         CALL GSPOS('LSBR',1,'LCMO',X,Y,Z,0,'ONLY')
         CALL GSPOS('LSBR',2,'LCMO',X,-Y,Z,0,'ONLY')
C   Define a small circle of aluminum to receive the 9 screws
         PTAB(1) = RTABL(KLCAL,1,JLCASR)-0.5*RTABL(KLCAL,1,JLCASD)
         PTAB(2) = RTABL(KLCAL,1,JLCASR)+0.5*RTABL(KLCAL,1,JLCASD)
         PTAB(3) = 0.5 *RTABL(KLCAL,1,JLCASL)
         PTAB(4) = RTABL(KLCAL,1,JLCASM)*RADEG
         PTAB(5) = RTABL(KLCAL,1,JLCASX)*RADEG
         CALL GSVOLU('LSFR','TUBS',IAGAL,PTAB,5,IVOL)
         Z = -0.5*RTABL(KLCAL,1,JLCAZL)+PTAB(3)
         CALL GSPOS('LSFR',1,'LCMO',0.,0.,Z,0,'ONLY')
C  Inox screws medium and volume
         IAGMED = IAGMED +1
         CALL GSTMED(IAGMED,'LCAL INOX SCREWS MEDI',IMINX,0,IAGFLD,
     $   ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
         PTAB(1) = 0.
         PTAB(2) = 0.5*RTABL(KLCAL,1,JLCASD)
         PTAB(3) = 0.5 *RTABL(KLCAL,1,JLCASL)
         CALL GSVOLU('LSCR','TUBE',IAGMED,PTAB,3,IVOL)
         DPHI = RTABL(KLCAL,1,JLCASP)
         PHI = -4.*DPHI
         R0 = RTABL(KLCAL,1,JLCASR)
         Z = 0.
         DO 20 ISC = 1,9
            X=R0*COS(PHI)
            Y=R0*SIN(PHI)
            CALL GSPOS('LSCR',ISC,'LSFR',X,Y,Z,0,'ONLY')
            PHI = PHI + DPHI
  20     CONTINUE
      ENDIF
C  -- this applies only if SATR is there
      IF (IGEOJO(6).NE.1) GO TO   7
C
C  Define a cylinder for brass dowel feet and place three of them into
C  LCMO
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'SA dowel brass$',IMBRA,0,IAGFLD,ALFIEL,
     1        TAB1(1,7),TAB1(2,7),TAB1(3,7),TAB1(4,7),TAB1(5,7),0,0)
      PTAB(1) = 0.
      IF (LCOLS(JSATR) .GE. JSATBR) THEN
        PTAB(2) = RTABL(JSATR,1,JSATFR)
        PTAB(3) = 0.5*RTABL(JSATR,1,JSATFD)
        NDOWL = ITABL(JSATR,1,JSATPD)
      ELSE
        PTAB(2) = RFDOW
        PTAB(3) = 0.5*DFDOW
        NDOWL = NDOW
      ENDIF
      CALL GSVOLU('LCDF','TUBE',IAGMED,PTAB,3,IVOL)
      Z = - 0.5*RTABL(KLCAL,1,JLCAZL) + PTAB(3)
      I = 0
      DO 6 IDOW = 1, NDOWL
        IF (LCOLS(JSATR) .GE. JSATBR) THEN
          ANG = RTABL(JSATR,1,JSATMP-1+IDOW) * DEGRA
        ELSE
          ANG = PHDOW(IDOW) * DEGRA
        ENDIF
        X = RTABL(JSATR,1,JSATPP) * COS(ANG)
        Y = RTABL(JSATR,1,JSATPP) * SIN(ANG)
        IF (X .GT. 0.) THEN
          I = I + 1
          CALL GSPOS('LCDF',I,'LCMO',X,Y,Z,0,'ONLY')
        ENDIF
    6 CONTINUE
C
    7 CONTINUE
C     Fill LCMO with inner volume of gas and iron back plate
C   The inner gas volume leaves out the front alu plate and the
C    inner and outer walls
C    The side walls will be placed later
C
      PTAB(1)=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)
      PTAB(2)=RTABL(KLCAL,1,JLCARO)-RTABL(KLCAL,1,JLCATO)
      PTAB(3)=0.5*(RTABL(KLCAL,1,JLCAZL)
     &            -RTABL(KLMTY,1,JLMTFL)-RTABL(KLMTY,1,JLMTBL))
      PTAB(4)=-D90
      PTAB(5)=D90
      ZLCIN=PTAB(3)
      CALL GSVOLU('LCIN','TUBS',IAGXC,PTAB,5,IVOL)
      DIFZ = 0.5*(RTABL(KLMTY,1,JLMTFL)-RTABL(KLMTY,1,JLMTBL))
      CALL GSPOS ('LCIN',1,'LCMO',0.,0.,DIFZ,0.,'ONLY')
C
C     Place iron back plate in LCMO
C
      PTAB(1) = RTABL(KLCAL,1,JLCARI)
      PTAB(2) = RTABL(KLCAL,1,JLCARO)
      PTAB(3)=0.5*RTABL(KLMTY,1,JLMTBL)
      PTAB(4)=-D90
      PTAB(5)=D90
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'LC BACK PLATE VOLUME$',IMLBP,0,IAGFLD,ALFIEL,
     1         TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)
      CALL GSVOLU('LCBP','TUBS',IAGMED,PTAB,5,IVOL)
      ZBP = 0.5*RTABL(KLCAL,1,JLCAZL)-PTAB(3)
      CALL GSPOS ('LCBP',1,'LCMO',0.,0.,ZBP,0,'ONLY')
C
C   PLACE ALU GAPS ALONG SIDES BUT OUTSIDE STACKS IN LCIN
C
      PTAB(1)=RTABL(KLCAL,1,JLCATI)*0.5
      PTAB(2)=RTABL(KLCAL,1,JLCAGI)*0.5
      PTAB(3)=ZLCIN
      CALL GSVOLU('LCSI','BOX ',IAGAL,PTAB,3,IVOL)
      X=PTAB(1)
      Y=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)+PTAB(2)
      Z=0.
      CALL GSPOS('LCSI',1,'LCIN',X,Y,Z,0,'ONLY')
      CALL GSPOS('LCSI',2,'LCIN',X,-Y,Z,0,'ONLY')
      PTAB(2)=RTABL(KLCAL,1,JLCAGO)*0.5
      CALL GSVOLU('LCSO','BOX ',IAGAL,PTAB,3,IVOL)
      Y=RTABL(KLCAL,1,JLCARO)-RTABL(KLCAL,1,JLCATO)-PTAB(2)
      CALL GSPOS('LCSO',1,'LCIN',X,Y,Z,0,'ONLY')
      CALL GSPOS('LCSO',2,'LCIN',X,-Y,Z,0,'ONLY')
C
C    PLACE SIDE INNER WALLS,AIR GAPS,DOWELS IN PIECES
C    SIZE BEEING DEFINED AT POSITIONING TIME
C
      CALL GSVOLU('LCSW','BOX ',IAGAL,PTAB,0,IVOL)
      CALL GSVOLU('LCSG','BOX ',IAGXC,PTAB,0,IVOL)
      CALL GSVOLU('LCDL','TUBE',IAGAL,PTAB,0,IVOL)
C
C   Define the volumes of Lead which should be replaced by gas near
C   the inner circumference.This has to be done in each stack LC12 and
C   LCS3.Define generic volumes and adjust dimensions at positionning
C   time.Volumes to be replaced are simulated by two sets of triangles
C   which are approximated in GEANT by shape TRAP
      IF (LCOLS(KLCAL).GE.LLCALA) THEN
C   Store fixed dimensions of the two types of TRAP in working space
C   WSPACE(50-60) and (70-80).Only WSPACE(50) and WSPACE(70) vary with
C   stack.
         WSPACE(50)= 0.
         WSPACE(51)= 0.
         WSPACE(52)= 0.
         WSPACE(53)= 0.5*RTABL(KLCAL,1,JLCAH1)
         WSPACE(54)= 0.5*RTABL(KLCAL,1,JLCAB1)
         WSPACE(55)= 0.0001
         WSPACE(56)= RTABL(KLCAL,1,JLCAM1)*RADEG
         WSPACE(57)= WSPACE(53)
         WSPACE(58)= WSPACE(54)
         WSPACE(59)= WSPACE(55)
         WSPACE(60)= WSPACE(56)
         WSPACE(70)= 0.
         WSPACE(71)= 0.
         WSPACE(72)= 0.
         WSPACE(73)= 0.5*RTABL(KLCAL,1,JLCAH2)
         WSPACE(74)= 0.5*RTABL(KLCAL,1,JLCAB2)
         WSPACE(75)= 0.0001
         WSPACE(76)= RTABL(KLCAL,1,JLCAM2)*RADEG
         WSPACE(77)= WSPACE(73)
         WSPACE(78)= WSPACE(74)
         WSPACE(79)= WSPACE(75)
         WSPACE(80)= WSPACE(76)
         CALL GSVOLU('LEAD','TRAP',IAGXC,WSPACE,0,IVOL)
      ENDIF
C
C    Place first and second stacks  as a whole
C
      IAGMAT=IAGMAT+1
C
      JTAB(1)=IMCAY
      JTAB(2)=IMLEA
C
C  Average medium for stacks 1 and 2 excluding the first ALU sheet
C  used instead of lead
      PTAB(11)=ELAY
      PTAB(12)=EPB1
      NAG=2
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DS12)
      CALL GSMIXT(IAGMAT,'LCAL LIGHT AVERAGE $',PTAB(21),PTAB(31),DS12,
     1   NAG,  PTAB(41))
      IAGMED=IAGMED+1
C
C      Define sensitive volume flag as defined if SET 'LCAL' is selected
C
      CALL GSTMED(IAGMED,'LC STACK1+2 VOLUME$',IAGMAT,ISV,IAGFLD,ALFIEL,
     1         TAB1(1,5),TAB1(2,5),TAB1(3,5),TAB1(4,5),TAB1(5,5),0,0)
      PTAB(1)=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)
     &       +RTABL(KLCAL,1,JLCAGI)
      PTAB(2)=RTABL(KLCAL,1,JLCARO)-RTABL(KLCAL,1,JLCATO)
     &       -RTABL(KLCAL,1,JLCAGO)
      PTAB(3)=0.5*(RTABL(KLMTY,1,JLMTST)+RTABL(KLMTY,1,JLMTST+1))
      R1STA=PTAB(1)
      R2STA=PTAB(2)
      YSTA=R2STA-R1STA
      PTAB(4)=-D90
      PTAB(5)=D90
      CALL GSVOLU('LC12','TUBS',IAGMED,PTAB,5,IVOL)
C
C   Define where to find slot number
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1)=JHOCHA('LC12')
      IAGSEN(IAGSLV,2)=4
      Z=PTAB(3)-ZLCIN
      CALL GSPOS('LC12',1,'LCIN',0.,0.,Z,0,'ONLY')
C
C   Place side wall ,side gas gap and dowel piece in LC12
C
      PTAB(1)=0.5*RTABL(KLCAL,1,JLCATI)
      PTAB(2)=0.5*YSTA
      X=PTAB(1)
      Y=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)
     & +RTABL(KLCAL,1,JLCAGI)+PTAB(2)
      CALL GSPOSP('LCSW',1,'LC12',X ,Y ,0.,0,'ONLY',PTAB,3)
      CALL GSPOSP('LCSW',2,'LC12',X ,-Y,0.,0,'ONLY',PTAB,3)
      PTAB(1)=0.5*RTABL(KLCAL,1,JLCAGI)
      X=RTABL(KLCAL,1,JLCATI)+PTAB(1)
      CALL GSPOSP('LCSG',1,'LC12',X ,Y ,0.,0,'ONLY',PTAB,3)
      CALL GSPOSP('LCSG',2,'LC12',X ,-Y,0.,0,'ONLY',PTAB,3)
      PTAB(1)=0.
      PTAB(2)=RTABL(KLMTY,1,JLMTRD)
      X=RTABL(KLMTY,1,JLMTXD)
      Y=RTABL(KLMTY,1,JLMTYD)
      CALL GSPOSP('LCDL',1,'LC12',X ,Y ,0.,0,'ONLY',PTAB,3)
      CALL GSPOSP('LCDL',2,'LC12',X ,-Y,0.,0,'ONLY',PTAB,3)
C Now replace some stack by gas.Depth is stored as PTAB(3) for LC12
      IF (LCOLS(KLCAL).GE.LLCALA) THEN
         WSPACE(50)=PTAB(3)
         WSPACE(70)=PTAB(3)
         X=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)
     &     +RTABL(KLCAL,1,JLCAGI)-0.5*WSPACE(54)
         Y=-WSPACE(53)
         Z=0.
         CALL GSPOSP('LEAD',1,'LC12',X,Y,Z,0,'ONLY',WSPACE(50),11)
         IAG1=IAGROT+1
         CALL GSROTM(IAG1,D90,0.,D90,D270,D180,0.)
         CALL GSPOSP('LEAD',2,'LC12',X,-Y,Z,IAG1,'ONLY',WSPACE(50),11)
         X=RTABL(KLCAL,1,JLCAGI)+RTABL(KLCAL,1,JLCATI)+WSPACE(73)
         Y=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)
     &     +RTABL(KLCAL,1,JLCAGI)
         IAG2=IAGROT+2
         CALL GSROTM(IAG2,D90,D270,D90,0.,0.,0.)
         CALL GSPOSP('LEAD',3,'LC12',X,Y,Z,IAG2,'ONLY',WSPACE(70),11)
         IAG3=IAGROT+3
         CALL GSROTM(IAG3,D90,D90,D90,0.,D180,0.)
         CALL GSPOSP('LEAD',4,'LC12',X,-Y,Z,IAG3,'ONLY',WSPACE(70),11)
         IAGROT = IAGROT+3
      ENDIF
C
C   PLACE THIRD  STACK
C
      IAGMAT=IAGMAT+1
C
C  Average medium for stacks 3  in LCAL
C
      JTAB(1)=IMCAY
      JTAB(2)=IMLEA
      PTAB(11)=ELAY
      PTAB(12)=EPB2
      NAG=2
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DST3)
      CALL GSMIXT(IAGMAT,'LCAL DENSE AVERAGE $',PTAB(21),PTAB(31),DST3,
     1   NAG,  PTAB(41))
      IAGMED=IAGMED+1
C Use same sensitive volume flag as selected by 'LCAL' SET card
      CALL GSTMED(IAGMED,'LC STACK3 VOLUME$',IAGMAT,ISV,IAGFLD,ALFIEL,
     1         TAB1(1,6),TAB1(2,6),TAB1(3,6),TAB1(4,6),TAB1(5,6),0,0)
      PTAB(1)=R1STA
      PTAB(2)=R2STA
      PTAB(3)=0.5*RTABL(KLMTY,1,JLMTST+2)
      CALL GSVOLU('LCS3','TUBS',IAGMED,PTAB,5,IVOL)
C
C   Define where to find slot number
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1)=JHOCHA('LCS3')
      IAGSEN(IAGSLV,2)=4
      Z=-ZLCIN+RTABL(KLMTY,1,JLMTST)+RTABL(KLMTY,1,JLMTST+1)+PTAB(3)
      CALL GSPOS('LCS3',1,'LCIN',0.,0.,Z ,0,'ONLY')
C
C   PLACE SIDE WALL,SIDE GAS GAP AND DOWEL PIECE IN LCS3
C
      PTAB(1)=0.5*RTABL(KLCAL,1,JLCATI)
      PTAB(2)=0.5*YSTA
      X=PTAB(1)
      Y=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)
     & +RTABL(KLCAL,1,JLCAGI)+PTAB(2)
      CALL GSPOSP('LCSW',1,'LCS3',X,Y,0.,0,'ONLY',PTAB,3)
      CALL GSPOSP('LCSW',2,'LCS3',X,-Y,0.,0,'ONLY',PTAB,3)
      PTAB(1)=0.5*RTABL(KLCAL,1,JLCAGI)
      X=RTABL(KLCAL,1,JLCATI)+PTAB(1)
      CALL GSPOSP('LCSG',1,'LCS3',X,Y,0.,0,'ONLY',PTAB,3)
      CALL GSPOSP('LCSG',2,'LCS3',X,-Y,0.,0,'ONLY',PTAB,3)
      PTAB(1)=0.
      PTAB(2)=RTABL(KLMTY,1,JLMTRD)
      X=RTABL(KLMTY,1,JLMTXD)
      Y=RTABL(KLMTY,1,JLMTYD)
      CALL GSPOSP('LCDL',1,'LCS3',X,Y,0.,0,'ONLY',PTAB,3)
      CALL GSPOSP('LCDL',2,'LCS3',X,-Y,0.,0,'ONLY',PTAB,3)
C Now replace some stack by gas.Depth is stored as PTAB(3) for LCS3
      IF (LCOLS(KLCAL).GE.LLCALA) THEN
         WSPACE(50)=PTAB(3)
         WSPACE(70)=PTAB(3)
         X=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)
     &     +RTABL(KLCAL,1,JLCAGI)-0.5*WSPACE(54)
         Y=-WSPACE(53)
         Z=0.
         CALL GSPOSP('LEAD',1,'LCS3',X,Y,Z,0,'ONLY',WSPACE(50),11)
         CALL GSPOSP('LEAD',2,'LCS3',X,-Y,Z,IAG1,'ONLY',WSPACE(50),11)
         X=RTABL(KLCAL,1,JLCAGI)+RTABL(KLCAL,1,JLCATI)+WSPACE(73)
         Y=RTABL(KLCAL,1,JLCARI)+RTABL(KLCAL,1,JLCATI)
     &     +RTABL(KLCAL,1,JLCAGI)
         CALL GSPOSP('LEAD',3,'LCS3',X,Y,Z,IAG2,'ONLY',WSPACE(70),11)
         CALL GSPOSP('LEAD',4,'LCS3',X,-Y,Z,IAG3,'ONLY',WSPACE(70),11)
      ENDIF
C
C   Add alu part after stack 3 including :
C   -extra alu plate (thickness ..JLMTBA)
C   -space with compression springs and wiring taken as equivalent
C    to aluminum (thickness ..JLMTBS)
      PTAB(1)=R1STA
      PTAB(2)=R2STA
      PTAB(3)=0.5*(RTABL(KLMTY,1,JLMTBA)+RTABL(KLMTY,1,JLMTBS))
      CALL GSVOLU('LCCA','TUBS',IAGAL,PTAB,5,IVOL)
      Z=ZLCIN-PTAB(3)
      CALL GSPOS('LCCA',1,'LCIN',0.,0.,Z,0,'ONLY')
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
        CALL AGDIMP('LCEA',3,'LCAL')
        CALL AGDIMP('LCEB',3,'LCAL')
      GOTO 999
C
C - not enough space to save sensitive module
C
 998  CONTINUE
      CALL ALTELL('AGLCAL: too many sensitive volumes ',0,'STOP')
C
C - end
C
 999  CONTINUE
      RETURN
      END
