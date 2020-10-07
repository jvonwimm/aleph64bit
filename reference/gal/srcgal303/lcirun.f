      SUBROUTINE LCIRUN
C--------------------------------------------------------------
C! Initialise LCal constants
C! Author   : J.Dines Hansen & P.Hansen - 860417
C! Modified : P.Hansen - 891120
C! Modified : P.Hansen - 950202
C! Description :
C! =============
C!   Fills /LCCOMC/ and /LCCOMD/ from data base
C! Modified : B.Bloch-Devaux - 920215 to access LALI via GTSTUP
C!   BOS bank initialisation
C!   Called by  USIJOB                          from this  .HLB
C!   Calls      LCNAMI                          from this  .HLB
C!              NAMIND,BKFMT                    from BOS
C -----------------------------------------------
      CHARACTER*20 NAVN
      EXTERNAL NAMIND
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
      COMMON /LCCOMC/ ADCOLC,    COHNLC,    DPR1LC,    DPR2LC,
     *                DPR3LC,    DPR4LC,    DPR5LC,    ECRTLC,
     *                ECUTLC,    EELALC,    GVARLC,    LCADCO,
     *                LCBHTR,    LCHBOK,    LCNLAY(3), LCNWPL,
     *                LCMATE(2), LCPRNT,    LCSTRH(3), CHTOE(3),
     *                PAR1LC,    PAR2LC,    PAR3LC,    PAR4LC,
     *                PAR5LC,    PAR6LC,    PAR7LC,    PAR8LC,
     *                RADLLC(2), SNOILC(3), SCONLC,    SSAMLC,
     *                SSTPLC(3), TNOILC(3), WNOILC(3),
     *                ZMATLC(2), ZREFLC(3), ZSTPLC(3), Z123LC(3),
     *                XYZOLC(3,2),DWIRLC
      COMMON /LCCOMD/ XLSUP(2,2),XHSUP(2,2),YSUP(2,2),
     *                Y0CUT,DYCUT,Y5CUT,DY1,DY3,HUNIT
      INTEGER  ALGTDB,GTSTUP
      CHARACTER*44 LIST
      DATA LIST /'LCALLLAYLMTYLSCOLSLOLWRGLCELLSHOLDRELCCA'/
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
C -------------------------------------------------------------
C
C? Initialize print flag
      IF(ICLCJO(1).GE.2) WRITE(LOUTIO,1000)
      LCPRNT = 0
C
C? Set name indices for BOS banks
      CALL LCNAMI
C
C? Get geometry banks from dbase
      NR = ALGTDB (LRDBIO,LIST,IRUNJO)
      IF(NR.EQ.0) GOTO 997
C? Get LALI bank from setup number and store it in Run header
      ILCST = GTSTUP('LC',1)
      NR = ALGTDB (LRDBIO,'LALI',ILCST)
      CALL BLIST(IW,'C+','LALI')
      IF (NR.EQ.0) GO TO 997
C
C? Get optional geometry banks
      NR = ALGTDB (LRDBIO,'LCPG',IRUNJO)
C
C? Get geometry and constant bank indices
      KLCAL = IW(NALCAL)
      KLMTY = IW(NALMTY)
      KLLAY = IW(NALLAY)
      KLSCO = IW(NALSCO)
      KLSLO = IW(NALSLO)
      KLWRG = IW(NALWRG)
      KLCEL = IW(NALCEL)
      KLSHO = IW(NALSHO)
      KLDRE = IW(NALDRE)
C
C? Get MeV per ADC count
      ADCOLC    = RTABL(KLCEL,1,JLCEAM)
      LCADCO    = NINT(ADCOLC)
C? Get energy sampling unit
      ECRTLC    = RTABL(KLSHO,1,JLSHEC)
C? Get average Zs
      ZMATLC(1) = RTABL(KLSHO,1,JLSHZ1)
      ZMATLC(2) = RTABL(KLSHO,1,JLSHZ2)
C? Get radiation lengths
      RADLLC(1) = RTABL(KLSHO,1,JLSHR1)
      RADLLC(2) = RTABL(KLSHO,1,JLSHR2)
C? Get threshold for starting a shower
      ECUTLC    = RTABL(KLSHO,1,JLSHET)
      IF(ECUTLC.GT.1.) ECUTLC=0.01
C? Get shower parameters
      PAR1LC   = RTABL(KLSHO,1,JLSHP1)
      PAR2LC   = RTABL(KLSHO,1,JLSHP2)
      PAR3LC   = RTABL(KLSHO,1,JLSHP3)
      PAR4LC   = RTABL(KLSHO,1,JLSHP4)
      PAR5LC   = RTABL(KLSHO,1,JLSHP5)
      PAR6LC   = RTABL(KLSHO,1,JLSHP6)
      PAR7LC   = RTABL(KLSHO,1,JLSHP7)
      PAR8LC   = RTABL(KLSHO,1,JLSHP8)
      DPR1LC   = RTABL(KLSHO,1,JLSHD1)
      DPR2LC   = RTABL(KLSHO,1,JLSHD2)
      DPR3LC   = RTABL(KLSHO,1,JLSHD3)
      DPR4LC   = RTABL(KLSHO,1,JLSHD4)
      DPR5LC   = RTABL(KLSHO,1,JLSHD5)
C? Get mip energy per sampling (Ecrtlc units)
      EELALC   = RTABL(KLSHO,1,JLSHEM)
C? Get number of layers in each storey
      LCNLAY(1) = ITABL(KLMTY,1,JLMTNL)
      LCNLAY(2) = ITABL(KLMTY,1,JLMTNL+1)
      LCNLAY(3) = ITABL(KLMTY,1,JLMTNL+2)
      LCNWPL    = LCNLAY(1) + LCNLAY(2) + LCNLAY(3)
C? Get subcomponent coordinates
        PLATE = RTABL(KLMTY,1,JLMTFL)
        DO 310 I=1,2
        DO 300 J=1,3
        JX = JLSCRP + J-1
 300    XYZOLC(J,I) = RTABL(KLSCO,I,JX)
 310    CONTINUE
        XYZOLC(3,1) = XYZOLC(3,1) - PLATE
        XYZOLC(3,2) = XYZOLC(3,2) + PLATE
        Z0 = RTABL(KLCAL,1,JLCAZD) + PLATE
        DZ = 0.
C? Get reference z of each storey
        DO 400 I = 1,3
         JDEPT = JLMTST+I-1
         Z123LC(I) = RTABL(KLMTY,1,JDEPT)
         ZSTPLC(I) = Z123LC(I)/FLOAT(LCNLAY(I))
         ZREFLC(I) = Z0 + DZ + 0.5*Z123LC(I)
         DZ        = DZ + Z123LC(I)
  400   CONTINUE
C? Get steplength for shower sampling in r.l.
         SSTPLC(1) = ZSTPLC(1)/RADLLC(1)
         SSTPLC(2) = ZSTPLC(2)/RADLLC(1)
         SSTPLC(3) = ZSTPLC(3)/RADLLC(2)
C? Hit to energy conversion (MeV)
      CHTOE(1) = 1000.*ECRTLC
      CHTOE(2) = CHTOE(1)
      CHTOE(3) = 1000.*ECRTLC*SSTPLC(3)/SSTPLC(2)
C? Coherent noise in ADC counts
      COHNLC       = RTABL(KLCEL,1,JLCECN)
C? Noise level in wireplanes in ADC counts
      WNOILC(1)    = RTABL(KLCEL,1,JLCEWN)
      WNOILC(2)    = RTABL(KLCEL,1,JLCEWN+1)
      WNOILC(3)    = RTABL(KLCEL,1,JLCEWN+2)
C? Noise level in storeys in ADC counts
      SNOILC(1)    = RTABL(KLCEL,1,JLCESN)
      SNOILC(2)    = RTABL(KLCEL,1,JLCESN+1)
      SNOILC(3)    = RTABL(KLCEL,1,JLCESN+2)
C? Noise level in trigger sums in ADC counts
      TNOILC(1)    = RTABL(KLCEL,1,JLCETN)
      TNOILC(2)    = RTABL(KLCEL,1,JLCETN+1)
      TNOILC(3)    = RTABL(KLCEL,1,JLCETN+2)
C? Readout threshold for storeys in MeV
      LCSTRH(1)    = ITABL(KLCEL,1,JLCETH)
      LCSTRH(2)    = ITABL(KLCEL,1,JLCETH+1)
      LCSTRH(3)    = ITABL(KLCEL,1,JLCETH+2)
C? Sigma of relative gain variation
      GVARLC       = RTABL(KLCEL,1,JLCEGV)
C? Extra DE/E constant term
      SCONLC   = RTABL(KLSHO,1,JLSHSC)
C? Extra DE/E term: SSAMLC/SQRT(E)
      SSAMLC   = RTABL(KLSHO,1,JLSHSS)
C? Trigger threshold in MeV
      LCBHTR       = ITABL(KLCEL,1,JLCETR)
C
C? Places where the pads are cut so that may not cover
C? the wires.
      Y0CUT = RTABL(KLDRE,1,JLDRY0)
      DYCUT = RTABL(KLDRE,1,JLDRDY)
      Y5CUT = RTABL(KLDRE,1,JLDRY5)
C
C? Positions of wiresupport in the middle of the chamber
C? XLSUP=low x position. XHSUP=high x position. YSUP=y position.
C? First index: 1 if odd plane and x*y<0 or even plane and x*y>0.
C?              2 otherwise
C? Second index: support number (two in each quadrant)
      XLSUP(1,1) = RTABL(KLDRE,1,JLDRXL)
      XHSUP(1,1) = RTABL(KLDRE,1,JLDRXH)
      XLSUP(1,2) = RTABL(KLDRE,1,JLDRXL+2)
      XHSUP(1,2) = RTABL(KLDRE,1,JLDRXH+2)
      XLSUP(2,1) = RTABL(KLDRE,1,JLDRXL+1)
      XHSUP(2,1) = RTABL(KLDRE,1,JLDRXH+1)
      XLSUP(2,2) = RTABL(KLDRE,1,JLDRXL+3)
      XHSUP(2,2) = RTABL(KLDRE,1,JLDRXH+3)
      YSUP(1,1) = RTABL(KLDRE,1,JLDRYS)
      YSUP(2,1) = RTABL(KLDRE,1,JLDRYS+1)
      YSUP(1,2) = RTABL(KLDRE,1,JLDRYS+2)
      YSUP(2,2) = RTABL(KLDRE,1,JLDRYS+3)
C
C? Distance from wire-plane to end-of-layer
      DWIRLC = RTABL(KLDRE,1,JLDRDW)
C
C? The wire support covers in each end
      DY1 = RTABL(KLDRE,1,JLDRDT)
C
C? Half width of an internal wiresupport
      DY3 = RTABL(KLDRE,1,JLDRDS)
C
C? Half an inter-pad space
      HUNIT = RTABL(KLMTY,1,JLMTCC)*0.5
C
C? Create job-summary bank
      IF(IDETJO(5).NE.0) THEN
         NALCWS = NAMIND('LCWS')
         CALL BKFMT('LCWS','I')
         LEN = 1 * 12 + LMHLEN
         CALL ALBOS ('LCWS',0,LEN,KINDX,IGARB)
         IW(KINDX+1) = 12
         IW(KINDX+2) = 1
      ENDIF
C
      GOTO 999
  997 CALL ALTELL('LCIRUN: missing data base banks - STOP',0,'STOP')
  999 CONTINUE
 1000 FORMAT(2X,'++ LCIRUN ++')
      END
