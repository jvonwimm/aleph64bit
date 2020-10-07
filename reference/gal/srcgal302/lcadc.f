      SUBROUTINE LCADC
C--------------------------------------------------------------
C! Fills banks LTDI and LWDI
C. - J.Dines Hansen & P.Hansen - 860417
C.                               modified by F.Ranjard - 890317
C.                               modified by P.H.Hansen -950202
C. - Stores digitizations for LCal towers and wireplanes in MeV
C. - Called by  LCDIGI                           from this .HLB
C -----------------------------------------------
      SAVE
      EXTERNAL RNDM
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
C         parameters in the LCal code
        PARAMETER (LWDIR = 4, LTTRR = 24, LTTRC = 3)
        PARAMETER (LWTRR = 4, LWTRC = 2 , LTDIC = 4)
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
      COMMON /LCNAMC/ NALTHT, NALTDI, NALWHT, NALWDI, NALTTR, NALWTR,
     *                NALWHI, NALSHI, NALCWS, NALCAL, NALLAY, NALMTY,
     *                NALALI, NALSCO, NALSLO, NALWRG, NALCEL, NALCSH,
     *                NALDRE, NALCCA, NALCPG, NALSHO
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
C         macro to decode pad-address
        LCMOD(IXXXX) = (IXXXX-1)/512 + 1
        LCROW(IXXXX) = MOD(IXXXX-1,512)/16 + 1
        LCCOL(IXXXX) = MOD(IXXXX-1,16) + 1
C
C -------------------------------------------------------------
C
C -  Find length of the wire hit bank
      KWH = IW(NALWHT)
      IF(KWH .LE. 0)                                  GOTO 999
      NROW = LROWS(KWH)
      IF(NROW .LE. 0)                                 GOTO 999
C
C - Geometric cell-to-cell variations in percent
      KLCCA = IW(NALCCA)
C
C - Book wire digitalisation bank
      LEN = NROW*(LCNWPL+1) + LMHLEN
      CALL ALBOS ('LWDI',0,LEN,KWD,IGARB)
      IW(KWD+1) = LCNWPL+1
      IW(KWD+2) = NROW
      CALL BLIST(IW,'E+','LWDI')
      IF (IGARB.EQ.1) KWH = IW(NALWHT)
      KWS  = IW(NALCWS)
C
C - Loop over modules, storeyes and wire planes
        DO 130 MODU = 1,NROW
          IF(MODU.GE.3) THEN
            IFB=2
          ELSE
            IFB=1
          ENDIF
          KRWH = KROW(KWH,MODU)
          KRWD = KROW(KWD,MODU)
          IW(KRWD+1) = IW(KRWH+1)
          LAY = 0
          DO 120 IST = 1,3
            LAYMX = LCNLAY(IST)
            DO 110 L = 1,LAYMX
              LAY = LAY + 1
C
C - Convert plane hits to MeV
              ADCC = 1000.*ECRTLC*FLOAT(IW(KRWH+1+LAY))/16.
C
C - Apply gain variations
              ADCC = ADCC*(1.+GVARLC*(RNDM(DUMMY)-0.5))
C
C - Check saturation
              ADCC = AMIN1(ADCC,6250.)
C
C - Convert to integer MeV
              FACT = 1.
              IF(IST.EQ.3) FACT = SSTPLC(3)/SSTPLC(2)
              IW(KRWD+1+LAY) = NINT(ADCC*FACT)
C
C - Update summary bank
              IF(KWS.GT.0) THEN
                IW(KWS+LMHLEN+8+IFB) = IW(KWS+LMHLEN+8+IFB)+
     &                                 IW(KRWD+1+LAY)
              ENDIF
  110       CONTINUE
  120     CONTINUE
  130   CONTINUE
C
C -  Find length of the tower hit bank
      KTH = IW(NALTHT)
      IF(KTH .LE. 0)                                  GOTO 999
      NROW = LROWS(KTH)
      IF(NROW .LE. 0)                                 GOTO 999
C
C - Book tower digitalisation bank
      LEN = NROW*LTDIC + LMHLEN
      CALL ALBOS ('LTDI',0,LEN,KDI,IGARB)
      IW(KDI+1) = LTDIC
      IW(KDI+2) = NROW
      CALL BLIST(IW,'E+','LTDI')
      IF (IGARB.EQ.1) KTH = IW(NALTHT)
      KWS  = IW(NALCWS)
C
C - Loop over tower hits
      DO 230 ITOW = 1,NROW
        KRTH = KROW(KTH,ITOW)
        KRDI = KROW(KDI,ITOW)
C - Store address
        IADDR = IW(KRTH+1)
        IW(KRDI+1) = IADDR
C - Decode cell address
        IROW = LCROW(IADDR)
        ICOL = LCCOL(IADDR)
        MODU = LCMOD(IADDR)
        IF(IROW.GE.16) IROW = 31-IROW
C - Find geometrical gain variation
        GCORR = 1.
C        IF(KLCCA.GT.0) THEN
C          ICELL = JLCCDI - 1 + ICOL
C          GCORR = 1.+FLOAT(ITABL(KLCCA,IROW,ICELL))/100.
C        ENDIF
C
        DO 220 IST  = 1,3
C
C - Convert storey hits to MeV
          ADCC = 1000.*ECRTLC*FLOAT(IW(KRTH+1+IST))/16.
C
C - Apply gain variations and smear Ecrit quantization
          ADCC = ADCC*(1.+GVARLC*(RNDM(DUMMY)-0.5))*GCORR
C
C - Check saturation
          ADCC = AMIN1(ADCC,100000.)
C
C - Convert to integer MeV
          FACT = 1.
          IF(IST.EQ.3) FACT = SSTPLC(3)/SSTPLC(2)
          IW(KRDI+1+IST) = NINT(ADCC*FACT)
C
C - Update summary bank
          IF(KWS.GT.0) THEN
             IW(KWS+LMHLEN+6+IFB) = IW(KWS+LMHLEN+6+IFB)+
     &                              IW(KRDI+1+IST)
          ENDIF
  220   CONTINUE
  230 CONTINUE
C
C - debug
      IF (FDEBJO .AND. IPRIJO(5).NE.0) THEN
         WRITE(LOUTIO,*) ' +++LCADC+++ debug banks LTDI and LWDI '
         CALL PRTABL ('LTDI',0)
         CALL PRTABL ('LWDI',0)
      ENDIF
C
  999 RETURN
      END
