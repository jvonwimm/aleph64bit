      SUBROUTINE LCFRAL(IFB,MODU,XIN,XOUT,DXIN)
C--------------------------------------------------------------
C! Track element coordinate tranformation
C. - J.Dines Hansen & P.Hansen - 860505
C. - Mdified P.Hansen - 900415
C. - Transform track element from global to subdetector frame
C. - Input  : TRKELE
C. - Output : MODU   = Module No. [1,4]
C.            IFB    = endcap #
C. -          XIN    = xyz of track element TRKELE
C. -          XOUT   = xyz of track element TRKNXT
C. -          DXIN   = direction cosines of TRKELE
C. - Called by  LCSHOW, LCTRAK                   from this .HLB
C -------------------------------------------------
      SAVE
      DIMENSION XIN(*),XOUT(*),DXIN(*)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
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
C -------------------------------------------------------------
C - Define origin of module at the start of the sensitive area
      MODU=1
      IF(TRKELE(3).GT.0.) MODU=3
      IF(TRKELE(1).GT.0.) MODU=MODU+1
      IFB=(MODU-1)/2+1
C
      KLMTY = IW(NALMTY)
      KLALI = IW(NALALI)
C
C
C - Translate track element to nominal local coordinates
      DO 10 I=1,3
        XIN(I)  = TRKELE(I) - XYZOLC(I,IFB)
        IF (ITRKEL(8).NE.1) XOUT(I)  = TRKNXT(I) - XYZOLC(I,IFB)
        DXIN(I) = TRKELE(3+I)
 10   CONTINUE
C
C Alignment correction :
C  Let the local module system be translated by
C  (Oxyz+Dxyz) and rotated by Rotvec with respect to ALEPH.
C  Let ALEPH coordinates be XYZ and local coordinates xyz:
C
C  XYZ = Oxyz + Dxyz + xyz + Rotvec x xyz
C
C  We modifify the nominal xyz in the following approximate way:
C
      ZIN  = RTABL(KLMTY,1,JLMTFL)*SIGN(1.,XIN(3))
     &      + XIN(3) - RTABL(KLALI,MODU,JLALDX+2)
C
      DXIN(3) = DXIN(3)-RTABL(KLALI,MODU,JLALDR+1)*DXIN(1)
     &                 +RTABL(KLALI,MODU,JLALDR)*DXIN(2)
      DXIN(1) = DXIN(1)
     &               - RTABL(KLALI,MODU,JLALDR+2)*DXIN(2)
     &               + RTABL(KLALI,MODU,JLALDR+1)*DXIN(3)
C
      DXIN(2) = DXIN(2)
     &               - RTABL(KLALI,MODU,JLALDR)*DXIN(3)
     &               + RTABL(KLALI,MODU,JLALDR+2)*DXIN(1)
C
C
      XIN(1) = XIN(1) - RTABL(KLALI,MODU,JLALDX)
     &                - RTABL(KLALI,MODU,JLALDR+2)*XIN(2)
     &                + RTABL(KLALI,MODU,JLALDR+1)*ZIN
C
      XIN(2)= XIN(2) - RTABL(KLALI,MODU,JLALDX+1)
     &               - RTABL(KLALI,MODU,JLALDR)*ZIN
     &               + RTABL(KLALI,MODU,JLALDR+2)*XIN(1)
C
      XIN(3)= XIN(3) - RTABL(KLALI,MODU,JLALDX+2)
     &               - RTABL(KLALI,MODU,JLALDR+1)*XIN(1)
     &               + RTABL(KLALI,MODU,JLALDR)*XIN(2)
C
      IF (ITRKEL(8).EQ.1) GOTO 999
C
      ZOUT = RTABL(KLMTY,1,JLMTFL)*SIGN(1.,XOUT(3))
     &      + XOUT(3) - RTABL(KLALI,MODU,JLALDX+2)
C
      XOUT(1) = XOUT(1) - RTABL(KLALI,MODU,JLALDX)
     &                - RTABL(KLALI,MODU,JLALDR+2)*XOUT(2)
     &                + RTABL(KLALI,MODU,JLALDR+1)*ZOUT
C
      XOUT(2)= XOUT(2) - RTABL(KLALI,MODU,JLALDX+1)
     &               - RTABL(KLALI,MODU,JLALDR)*ZOUT
     &               + RTABL(KLALI,MODU,JLALDR+2)*XOUT(1)
C
      XOUT(3)= XOUT(3) - RTABL(KLALI,MODU,JLALDX+2)
     &               - RTABL(KLALI,MODU,JLALDR+1)*XOUT(1)
     &               + RTABL(KLALI,MODU,JLALDR)*XOUT(2)
C
 999  CONTINUE
      END
