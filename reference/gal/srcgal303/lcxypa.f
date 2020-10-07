      SUBROUTINE LCXYPA(MODU,LAY,XYZ,IPAD)
C--------------------------------------------------------------
C!   Convert coordinates into tower address
C!   Author   : J.Dines Hansen & P.Hansen - 860301
C!   Modified : P.Hansen - 870920
C!
C! - INPUT : LAY      = Layer No.               [1,38]
C!           MODU     = Module No.              [1,4]
C!           XYZ(3)   = subcomponent coordinates
C! - OUTPUT: IPAD     = Addr. (ICOL + 16*(IROW-1) + 512*(MODU-1))
C!                      (0 if XYZ is outside the active volume)
C!                      -1 if XYZ is outside the pads but inside
C!                       the wires)
C!
C! - Called by  LCSHOW                          from this .HLB
      SAVE
      DIMENSION XYZ(*)
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
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
      COMMON /LCCOMD/ XLSUP(2,2),XHSUP(2,2),YSUP(2,2),
     *                Y0CUT,DYCUT,Y5CUT,DY1,DY3,HUNIT
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
C? IF input is out of range THEN report error
      IPAD=0
      IF(LAY.GT.LCNWPL .OR. LAY.LE.0)     GOTO 998
      IF(MODU.LE.0.OR.MODU.GT.4)          GOTO 998
C
C? Geometry bank pointers
      KLMTY = IW(NALMTY)
      KLLAY = IW(NALLAY)
      KLWRG = IW(NALWRG)
      KLCPG = IW(NALCPG)
C
C? Impact point in wire plane quadrant
      X = ABS(XYZ(1))
      Y = ABS(XYZ(2))
C
C? Set flag for alternating dead zones
      IF (MOD(LAY,2).EQ.0 .AND. XYZ(1)*XYZ(2).GT.0.) THEN
        IFDED = 1
        IODD = 2
      ELSEIF (MOD(LAY,2).EQ.1 .AND. XYZ(1)*XYZ(2).LT.0.) THEN
        IFDED = 0
        IODD = 2
      ELSE
        IFDED = 0
        IODD =1
      ENDIF
C
C? IF impact point not in the active area THEN IPAD=0
C? IF impact point is not covered by pads THEN IPAD=-1
      IF (X .LT. RTABL(KLWRG,1,JLWRXL))          GOTO 999
      IF (X .GT. RTABL(KLWRG,20,JLWRXL)
     &          +RTABL(KLWRG,20,JLWRTW))         GOTO 999
C? Loop over tubes
      DO 10 I=1,19
        IF(X.GT.RTABL(KLWRG,I+1,JLWRXL))        GOTO  10
        DY2 = RTABL(KLWRG,I,JLWRD2)
        YHIGH = RTABL(KLWRG,I,JLWRYH)
        YLOW  = RTABL(KLWRG,I,JLWRYL)
        IF(YLOW.GT.DY1) YLOW = YLOW+DY1
        IF(IFDED.NE.0) THEN
          YHIGH = YHIGH - DY2
        ELSE
          YHIGH = YHIGH - DY1
        ENDIF
C? Throw hits outside end supports
        IF(Y.GT.YHIGH.OR.Y.LT.YLOW)             GOTO 999
C
C? Throw hits on the cathode the places where it is cut
        IF(I.EQ.5.AND.Y.LT.Y5CUT)               GOTO 997
        IF(I.LE.5.AND.Y.LT.Y0CUT-X*DYCUT)       GOTO 997
C
C? Throw hits on top of internal supports
C?  These are placed differently in the four modules
C?  Module 1
        IF(MODU.EQ.1) THEN
          IF(XYZ(2).GT.0.) THEN
            IF(Y.GT.YSUP(1,1)-DY3.AND.
     &         Y.LT.YSUP(1,1)+DY3.AND.
     &         X.GT.XLSUP(1,1).AND.X.LT.XHSUP(1,1)) GOTO 999
          ELSE
            IF(Y.GT.YSUP(2,1)-DY3.AND.
     &         Y.LT.YSUP(2,1)+DY3.AND.
     &         X.GT.XLSUP(2,1).AND.X.LT.XHSUP(2,1)) GOTO 999
          ENDIF
C?  Module 2
        ELSEIF(MODU.EQ.2) THEN
          DY4 = 0.
          IF(MOD(LAY,2).EQ.1) THEN
            IF(XYZ(2).GT.0) THEN
              DY4 = -1.75
            ELSE
              DY4 =  1.75
            ENDIF
          ENDIF
          IF(Y.GT.YSUP(IODD,1)-DY3+DY4.AND.
     &       Y.LT.YSUP(IODD,1)+DY3+DY4.AND.
     &       X.GT.XLSUP(IODD,1).AND.X.LT.XHSUP(IODD,1)) GOTO 999
C?  Module 3
        ELSEIF(MODU.EQ.3) THEN
          IF(XYZ(2).GT.0.) THEN
            IF(Y.GT.YSUP(2,1)-DY3.AND.
     &         Y.LT.YSUP(2,1)+DY3.AND.
     &         X.GT.XLSUP(2,1).AND.X.LT.XHSUP(2,1)) GOTO 999
          ELSE
            IF(Y.GT.YSUP(1,1)-DY3.AND.
     &         Y.LT.YSUP(1,1)+DY3.AND.
     &         X.GT.XLSUP(1,1).AND.X.LT.XHSUP(1,1)) GOTO 999
          ENDIF
C?  Module 4
        ELSE
          IF(Y.GT.YSUP(IODD,1)-DY3.AND.
     &       Y.LT.YSUP(IODD,1)+DY3.AND.
     &       X.GT.XLSUP(IODD,1).AND.X.LT.XHSUP(IODD,1)) GOTO 999
        ENDIF
C
C? Finally check the internal support at large x
        IF(IODD.EQ.2.AND.Y.GT.YSUP(1,2)-DY3
     &              .AND.Y.LT.YSUP(1,2)+DY3.AND.
     &       X.GT.XLSUP(1,2).AND.X.LT.XHSUP(1,2)) GOTO 999
C
        GOTO 20
   10 CONTINUE
C
C? Determine pad size and offset
   20 SIZE = RTABL(KLMTY,1,JLMTCC)*FLOAT(ITABL(KLLAY,LAY,JLLAPS))
      DX = FLOAT(MOD(ITABL(KLLAY,LAY,JLLAPS),2))*HUNIT
C
C? Apply corrections if present
      IF(KLCPG.GT.0) THEN
        SIZE = SIZE+RTABL(KLCPG,LAY,JLCPPS)
        DX   = DX  +RTABL(KLCPG,LAY,JLCPPO)
      ENDIF
C
C? Determine row and column
      IY = MIN0(15,INT((Y-HUNIT)/SIZE)+1)
      IX = MIN0(16,INT((X-DX)/SIZE+.5))
      IF(IX.LE.0)                                 GOTO 999
      IF(IY.EQ.0) IY=1
C
      IF(IY.GE.1.AND.IY.LE.3) THEN
        IX = MAX0(4,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.4) THEN
        IX = MAX0(3,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.5) THEN
        IX = MAX0(2,IX)
        GOTO 30
      ENDIF
C
      IF(IY.GE.6.AND.IY.LE.8) THEN
        IX = MIN0(15,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.9) THEN
        IX = MIN0(14,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.10.OR.IY.EQ.11) THEN
        IX = MIN0(13,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.12) THEN
        IX = MIN0(12,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.13) THEN
        IX = MIN0(11,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.14) THEN
        IX = MIN0(10,IX)
        GOTO 30
      ENDIF
C
      IF(IY.EQ.15) THEN
        IX = MIN0(8,IX)
        IX = MAX0(3,IX)
        GOTO 30
      ENDIF
   30 CONTINUE
C
      IF (XYZ(2) .GE. 0.) THEN
        IY = IY + 15
      ELSE
        IY = 16 - IY
      ENDIF
C
C? Encode channel address
      IPAD    = IX + 16*(IY-1) + 512*(MODU-1)
      GOTO 999
C
  997 IPAD = -1
      GOTO 999
C
  998 WRITE(LOUTIO,1000) MODU,LAY
  999 CONTINUE
C
 1000 FORMAT(' +++ LCXYPA +++ Argument out of range',2I10)
      END
