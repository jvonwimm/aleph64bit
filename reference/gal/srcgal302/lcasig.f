      SUBROUTINE LCASIG
C--------------------------------------------------------------
C! Controls the analog signal processing in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C. - Called by  ASASIG                           from this .HLB
C. - Calls      LCSATU                           from this .HLB
C -----------------------------------------------
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
      PARAMETER(JLSHPT=1,JLSHTI=2,JLSHDE=3,LLSHIA=5)
      PARAMETER(JLWHPT=1,JLWHMI=2,JLWHDE=3,LLWHIA=40)
      INTEGER CAFIHT
      LOGICAL FDEB
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
      FDEB = FDEBJO .AND. IPRIJO(5).NE.0
C
C - Drop banks which exist already
      IF (IW(NALTHT) .NE. 0) CALL BDROP (IW,'LTHTLWHT')
C
C - Reduce bank size to the real one
      CALL AUBPRS ('LSHILWHI')
      IF (FDEB) THEN
         CALL PRTABL('LSHI',0)
         CALL PRTABL('LWHI',0)
      ENDIF
C
C - Fill hit banks
      JLTHT = CAFIHT (NALSHI,'LTHT')
      IF (JLTHT.EQ.0) GOTO 998
      IF (FDEB) CALL PRTABL ('LTHT',0)
      JLWHT = CAFIHT (NALWHI,'LWHT')
      IF (JLWHT.EQ.0) GOTO 998
      IF (FDEB) CALL PRTABL ('LWHT',0)
C
C Is there any tower hits ?
      KINDX = IW(NALTHT)
      IF (KINDX .EQ. 0) RETURN
      IF (LROWS(KINDX) .EQ. 0) RETURN
C
      IF (LCPRNT.GE.2) THEN
         WRITE (LOUTIO,'(/1X,''+++LCASIG+++'')')
         WRITE (LOUTIO,'(1X,'' LTHT bank : nrows/ncols '',2I5)')
     &          LROWS(KINDX),LCOLS(KINDX)
         WRITE (LOUTIO,'(1X,''Tower hits bank LTHT''/3X,''Module'',4X,
     &              ''icol'',4X,''irow'',''       Hits'')')
        DO 130 J = 1,LROWS(KINDX)
          KR = KROW(KINDX,J)
          MODU=LCMOD(IW(KR+1))
          IROW=LCROW(IW(KR+1))
          ICOL=LCCOL(IW(KR+1))
          WRITE(LOUTIO,5300) MODU,ICOL,IROW,(IW(KR+K),K=2,4)
  130   CONTINUE
 5300 FORMAT(6I8)
       ENDIF
C
C - Saturate Analog signals
      CALL LCSATU
C
      RETURN
C
C - not enough space
C
 998  CONTINUE
      CALL ALTELL ('LCASIG: not enough space for HIT banks ',1,'NEXT')
      END
