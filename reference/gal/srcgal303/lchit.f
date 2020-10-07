      SUBROUTINE LCHIT
C-------------------------------------------------------------
C! Controls tracking in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C.                               modified by F.Ranjard - 890317
C. - Called by   GUSTEP                          from this .HLB
C. - Calls       LCSHOW, LCTRAK                  from this .HLB
C.               BLIST                           from BOS
C -----------------------------------------------
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
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
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
      PARAMETER (LWROW = 50, LSROW = 100)
      COMMON /LCNAMC/ NALTHT, NALTDI, NALWHT, NALWDI, NALTTR, NALWTR,
     *                NALWHI, NALSHI, NALCWS, NALCAL, NALLAY, NALMTY,
     *                NALALI, NALSCO, NALSLO, NALWRG, NALCEL, NALCSH,
     *                NALDRE, NALCCA, NALCPG, NALSHO
      PARAMETER(JLSHPT=1,JLSHTI=2,JLSHDE=3,LLSHIA=5)
      PARAMETER(JLWHPT=1,JLWHMI=2,JLWHDE=3,LLWHIA=40)
C -------------------------------------------------------------
      IF(FBEGJO(5)) THEN
C - Drop existing banks
         CALL BDROP (IW,'LWHTLTHTLSHILWHI')
C
C - Book hit banks at the beginning of events
C
C -  Wire plane history bank LWHI
         LEN = LWROW * LLWHIA + LMHLEN
         CALL ALBOS ('LWHI',0,LEN,KINDX,IGARB)
         IW(KINDX+LMHCOL) = LLWHIA
C
C - Tower history bank LSHI
         LEN = LSROW * LLSHIA + LMHLEN
         CALL ALBOS ('LSHI',0,LEN,KINDX,IGARB)
         IW(KINDX+LMHCOL) = LLSHIA
C
C - Add to the 'E' list or 'T' list
         CALL BLIST(IW,'E+','LSHI')
         CALL BLIST(IW,'T+','LWHILTHTLWHT')
C
C - Count events with tracks hitting LCAL
         KWS = IW(NALCWS)
         IF(KWS .LE. 0) GOTO 10
         IW(KWS+LMHLEN+1) = IW(KWS+LMHLEN+1) + 1
   10    CONTINUE
C
C - Set LC print flag
         IF(FDEBJO) THEN
         IF (IPRIJO(5) .NE. 0) LCPRNT = MAX0 (ICLCJO(1),1)
         ELSE
            LCPRNT = 0
         ENDIF
C
C - Set begin of event flag to false
         FBEGJO(5) = .FALSE.
      ENDIF
C
C - Use shower parametrisation if electron
C - and full tracking if hadron or muon
      IF(ITRKEL(4).EQ.2.OR.ITRKEL(4).EQ.3) THEN
        CALL LCSHOW
      ELSE
        CALL LCTRAK
      ENDIF
C
  999 CONTINUE
      RETURN
      END
