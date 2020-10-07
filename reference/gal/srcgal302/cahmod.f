      SUBROUTINE CAHMOD ( IPAR,IHPLN)
C
C   M.N Minard 880508
C   1 st : lift working bank HCVL
C          initialisation start volume in HCAL
C          HCVL allows to store volume parameter for geantino tracking
C   ELSE
C          Calculate absorption and radiation length in non sensistive
C          volume
C          Fill common CAHTRA at the end of volume
C - Output arguments:
C              IPAR = 1 if bank HCVL and common /CAHTRA/ are filled
C              IHPLN= plane # where the shower starts
C-  Called  in HCSHOW
C-  Calls HCSETP
C
C   Modify by M.N. Minard    29/1/93
C ------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JCAPST=1,JCAPPT=2,JCAPTE=3,JCAPTX=4,JCAPTL=5,JCAPEM=6,
     +          JCAPEA=7,JCAPEB=8,JCAPA1=9,JCAPER=10,JCAPEF=11,
     +          JCAPED=12,JCAPEZ=13,JCAPHE=14,JCAPHA=15,JCAPHB=16,
     +          JCAPHD=17,JCAPHP=18,JCAPGA=19,JCAPSX=20,JCAPSA=21,
     +          JCAPNA=22,LCAPAA=22)
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
      COMMON/GCMATE/NGMAT,NGAMAT(5),GA,GZ,GDENS,GRADL,GABSL
C
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
      COMMON /HCCONG/ HCTUAC,HCSTDT,HCADCE,HADCMX,HCPFAC,
     &                HCTINS,RHBAMN,ZHECMN,ZHBAMX,HSTREA,HSTUST,
     +                NHCFSS,HCFSS1,HCFSS2,HCFLSS(100)
     &               ,HTLEMX,HCTEFF(3),HPINDU
C
      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      COMMON/HCGEGA/ HCSMTH,HCIRTH,HCLSLA,HCTUTH, NHCINL,NHCOUL,NHCTRE,
     +HCPHOF,NHCTU1(LHCNL), HCLARA(LHCNL),HCLAWI(LHCNL),IHCREG(LHCTR),
     +HCZMIN(LPHC),HCZMAX(LPHC),HCRMIN(LPHC), HCRMAX(LPHC),HCTIRF(LPHC),
     +NHCPLA(LPHC), HCWINO(LPHC),HCLTNO(LPHC)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      COMMON /CAHTRA/ HCTRKM (27,2 ),HCAXL0,HTIN01,HTIX01,HSHGRA,HSHGAB
      PARAMETER(JHCVUH=1,JHCVTA=2,JHCVTR=3,JHCVTL=4,JHCVAV=5,JHCVRV=6,
     +          JHCVTE=7,LHCVLA=33)
      LOGICAL FPSEN , FCSEN
C -------------------------------------------------------------
C--   INITIALISATION
C
      IPAR = 0
C     current volume sensitive flag
      FCSEN = ITRKEL(7).GT.0
      JHCVL = NLINK ('HCVL',ITRKEL(1))
C
C - IF 1st entry THEN
C      create bank 'HCVL',NR=track#
C      fill it with absoption and radiation lengths
C      set the sensitive volume flag to 0 (volume not sensitive)
C
      IF (JHCVL .EQ. 0) THEN
C
         CALL ALBOS('HCVL',ITRKEL(1),LHCVLA+LMHLEN,JHCVL,IGARB)
         IW ( JHCVL+LMHCOL ) = LHCVLA
         IW ( JHCVL+LMHROW ) = 1
         CALL BLIST (IW,'T+','HCVL')
         KHCVL = JHCVL + LMHLEN
C
         DO 10 IL = 1, LETRK
            RW(KHCVL+JHCVTE+IL-1) = TRKELE (IL)
  10     CONTINUE
         RW ( KHCVL+JHCVRV ) = TRKELE (11)/GRADL
         RW ( KHCVL+JHCVAV ) = TRKELE (11)/GABSL
         RW ( KHCVL+JHCVTL ) = TRKELE (11)
         RW ( KHCVL+JHCVTA ) = TINOL0 - TRKELE (11)/GABSL
         RW ( KHCVL+JHCVTR ) = TINOX0 - TRKELE (11)/GRADL
         IW ( KHCVL+JHCVUH ) = 0
C
      ELSE
C
C - ELSE (bank HCVL,NR=ITRKEL(1) exists)
C      IF current volume is not sensitive THEN
C         IF previous volume was sensitive THEN
C            reset 'HCVL' and set sensitive flag to 0
C         ELSE previous volume was not sensitive
C            update 'HCVL' , sensitive flag = 1
C         ENDIF
C      ELSE current volume is sensitive
C         IF previous volume was not sensitive THEN
C            find out where the interaction occured
C            update 'HCVL', set sensitive flag to 1
C            fill common /CAHPAR/
C            set IPAR to 1
C
         KHCVL = JHCVL+LMHLEN
C        previous volume sensitive flaf
         FPSEN = IW ( KHCVL + JHCVUH ) .GT. 0
         IF (.NOT.FCSEN) THEN
            IF (FPSEN) THEN
               RW ( KHCVL+JHCVRV ) = TRKELE (11)/GRADL
               RW ( KHCVL+JHCVAV ) = TRKELE (11)/GABSL
               RW ( KHCVL+JHCVTL ) = TRKELE (11)
               RW ( KHCVL+JHCVTA ) = TINOL0 - TRKELE (11)/GABSL
               RW ( KHCVL+JHCVTR ) = TINOX0 - TRKELE (11)/GRADL
               IW ( KHCVL+JHCVUH ) = 0
            ELSE
               RW ( KHCVL+JHCVRV ) = RW ( KHCVL+JHCVRV )
     +                                   + TRKELE (11)/GRADL
               RW ( KHCVL+JHCVAV ) = RW ( KHCVL+JHCVAV )
     +                                 + TRKELE (11)/GABSL
               RW ( KHCVL+JHCVTL ) = RW( KHCVL+JHCVTL )
     +                                   + TRKELE (11)
            ENDIF
         ELSE
            IF (.NOT.FPSEN) THEN
               CALL HCSETP
               SLENG = HCPAZ0 + HCTIRF(IHCPOR)+HCTINS + 0.1
               IHPLN = ITRKEL(6)
               IW ( KHCVL+JHCVUH ) = 1
               RW ( KHCVL+JHCVRV ) = RW ( KHCVL+JHCVRV )
     +                                   + TRKELE (11)/GRADL
               RW ( KHCVL+JHCVAV ) = RW ( KHCVL+JHCVAV )
     +                                   + TRKELE (11)/GABSL
               RW ( KHCVL+JHCVTL ) = RW( KHCVL+JHCVTL )
     +                                   + TRKELE (11)
C
               DO 20 IL = 1,LETRK
                  HCTRKM ( IL,1 ) = RW (KHCVL+JHCVTE+IL-1)
                  IF(IL.LE.LNTRK)HCTRKM ( IL,2 ) = TRKNXT (IL)
  20           CONTINUE
               HTIN01 = RW (KHCVL + JHCVTA)
               HTIX01 = RW (KHCVL + JHCVTR)
               HSHGRA = RW (KHCVL+JHCVTL)/RW(KHCVL+JHCVRV)
               HSHGAB = RW (KHCVL+JHCVTL)/RW(KHCVL+JHCVAV)
               IPAR = 1
            ENDIF
         ENDIF
       ENDIF
       RETURN
       END
