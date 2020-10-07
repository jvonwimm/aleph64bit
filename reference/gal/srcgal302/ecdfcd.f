      SUBROUTINE ECDFCD
C.-----------------------------------------------------------------
C  M.Rumpf may 86                            modification may 87
C!   Modified :- E. Lancon              3-DEC-1990
C! ECAL Run cond. interface
C  Correspondance  Integers <--> Characters
C  Can be changed using Steering data cards
C           RUNC   'ECAL' n1  n2 ....    where n1 etc are integers
C  - Called by ECIRUN
C.------------------------------------------------------------------
C
C     EXTERNAL   INTERNAL                      VALUES
C     Integer    Character     Default
C     --------------------------------------------------------------
C      ICECJO                     0                1            2
C     --------------------------------------------------------------
C        (1)     TCUTRK       NO CUT           CUT TRACK        -
C        (2)     TSMEAR       NO SMEARING      SMEARING         -
C        (3)     TIRAGE       FLUCTUATED       VERY FAST        -
C        (4)     TEDEPO       FLUCTUATED       VERY FAST        -
C        (5)     TPARAM       NO               ELEC           ALL
C        (6)     EDNOXT       +ONE             HITS           ALL
C        (7)     EDMIXR       NO               YES              -
C        (8)     EDSAVG       NO               YES              -
C        (9)     EDZSUP       SINGLE CUT       DOUBLE CUT       -
C       (10)     not used         -                -            -
C     ------------------------------------------------------------
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
      COMMON /EDCOND/ EDNOXT,EDMIXR,EDSAVG,EDZSUP
      CHARACTER*16    EDNOXT,EDMIXR,EDSAVG,EDZSUP
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
C
C For Analog Signals
C
      IF( ICECJO(1).EQ.0)     TCUTRK = 'NO CUT'
      IF( ICECJO(1).EQ.1)     TCUTRK = 'CUT TRACK'
      IF( ICECJO(2).EQ.0)     TSMEAR = 'NO SMEARING'
      IF( ICECJO(2).EQ.1)     TSMEAR = 'SMEARING'
      IF( ICECJO(3).EQ.0)     TIRAGE = 'FLUCTUATED'
      IF( ICECJO(3).EQ.1)     TIRAGE = 'VERY FAST'
      IF( ICECJO(4).EQ.0)     TEDEPO = 'FLUCTUATED'
      IF( ICECJO(4).EQ.1)     TEDEPO = 'VERY FAST'
      IF( ICECJO(5).EQ.0)     TPARAM = 'NO'
      IF( ICECJO(5).EQ.1)     TPARAM = 'ELEC'
      IF( ICECJO(5).EQ.2)     TPARAM = 'ALL'

C
C For Digitization
C
      IF( ICECJO(6).EQ.0)     EDNOXT = '+ONE'
      IF( ICECJO(6).EQ.1)     EDNOXT = 'HITS'
      IF( ICECJO(6).EQ.2)     EDNOXT = 'ALL'
      IF( ICECJO(7).EQ.0)     EDMIXR = 'NO'
      IF( ICECJO(7).EQ.1)     EDMIXR = 'YES'
      IF( ICECJO(8).EQ.0)     EDSAVG = 'NO'
      IF( ICECJO(8).EQ.1)     EDSAVG = 'YES'
      IF( ICECJO(9).EQ.0)     EDZSUP = 'SINGLE CUT'
      IF( ICECJO(9).EQ.1)     EDZSUP = 'DOUBLE CUT'
C
      END
