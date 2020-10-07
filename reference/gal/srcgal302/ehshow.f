      SUBROUTINE EHSHOW (ISCMP,MODUL)
C.----------------------------------------------------------------------
C   J.Badier 11/12/85    M.Rumpf  00/01/86
C! Shower creation steering
C Generate SHOWER for track element selected by EHCUTF
C Points and signals stored in temporary bank 'PSIG'
C   - avec NSIGNL points de cordonnees POSSG
C ----- TIRAGE : 'FLUCTUATED' pour tirage standard
C -----          'VERY FAST'  pour tirage rapide
C   - Called by ECHIT
C   - Calls     EHGERB,ECSGEO,EHSITW,UCOPY
C
C.----------------------------------------------------------------------
      SAVE
C
C ----- Version du 16/03/87
      COMMON / EHPASH /
     1 RHODEP,PARGV1,ENMAX1,ENMAX2,PARGV2,NRAPID,RHOVIT(14),
     2 FASTNR(14),FLUCT1(14),FLUCT2(14),EMINPI(14),EPICUT(14),
     3 EMINLO,ETRANS,ASURB1(5),ASURB2(5),UNSRB1(5),UNSRB2(5),
     4 SIGMA1(5),SIGMA2(5),SIGMB1(5),SIGMB2(5),SIGMA3(5),SIGMB3(5),
     5 SEUSIG,USAMIN,USAMAX,BSUMIN,BETMIN,BETMAX,
     6 ZONSH1,ZONSH2,DEPMIN,
     7 EMINRA,EXPRD0,EXPRD1,RFACT0,RFACT1,KPAMAX,X1MAXI,EPSRAD,
     8 EMFRAC,ALPHA0,ALPHA1,BETAH0,BETAH1,RAYHA0,RAYHA1,PUISH0,
     9 PARGVH,NRJHAD,IPLMIN(14),IPLMAX(14),DESATU,FNFRAG,
     + ECHMX,ECHDC,EKEVH,ECHDN,ERADMX,
     + ERMAX,ETANG,ERHMX,EZMAX,EDSELM,EDSHAD,ECUTTE,
     + ST3BA0,ST3EC0,ST3BA1,ST3EC1,ST3BA2,ST3EC2
      COMMON / EHPADA /
     1   CNRJDA,C1PRDA,C2PRDA,C3PRDA,PIMADA,ANRJDA,A1PRDA,A2PRDA,
     2   A3PRDA,A4PRDA,A5PRDA,AMRJDA
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
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
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
      COMMON / ECDBUG / BUGSHR
      CHARACTER*16 BUGSHR
C
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
C     Calls EHGERB,ECSGEO,EHSITW
      REAL POSSG(3)
C
      DIMENSION XGERB(5)
      CHARACTER * 5 TYGEO
      EXTERNAL RNDM
         EXTERNAL EFNLAY
         INTEGER EFNLAY
         LOGICAL INSTK
C - user stop particle flag
      PARAMETER (NOMOR=3)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      LOGICAL FTINO, FMUON, FELEC, FHADC
      EQUIVALENCE (ENER,TRKNXT(8))
      PARAMETER (NMIL=14)
      CHARACTER*4 TEVOL(NMIL)
C
      DATA TEVOL / 'EBS2', 'EBS1', 'EBS3',
     +             'ECS2', 'ECS1', 'ECS3', 'ECS0',
     +             'EDS2', 'EDS1', 'EDS3', 'EDS0',
     +             'EB12', 'EC12', 'ED12' /
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
C - geantino particle
      FTINO = ITRKEL(11).EQ.6
C - muon particle
      FMUON = ITRKEL(11).EQ.5
C - e+/e- particle
      FELEC = ITRKEL(11).EQ.2
C - charged hadron
      FHADC = ITRKEL(11).EQ.4
C
C
C -----------------------------------------------------------
C ----- Recherche du milieu
      IF( ITRKEL(8) .NE. 2 .AND. FTINO ) GO TO 20
      DO 1 MILI = 1,NMIL
         IF (TRKVOL .EQ. TEVOL(MILI)) GOTO 2
   1  CONTINUE
      MILI = 0
   2  CONTINUE
C
      ENER =TRKNXT(8)
C  Compute Number of points for this Shower
C
      IFIN = 0
      IEMG = 0
      IHAD = 0
      IF( FTINO ) THEN
         CALL CAGERB(EMGNRJ,IEMG,XGERB,NRJDP)
         CALL CAHGRB(HADNRJ,IHAD,XGERB,NRJDP)
         CALL CAPSTO ( ITRKEL(1))
         IFIN = IEMG + IHAD
         NSIGNL = 200. * (TOTNRJ+1.)
      ELSE
         CALL EHGERB ( ENER,IFIN,XGERB,NRJDP )
C        IFIN est le nombre de points a tirer.
         NSIGNL = IFIN
      ENDIF
      IF( IFIN .LE.  0) GO TO 20
C
C  Define Readout Geometry in Shower system  'GERBE'
C
      CALL ECSGEO(TRKNXT(1),TRKNXT(4),ISCMP,MODUL)
C
      TYGEO = 'GERBE'
      NECONT(3) = NECONT(3) + 1
C
C    Book working bank PSIG if does not exist or extend it if too small
C
      LEN = LPS1*NSIGNL+LMHLEN
      IF (IDPSIG.LE.0) THEN
         CALL WBANK (IW,IDPSIG,LEN,*20)
         IW(IDPSIG-3) = INTCHA ('PSIG')
         IW(IDPSIG+LMHCOL) = LPS1
      ELSE
         IF (LEN .GT. IW(IDPSIG)) THEN
            CALL WBANK (IW,IDPSIG,LEN,*20)
         ENDIF
      ENDIF
      IW(IDPSIG+LMHROW) = 0
C
C  Fill temporary bank 'PSIG'
C
      NECSG = 0
      IPX = KNEXT(IDPSIG)
   10 CONTINUE
      IF( FTINO ) THEN
         IF( IEMG .NE.  0) THEN
            CALL CAGERB(EMGNRJ,IEMG,POSSG,NRJDP)
         ELSE
            CALL CAHGRB(HADNRJ,IHAD,POSSG,NRJDP)
         ENDIF
         IFIN = IEMG + IHAD
         IF( NSIGNL .LE.  0) GO TO 19
      ELSE
         CALL EHGERB(ENER,IFIN,POSSG,NRJDP)
         IFIN = IFIN - 1
      ENDIF
      IF (NRJDP.GT.0) THEN
      IPLAN = EFNLAY( ISCMP , MODUL , POSSG , TYGEO , INSTK )
      IF( FTINO ) THEN
         IF(  IPLAN .LT. IPLMIN(MILI)
     +     .OR. IPLAN .GT. IPLMAX(MILI)
     +     .OR. .NOT. INSTK ) THEN
             NRJDP = 0
      ENDIF
         ENDIF
      ENDIF
      IF(NRJDP.GT.0. AND. IPLAN.GT.0) THEN
         NECSG = NECSG + 1
         NSIGNL = NSIGNL - 1
         RW(IPX+1) = POSSG(1)
         RW(IPX+2) = POSSG(2)
         RW(IPX+3) = POSSG(3)
         IW(IPX+4) = NRJDP
         IW(IPX+5) = NRJDP
         IW(IPX+6) = IPLAN
         IPX = IPX + LCOLS(IDPSIG)
      ENDIF
C
      IF(IFIN .GT. 0) GO TO 10
C
 19   CONTINUE
      IW(IDPSIG+2) = NECSG
C
C  Allocate Signals to Towers ans Wires
C
      CALL EHSITW ( ISCMP,MODUL,TYGEO)
C
   20 CONTINUE
      RETURN
      END
