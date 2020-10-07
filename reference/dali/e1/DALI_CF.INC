*CD DALLCO
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C                           No more EXTERNAL statements.
C------------------------------------------------------------------- D0
      COMMON /D0STRT/ TISTD0
      CHARACTER *18 TISTD0
C------------------------------------------------------------------- D0
      COMMON /D0PLAF/ TPLFD0,TWRTD0
      CHARACTER *6 TPLFD0
      CHARACTER *1 TWRTD0
      COMMON /D0PFLG/ FTRMD0,FLOGD0,FPROD0,FSOLD0
      LOGICAL FTRMD0,FLOGD0,FPROD0,FSOLD0
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DA
      COMMON /DAUTOM/ IFILDA
C------------------------------------------------------------------- DA
      PARAMETER (MLINDA=199,MCALDA=-99,MTOTDA=299,MLSTDA=9)
      COMMON /DALCA1/
     &  IALPDA(MCALDA:MLINDA),
     &  ILSTDA(MCALDA:MLINDA),
     &  FSELDA(MCALDA:MLINDA)
      LOGICAL FSELDA
      COMMON /DALCA2/TNAMDA(MCALDA:-1),TLSTDA(0:MLSTDA)
      CHARACTER *2 TLSTDA
      CHARACTER *5 TNAMDA
      COMMON /DALCA3/NCALDA,NLINDA,FMISDA
      LOGICAL FMISDA
      COMMON /DALCA4/IOCODA(0:MLSTDA)
      COMMON /DALCA5/ICORDA(MTOTDA,MCALDA:-1),NCORDA(MCALDA:-1)
      COMMON /DALCA6/TCORDA(MTOTDA,MCALDA:-1)
      CHARACTER *2 TCORDA
      COMMON /DALCA7/NFSTDA(MLSTDA),NLSTDA(MLSTDA)
C     0 = working list, 1 to 9 = alpha lists
      COMMON /DALCA8/FIRODA,TERODA
C------------------------------------------------------------------- DA
      COMMON /DALEF1/ENFLDA
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DB
      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
      PARAMETER ( NCMXB = 16 )
      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
     &                 CORNDB ( 3, NCMXB, NSLTB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C------------------------------------------------------------------- DB
      COMMON /DBRFSP/ FSPIDB
      LOGICAL FSPIDB
C------------------------------------------------------------------- DB
      COMMON /DBOSUN/ LUNEDB
C------------------------------------------------------------------- DB
      COMMON /DBTROE/ FLEYDB
      LOGICAL FLEYDB
C------------------------------------------------------------------- DC
      PARAMETER (MONUDC=10)
      COMMON /DCOLM/ MOTRDC,MOSADC,MOVDDC,MOITDC,MOTPDC,
     &  MOLCDC,MOECDC,MOEODC,MOHCDC,MOHTDC
C------------------------------------------------------------------- DC
      PARAMETER (MCLCDC=2000)
      COMMON /DCOLC/ FDLCDC,FVLCDC,NCLCDC(MCLCDC)
      LOGICAL FDLCDC,FVLCDC
C------------------------------------------------------------------- DC
      PARAMETER (MCITDC=2000)
      COMMON /DCOIT/ FDITDC,FVITDC,NCITDC(MCITDC)
      LOGICAL FDITDC,FVITDC
C------------------------------------------------------------------- DC
C      PARAMETER (MCTPDC=2752)
      PARAMETER (MCTPDC=50000)
      COMMON /DCOTP/ FDTPDC,FVTPDC,NCTPDC(MCTPDC)
      LOGICAL FDTPDC,FVTPDC
      COMMON /DCOTP1/FNOCDC
      LOGICAL FNOCDC
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DC
      PARAMETER (MCECDC=9000)
      COMMON /DCOEC/ FDECDC,FVECDC,NCECDC(MCECDC)
      LOGICAL FDECDC,FVECDC
C------------------------------------------------------------------- DC
      PARAMETER (MCEODC=2000)
      COMMON /DCOEO/ FDEODC,FVEODC,NCEODC(MCEODC)
      LOGICAL FDEODC,FVEODC
C------------------------------------------------------------------- DC
      PARAMETER (MCHCDC=2000)
      COMMON /DCOHC/ FDHCDC,FVHCDC,NCHCDC(MCHCDC)
      COMMON /DCOHC1/ LCHCDC
      LOGICAL FDHCDC,FVHCDC
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
C------------------------------------------------------------------- DC
      COMMON /DCINEC/ FCINDC,FSTRDC,WAITDC
      LOGICAL FCINDC,FSTRDC
C------------------------------------------------------------------- DC
      COMMON /DCOLR1/ MCOLTR
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DD
      COMMON /DDEFC1/ RDBGDD,GRBGDD,BLBGDD
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DE
      COMMON /DEVTIC/NFILDE,IRUNDE(2),IEVTDE(2),LNINDE(2),LCLSDE
      COMMON /DEVTIT/TFINDE(2)
      CHARACTER *80 TFINDE
C------------------------------------------------------------------- DE
      COMMON /DMINIC/FMINDE
      LOGICAL        FMINDE
C------------------------------------------------------------------- DE
      COMMON /DECALC/ RBARDE,ZBARDE(51:115),RENDDE(-6:51),ZENDDE
C      COMMON /DEHORD/ EMINDE,EMAXDE
C------------------------------------------------------------------- DE
      COMMON /DECAEN/ FECEDE,ENECDE(7)
      LOGICAL FECEDE
C------------------------------------------------------------------- DE
      PARAMETER (NEBAM=12,NEECM=12)
      COMMON /DECGEO/ CORADE(3,12,NEECM),CORBDE(3,8,NEBAM),
     &                CORCDE(3,12,NEECM),ETILDE ,
     &                POLADE(2,12,NEECM),POLBDE(2,8,NEBAM),
     &                POLCDE(2,12,NEECM),
     &                RMNBDE ,RMXBDE ,RICBDE ,ROCBDE,
     &                RMNEDE ,RMXEDE ,RICEDE ,RMCEDE ,ROCEDE   
C------------------------------------------------------------------- DE
      PARAMETER ( NSLTE = 16 )
      PARAMETER ( NSLHE =  8 )
      PARAMETER ( NSLIE =  4 , NSLOE =  4 )
      PARAMETER ( NMTPE =  8 )
      PARAMETER ( NCMXE =  8 )
      COMMON /DEMGEO/  ITYPDE ( NSLTE ) , NCORDE ( NMTPE ) ,
     &                 CORNDE ( 3, NCMXE , NSLTE ),
     &                 RMNIDE, RMXIDE, RMNODE, RMXODE
C------------------------------------------------------------------- DE
      COMMON /DEVPAR/ FIMXDE,TEMXDE
C------------------------------------------------------------------- DF
      COMMON /DFARBE/ TCOLDF(0:31)
      CHARACTER *2 TCOLDF
C------------------------------------------------------------------- DF
      PARAMETER (MCOLDF=10)
      COMMON /DFTCLC/ COLIDF(4,MCOLDF),COLMDF(4,3)
C------------------------------------------------------------------- DF
      COMMON /DFNEWF/ FNOFDF
      LOGICAL FNOFDF
C------------------------------------------------------------------- DG
      PARAMETER (MPOSDG=30,MPNWDG=12)
      COMMON /DGKSPC/POSIDG(MPOSDG)
      COMMON /DGKSWT/TWINDG(0:MPNWDG)
      CHARACTER *1 TWINDG
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DH
      COMMON /DHELPT/ TAN1DH,TPRGDH,TLT3DH
      CHARACTER *1 TPRGDH,TLT3DH
      CHARACTER *3 TAN1DH
C------------------------------------------------------------------- DH
      COMMON /DHITRC/ DMAXDH,IHTRDH(6)
C------------------------------------------------------------------- DH
      PARAMETER (NHBAM=12,NHECM=6)
      COMMON /DHCGEO/ CORADH(3,16,NHECM),CORBDH(3,8,NHBAM),
     &                CORCDH(3,16,NHECM), 
     &                POLADH(2,16,NHECM),POLBDH(2,8,NHBAM),
     &                POLCDH(2,16,NHECM),
     &                RMNBDH,RMXBDH,RICBDH,ROCBDH,
     &                RMNEDH,RMXEDH,RMXIDH,RICEDH,RMCEDH,ROCEDH
C------------------------------------------------------------------- DI
      COMMON /DINPUC/ IFILDI,IEVEDI,ZOFFDI,
     1 KVDEDI,KITCDI,KTPCDI,KECADI,KHCADI,KMDEDI
C------------------------------------------------------------------- DI
      COMMON /DITGEO/ RMAXDI,RMINDI,ZMAXDI
C------------------------------------------------------------------- DJ
      COMMON /DJCHEK/ TSUBDJ
      CHARACTER *30 TSUBDJ
C------------------------------------------------------------------- DL
      PARAMETER (MPNPDL=62,MPDVDL=6,MPWDDL=8,MPRADL=8)
      COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
     1  MGRCDL(MPDVDL),MVARDL(MPDVDL,2),MODEDL(MPDVDL,2),
     1  MCOLDL(MPDVDL),MSYMDL(MPDVDL,0:1),
     1  MLABDL(MPDVDL),MDUMDL,
     1  WDSNDL(4,MPWDDL,MPDVDL),RAMPDL(4,MPRADL,MPDVDL,2)
C     COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
C    1  MGRCDL(MPDVDL),
C    1  MCOLDL(MPDVDL),
C    1  MLABDL(MPDVDL),MDUMDL
C------------------------------------------------------------------- DL
      COMMON /DLCGEO/ ZDISDL,RINNDL,ROUTDL,ZLENDL
C------------------------------------------------------------------- DM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
C------------------------------------------------------------------- DM
      COMMON /DMCTRK/ NCTRDM
C------------------------------------------------------------------- DM
      PARAMETER ( NSLTM = 38 )
      PARAMETER ( NSLHM = 19 )
      PARAMETER ( NSLIM = 10 , NSLOM = 9 )
      PARAMETER ( NMTPM = 9  )
      PARAMETER ( NCMXM = 8  )
      COMMON /DMMGEO/  ITYPDM ( NSLTM ) , NCORDM ( NMTPM ) ,
     &                 CORNDM ( 3, NCMXM , NSLTM ),
     &                 RMNIDM, RMXIDM, RMNODM, RMXODM
C------------------------------------------------------------------  DN
      PARAMETER (MTRK=19)
      COMMON /DNTRK1/ NTRKDN,XYZTDN(3,2,MTRK),
     &  ICOLDN(MTRK),IDSHDN(MTRK)
C------------------------------------------------------------------  DN
      COMMON /DNEWEV/ FNLADN,FMHTDN
      LOGICAL FNLADN,FMHTDN
C------------------------------------------------------------------- DK
      COMMON /DKONST/ PIDGDK
      DOUBLE PRECISION PIDGDK
C------------------------------------------------------------------- DK
      COMMON /DKNEWC/ KNECDK(45:52,3,4:5)
C------------------------------------------------------------------- DK
      COMMON /DKNSTC/ PIFCDK,RMAXDK,ZMAXDK,TCO1DK,TCO2DK,
     1   ROTODK(-1:10),ZZTODK(-1:10),ZMTODK(-1:10),DMAXDK,RMINDK,
     1   RLOWDK
C------------------------------------------------------------------- DR
      COMMON /DROVX1/ PAR2DR(4,4)
C------------------------------------------------------------------- DO
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DO
C             IF MPNPDO IS CHANGED CHANGE ALSO MPNPDS
      PARAMETER (MPNPDO=190)
C       If IDLSDO is changed, it must be changed also in DOPRIN!
      COMMON /DOPR3C/ PARADO(4,MPNPDO)
      COMMON /DOPR6C/ PLSTDO(2,MPNPDO)
      COMMON /DOPR7C/ ILSTDO(8)
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO
C------------------------------------------------------------------- DP
      COMMON /DPERSC/ FRPRDP,FBUGDP,FFDRDP
      LOGICAL FRPRDP,FBUGDP,FFDRDP
C------------------------------------------------------------------- DP
      PARAMETER (MAXPDP=120,M11PDP=131)
      COMMON /DPICT/ TPICDP(-10:MAXPDP)
      CHARACTER *2 TPICDP
C------------------------------------------------------------------- DP
      COMMON /DPICC/ MINPDP
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
      COMMON /DPICK1/ IPTNDP
      LOGICAL FPIKDP,FPIMDP
C------------------------------------------------------------------- DP
      PARAMETER (MXYZDP=19)
      COMMON /DPICK2/ NXYZDP,XYZPDP(3,19)
      COMMON /DPICK3/ NTRKDP,LTRKDP(8)
      COMMON /DPICK4/ NUMPDP,LORPDP
      PARAMETER (MTRSDP=26)
      COMMON /DPICK5/ TRSTDP(MTRSDP)
C------------------------------------------------------------------- DP
      COMMON /DPCOLR/ ICOLDP
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DR
      COMMON /DRESIC/ NPNTDR,FGRYDR,PARADR(4,3)
      LOGICAL FGRYDR
C------------------------------------------------------------------- DS
      COMMON /DSCALE/ SCALDS(0:12,3)
      PARAMETER (MSECDS=1,MSHCDS=2,MSWIDS=3)
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C------------------------------------------------------------------- DS
      COMMON /DSYMBC/ NTRNDS
C------------------------------------------------------------------- DS
      PARAMETER (NSEC=36)
      COMMON /DSLDEX/ NSIODS,NSECDS(NSEC),DEDXDS(NSEC),ERORDS(NSEC),
     &  NSAMDS(NSEC),TRLGDS(NSEC)
C------------------------------------------------------------------- DS
      COMMON /DSAGEO/ ZDISDS,RINNDS,ROUTDS,ZLENDS,FESADS
      LOGICAL FESADS
C------------------------------------------------------------------- DS
      COMMON /DSIGEO/ ZAMIDS,ZAMADS,RAMIDS,RAMADS,
     &                ZBMIDS,ZBMADS,RBMIDS,RBMADS,FESIDS
      LOGICAL FESIDS
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DT
      COMMON /DTESCT/ TREVDT,TULIDT,TNORDT
      CHARACTER *4 TREVDT,TNORDT
      CHARACTER *8 TULIDT
C------------------------------------------------------------------- DT
      COMMON /DTRAKC/ FORDDT,NORDDT(999),NCOLDT(999)
      COMMON /DTRAKT/ ULABDT(999),COLRDT(999)
      COMMON /DTRAKF/ FHTRDT,FCOLDT
      LOGICAL FORDDT,FHTRDT,FCOLDT
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=100,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C------------------------------------------------------------------- DT
      COMMON /DTPGEO/ FISTDT(3),
     &       CORNDT (2,10,6,3), POLCDT ( 2,10,3 ),
     &       CRNGDT (2,10,6,3),
     &       NTYPDT,NCORDT(3),NSLTDT,
     &       RMAXDT , RMINDT , ZMAXDT
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DU
      COMMON /DUSD2C/NUN6DU,NUN9DU
      PARAMETER (MUN5DU=5,MUN6DU=6,MUN9DU=9)
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DV
      COMMON /DVARIT/ TVRNDV(0:20)
      CHARACTER *2 TVRNDV
C------------------------------------------------------------------- DV
      COMMON /DVRTDV/ FNEVDV,MOCADV,FVXYDV,FVZZDV,VRTXDV,VRTYDV,VRTZDV,
     &  VRDZDV,QTETDV
      LOGICAL FVXYDV,FVZZDV,FNEVDV
C------------------------------------------------------------------- DV
C      PARAMETER (MTPCDV=2752)
      PARAMETER (MTPCDV=50000)
      COMMON /DVSTPC/ VTPCDV(11,MTPCDV)
      DIMENSION NTPCDV(11,MTPCDV)
      EQUIVALENCE (VTPCDV,NTPCDV)
      PARAMETER (MITCDV=960)
      COMMON /DVSITC/ VITCDV(11,MITCDV)
      DIMENSION NITCDV(11,MITCDV)
      EQUIVALENCE (VITCDV,NITCDV)
C------------------------------------------------------------------- DV
      COMMON /DVDGEO/ NSLIDV,NSLODV,NWAFDV,PALWDV,ZALWDV,
     &                TIILDV(15),TIOLDV(15),
     &                RMNIDV,RMXIDV,RMNODV,RMXODV,
     &                DELXDV,DELYDV,DELZDV,DELPDV,
     &                ZOFWDV(6), NWTSDV(15,2),
     &                RAILDV,RAOLDV,DFILDV(12),DFOLDV(15),
     &                CRILDV(2,2,15),CROLDV(2,2,15),
     &                NFRSDV(2),NZRSDV(2),
     &                PFRSDV(2),PZRSDV(2)
C     HARD WIRED IN P_DALB_ALEPH.FOR
      COMMON /DVDGE1/ FEVDDV
      LOGICAL FEVDDV
C------------------------------------------------------------------- DV
      COMMON /DVDDFZ/ DLSTDV,NTOTDV(2),
     &  FIMDDV(-2:18,2),DLFIDV(-2:18,2),
     &  ZLFTDV(6,2),ZRGTDV(6,2)
      COMMON /DVDLTF/ FISCDV
C------------------------------------------------------------------- DW
      PARAMETER (MPOSDW=30,MPNWDW=12,MNUWDW=14)
      COMMON /DWINDC/NWINDW(0:MPNWDW,0:MPNWDW),
     &  NEXTDW(0:MPNWDW),LASTDW(0:MPNWDW),
     &  POSIDW(MPOSDW)
      COMMON /DWINDO/TWINDW(0:MNUWDW)
      CHARACTER *1 TWINDW
C------------------------------------------------------------------- DW
      COMMON /DWINSU/ WISUDW
C------------------------------------------------------------------- DW
      COMMON /DWORKC/ WORKDW(3000)
      DIMENSION NWRKDW(3000)
      EQUIVALENCE (WORKDW,NWRKDW)
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DW
      COMMON /DWARFL/ FNEWDW
      LOGICAL FNEWDW
C------------------------------------------------------------------- DW
      COMMON /DWIZCC/ FWIZDW,FCONDW
      LOGICAL FWIZDW,FCONDW
C------------------------------------------------------------------- DW
      COMMON /DWUSED/  JWUSDW
C------------------------------------------------------------------- DZ
      COMMON/DZOOMC/ QASRDZ,MOD1DZ,MOD2DZ,NDIRDZ,FSTRDZ,NWINDZ
      LOGICAL FSTRDZ
C--------------------------------------------------------------------
