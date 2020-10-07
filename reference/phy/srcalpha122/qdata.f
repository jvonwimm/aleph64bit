      SUBROUTINE QDATA
CKEY INIT /INTERNAL
C----------------------------------------------------------------------
C! quasi BLOCK DATA
C as long as this routine is not too long, avoid a real BLOCK DATA.
C called from : QMINIT
C                                                   H.Albrecht 22.09.88
C                                                   E. Blucher 10.05.89
C                                                   G. Graefe  29.04.94
C----------------------------------------------------------------------
C!  ALPHA Current version number and correction version
      REAL ALFAVR
      PARAMETER ( ALFAVR = 122.00 )
C ALPHA Version 122   Released 29 April 1996
C Start of QCDESH ----------------------- Description in QDATA ---------
      PARAMETER (KCQDET=34, KCQFPA=8, KCQPAR=10, KCQVEC=73, KCQVRT=30,
     & KHE=1, KHMU=2, KHPI=3, KHK=4, KHP=5, KHPHOT=1, KHNHAD=2,
     & KMONTE=-2, KRECO=-1, KLOCKM=14, KSOVT=1, KSCHT=2, KSIST=3,
     & KSAST=4, KSEHT=5, KSV0T=6, KSDCT=7, KSEFT=8, KSNET=9, KSGAT=10,
     & KSJET=11, KSMCT=12, KSREV=13,
     & KSMCV=14, KUNDEF=-12344321, QQPI=3.141593, QQE=2.718282,
     & QQ2PI=QQPI*2., QQPIH=QQPI/2., QQRADP=180./QQPI, QQC=2.997925E10,
     & QQH=6.582173E-25, QQHC=QQH*QQC, QQIRP=.00029979)
      CHARACTER CQDATE*8, CQDWRK*80, CQFHIS*80, CQFOUT*80, CQFWRK*80,
     & CQHTIT*80, CQSEC*3, CQTIME*8, CQUNPK*30, CQVERS*6, CQRLST*800,
     & CQELST*800
      COMMON /QCDEC/ CQDATE, CQDWRK, CQFHIS, CQFOUT, CQFWRK, CQHTIT,
     & CQSEC(14), CQTIME, CQUNPK, CQVERS, CQRLST, CQELST
      COMMON /QCDE/ QELEP, QMFLD ,QMFLDC, QTIME, QTIMEI, QTIMEL,
     & QTIMES, QTIMET, QDHEEC, QDHEEL, QDHEPF, QDHETH, QDHEPH, QDHEEF,
     & QDHEET, QDHET1, QDHEP1, QDHET2, QDHEP2, QDHEE1, QDHEE2, QDHEE3,
     & QKEVRN, QKEVWT, QVXNOM, QVYNOM, QVZNOM, QVXNSG, QVYNSG, QVZNSG,
     & QINLUM, QRINLN, QRINLU, QDBOFS, QEECWI(36), QVXBOM, QVYBOM,
     & QSILUM, QRSLLU, QRSLBK, QRSLEW , QVTXBP(3), QVTEBP(3), QVTSBP(3)
      COMMON /QCDE/  KFOVT, KLOVT, KNOVT, KFCHT, KLCHT, KNCHT, KFIST,
     & KLIST, KNIST, KFAST, KLAST, KNAST, KFEHT, KLEHT, KNEHT, KFV0T,
     & KLV0T, KNV0T, KFDCT, KLDCT, KNDCT, KFEFT, KLEFT, KNEFT, KFNET,
     & KLNET, KNNET, KFGAT, KLGAT, KNGAT, KFJET, KLJET, KNJET, KFMCT,
     & KLMCT, KNMCT, KFREV, KLREV, KNREV, KFMCV, KLMCV, KNMCV, KLUST,
     & KLUSV, KFFRT, KLFRT, KFFRV, KLFRV, KNRET, KNCOT, KFFRD,
     & KLFJET,KLLJET,KLNJET
      COMMON /QCDE/ KBIT(32), KCLACO(KCQFPA), KCLAFR(KCQFPA), KCLARM
     & (KCQFPA), KDEBUG, KEVT, KEXP, KFFILL, KFEOUT, KJOPTN(2,2),
     & KLEOUT, KLROUT, KLOCK0(KLOCKM,2), KLOCK1(KLOCKM,2), KLOCK2(
     & KLOCKM,2), KMATIX(6,6), KMQFPA, KNEFIL, KNEOUT, KNEVT, KNPAST,
     & KNQDET, KNQFPA, KNQLIN, KNQMTX, KNQPAR, KNQPST, KNREIN, KNREOU,
     & KOQDET, KOQFPA, KOQLIN, KOQMTL, KOQMTS, KOQPAR, KOQPBT, KOQPLI,
     & KOQTRA, KOQVEC, KOQVRT, KQPAR, KQVEC, KQVRT, KQWRK, KQZER, KRUN,
     & KSTATU, KTDROP, KUCARD, KUCONS, KUHIST, KUINPU, KUOUTP, KUPRNT,
     & KUPTER, KDEBU1, KDEBU2, KNWRLM, KEFOPT, KUEDIN, KUEDOU, KURTOX,
     & KUCAR2, KNHDRN, KNBHAB, KSBHAB, KRSLLQ, KRSLNB
      COMMON /QCDE/ INDATA
      COMMON /QCDE/ KRINNE, KRINNF, KRINDC, KRINDQ, KRINNZ, KRINNB,
     & KRINBM, KRINFR, KRINLR, KRINLF
      COMMON /QCDE/ KEVERT, KEVEDA, KEVETI, KEVEMI(4), KEVETY, KEVEES,
     & KDHEFP, KDHENP, KDHENM, KKEVNT, KKEVNV, KKEVID, KDHENX, KDHENV,
     & KDHENJ, KREVDS, KXTET1, KXTET2, KXTEL2, KXTCGC, KXTCLL, KXTCBN,
     & KXTCCL, KXTCHV, KXTCEN, KCLASW, KERBOM, KBPSTA
      DIMENSION KLOCUS(3,14)
      EQUIVALENCE (KLOCUS(1,1),KFOVT), (KFOVT,KFRET), (KLIST,KLRET),
     & (KFIST,KFCOT), (KLAST,KLCOT)
      COMMON /QCDE/ XCOPYJ, XFLIAC, XHISTO, XLREV(2), XLREV2(2), XMCEV,
     & XMINI, XSYNTX, XWREVT, XWRRUN, XFILMC, XFILCH, XFILV0, XFILCO,
     & XFILEF, XFILPC, XFILGA, XFILJE,
     & XPRHIS, XFILL, XVITC, XVTPC, XVECAL, XVLCAL, XVTPCD,
     & XVSATR, XVHCAL, XHVTRG, XSREC, XWMINI, XIOKLU, XIOKSI, XFRF2,
     & XNSEQ, XVDOK, XFRF0, XFMUID, XFILEM, XWNANO, XROKSI, XGETBP,
     & XJTHRU
      LOGICAL XCOPYJ, XFLIAC, XHISTO, XLREV, XLREV2, XMCEV, XMINI,
     & XSYNTX, XWREVT, XWRRUN, XFILMC, XFILCH, XFILV0, XFILCO, XFILEF,
     & XFILPC, XPRHIS, XFILL, XVITC, XVTPC, XVECAL, XVLCAL, XVTPCD,
     & XVSATR, XVHCAL, XHVTRG, XSREC, XWMINI, XIOKLU, XFRF2, XFILJE,
     & XNSEQ, XFILGA, XVDOK, XFRF0, XFMUID, XFILEM, XWNANO, XIOKSI,
     & XROKSI, XGETBP, XJTHRU
C-------------------- /NANCOM/ --- NanoDst steering -------------------
C! XNANO   .TRUE. if input is a NanoDst (in NANO or EPIO format, dependi
C!                   XNANOR)
C!
      LOGICAL XNANO
      COMMON /NANCOM/XNANO
C--------------------- end of NANCOM ----------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
C------------------ /QCNAMI/ --- name indices -------------------------
      COMMON /QCNAMI/ NAAFID,NAAJOB,NAASEV,NABCNT,NABHIT,NABOMB,
     1 NABOME,NABOMP,NABOMR,NABPTR,NADCAL,NADCRL,NADECO,NADEID,NADENF,
     2 NADEVT,NADEWI,NADFMC,NADFOT,NADGAM,NADHCO,NADHEA,NADHRL,NADJET,
     3 NADMID,NADMJT,NADMUO,NADNEU,NADPOB,NADRES,NADTBP,NADTMC,NADTRA,
     4 NADVER,NADVMC,NAECRQ,NAECTE,NAEGID,NAEGPC,NAEIDT,NAEJET,NAERRF,
     5 NAETDI,NAETKC,NAEVEH,NAEWHE,NAFICL,NAFKIN,NAFPOI,NAFPOL,NAFRFT,
     6 NAFRID,NAFRTL,NAFSTR,NAFTCL,NAFTCM,NAFTOC,NAFTTM,NAFVCL,NAFVER,
     7 NAFZFR,NAHCCV,NAHCTE,NAHINF,NAHMAD,NAHPDI,NAHROA,NAHSDA,NAHTUB,
     8 NAIASL,NAIPJT,NAIRJT,NAITCO,NAITMA,NAIXTR,NAIZBD,NAJBER,NAJEST,
     9 NAJSUM,NAKEVH,NAKINE,NAKJOB,NAKLIN,NAKPOL,NAKRUN,NALIDT,NALOLE,
     A NALUPA,NAMCAD,NAMHIT,NAMTHR,NAMUDG,NAMUEX,NAOSTS,NAPART,NAPASL,
     B NAPCHY,NAPCOB,NAPCOI,NAPCPA,NAPCQA,NAPCRL,NAPECO,NAPEHY,NAPEID,
     C NAPEMH,NAPEPT,NAPEST,NAPEWI,NAPFER,NAPFHR,NAPFRF,NAPFRT,NAPHCO,
     D NAPHER,NAPHHY,NAPHMH,NAPHST,NAPIDI,NAPITM,NAPLID,NAPLPD,NAPLSD,
     E NAPMSK,NAPNEU,NAPPDS,NAPPOB,NAPPRL,NAPRTM,NAPSCO,NAPSPO,NAPSTR,
     F NAPT2X,NAPTBC,NAPTCO,NAPTEX,NAPTMA,NAPTML,NAPTNC,NAPTST,NAPVCO,
     G NAPYER,NAPYFR,NAPYNE,NAQDET,NAQFPA,NAQLIN,NAQMTL,NAQMTS,NAQPAR,
     H NAQPBT,NAQPLI,NAQTRA,NAQVEC,NAQVRT,NAQWRK,NAQZER,NAREVH,NARHAH,
     I NARTLO,NARTLS,NARUNH,NARUNR,NASFTR,NATEXS,NATGMA,NATMTL,NATPCO,
     J NAVCOM,NAVCPL,NAVDCO,NAVDHT,NAVDXY,NAVDZT,NAVERT,NAVFHL,NAVFLG,
     K NAVFPH,NAVHLS,NAVPLH,NAX1AD,NAX1SC,NAX1TI,NAX2DF,NAX3EC,NAX3EW,
     L NAX3HC,NAX3IT,NAX3L2,NAX3LU,NAX3TM,NAX3TO,NAX3TP,NAX3X3,NAXTBN,
     M NAXTBP,NAXTCN,NAXTEB,NAXTOP,NAXTRB,NAYV0V,NAZPFR,NAEFOL,NAMUID,
     N NAPGID,NAPGPC,NAPGAC,NAPMSC,NAPTHR,NANBIP,NAPDLT,NAPMLT,NAPLJT
C--------------------- end of QCNAMI ----------------------------------
C------------------ /QQQQJJ/ --- HAC parameters for ALPHA banks -------
      PARAMETER (JQVEQX= 1,JQVEQY= 2,JQVEQZ= 3,JQVEQE= 4,JQVEQM= 5,
     & JQVEQP= 6,JQVECH= 7,JQVETN= 8,JQVESC= 9,JQVEKS=10,JQVECL=11,
     & JQVEPA=12,JQVEQD=13,JQVENP=14,JQVESP=15,JQVEOV=16,JQVEEV=17,
     & JQVEND=18,JQVEDL=19,JQVENO=20,JQVEOL=21,JQVENM=22,JQVEML=23,
     & JQVEBM=24,JQVELK=38,JQVEDB=39,JQVEZB=40,JQVESD=41,JQVESZ=42,
     & JQVECB=43,JQVEEM=44,JQVECF=54,JQVEEW=55,JQVEUS=56)
      PARAMETER ( JQVRVX=1,JQVRVY=2,JQVRVZ=3,JQVRVN=4,JQVRTY=5,
     1   JQVRIP=6,JQVRND=7,JQVRDL=8,JQVRAY=9,JQVRAF=10,JQVREM=11,
     2   JQVRCF=17,JQVRET=18)
      PARAMETER ( JQDEAF= 1,JQDEAL= 2,JQDENT= 3,JQDEAT= 4,JQDELT= 8,
     &  JQDEAE= 9,JQDEAH=10,JQDEAM=11,JQDECF=12,JQDEEC=13,JQDEHC=14,
     &  JQDEET=15,JQDEFI=16,JQDENF=17,JQDEFL=18,JQDENE=19,JQDEEL=20,
     &  JQDENH=21,JQDEHL=22,JQDELH=23,JQDEEF=24,JQDEPC=25,JQDEEG=26,
     &  JQDEMU=27,JQDEDX=28,JQDEPG=29,JQDEPD=30,JQDEPM=31)
      PARAMETER ( JQPAGN=1, JQPANA=2, JQPACO=5, JQPAMA=6, JQPACH=7,
     & JQPALT=8,JQPAWI=9,JQPAAN=10)
C--------------------- end of QCDESH ----------------------------------
      COMMON / ALFORMA / LUFORM,IFALARM
C----------------------------------------------------------------------
C
C CQDATE Date at start of job. Set in QADATI.
C CQDWRK Working space for device names.
C CQFHIS Histogram output file name. Set in QMIHIS.
      CQFHIS = ' '
C CQFOUT Event output file name. Set in QMCARD.
      CQFOUT = ' '
C CQFWRK Working space for file names.
C CQHTIT General title for histogram editing. Set in QMIHIS.
      CQHTIT = ' '
C CQPART External character function.
C CQSEC  Section names in QVEC, QVRT.
      CQSEC( 1) = 'OVT'
      CQSEC( 2) = 'CHT'
      CQSEC( 3) = 'IST'
      CQSEC( 4) = 'AST'
      CQSEC( 5) = 'EHT'
      CQSEC( 6) = 'V0T'
      CQSEC( 7) = 'DCT'
      CQSEC( 8) = 'EFT'
      CQSEC( 9) = 'NET'
      CQSEC(10) = 'GAT'
      CQSEC(11) = 'JET'
      CQSEC(12) = 'MCT'
      CQSEC(13) = 'REV'
      CQSEC(14) = 'MCV'
C CQTIME Time at start of job. Set in QADATI.
C CQTPN  External character function.
C CQUNPK unpack flag for AUNPCK. Set in QMCARD (from UNPK data card).
      CQUNPK = 'EC TE HC MU FI '
C CQVERS 3 digit ALPHA version number
      IVNU=NINT(ALFAVR)
      IVCO=NINT(100.*(ALFAVR-FLOAT(IVNU)))
      CALL BKCARI(IVNU,3,CQVERS(1:3))
      CQVERS(4:4)='.'
      CALL BKCARI(IVCO,2,CQVERS(5:6))
C
C KBIT   Bits 1 ... 32.
      DO 10 N=1,32
        KBIT(N) = 0
   10 CALL SBIT1 (KBIT(N),N)
C
C KBPSTA   Set in QMNEWR
C KCLACO Integer class code
      KCLACO(1)  = -1
      KCLACO(2)  = -2
      KCLACO(3)  = KUNDEF
      KCLACO(4)  = KUNDEF
      KCLACO(5)  = KUNDEF
      KCLACO(6)  = KUNDEF
      KCLACO(7)  = KUNDEF
      KCLACO(8)  = KUNDEF
C
C KCLAFR Original class
      KCLAFR(1) = 1
      KCLAFR(2) = 2
C
C KCLARM RECO (1) or MC (2) class
      KCLARM(1) = 1
      KCLARM(2) = 2
C
C KCQDET Number of columns in bank QDET (Fortran parameter).
C KCQFPA Number of columns in bank QFPA (Fortran parameter).
C KCQPAR Number of columns in bank QPAR (Fortran parameter).
C KCQVEC Number of columns in bank QVEC (Fortran parameter).
C KCQVRT Number of columns in bank QVRT (Fortran parameter).
C
C
C KDEBUG Debug level, first event to debug,
C   last event to debug.   Set in QMCARD (from DEBU card).
      KDEBUG = 1
      KDEBU1 = 1
      KDEBU2 = 99999999
C
C KEFOPT Choice of energy flow option
      KEFOPT = 0
C KEVExx Content of bank EVEH. Set in QFHEAD.
C KERBOM  BOM vertex position error code.
      KERBOM = 0
C KEVT   Current event number. Set in QMREAD from EVEH bank.
      KEVT = 0
C KEXP   Experiment number. Set in QFHEAD from KEVH bank.
      KEXP = 0
C
C KDHExx Content of bank DHEA. Set in QFHEAD.
C
C KFCOT  First CAL object. Equivalenced with KFIST.
C KFFILL QFILL already called ? (1 = yes, 0 = no). Set in QMREAD/QFILL.
      KFFILL = 0
C KFEOUT Event output flag : -1 : no output
C                             0 : ALLRUN option
C                             1 : NORUNR option
C                             2 : SELRUN option. Set in QMCARD.
      KFEOUT = -1
C
C KFFRD  First free spot in QDET.
      KFFRD = 1
C
C KFFRT  First free track.
      KFFRT = 1
C KFFRV  First free vertex.
      KFFRV = 1
C KFRET  First reconstructed track. Equivalenced with KFOVT.
C KFxxx  First row of section xxx (xxx corresponds to CQSEC).
C        Equivalenced with KLOCUS(1,nsec). Set in QFxxxx routines.
C
C KKEVxx Content of bank KEVH. Set in QFHEAD.
C
C KJOPTN Options for QJ routines; set in QMCLR and QJOPTx
C KLCOT  Last CAL object. Equivalenced with KLAST.
C KLEOUT Last output event. Set in QWRITE.
      KLEOUT = 0
C
C KLFRT  Last free track.
C KLFRV  Last free vertex.
C KLOCKM Number of bit masks in QVEC (Fortran parameter).
C KLOCUS Segmentation of QVEC,QVRT. Set in QFxxxx routines.
C KLOCK0 Combined lock bit mask. Cleared in QMCLR. Set in QLxxx.
C KLOCK1 Lock bit mask 1. Cleared in QMCLR. Set in QLxxx.
C KLOCK2 Lock bit mask 2. Cleared in QMCLR. Set in QLxxx.
C KLOVT  Last track in section KSOVT.
C KLROUT Last output run. Set in QWRITE.
      KLROUT = 0
C KLRET  Last reconstructed track. Equivalenced with KLIST.
C KLUST  Last user track.
      KLUST = 0
C KLUSV  Last user vertex.
      KLUSV = 0
C KLxxx  Last row of section xxx (xxx corresponds to CQSEC).
C        Equivalenced with KLOCUS(2,nsec). Set in QFxxxx routines.
C
C KMATIX Index of quadr. matrix --> index in triangular matrix.
C        See below.
C KMONTE Class number of MC class (Fortran parameter).
C KMQFPA number of columns currently used in bank QFPA.
      KMQFPA = 2
C
C KNCOT  Number of CAL objects. Set in QFILL.
C KNBHAB Number of method 5 Bhabha events.
      KNBHAB = 0
C KSBHAB Number of SICAL Bhabha events.
      KSBHAB = 0
C KNEFIL Number of events read from the current file. Set in QMREAD.
      KNEFIL = 0
C KNEOUT Number of events written to the output file. Set in QWRITE.
      KNEOUT = 0
C KNHDRN Number of hadronic events.
      KNHDRN = 0
C KNWRLM Maximum number of events to write out.
      KNWRLM = 99999999
C KNEVT  Number of events read in. Set in QMREAD.
      KNEVT  = 0
C KNPAST Number of standard particles on MC table. Set in QMNEWR.
      KNPAST = 0
C KNQDET Number of rows currently used in bank QDET.
      KNQDET = 0
C KNQFPA Number of rows currently used in bank QFPA.
      KNQFPA = 0
C KNQLIN Number of words currently used in bank QLIN.
      KNQLIN = 0
C KNQMTX Number of words currently used in banks QMTL and QMTS.
      KNQMTX = 0
C KNQPAR Number of rows currently used in bank QPAR.
      KNQPAR = 0
C KNQPST Number of standard particles. Set in QMPART.
      KNQPST = 0
C KNREIN Number of records on current event input file. Set in QMREAD.
      KNREIN = 0
C KNREOU Number of records on current event output file. Set in QWRITE.
      KNREOU = 0
C KNRET  Number of reconstructed tracks. Set in QFILL.
C KNxxx  Number of rows in section xxx (xxx corresponds to CQSEC).
C        Equivalenced with KLOCUS(3,nsec). Set in QFxxxx routines.
C
C KOQDET Current offset of bank QDET + LMHLEN. Set in QMCLR.
C KOQFPA Current offset of bank QFPA + LMHLEN - KCQFPA. Set in QMCLR.
C KOQLIN Current offset of bank QLIN + LMHLEN. Set in QMCLR.
C KOQMTL Current offset of bank QMTL + LMHLEN. Set in QMCLR.
C KOQMTS Current offset of bank QMTS + LMHLEN. Set in QMCLR.
C KOQPAR Current offset of bank QPAR + LMHLEN - KCQPAR. Set in QMCLR.
C KOQPBT Current offset of bank QPBT + LMHLEN. Set in QMCLR.
C KOQPLI Current offset of bank QPLI + LMHLEN. Set in QMCLR.
C KOQTRA Current offset of bank QTRA + LMHLEN. Set in QMCLR.
C KOQVEC Current offset of bank QVEC + LMHLEN - KCQVEC. Set in QMCLR.
C KOQVRT Current offset of bank QVRT + LMHLEN - KCQVEC. Set in QMCLR.
C
C KQPAR  Current address of bank QPAR. Set in QMCLR.
C KQVEC  Current address of bank QVEC. Set in QMCLR.
C KQVRT  Current address of bank QVRT. Set in QMCLR.
C
C KRECO  Class number of RECO class (Fortran parameter).
C KREVDS  Detector status
      KREVDS=0
C KRUN   Current run number. Set in QMREAD from EVEH bank.
      KRUN = 0
C
C KSCHT  Sector number for charged tracks (Fortran parameter).
C KSECT  Sector number for ECAL tracks (Fortran parameter).
C KSHCT  Sector number for HCAL tracks (Fortran parameter).
C KSEFT  Sector number for EFLOW (Fortran parameter).
C KSGAT  Sector number for EGPC objects (Fortran parameter).
C KSJET  Sector number for JETS (Fortran parameter).
C KSMCT  Sector number for MC tracks (Fortran parameter).
C KSMCV  Sector number for MC vertices (Fortran parameter).
C KSREV  Sector number for reconstructed vertices (Fortran parameter).
C KSTATU Program status : -1 = init, 0 = event proc ; 1 = term.
      KSTATU = -1
C KSV0T  Sector number for V0 tracks (Fortran parameter).
C
C KTDROP Pointer to next dropped track
C KXxxxx - trigger variables. Filled in QFHEAD.
      KXTET1=0
      KXTET2=0
      KXTEL2=0
      KXTCGC=0
      KXTCLL=0
      KXTCBN=0
      KXTCCL=0
      KXTCHV=0
      KXTCEN=0
C
C KUCARD Logical unit: card input.
      KUCARD = 7
C KUCAR2 Logical unit: next card input.
      KUCAR2 = 8
C KUEDIN Logical Unit for EDIR input- set in QMREAD
C KUEDOU Logical Unit for EDIR output- set in QMREAD
      KUEDIN=0
      KUEDOU=0
C KUCONS Logical unit : data base - set in QMINIT. (default=unit 4)
C KUHIST Logical unit : histogram output.
      KUHIST = 15
C KURTOX Logical unit : hist. conversion on CRAY
      KURTOX = 16
C KUINPU Logical unit : event input.
      KUINPU = 20
C KUOUTP Logical unit : event output.
      KUOUTP = 50
C KUPRNT Logical unit : log file.
      KUPRNT = 6
C KUPTER Logical unit : terminal (may be set to 0 in QAINTA).
      KUPTER = 76
C LUFORM : Logical unit to read BOS bank formats :
      LUFORM = 93
      IFALARM = 0
C QDBOFS: D0 offset
      QDBOFS = 0.
C QEECWI:ECAL wire energy by module
      DO 20 IDUM=1,36
        QEECWI(IDUM)=0.
   20 CONTINUE
C
C QELEP  LEP energy = 2 * beam energy.
      QELEP = 0.
C QDHExx Content of bank DHEA. Set in QFHEAD.
C QINLUM Integrated LCAL luminosity
      QINLUM = 0.
C QSILUM Integrated SICAL luminosity
      QSILUM = 0.
C QKEVxx Content of bank KEVH. Set in QFHEAD.
C QMFLD  Mag. field (kGauss). Set in QMNEWR.
C QMFLDC Mag. field (GeV/cm). Set in QMNEWR.
C QQxxxx Constants (Fortran parameters).
C
C QTIME  Time given on TIME card. Set in QMCARD.
      QTIME  = 65.
C QTIMEI Time at the end of QMINIT.
      QTIMEI = 0.
C QTIMEL Remaining job time. Set in QMREAD.
      QTIMEL = 9999.
C QTIMES Time at start of job. Set in QADATI.
      QTIMES = 0.
C QTIMET Time at the begin of QMTERM.
      QTIMET = 0.
C QVXBOM vertex position from BOMs.
      QVXBOM = 0.
      QVYBOM = 0.
C QVXXXX measured vertex positions and sigmas.
      QVXNOM = 0.
      QVYNOM = 0.
      QVZNOM = 0.
      QVXNSG = 0.
      QVYNSG = 0.
      QVZNSG = 0.
C XCEQAN Logical external function
C XCEQOR Logical statement function
C XCEQU  Logical statement function
C XCOPYJ Copy job ? Set in QMCARD (from COPY card).
      XCOPYJ = .FALSE.
C
C QFILL control flags
C
      XFILL  = .TRUE.
      XFILMC = .TRUE.
      XFILCH = .TRUE.
      XFILV0 = .TRUE.
      XFILEF = .FALSE.
C     XFILCO   (TRUE) set in QMCARD
C     XFILPC   (TRUE) set in QMCARD
C     XFILGA   (TRUE) set in QMCARD
C     XFILEM   (TRUE) set in QMCARD
C XJTHRU controls thrust axis determination
      XJTHRU = .FALSE.
C XFRF0  Card to select NR=0 FRFT
      XFRF0=.FALSE.
C XFRF2  Card to select NR=2 FRFT
      XFRF2=.TRUE.
C XEID   Logical statement funtion.
C XFLIAC Interactive job ? Set in QAINTA.
      XFLIAC = .FALSE.
C XFRF   Logical statement funtion.
C XHISTO Histogram output ? Sei in QMIHIS.
      XHISTO = .FALSE.
C XPRHIS Print histograms to line printer? Set in QMCARD
      XPRHIS = .TRUE.
C XHMA   Logical statement funtion.
C XHVTRG  HV and trigger OK -- set in QFHEAD.
      XHVTRG=.FALSE.
C XIOKLU  TRUE if all LCAL luminosity information OK in database
      XIOKLU=.TRUE.
C XIOKSI  TRUE if all SICAL luminosity information OK in database
      XIOKSI=.TRUE.
C XROKSI  TRUE if at least 1 run with possible SICAL information
      XROKSI=.FALSE.
C XLOCK  Logical statement funtion.
C XLOCKN Logical statement funtion.
C XLREV  QLREV called ?
C XLREV2 QLREV2 called ?
C XMCA   Logical statement funtion.
C XMCEV  Is event a MC event ? Set in QFIMCD.
      XMCEV  = .FALSE.
C XMINI  Input from MINI DST ? Set in QFILL.
      XMINI = .FALSE.
C XNANO  Flag .TRUE. if NANO is being read - set in QFILL
      XNANO=.FALSE.
C XNSEQ  Seq. input
      XNSEQ=.FALSE.
C XPEQAN External logical function
C XPEQOR External logical function
C XPEQU  External logical function
C XSAME  Logical statement funtion.
C XSREC  Write special records to output file
      XSREC = .FALSE.
C XSYNTX Syntax flag. Set in QMCARD ( = SYNT data card).
      XSYNTX = .FALSE.
C XTEX   Logical statement funtion.
C XWNANO  Flag .TRUE. if NANO is being written
      XWNANO=.FALSE.
C XWREVT QWRITE already called for this event ? Cleared in QMREAD.
      XWREVT = .FALSE.
C XWRRUN Output run record in the next QWRITE call ? Set in QMREAD.
      XWRRUN = .FALSE.
C
C         KMATIX
C
      KMATIX(1,1) = 0
      KMATIX(2,1) =     1
      KMATIX(3,1) =         3
      KMATIX(4,1) =             6
      KMATIX(5,1) =                10
      KMATIX(6,1) =                    15
      KMATIX(1,2) = 1
      KMATIX(2,2) =     2
      KMATIX(3,2) =         4
      KMATIX(4,2) =             7
      KMATIX(5,2) =                11
      KMATIX(6,2) =                    16
      KMATIX(1,3) = 3
      KMATIX(2,3) =     4
      KMATIX(3,3) =         5
      KMATIX(4,3) =             8
      KMATIX(5,3) =                12
      KMATIX(6,3) =                    17
      KMATIX(1,4) = 6
      KMATIX(2,4) =     7
      KMATIX(3,4) =         8
      KMATIX(4,4) =             9
      KMATIX(5,4) =                13
      KMATIX(6,4) =                    18
      KMATIX(1,5) = 10
      KMATIX(2,5) =    11
      KMATIX(3,5) =        12
      KMATIX(4,5) =            13
      KMATIX(5,5) =                14
      KMATIX(6,5) =                    19
      KMATIX(1,6) = 15
      KMATIX(2,6) =    16
      KMATIX(3,6) =        17
      KMATIX(4,6) =            18
      KMATIX(5,6) =                19
      KMATIX(6,6) =                    20
C
      END
