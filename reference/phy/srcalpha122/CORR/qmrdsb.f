      SUBROUTINE QMRDSB(IEND)
CKEY EVENT /INTERNAL
C----------------------------------------------------------------------
C! read next event.
C  called from QMREAD
C                                        Author:    H.Albrecht 20.09.88
C                                        Modified:  E.Blucher  01.06.89
C                                        Modified:  M.Talby    17.07.89
C                                        Modified:  E.Blucher  27.07.90
C                                        Modified:  G.Graefe   22.04.94
C                                        Modified:  G.Graefe   13.07.94
C                                        Modified:  T.Oest     26.07.95
C----------------------------------------------------------------------
      SAVE LNEWR,IROLD,IEOLD
C
      DIMENSION LINDAT(2),LUTDAT(5)
      DIMENSION IPV(10),IAV(10)
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
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      INTEGER JRUNEN,JRUNRN,JRUNRT,JRUNSD,JRUNST,JRUNTT,JRUNRC,LRUNHA
      PARAMETER(JRUNEN=1,JRUNRN=2,JRUNRT=3,JRUNSD=4,JRUNST=5,LRUNHA=5)
      INTEGER BKINCA
      LOGICAL LNEWR
      LOGICAL CHKCLAS
      CHARACTER * 50 MESS
      COMMON / ALFORMA / LUFORM,IFALARM
C
      DATA LNEWR /.TRUE./, IROLD /0/, IEOLD /0/
C----------------------------------------------------------------------
      IEND=0
C
C       get next record
C
 1    CONTINUE
      KCLASW = 0
      CALL ABRSEL ('E', CQUNPK, IRET)
C       get record class word if read from EDIR
      CALL ABGTCL ( KCLASW )
C
C       determine whether we are reading a Mini or a Nano , or not :
C
      IF (IRET.LE.2) THEN
         CALL ALVSN(ITYP,IPV,IAV,IYR)
         INDATA=ITYP
         IF (ITYP.EQ.5) THEN
            XMINI = .TRUE.
         ELSE
            XMINI = .FALSE.
         ENDIF
         IF (ITYP.EQ.7) THEN
            XNANO = .TRUE.
         ELSE
            XNANO = .FALSE.
         ENDIF
      ELSE
         INDATA = 0
      ENDIF
C
C If one reads a MINI and if the BOS banks format reading was not OK ,
C one must stop the job :
C
      IF (XMINI.AND.IFALARM.NE.0) THEN
         CALL QMTERM
     +   ('_QMRDSB_ Error in BOS formats when reading a MINI')
         GO TO 900
      ENDIF
C
C       clear ALPHA banks
C
      IF (.NOT. XCOPYJ)  CALL QMCLR
C
      GO TO ( 10, 20, 30,  1, 50, 60, 70, 80, 90,130,
     +       110,120,130,140,150,160,170,180,190,200,210), IRET
C----------------------------------------------------------------------
C       event record
C
 10   CONTINUE
      IF (KCLASW.EQ.0) THEN
C       do NOT check class if only 1 bank (laser event)
C
        IF (IGTLEN(IW,'E') .GT. 1) THEN
C         class word not read from EDIR , get it from REVH bank
C         skip the event if not selected
C         Don't check for class if input is a NanoDst
          IF (.NOT.CHKCLAS(KCLASW).AND..NOT.XNANO) GO TO 1
        ENDIF
      ENDIF
C
      KNREIN = KNREIN + 1
      KFFILL = 0
      KNEVT = KNEVT + 1
      KNEFIL = KNEFIL + 1
      XWREVT = .FALSE.
      CALL ABRUEV (KRUN, KEVT)
      IF (IROLD - KRUN)  12, 13, 11
C
 11   IF (.NOT.XNSEQ) CALL QWMESE
     +  ('_QMREAD_ Run numbers on input file not in increasing order')
 12   IF (.NOT. LNEWR)
     +  CALL QWMESE ('_QMREAD_ New run - no run record found')
      CALL ABOLDR (-KRUN)
      LNEWR = .FALSE.
      IROLD = KRUN
      IEOLD = 0
      GO TO 19
 13   IF (LNEWR)  THEN
C         run record was preceded by events of the same run :
        CALL QMNEWR (KRUN, KRUN)
        LNEWR = .FALSE.
      ENDIF
      IF (IEOLD - KEVT)  19, 14, 15
 14   MESS = 'Same run/event number as previous event. Skipped'
      GO TO 400
 15   IF(.NOT.XNSEQ)CALL QWMESE
     +  ('_QMREAD_ Event numbers on input file not increasing order')
 19   IF (KNEFIL .EQ. 1)  CALL QWMESE
     +    ('_QMREAD_ First selected event on input file ')
C_PVM
C
C    PVM: send new event to slave
C
      IEOLD = KEVT
      CALL QPVMES(KRUN,KEVT,IRET,IEND)
C_PVM
      GO TO 900
C----------------------------------------------------------------------
C       run record
C
 20   KNREIN = KNREIN + 1
C
C       special treatment of run number = infinity
C       remove if run numbers on POT are ok.
C       In future, these run numbers may be ok !
C
      CALL ABRUEV (I, DUMMY)
      IF (I .GT. 99999)  THEN
        CALL QWMESE ('_QMREAD_ New run number > 99999')
        GO TO 1
      ENDIF
      KRUN = I
C       Replace the previous statements by
C...  CALL ABRUEV (KRUN, DUMMY)
      IF (KRUN .NE. IROLD)  KEVT = 0
      LNEWR = .TRUE.
C
      XWRRUN = .FALSE.
      IF (KFEOUT .EQ. 2)  XWRRUN = .TRUE.
      IF (KFEOUT .EQ. 0)  CALL QWRUNR
C
      IF (IROLD.GT.KRUN.AND.(.NOT.XNSEQ))  CALL QWMESE
     +  ('_QMREAD_ Run numbers on input file not in increasing order')
      GO TO 1
C----------------------------------------------------------------------
C       special records
C
 30   KNREIN = KNREIN + 1
      KEVT = 0
      XWREVT = .FALSE.
      CALL QMSREC
      IF (.NOT. XSREC)  GO TO 1
C       write special records only if the current run is selected
      CALL ABSEVT (KRUN, 0, IRET)
      IF (IRET .NE. 1)  GO TO 1
      CALL QWRITE
      CALL ABCLAS (-1)
      GO TO 1
C
C       end of file
C
 50   CONTINUE
      KNEFIL = 0
      KNREIN = 0
      GO TO 1
C
C       normal end of job
C
 60   MESS = 'All event input files are processed'
      GO TO 300
 70   MESS = 'NEVT card : all events processed'
      GO TO 300
 80   IF(XNSEQ)GOTO 1
      MESS = 'SEVT / SRUN card : all events processed'
      GO TO 300
 90   MESS = 'Time Limit Reached'
      GO TO 300
 110  MESS = 'Error in SEVT,NEVT,SRUN,IRUN,TIME, or CLAS card'
      GO TO 300
 120  MESS = 'Read error in event directory'
      GO TO 300
 130  MESS = 'Error in FILI cards'
      GO TO 300
 140  MESS = 'Input file cannot be opened'
      GO TO 300
 150  MESS = 'Error in FILO cards'
      GO TO 300
 160  MESS = 'Output file cannot be opened'
      GO TO 300
C----------------------------------------------------------------------
C       error conditions :
C
 170  MESS = 'Read error. Record skipped.'
      GO TO 400
 180  MESS = 'Error on decompressing. Record skipped.'
      GO TO 400
 190  MESS = 'Not enough space for unpacking. Record skipped.'
      GO TO 400
 200  MESS = 'TPC unpacking error. Record skipped.'
      GO TO 400
 210  MESS = 'Error in data base reading. Record skipped.'
      GO TO 400
C
 300  CALL QWMESE ('0_QMREAD_ ' // MESS)
      IEND=1
      GOTO 900
C
 400  CALL QWMESE ('_QMREAD_ ' // MESS)
      GO TO 1
C
 900  CONTINUE
C_PVM
C
C    GOTO 1, IF PVM MASTER PROCESS
C
      IF (IEND.EQ.0) CALL QPVMGT(*1)
C_PVM
      END
