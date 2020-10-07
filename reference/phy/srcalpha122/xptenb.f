      SUBROUTINE XPTENB(IRUN,XTGQOK,XTGLOK,XTGSOK)
CKEY XLUMOK TRIG /INTERNAL
C----------------------------------------------------------------------
C!  - Check which triggers are enabled.
C!    Called from XLSLUM
C!    Author   :- J.Wear                  22-MAR-1990
C!    Modified:   H. Meinhard       27-Apr-1993  (1) - Add Sical, return
C!                different flags for LCAL and SICAL
C!    Modified:   H. Meinhard       24-May-1993  (2) - Introduce changes
C!                due to 1993 trigger setup
C!    Modified:   B.Bloch           11-May-1994  (1) - Use X1RG instead
C!                of XTOP dropped in the trigger upgrade of 1994
C!
C!   Inputs:  IRUN -- Run number
C!        -
C!   Outputs: XTGQOK  /L    all essential large-angle triggers enabled
C!            XTGLOK  /L    all essential LCAL triggers enabled
C!            XTGSOK  /L    all essential SICAL triggers enabled
C!        -
C!   Libraries required:
C!
C!   Description
C!   ===========  If principal physics triggers and bhabha
C!   triggers are enabled XPTENB returns true.  For use in determining
C!   whether to count runs/events in the "ideal case" luminosity
C!   determination.  Each running period has distinct trigger names for
C!   these required triggers, so each period must have its own list.
C?
C!======================================================================
      SAVE MXBRN,MXBMK,TTREQ,TTREL,TTRES,MHDRQ,MHDRL,MHDRS
      LOGICAL XTGQOK, XTGLOK, XTGSOK
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
      PARAMETER(JXTBID=1,JXTBVR=2,JXTBTN=4,JXTBTB=5,JXTBBM=6,JXTBBN=8,
     +          JXTBAD=18,JXTBL1=23,JXTBL2=24,LXTBNA=24)
      PARAMETER(JXTCTT=1,JXTCGC=3,JXTCLL=4,JXTCBN=5,JXTCCL=6,JXTCTR=7,
     +          LXTCNA=16)
      PARAMETER(JXTOID=1,JXTOVR=2,JXTONA=4,JXTOTR=6,JXTOGS=7,JXTODS=8,
     +          JXTOTT=16,LXTOPA=16)
      PARAMETER(JX1RNA=1,JX1RCO=2,LX1RGA=4)
      PARAMETER (MXBIT=6, MXBIL=2, MXBIS=1)
      PARAMETER (NXBAD=19)
      PARAMETER (JSEP89=1,JFAL89=2,JMAR90=3,JMAR92=4,JSEP92=5,JMAY93=6,
     +  NPER=6)
      CHARACTER*4 CHAINT
      CHARACTER*8 MNM
      CHARACTER*8 TTREQ(MXBIT,NPER),TTREL(MXBIL,NPER),TTRES(MXBIS,NPER)
      INTEGER     MXBRN(NXBAD),MXBMK(NXBAD),
     +            MHDRQ(NPER),MHDRL(NPER),MHDRS(NPER)
      LOGICAL     XTGQ, XTGL, XTGS
C.....Required triggers: large angle
      DATA TTREQ /
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB'/
C.....Required triggers: LCAL
      DATA TTREL /
     +  'LC_HI_LO','LC_LO_HI',
     +  'LC_HI_LO','LC_LO_HI',
     +  'LT_HI_LO','LT_LO_HI',
     +  'LW_ET_HI','        ',
     +  'LW_ET_HI','        ',
     +  'LW_ET_HI','        '/
C.....Required triggers: SiCAL
      DATA TTRES /
     +  '        ',
     +  '        ',
     +  '        ',
     +  '        ',
     +  'SICAL_ME',
     +  'SICAL_ME'/
C.....Required trigger mask (hardwired): large angle
C     51512064=Z03120300, 1966848=Z001E0300
      DATA MHDRQ / 51512064, 51512064,  1966848,  1966848,  1966848,
     +              1966848 /
C.....Required trigger mask (hardwired): LCAL
C     49152=Z0000C000, 65536=Z00010000, 64=Z00000040
      DATA MHDRL /    49152,    49152,    49152,    65536,    65536,
     +                   64 /
C.....Required trigger mask (hardwired): SiCAL
C     16=Z00000010
      DATA MHDRS /        0,        0,        0,        0,       16,
     +                   16 /
C.....Hardwired trigger enable masks (XTOP corrupted)
      DATA MXBRN /       4017,       4067,       4351,       4387,
     &                   4476,       4525,
     &                   4530,       5112,       5329,       5373,
     &                   5852,       7240,       8419,       8420,
     &                   8423,       8424,      12528,      12536,
     &                12718/
      DATA MXBMK /-2083339265,-2083339265,-2083339274,-2083339266,
     &            -2083339266,-2083339266,
     &            -2083339266,-2095922177,-2095922177,-2095922177,
     &               51569663,-2145393668,-2011175940,-2011175940,
     &            -2011175940,-2011175940,-2009078786,-2009078786,
     &            -2009078786/
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+LMHCOL)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+LMHROW)
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
C----------------------------------------------------------------------
C For MC, all triggers are always enabled
      IF (IRUN.LE.2000) THEN
        XTGQOK = .TRUE.
        XTGLOK = .TRUE.
        XTGSOK = .TRUE.
        GOTO 999
      ENDIF
C
C link to trigger banks
      IERR = 0
      KXTBN=IW(NAXTBN)
      KXTOP=IW(NAXTOP)
      NAX1RG=NAMIND('X1RG')
      KX1RG=IW(NAX1RG)
C
C Find which period this run belongs to
      IF (IRUN.LE.4535) THEN
        KPER = JSEP89
      ELSEIF (IRUN.LE.5909) THEN
        KPER = JFAL89
      ELSEIF (IRUN.LT.14000) THEN
        KPER = JMAR90
      ELSEIF (IRUN .LT. 16500) THEN
        KPER = JMAR92
      ELSEIF (IRUN .LT. 20000) THEN
        KPER = JSEP92
      ELSE
        KPER = JMAY93
      ENDIF
C
C reset the required bit masks
      MKREQ = 0
      MKREL = 0
      MKRES = 0
C
C scan the XTBN bank for the required triggers
      IF (KXTBN .NE. 0) THEN
        DO 330 ITBIT = 1, LROWS(KXTBN)
          IDEF = ITABL(KXTBN,ITBIT,JXTBTN)
          INUM = ITABL(KXTBN,ITBIT,JXTBTB)
C XTBN corrupted?
          IF (INUM .LT. 0 .OR. INUM .GT. 31) THEN
            IERR = 1
            GO TO 888
          END IF
C get the name of this trigger; convert it to upper case; check whether
C non-blank
          IF (IDEF .NE. 0 .AND. INUM .NE. 31) THEN
            MNM(1:4) = CHAINT(ITABL(KXTBN,ITBIT,JXTBBM))
            MNM(5:8) = CHAINT(ITABL(KXTBN,ITBIT,JXTBBM+1))
            CALL CLTOU(MNM)
            IF (MNM .NE. '        ') THEN
C compare with the required trigger names
              DO 300 IREQ = 1, MXBIT
                IF (MNM .EQ. TTREQ(IREQ,KPER)) MKREQ = IBSET(MKREQ,INUM)
  300         CONTINUE
              DO 310 IREQ = 1, MXBIL
                IF (MNM .EQ. TTREL(IREQ,KPER)) MKREL = IBSET(MKREL,INUM)
  310         CONTINUE
              DO 320 IREQ = 1, MXBIS
                IF (MNM .EQ. TTRES(IREQ,KPER)) MKRES = IBSET(MKRES,INUM)
  320         CONTINUE
            END IF
          END IF
  330   CONTINUE
C if required bit mask empty, take it from hardwired mask
        IF (MKREQ .EQ. 0) MKREQ = MHDRQ(KPER)
        IF (MKREL .EQ. 0) MKREL = MHDRL(KPER)
        IF (MKRES .EQ. 0) MKRES = MHDRS(KPER)
      ELSE
C XTBN is missing!
        IERR = 1
        GO TO 888
      END IF
C
C Error handling: XTBN missing or corrupted - use hardwired masks
  888 CONTINUE
      IF (IERR .NE. 0) THEN
        MKREQ = MHDRQ(KPER)
        MKREL = MHDRL(KPER)
        MKRES = MHDRS(KPER)
      END IF
C
C==== We have the required mask. Get the mask of enabled triggers now.
C reset the mask first
      MKENB = 0
C
C Use XTOP, word 6 of run header to get the trigger enabled mask.
C or 3rd word of constants stored in 1st row of X1RG from 1994 onwards
C If this run was missing or had upt_corr trigger banks in the run heade
C hardwire the enabled mask.
      IF (KXTOP.NE.0) THEN
         MKENB = ITABL(KXTOP,1,JXTOTR)
      ELSEIF (KX1RG.NE.0) THEN
         MKENB = ITABL(KX1RG,1,JX1RCO+2)
      ENDIF
      DO 20 IXBAD = 1,NXBAD
   20  IF (KRUN.EQ.MXBRN(IXBAD)) MKENB = MXBMK(IXBAD)
C
C==== Compare required mask with enabled mask to find whether all re-
C==== quired triggers are enabled.
C Do bit-wise AND of Trigger Enable mask and Required Physics mask
C Check if physics triggers enabled equal the required physics mask.
      XTGQ = .FALSE.
      XTGL = .FALSE.
      XTGS = .FALSE.
      IF (MKREQ .NE. 0) XTGQ = MKREQ .EQ. IAND(MKREQ,MKENB)
      IF (MKREL .NE. 0) XTGL = MKREL .EQ. IAND(MKREL,MKENB)
      IF (MKRES .NE. 0) XTGS = MKRES .EQ. IAND(MKRES,MKENB)
C
C Luminosity trigger patch for runs 13191-13199.
C Here triggers LT_A+BVH and LT_LO_HI were disabled due to noisy towers
C Subsequent analysis showed that LW_ET_HI found all Bhabhas, therefore
C these runs are allowed to pass.
      IF (IRUN .GE. 13191 .AND. IRUN .LE. 13199) XTGL = .TRUE.
C
C give the final answer now
      XTGQOK = XTGQ
      XTGLOK = XTGL
      XTGSOK = XTGS
  999 RETURN
      END
