
      SUBROUTINE QFMTCH
CKEY FILL MC MATCH/INTERNAL
C-----------------------------------------------------------------------
C   Author   : D. CINABRO, H. ALBRECHT   5-OCT-1988
C
C   Description
C   ===========
C!Fill match infomation into QVEC
C
C 05/05/88 Rewritten 14/10/88 to add ITC Altered 26/04/89 to use
C ITMA and IASL
C
C Uses the TGMA,TMTL,IPJT, and IRJT; or the ITMA and IASL banks to relat
C FRFT track to FKIN tracks.
C
C TGMA and IPJT1 are parallel with the first tracks in FRFT.  First use
C TGMA and TMTL to get the MC track number and the number of associated
C hits in the TPC.  Then use IPJT1 and IRJT1 to add extra hits and possi
C tracks from the ITC.  Store the hits and tracks in IHIT and IPART.  No
C that FPOI is necessary as IPJT points to KINE tracks, so the tranlatio
C to FKIN is done via FPOI.  Store in QVEC and point in QMTL/QMTS.  For
C IPJT2 it is the same but simpler as extra tracks from another detector
C do not have to be considered.
C
C The use of ITMA and IASL is exactly as ITJT2 as the information from
C the ITC and TPC are combined in these banks.  FPOI is not necessary
C in this case as the banks point to FKIN.  These banks are used
C preferntially and the others are used if ITMA and IASL are not
C present.
C
C If more than 20 MC tracks are associated to one MC particle then
C there will be an overflow.  An error message is printed and we go
C to the next track.
C
C  Called from QFILMC
C
C-----------------------------------------------------------------------
      SAVE IERR,IERCT
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
C
      PARAMETER(JTGMNM=1,JTGMOM=2,JTGMNC=3,JTGMNA=4,LTGMAA=4)
      PARAMETER(JTMTMT=1,JTMTNH=2,JTMTCS=3,LTMTLA=3)
      PARAMETER(JIPJNH=1,JIPJNS=2,JIPJRN=3,LIPJTA=3)
      PARAMETER(JIRJGN=1,JIRJNH=2,JIRJSH=3,LIRJTA=3)
      INTEGER JIASFK,JIASNH,JIASCS,LIASLA
      PARAMETER(JIASFK=1,JIASNH=2,JIASCS=3,LIASLA=3)
      INTEGER JITMNA,JITMOF,JITMNC,LITMAA
      PARAMETER(JITMNA=1,JITMOF=2,JITMNC=3,LITMAA=3)
C
      PARAMETER (MXMC=20)
      DATA IERR,IERCT /0,0/
C
C The common used to sort the pointers in FTTM to the MC tracks by the
C number of hits associated with the MC tracks
      COMMON/SORT/IHIT(MXMC),IPART(MXMC)
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
C-----------------------------------------------------------------------
C
C Other statement functions
C
C       address of JQVENM for track I
      INM(I) = KOQVEC + I * KCQVEC + JQVENM
C       address of JQVEML for track I
      IML(I) = KOQVEC + I * KCQVEC + JQVEML
C-----------------------------------------------------------------------
C
C If there are no charged tracks forget the whole thing
C
      IF (KNCHT .EQ. 0)  GO TO 999
C
C Are ITMA and IASL present
C
      IFLAG = 0
      JITMA = IW(NAITMA)
      JIASL = IW(NAIASL)
      IF (JITMA.LE.0.AND.JIASL.LE.0) THEN
C
C OK. See if the other guys are here
C
        JFPOI = IW(NAFPOI)
        JTGMA = IW(NATGMA)
        JIPJ1 = NLINK('IPJT',1)
        IF (JTGMA.LE.0.OR.JIPJ1.LE.0..OR.JFPOI.LE.0) THEN
          IERCT=IERCT+1
          IF(IERCT.LE.5)
     &     CALL QWMESE('_QFMTCH_ Unable to fill matching information')
          IF(IERCT.EQ.1)
     &      CALL QWMESS('_QFMTCH_ Warning message printed only 5 times')
          GO TO 999
        ELSE
          IFLAG = 1
        ENDIF
        JIPJ2 = NLINK('IPJT',2)
        NTTRKS = LROWS(JTGMA)
        NITRKS = LROWS(JIPJ2)
        JTMTL = IW(NATMTL)
        IF (JTMTL.LE.0) GOTO 999
        NPOIT = LROWS(JTMTL)
        JIRJ1 = NLINK('IRJT',1)
        NPOI1 = LROWS(JIRJ1)
        JITJ2 = NLINK('IRJT',2)
        NPOI2 = LROWS(JITJ2)
        NPOI = NPOIT+NPOI1+NPOI2
      ELSE
        NTRKS=LROWS(JITMA)
      ENDIF
C
C Fill the banks.  First branch around to us ITMA and IASL
C
      LQVEC = KOQVEC + KFCHT * KCQVEC
      IF (IFLAG.EQ.0) GOTO 410
C
C Using the old stuff
C
      IF (NTTRKS.LE.0) GOTO 301
      DO 300 I=1,NTTRKS
        NPOT = ITABL(JTGMA,I,JTGMNM)
        NPOI = ITABL(JIPJ1,I,JIPJNS)
        IF ((NPOT+NPOI).EQ.0) GO TO 300
C
C Here if too many MC tracks associated with a charged track print
C an error message and get out.
C
        IF ((NPOT+NPOI).GT.MXMC) THEN
          IERR = IERR + 1
          IF (IERR .LE. 20)  CALL QWMESE
     &         ('_QFMTCH_ Unable to fill matching information')
          GO TO 300
        ENDIF
C
C First get MC tracks and associated number of hits to the reconstructed
C track, for the TPC
C
        IPOT = ITABL(JTGMA,I,JTGMOM)
        DO 100 J=1,NPOT
          IHIT(J) = ITABL(JTMTL,IPOT+J,JTMTNH)
          IPART(J) = ITABL(JTMTL,IPOT+J,JTMTMT)
  100   CONTINUE
C
C Now for the ITC, if same MC particle as all ready associated in TPC
C simply add hits, if not add a new particle and number of hits
C
        NIOF = 0
        IPOI = ITABL(JIPJ1,I,JIPJRN) - 1
        DO 101 J=1,NPOI
          IIPAR = ITABL(JFPOI,IABS(ITABL(JIRJ1,IPOI+J,JIRJGN)),1)
          DO 102 K=1,NPOT
            IF (IPART(K).EQ.IIPAR) THEN
              IHIT(K) = IHIT(K) + ITABL(JIRJ1,IPOI+J,JIRJSH)
              GOTO 101
            ENDIF
  102     CONTINUE
          NIOF = NIOF + 1
          IHIT(NPOT+NIOF) = ITABL(JIRJ1,IPOI+J,JIRJSH)
          IPART(NPOT+NIOF) = IIPAR
  101   CONTINUE
C
C Fill in QVEC
C
        IW(LQVEC+JQVENM) = NPOT+NIOF
        IW(LQVEC+JQVEML) = KNQMTX
C
C Check length of QMTX
C
        IF (KNQMTX + NPOT + NIOF .GE. IW(KOQMTL))
     &     CALL QSBANK ('QMTX', KNQMTX+200)
C
C Now fill in QMTX and QMTL
C
        DO 200 J=1,NPOT+NIOF
          KNQMTX = KNQMTX + 1
          IKIN = IPART(J) + KFMCT - 1
          IW(INM(IKIN)) = IW(INM(IKIN)) + 1
          IW(KOQMTL+KNQMTX) = IKIN
          IW(KOQMTS+KNQMTX) = IHIT(J)
  200   CONTINUE
C
  300 LQVEC = LQVEC + KCQVEC
C
C Now tracks that are only in the ITC
C
  301 JIPJ2=NLINK('IPJT',2)
      JITJ2=NLINK('IRJT',2)
      IF (NITRKS.LE.0) GOTO 499
C
      DO 400 I = 1,NITRKS
        NPOI = ITABL(JIPJ2,I,JIPJNS)
C
        IF (NPOI.EQ.0) GO TO 400
C
C Here if too many MC tracks associated with a charged track print
C an error message and get out.
C
        IF (NPOI.GT.MXMC) THEN
          IERR = IERR + 1
          IF (IERR .LE. 20)  CALL QWMESE
     &        ('_QFMTCH_ Unable to fill matching information')
          GO TO 400
        ENDIF
        IW(LQVEC+JQVENM) = NPOI
        IW(LQVEC+JQVEML) = KNQMTX
C
        IPOI = ITABL(JIPJ2,I,JIPJRN) - 1
        DO 401 J=1,NPOI
          IHIT(J) = ITABL(JITJ2,IPOI+J,JIRJSH)
  401   CONTINUE
        IF (KNQMTX + NPOI .GE. IW(KOQMTL))
     &       CALL QSBANK ('QMTX', KNQMTX+200)
        DO 402 J=1,NPOI
          KNQMTX = KNQMTX + 1
          IKIN=ITABL(JFPOI,IABS(ITABL(JITJ2,IPOI+J,JIRJGN)),1)+KFMCT-1
          IW(INM(IKIN)) = IW(INM(IKIN)) + 1
          IW(KOQMTL+KNQMTX) = IKIN
          IW(KOQMTS+KNQMTX) = IHIT(J)
  402   CONTINUE
C
  400 LQVEC = LQVEC + KCQVEC
      GOTO 499
C
C Here if using ITMA and IASL.  Much simpler than above.
C
  410 IF (NTRKS.LE.0) GOTO 999
C
      DO 450 I = 1,NTRKS
        NPOI = ITABL(JITMA,I,JITMNA)
C
        IF (NPOI.EQ.0) GO TO 450
C
C Here if too many MC tracks associated with a charged track print
C an error message and get out.
C
        IF (NPOI.GT.MXMC) THEN
          IERR = IERR + 1
          IF (IERR .LE. 20)  CALL QWMESE
     &        ('_QFMTCH_ Unable to fill matching information')
          GO TO 450
        ENDIF
        IW(LQVEC+JQVENM) = NPOI
        IW(LQVEC+JQVEML) = KNQMTX
C
        IPOI = ITABL(JITMA,I,JITMOF)
        DO 451 J=1,NPOI
          IHIT(J) = ITABL(JIASL,IPOI+J,JIASNH)
  451   CONTINUE
        IF (KNQMTX + NPOI .GE. IW(KOQMTL))
     &       CALL QSBANK ('QMTX', KNQMTX+200)
        DO 452 J=1,NPOI
          KNQMTX = KNQMTX + 1
          IKIN=ITABL(JIASL,IPOI+J,JIASFK)+KFMCT-1
          IW(INM(IKIN)) = IW(INM(IKIN)) + 1
          IW(KOQMTL+KNQMTX) = IKIN
          IW(KOQMTS+KNQMTX) = IHIT(J)
  452   CONTINUE
C
  450 LQVEC = LQVEC + KCQVEC
C
C Now fill the inverse relation
C
  499 LQVEC = KOQVEC + KFMCT * KCQVEC
      DO 500 I=KFMCT,KLMCT
        IW(LQVEC+JQVEML) = KNQMTX - IW(LQVEC+JQVENM)
        KNQMTX = KNQMTX + IW(LQVEC+JQVENM)
  500 LQVEC = LQVEC + KCQVEC
C
      IF (KNQMTX .GE. IW(KOQMTL))  CALL QSBANK ('QMTX',KNQMTX+200)
C
      LQVEC = KOQVEC + KFCHT * KCQVEC
      DO 600 I=KFCHT,KLCHT
        DO 510 J=1,IW(LQVEC+JQVENM)
          IMC = IW(KOQMTL+IW(LQVEC+JQVEML)+J)
          IW(IML(IMC)) = IW(IML(IMC)) + 1
          IW(KOQMTL+IW(IML(IMC))+IW(INM(IMC))) = I
          IW(KOQMTS+IW(IML(IMC))+IW(INM(IMC))) =
     &      IW(KOQMTS+IW(LQVEC+JQVEML)+J)
  510   CONTINUE
  600 LQVEC = LQVEC + KCQVEC
C
  999 CONTINUE
      END
