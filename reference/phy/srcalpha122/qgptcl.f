      SUBROUTINE QGPTCL (YVALUT,THRUST,NJTLIM,YJTLIM,EVIS)
CKEY JETS /INTERNAL
C----------------------------------------------------------------------
C!  - PTCLUS Jet ALgorithm (see ALEPH  89-150)
C!
C!   Author   :- J.M.SCARR
C------CLUSTER (JET) ALGORITHM FOR HIGH ENERGY FINAL STATES.
C------SCARR JM. 89.II.13 DOX#89.3.                     LST CHG 89.II.28
C!
C!   Inputs:
C!
C!      NJTLIM  =  LIMIT MINIMUM NUMBER OF CLUSTERS.
C!                 (ALWAYS TEST  KTBO)
C!      YJTLIM  =  UPPER LIMIT OF  SEMS  OF 2 CLUSTERS MERGED.
C!
C!
C!   Outputs:
C!        -     KTBO  =  NUMBER OF CLUSTERS FOUND.(in QCTBUF.INC)
C!        -   THRUST  =  GENERALISED THRUST.
C!      YVALUT(1) =   SEMS  OF LAST MERGED CLUSTERS.
C!      YVALUT(2) =   LOWEST  SEMS  OF TWO CLUSTERS.
C!                 SEMS = SCALED EFFECTIV MASS SQUARED, M*M/S.
C!      YVALUT(3)=   FRACTIONAL ENERGY REASSIGNED IN LAST MERGE.
C!                 SEMS = SCALED EFFECTIV MASS SQUARED, M*M/S.
C!======================================================================
      SAVE LBEGIN
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
C-------------------- /QCTBUF/ --- Buffer for topological routines -----
      PARAMETER (KTBIMX = 2000,KTBOMX = 20)
      COMMON /QCTBUF/ KTBI,QTBIX(KTBIMX),QTBIY(KTBIMX),QTBIZ(KTBIMX),
     &  QTBIE(KTBIMX),KTBIT(KTBIMX),KTBOF(KTBIMX),KTBO,QTBOX(KTBOMX),
     &  QTBOY(KTBOMX),QTBOZ(KTBOMX),QTBOE(KTBOMX),QTBOR(KTBOMX)
C     KTBI : Number of input vectors (max : KTBIMX).
C     QTBIX/Y/Z/E : Input vectors (filled contiguously without unused ve
C                   The vectors 1 to KTBI must NOT be modified.
C     KTBIT : Input vector ident. used for bookkeeping in the calling ro
C     KTBO  : Number of output vectors (max : KBTOMX).
C     QTBOX/Y/Z/E : Output vector(s).
C     QTBOR : Scalar output result(s).
C     KTBOF : If several output vectors are calculated and every input v
C             associated to exactly one of them : Output vector number w
C             the input vector is associated to. Otherwise : Dont't care
C If a severe error condition is detected : Set KTBO to a non-positive n
C which may serve as error flag. Set QTBOR to a non-physical value (or v
C Fill zeros into the appropriate number of output vectors. Do not write
C messages.
C--------------------- end of QCTBUF ---------------------------------
C     KTBOMX is a parameter which is maximum number jets
      INTEGER TOPEN, PARTIC
      REAL EVIS
C
*      NOTE.      SEMS (SCALED EFFECTIVE MASS SQUARED), Y = MASS**2/S.
*      ====     IF LSCALE FALSE, YJTLIM,YVALUT ARE CORRESPONDING MASSES.
*                 (CURRENTLY, PSEUDO-MASSES USED)
*
*               IF LPRINT TRUE, DIAGNOSTIC PRINT IS OBTAINED.
*------
      LOGICAL   LBEGIN,LPRINT,LSCALE, LREASS
      PARAMETER (PPSQLM=0.15,LPRINT=.FALSE.,LSCALE=.TRUE. )
      DATA       LBEGIN /.TRUE./
C      SAVE       HWSQLM
C     limit on iterations
      PARAMETER (NITLIM = 5)
C
C     these arrays hold the jets internal to the program
      INTEGER MXTRKS
      PARAMETER ( MXTRKS = 200)
      REAL PJETX(MXTRKS), PJETY(MXTRKS)
      REAL PJETZ(MXTRKS), PJETE(MXTRKS)
C            X-MOMENTUM     Y-MOMENTUM     Z-MOMENTUM     ENERGY
      INTEGER KJETNU (MXTRKS)
C     KJETNU(I) is the number of tracks in jet I
C     KTBIMX is a parameter which is maximum number of input tracks
      REAL     PM(MXTRKS), PP(MXTRKS), YVALUT(3)
      INTEGER  IORDER(MXTRKS)
C
C======================================================
C
      IF(KTBI.GT.KTBIMX) THEN
        KTBO = -1
C       QGPTCLUS -INPUT TRACKS EXCEEDS MAX NUMBER ALLOWED
        GOTO 999
      ENDIF
C
      IF (KTBI .LT. 2) THEN
        KTBO = -1
C       QJPTCLS - ZERO OR ONE TRACKS INPUT
        GOTO 999
      ENDIF
C
C     check the integrity of the input data
      DO 10 ICOUNT = 1, KTBI
        IF( (QTBIX(ICOUNT) .EQ. 0.0) .AND.
     &       (QTBIY(ICOUNT) .EQ. 0.0) .AND.
     &       (QTBIZ(ICOUNT) .EQ. 0.0)   ) THEN
          CALL QWMESE('_QGPTCL_ Track with zero momentum found ')
          KTBO = -1
          GOTO 999
        ENDIF
   10 CONTINUE
C
      IF(LBEGIN) THEN
        IF(LSCALE) THEN
          WRITE (KUPRNT,4) PPSQLM,YJTLIM,NJTLIM
    4     FORMAT(//,10X,'USING PTCLUS: PPSQLM,YJTLIM,NJTLIM =',
     &       2F8.3,I5,//)
        ELSE
          WRITE (KUPRNT,5) PPSQLM,YJTLIM,NJTLIM
    5     FORMAT(//,10X,'USING PTCLUS: PPSQLM,WJTLIM,NJTLIM =',
     &       2F8.3,I5,//)
          HWSQLM = YJTLIM*YJTLIM*0.5
        ENDIF
        LBEGIN = .FALSE.
      ENDIF
C
      LNJ=0
C------STEP 1.  FORM INITIAL CLUSTERS ABOUT MOST ENERGETIC PARTICLES.---
C     sort in energy order
      IORDER(1)=1
      DO 12  I=2,KTBI
        IORDER(I)=I
        J=I
        DO  13 JJ=1,I-1
          K1=J-1
          IF ( QTBIE(IORDER(J)).LE.QTBIE(IORDER(K1))) GO TO 12
            ISAVE=IORDER(K1)
            IORDER(K1)=IORDER(J)
         IORDER(J)=ISAVE
   13    J=J-1
   12 CONTINUE
C----------------------------
C
C     start cluster about TOPEN, the most energetic particle
      TOPEN=IORDER(1)
      PJETX(1) = QTBIX(TOPEN)
      PJETY(1) = QTBIY(TOPEN)
      PJETZ(1) = QTBIZ(TOPEN)
      PJETE(1) = QTBIE(TOPEN)
      KJETNU(1)=1
      KTBOF(TOPEN)=1
      PP(1)= QTBIX(TOPEN)*QTBIX(TOPEN)
     &       + QTBIY(TOPEN)*QTBIY(TOPEN)
     &       + QTBIZ(TOPEN)*QTBIZ(TOPEN)
      PM(1)=SQRT(PP(1))
      NJ=1
      ENYTOT = QTBIE(TOPEN)
C
C     now loop over all particles
C
      DO  20 I=2, KTBI
        PARTIC = IORDER(I)
        ENYTOT = QTBIE(PARTIC)+ENYTOT
        PP(I)=QTBIX(PARTIC)*QTBIX(PARTIC)
     &         + QTBIY(PARTIC)*QTBIY(PARTIC)
     &         + QTBIZ(PARTIC)*QTBIZ(PARTIC)
        PM(I)=SQRT(PP(I))
        PERPMN = 1.E30
        DO  21 JET = 1, NJ
          PDOTP =  QTBIX(PARTIC)* PJETX(JET)
     &           + QTBIY(PARTIC)* PJETY(JET)
     &           + QTBIZ(PARTIC)* PJETZ(JET)
          IF(PDOTP .GT. 0.0) THEN
            PERPSQ=(PP(JET)*PP(I)-PDOTP*PDOTP)/
     &             ( (PM(JET)+PM(I))*(PM(JET)+PM(I)) )
            IF(PERPSQ.LT.PERPMN) THEN
              PERPMN=PERPSQ
              JMIN=JET
            ENDIF
          ENDIF
   21   CONTINUE
C
C
        IF (PERPMN .LT. PPSQLM) THEN
C         if perpendicular momentum is less than the limit
C         combine track into existing cluster
          PJETX(JMIN) = PJETX(JMIN) + QTBIX(PARTIC)
          PJETY(JMIN) = PJETY(JMIN) + QTBIY(PARTIC)
          PJETZ(JMIN) = PJETZ(JMIN) + QTBIZ(PARTIC)
          PJETE(JMIN) = PJETE(JMIN) + QTBIE(PARTIC)
          KJETNU(JMIN) = KJETNU(JMIN) + 1
          KTBOF(PARTIC)=JMIN
          PP(JMIN) = PJETX(JMIN)*PJETX(JMIN)
     &               + PJETY(JMIN)*PJETY(JMIN)
     &               + PJETZ(JMIN)*PJETZ(JMIN)
          PM(JMIN) = SQRT(PP(JMIN))
        ELSE
C         form a new cluster
          NJ = NJ+1
C
          PJETX(NJ) =QTBIX(PARTIC)
          PJETY(NJ) =QTBIY(PARTIC)
          PJETZ(NJ) =QTBIZ(PARTIC)
          PJETE(NJ) =QTBIE(PARTIC)
          KJETNU (NJ)=1
          KTBOF(PARTIC)=NJ
          PP(NJ)=PP(I)
          PM(NJ)=PM(I)
        ENDIF
   20 CONTINUE
C
C     end loop over particles
C
C     check here on total energy is OK
C     and also on a negative EVIS
      IF ( (ENYTOT .LE. 0.0) .OR. (EVIS .LT. 0.0) ) THEN
        CALL QWMESE('_QGPTCL_ Warning: Negative total energy ')
      ENDIF
C
      IF (LSCALE) THEN
        IF ( EVIS .EQ. 0.0) THEN
          HWSQLM = YJTLIM*ENYTOT*ENYTOT*0.5
        ELSE
          HWSQLM = YJTLIM*EVIS*EVIS*0.5
        ENDIF
      ELSE
        HWSQLM = YJTLIM*YJTLIM*0.5
      ENDIF
C
C------------------------------------------------------------------
C------STEP 2.  MERGE INITIAL CLUSTERS DOWN TO  NJTLIM. ----------------
  100 CONTINUE
      PERPMN = 1.E30
      DO 30  I = 2,NJ
        DO 31 J = 1,I-1
          PDOTP=   PJETX(I) *PJETX(J)
     &           + PJETY(I) *PJETY(J)
     &           + PJETZ(I) *PJETZ(J)
          IF(PDOTP.GT.0.0) THEN
C------     USING PSEUDO-MASS = E1*E2*(1.-COSINE)  SEEMS BEST.
            PERPSQ=(-PDOTP/(PM(I)*PM(J))+1.) * PJETE(I) * PJETE(J)
            IF(PERPSQ.LT.PERPMN) THEN
              PERPMN=PERPSQ
              JM = J
              IM = I
            ENDIF
          ENDIF
   31   CONTINUE
   30 CONTINUE
C
C     it this the finish of the algorithm ?
      IF( (PERPMN .GT. HWSQLM) .OR. (NJ .LE. NJTLIM)) GO TO 200
      PERPML=PERPMN
C     we havent finished yet. Merge the two clusters separated by
C     the least perpendicular momentum
C     merge cluster IM into cluster JM
      PJETX(JM) = PJETX(JM) + PJETX(IM)
      PJETY(JM) = PJETY(JM) + PJETY(IM)
      PJETZ(JM) = PJETZ(JM) + PJETZ(IM)
      PJETE(JM) = PJETE(JM) + PJETE(IM)
      KJETNU(JM) = KJETNU(JM) + KJETNU(IM)
      PM(JM)=SQRT( PJETX(JM)*PJETX(JM)
     &         + PJETY(JM)*PJETY(JM)
     &         + PJETZ(JM)*PJETZ(JM)       )
C
C------THIS LOOP PREVENTS UNNECESSARY ITERATIONS.
      DO 40  I = 1, KTBI
        IF(KTBOF(I) .EQ. IM) KTBOF(I) = JM
        IF(KTBOF(I) .GT. IM) KTBOF(I) = KTBOF(I)-1
   40 CONTINUE
C
C     reduce the number of jets by one
      NJ=NJ-1
C
      DO 50  ICOUNT = IM,NJ
        PJETX(ICOUNT) = PJETX(ICOUNT +1)
        PJETY(ICOUNT) = PJETY(ICOUNT +1)
        PJETZ(ICOUNT) = PJETZ(ICOUNT +1)
        PJETE(ICOUNT) = PJETE(ICOUNT +1)
        KJETNU(ICOUNT) = KJETNU(ICOUNT +1)
        PM(ICOUNT) = PM(ICOUNT +1)
   50 CONTINUE
C------STEP 3.  REASSIGN PARTICLES TO CLUSTERS.-------------------------
      EMOVED = 0.0
      NITERS = 0
  125 CONTINUE
      LREASS = .FALSE.
C
C     loop through the particles I
      DO 60  I=1, KTBI
        PLNGMX = -1.E30
C       loop through the jets
        DO 61 JET = 1,NJ
          PLONG=(QTBIX(I)* PJETX(JET)+QTBIY(I) *PJETY(JET)
     &       + QTBIZ(I) *PJETZ(JET)) /PM(JET)
          IF(PLONG.GT.PLNGMX) THEN
            PLNGMX=PLONG
            JM=JET
          ENDIF
   61   CONTINUE
        IF (KTBOF(I) .NE. JM) THEN
          KTBOF(I)=JM
          LREASS = .TRUE.
          EMOVED = QTBIE(I) +EMOVED
        ENDIF
   60 CONTINUE
C
C------REFORM CLUSTERS.
C      if a reassignment has happened then...
      IF (LREASS) THEN
        NITERS = NITERS + 1
        JET = 0
        DO 300  J1 = 1,NJ
          JET=JET+1
          PJETX(JET) = 0.0
          PJETY(JET) = 0.0
          PJETZ(JET) = 0.0
          PJETE(JET) = 0.0
          KJETNU(JET)= 0
C         loop over particles I
          DO 70  I = 1, KTBI
            IF(KTBOF(I) .EQ. JET) THEN
              PJETX(JET) = PJETX(JET) + QTBIX(I)
              PJETY(JET) = PJETY(JET) + QTBIY(I)
              PJETZ(JET) = PJETZ(JET) + QTBIZ(I)
              PJETE(JET) = PJETE(JET) + QTBIE(I)
              KJETNU(JET) = KJETNU(JET) + 1
            ENDIF
   70     CONTINUE
C
          IF( KJETNU(JET) .EQ. 0) THEN
C------      REASSIGNMENT HAS ELIMINATED A CLUSTER.
            DO 80  I=1, KTBI
              IF(KTBOF(I) .GT. JET) KTBOF(I)=KTBOF(I)-1
   80       CONTINUE
            JET=JET-1
          ELSE
            PM(JET)=SQRT(PJETX(JET)*PJETX(JET)
     &             + PJETY(JET)*PJETY(JET)
     &             + PJETZ(JET)*PJETZ(JET)    )
          ENDIF
  300   CONTINUE
        NJ=JET
      ENDIF
C
C------IF A CLUSTER HAS SPLIT, ITERATE TRACK ALLOCATION TO CLUSTERS.
      IF ( LREASS .AND. (NITERS .LT. NITLIM) ) GO TO 125
      IF (NJ .GE. 2) GO TO 100
C------TERMINATION.
  200 CONTINUE
C
C     finish of program. Prepare the output data
C
C     now put number of found jets into KTBO
      KTBO = NJ
C
C     jet arrays have space for at most fourty jets
C     have we exceeded this ?
      IF (NJ .GT. KTBOMX) THEN
        KTBO = -1
C       QGPTCLUS - too many jets found
        GOTO 999
      ENDIF
C     have now found NJ jets
C     put their momenta and energies into the output jet vectors
C     in arrays QTBOX etc.
      DO 105 IJET=1,NJ
        QTBOX(IJET) = PJETX(IJET)
        QTBOY(IJET) = PJETY(IJET)
        QTBOZ(IJET) = PJETZ(IJET)
        QTBOE(IJET) = PJETE(IJET)
  105 CONTINUE
C
      IF(LSCALE) THEN
        YVALUT(1)=(PERPML*2.)/(ENYTOT*ENYTOT)
        YVALUT(2)=(PERPMN*2.)/(ENYTOT*ENYTOT)
      ELSE
        YVALUT(1)=SQRT(PERPML*2.)
        YVALUT(2)=SQRT(PERPMN*2.)
      ENDIF
      YVALUT(3) = EMOVED/ENYTOT
      PLONGT=0.0
      DO 110  J=1,NJ
        PLONGT=PM(J)+PLONGT
  110 CONTINUE
      THRUST=PLONGT/ENYTOT
C
  999 CONTINUE
      END
