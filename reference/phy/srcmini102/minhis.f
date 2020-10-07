      SUBROUTINE MINHIS(IMAQQ)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill some simple histograms to monitor the Mini-DST.
C
C     Author: Stephen Haywood      25-Apr-90
C     Modify: Agnieszka Jacholkowska  16-Nov-94
C
C     To obtain histograms, HBOOK must already be initialised.
C
C     Output : IMAQQ  = 1 for hadronic event candidate (Class 16)
C                     = 0 for other events written to Mini-DST
C                     =-1 for events which are not writen to Mini-DST
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.,FFA=100000.)
      PARAMETER(JREVDS=1,JREVFE=2,JREVNE=4,JREVSB=6,JREVTI=7,JREVRB=8,
     +          JREVEC=10,LREVHA=10)
      PARAMETER(JDVEX0=1,JDVEY0=2,JDVEZ0=3,JDVEFP=4,JDVEMV=5,JDVEDT=6,
     +          LDVERA=6)
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTRTF=12,JDTRHO=13,JDTRHM=14,JDTRVB=15,
     +          JDTRQF=16,JDTREA=17,JDTRVI=27,LDTRAA=27)
      PARAMETER(JDEIR2=1,JDEIR3=2,JDEIQF=3,JDEIDE=4,JDEIDT=5,LDEIDA=5)
      PARAMETER(JHMANF=1,JHMANE=2,JHMANL=3,JHMAMH=4,JHMAIG=5,JHMAED=6,
     +          JHMACS=7,JHMAND=8,JHMAIE=9,JHMAIT=10,JHMAIF=11,
     +          JHMATN=12,LHMADA=12)
      PARAMETER(JMUIIF=1,JMUISR=2,JMUIDM=3,JMUIST=4,JMUITN=5,LMUIDA=5)
      PARAMETER(JDGIIF=1,JDGIDE=2,JDGICM=3,JDGIM1=4,JDGIM2=5,JDGIM3=6,
     +          JDGICE=7,JDGITH=8,JDGIPH=9,JDGIPE=10,LDGIDA=10)
      PARAMETER(JDGPEC=1,JDGPTC=2,JDGPPC=3,JDGPR1=4,JDGPR2=5,JDGPF4=6,
     +          JDGPDM=7,JDGPST=8,JDGPQU=9,JDGPQ1=10,JDGPQ2=11,
     +          JDGPM1=12,JDGPM2=13,JDGPMA=14,JDGPER=15,JDGPTR=16,
     +          JDGPPR=17,JDGPPE=18,LDGPCA=18)
      PARAMETER(JDECE0=1,JDECTH=2,JDECPH=3,JDECEF=4,JDECCC=6,LDECOA=6)
      PARAMETER(JDHCE0=1,JDHCTH=2,JDHCPH=3,LDHCOA=3)
      PARAMETER(JDEWMN=1,JDEWE0=2,JDEWEF=3,LDEWIA=4)
      PARAMETER(JDENPX=1,JDENPY=2,JDENPZ=3,JDENE0=4,JDENWE=5,JDENTY=6,
     +          JDENPC=7,JDENDT=8,JDENDJ=9,LDENFA=9)
      PARAMETER(JDJEPX=1,JDJEPY=2,JDJEPZ=3,JDJEE0=4,LDJETA=4)
      PARAMETER(JDTHPR=1,JDTHPX=2,JDTHPY=3,JDTHPZ=4,JDTHE0=5,LDTHRA=5)
      PARAMETER(JDGAEC=1,JDGATC=2,JDGAPC=3,JDGAR1=4,JDGAR2=5,JDGAF4=6,
     +          JDGADM=7,JDGAST=8,JDGAQU=9,JDGAQ1=10,JDGAQ2=11,
     +          JDGAM1=12,JDGAM2=13,JDGAMA=14,JDGAER=15,JDGATR=16,
     +          JDGAPR=17,JDGAEF=18,JDGAGC=19,JDGAZS=20,JDGAPL=21,
     +          JDGAPF=22,JDGAPN=23,JDGAFA=24,JDGAPE=25,LDGACA=25)
      PARAMETER(JDDLPA=1,JDDLJT=2,JDDLPI=3,JDDLPE=4,JDDLVJ=5,
     &          JDDLFR=6,LDDLTA=6)
      PARAMETER (MAXBNK=200,MAXCHA=4*MAXBNK)
      CHARACTER*800 MLISTE,MLISTR
      LOGICAL MINIMC,NOFORM
      COMMON / MINCOM / MLISTE,MLISTR,MINIMC,NOFORM
C
      LOGICAL FIRST,LUMI,BTEST,CL24
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
C!    set of intrinsic functions to handle BOS banks
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
C
      FTABL(ID,NRBOS,L) = FLOAT(ITABL(ID,NRBOS,L))
C
      CALL TIMAD(T)
C
      IF(FIRST) THEN
         T = -999.
         CALL HBOOK1(  1,'Timing for Mini',100,0.,10.,0.)
         CALL HBOOK1(101,'Timing for Mini',100,0.,10.,0.)
         CALL HBOOK1(102,'DVER: r',100,0.,2.,0.)
         CALL HBOOK1(103,'DTRA: sum(p)',100,0.,400.,0.)
         CALL HBOOK1(104,'DGID: sum(E)',100,0.,200.,0.)
         CALL HBOOK1(105,'DGPC: sum(E)',100,0.,200.,0.)
         CALL HBOOK1(106,'DEID: R2 and R3',100,0.,100.,0.)
         CALL HBOOK1(107,'HMAD: sum(hits); MUID: flag',100,0.,100.,0.)
         CALL HBOOK1(108,'DTRA: # vertices',100,0.,50.,0.)
         CALL HBOOK1(109,'DTRA: Vdet info',100,0.,20.,0.)
         CALL HBOOK1(113,'DENF: sum(E)',100,0.,200.,0.)
         CALL HBOOK1(114,'DJET: sum(E)',100,0.,200.,0.)
         CALL HBOOK1(203,'DTRA: sum(p)',100,0.,200.,0.)
         CALL HBOOK1(210,'DECO: sum(E)',100,0.,200.,0.)
         CALL HBOOK1(211,'DHCO: sum(E)',100,0.,200.,0.)
         CALL HBOOK1(212,'DEWI: sum(E)',100,0.,200.,0.)
         CALL HBOOK1(122,'DTHR: Thrust value',50,0.,1.,0.)
         CALL HBOOK1(123,'DBTG: QIPBTAG signif',50,-10.,10.,0.)
         CALL HBOOK1(124,'DBTG: QIPBTAG flag ',50,0.,1000.,0.)
         CALL HBOOK1(125,'DDLT: QSELEP part type',25,0.,25.,0.)
         CALL HBOOK1(126,'DDLT: QSELEP incl pt  ',50,0.,20.,0.)
         CALL HIDOPT(0,'STAT')
         CALL HIDOPT(0,'BLAC')
         CALL HIDOPT(0,'1EVL')
         CALL HIDOPT(0,'NPCO')
         CALL HIDOPT(0,'NPCH')
         FIRST = .FALSE.
      ENDIF
C
C++   Check if we are really writing a Mini-DST event.
C
      IF(MLISTE.EQ.' ') THEN
         IMAQQ = -1
         RETURN
      ENDIF
C
C++   Don't fill histograms for Lumi events.
C
      LUMI = INDEX(MLISTE,'LUPA').NE.0 .OR. INDEX(MLISTE,'DSIC').NE.0
      IF(.NOT.MINIMC .AND. LUMI) THEN
         IMAQQ = 0
         RETURN
      ENDIF
C
C++   Guess if this is a qq event from class word.
C
      IMAQQ = 0
      CL24 = .FALSE.
      KREVH = IW(NAMIND('REVH'))
      IF (KREVH.GT.0) THEN
         IF (LCOLS(KREVH).GE.JREVEC) THEN
            IF (BTEST(ITABL(KREVH,1,JREVEC),16-1)) IMAQQ = 1
            IF (BTEST(ITABL(KREVH,1,JREVEC),24-1)) CL24 = .TRUE.
         ENDIF
      ENDIF
C
      CALL HFILL(  1,T,0.,1.)
      IF (IMAQQ.EQ.1) CALL HFILL(101,T,0.,1.)
C
C++   Fill track momenta and basic calorimetry histograms for Class 24.
C
      IF (CL24) THEN
         CALL HFILL(203,SUMMIN('DTRA',JDTRP0),0.,1.)
         CALL HFILL(210,SUMMIN('DECO',JDECE0),0.,1.)
         CALL HFILL(211,SUMMIN('DHCO',JDHCE0),0.,1.)
         CALL HFILL(212,SUMMIN('DEWI',JDEWE0),0.,1.)
      ENDIF
C
C++   Continue filling histograms for qq events and MC data.
C
      IF (MINIMC) IMAQQ = +1
      IF (IMAQQ.LE.0) RETURN
C
C++   DVER: radius.
C
      KDVER = NLINK('DVER',0)
      IF(KDVER.GT.0) THEN
         RAD = SQRT(FTABL(KDVER,1,JDVEX0)**2 + FTABL(KDVER,1,JDVEY0)**2)
     &     / DFACTM
      ELSE
         RAD = 0.
      ENDIF
      CALL HFILL(102,RAD,0.,1.)
C
C++   DTRA, DGID, DGPC: sum energy.
C
      CALL HFILL(103,SUMMIN('DTRA',JDTRP0),0.,1.)
      CALL HFILL(104,SUMMIN('DGID',JDGICE),0.,1.)
      CALL HFILL(121,SUMMIN('DGAC',JDGAEC),0.,1.)
C
C++   DEID: R2 and R3.
C
      KDEID = NLINK('DEID',0)
      IF(KDEID.GT.0) THEN
         NDEID = LROWS(KDEID)
      ELSE
         NDEID = 0
      ENDIF
C
      DO 160 I=1,NDEID
         R2 = FTABL(KDEID,I,JDEIR2) / 100.
         R3 = FTABL(KDEID,I,JDEIR3) / 100.
         CHISQ = R2**2 + R3**2
         CALL HFILL(106,CHISQ,0.,1.)
  160 CONTINUE
C
C++   HMAD: sum hits.
C
      KHMAD = NLINK('HMAD',0)
      IF(KHMAD.GT.0) THEN
         NHMAD = LROWS(KHMAD)
         IF (INDEX(MLISTE,'HMAD').LE.0) NHMAD = 0
      ELSE
         NHMAD = 0
      ENDIF
C
      DO 170 I=1,NHMAD
         NFIRE = ITABL(KHMAD,I,JHMANF)
         CALL HFILL(107,FLOAT(NFIRE),0.,1.)
  170 CONTINUE
C
C++   MUID: flag.
C
      KMUID = NLINK('MUID',0)
      IF(KMUID.GT.0) THEN
         NMUID = LROWS(KMUID)
      ELSE
         NMUID = 0
      ENDIF
C
      DO 175 I=1,NMUID
         IFLAG = ITABL(KMUID,I,JMUIIF)
         CALL HFILL(107,50+FLOAT(IFLAG),0.,1.)
  175 CONTINUE
C
C++   DTRA: number of vertices in an event.
C
      MXWRD = 0
      KDTRA = IW(NAMIND('DTRA'))
      IF (KDTRA.GT.0) THEN
         DO 180 I=1,LROWS(KDTRA)
            IWRD = ITABL(KDTRA,I,JDTRVB)
  180    IF (IWRD.GT.MXWRD) MXWRD = IWRD
      ENDIF
C
      IF (MXWRD.GT.0) THEN
         DO 185 I=30,1,-1
  185    IF (JBIT(MXWRD,I).EQ.1) GOTO 186
  186    NVER = I
      ELSE
         NVER = 0
      ENDIF
      CALL HFILL(108,FLOAT(NVER),0.,1.)
C
C++   DTRA: number of Vdet hits and number of ambiguities.
C
      IF (KDTRA.GT.0) THEN
         DO 190 I=1,LROWS(KDTRA)
            IWRD = ITABL(KDTRA,I,JDTRVI)
            NVDET = JBYT(IWRD,1,2) + JBYT(IWRD,3,2) + JBYT(IWRD,5,2)
     &            + JBYT(IWRD,7,2)
            NAMB = JBIT(IWRD,9) + JBIT(IWRD,10)
            CALL HFILL(109,FLOAT(NVDET),0.,1.)
            CALL HFILL(109,FLOAT(10+NAMB),0.,1.)
  190    CONTINUE
      ENDIF
C
C++   DENF, DJET: sum energy.
C
      SUME = 0.
      KDENF = NLINK('DENF',3)
      IF (KDENF.GT.0) THEN
         DO 230 I=1,LROWS(KDENF)
            PX = FLOAT(ITABL(KDENF,I,JDENPX)) / EFACTM
            PY = FLOAT(ITABL(KDENF,I,JDENPY)) / EFACTM
            PZ = FLOAT(ITABL(KDENF,I,JDENPZ)) / EFACTM
            AM = FLOAT(ITABL(KDENF,I,JDENE0)) / EFACTM
            SUME = SUME + SQRT(PX**2+PY**2+PZ**2+AM**2)
  230    CONTINUE
      ENDIF
C
      CALL HFILL(113,SUME                 ,0.,1.)
      CALL HFILL(114,SUMMIN('DJET',JDJEE0),0.,1.)
C
C
C++   DTHR thrust.
C
      KDTHR = NLINK('DTHR',0)
      IF (KDTHR.GT.0) THEN
         DO 250 I=1,LROWS(KDTHR)
            THRUST = FLOAT(ITABL(KDTHR,I,JDTHPR)) / FFA
            CALL HFILL(122,THRUST,0.,1.)
  250    CONTINUE
      ENDIF
C
C
C++   DDLT particle type and pt.
C
      KDDLT = NLINK('DDLT',0)
      IF (KDDLT.GT.0) THEN
         DO 260 I=1,LROWS(KDDLT)
            PTYPE = FLOAT(ITABL(KDDLT,I,JDDLPA))
            PTINC = FLOAT(ITABL(KDDLT,I,JDDLPI)) / FFA
            CALL HFILL(125,PTYPE,0.,1.)
            CALL HFILL(126,10.*PTINC,0.,1.)
  260    CONTINUE
      ENDIF
C
C++   DBTG probility and reject flag.
C
      KDBTG = NLINK('DBTG',1)
      IF (KDBTG.GT.0) THEN
         DO 270 I=1,LROWS(KDBTG)
            IWRD1 = ITABL(KDBTG,I,1)
            PFLAG = FLOAT(JBYT(IWRD1,9,14))
            PROBA = FLOAT(ITABL(KDBTG,I,2)) / 1.0E6

            CALL HFILL(123,PROBA,0.,1.)
            CALL HFILL(124,PFLAG,0.,1.)
  270    CONTINUE
      ENDIF
C
      RETURN
      END
