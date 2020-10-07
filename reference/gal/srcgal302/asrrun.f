      SUBROUTINE ASRRUN
C ------------------------------------------------------------------
C - F.Ranjard - 861013
C!  Decode contents of RUNH and ASRU
C - Called from   ASIJOB, ASPRUN                  from this .HLB
C ------------------------------------------------------------------
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
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
      PARAMETER(JACUMN=1,JACUGC=2,JACUEC=3,JACUHC=4,JACUNC=5,JACUMC=6,
     +          LACUTA=6)
      PARAMETER(JAFIAR=1,JAFIAZ=2,JAFIMF=3,JAFIBE=4,LAFIDA=4)
      PARAMETER(JAJOBM=1,JAJORM=2,JAJOGI=3,JAJOSO=4,JAJOSD=5,JAJOGC=6,
     +          JAJOJD=7,JAJOJT=8,JAJOGV=9,JAJOAV=10,JAJOFT=11,
     +          JAJOFS=12,JAJOFC=13,JAJODV=14,JAJODD=15,JAJOTV=16,
     +          JAJOCV=17,JAJOGN=18,LAJOBA=18)
      PARAMETER(JAKIKT=1,JAKIKP=2,LAKINA=9)
      PARAMETER(JAPRPF=1,JAPRRG=2,LAPROA=4)
      PARAMETER(JARURC=1,LARUNA=10)
      PARAMETER(JASERG=1,LASEVA=3)
      PARAMETER(JATIRT=1,LATITA=1)
      PARAMETER(JKEVRN=1,JKEVNT=2,JKEVNV=3,JKEVPI=4,JKEVWT=5,JKEVSR=6,
     +          JKEVTR=7,LKEVHA=7)
      PARAMETER(JKHIHC=1,LKHISA=1)
      PARAMETER(JKJOJD=1,JKJOJT=2,JKJOAV=3,JKJODV=4,JKJODC=5,LKJOBA=5)
      PARAMETER(JKLIGN=1,LKLINA=1)
      PARAMETER(JKPOKI=1,JKPOHX=2,JKPOHY=3,JKPOHZ=4,LKPOLA=4)
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
      PARAMETER(JRLELE=1,JRLELB=2,JRLELD=3,JRLELF=4,JRLELP=5,LRLEPA=5)
      PARAMETER (LNAM=4)
      CHARACTER*4 CHAINT,TNAME(LNAM)
      DATA TNAME /'LUND','SJET','PART','USER'/
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
C ------------------------------------------------------------------
C
C - Decode RUNH bank
      KRUNH  = IW(NARUNH)
      KEVEH = IW(NAEVEH)
      IF (KRUNH .EQ. 0) KRUNH = KEVEH
      IF (KRUNH .GT. 0) THEN
        IEXPJO = IW(KRUNH+1)
        IRUNJO = IW(KRUNH+2)
        IPROKI = IW(KRUNH+3)
        MPRO   = IPROKI / LOFFMC
        IF (MPRO .EQ. 0) THEN
          CALL ALTELL ('ASRRUN: not a MonteCarlo runtype ',0,'STOP')
        ELSE
          MPRO = MIN (MPRO,LNAM)
          IF (KRUNH .NE. KEVEH) THEN
            WRITE (LOUTIO,810) IRUNJO,IW(KRUNH+3),TNAME(MPRO)
     &                     ,(IW(KRUNH+M),M=4,5)
  810       FORMAT (/1X,'+++ASRRUN+++ RUN # ',I6,3X,'run type = '
     &            ,I6,2X,A4,3X,'date and time ',I6,2X,I8/)
          ELSE
            WRITE (LOUTIO,820) IRUNJO,IW(KRUNH+3),TNAME(MPRO)
  820       FORMAT (/1X,'+++ASRRUN+++ RUN # ',I6,3X,'run type = '
     &                    ,I6,2X,A4)
          ENDIF
        ENDIF
      ENDIF
C
C - Decode RLEP bank
      JRLEP = IW(NAMIND('RLEP'))
      IF (JRLEP .NE. 0) THEN
        ALECMS = 2.*REAL(ITABL(JRLEP,1,JRLELE)) * .001
      ENDIF
C
C - Decode Axxx banks
      JATIT = IW(NAMIND('ATIT'))
      IF (JATIT .NE. 0) THEN
        NATIT = LROWS(JATIT)
        WRITE(LOUTIO,'(/1X,''+++ASRRUN+++ Galeph title: '',
     &                   20A4)')
     &                   (CHAINT(IW(JATIT+LMHLEN+J)),J=1,NATIT)
      ENDIF
C
      JAKIN = IW(NAMIND('AKIN'))
      IF (JAKIN .NE. 0) THEN
        KAKIN = JAKIN + LMHLEN
        TKINJO = CHAINT (IW(KAKIN+JAKIKT))
        LKIN = MIN (LKINP,LCOLS(JAKIN)-JAKIKP+1)
        CALL UCOPY (RW(KAKIN+JAKIKP),BKINJO(1),LKIN)
      ENDIF
C
      JAFID = IW(NAMIND('AFID'))
      IF (JAFID .NE. 0) THEN
        IF (IPROJO(2) .EQ. 0) THEN
          ALRMAX = RTABL(JAFID,1,JAFIAR)
          ALZMAX = RTABL(JAFID,1,JAFIAZ)
          ALFIEL = RTABL(JAFID,1,JAFIMF)
          ALECMS = RTABL(JAFID,1,JAFIBE)
        ENDIF
      ENDIF
C
C - Decode KRUN bank
      JKRUN = IW(NAKRUN)
      IF (JKRUN .NE. 0) THEN
        NOTRKI = ITABL (JKRUN,1,JKRUNO)
        IPROKI = ITABL (JKRUN,1,JKRUGI)
        WRITE (LOUTIO,'(/1X,''+++ASRRUN+++ Kine title: '',12A4/
     &                       ''  - generator identifier: '',I10,3X,
     &                       ''  - NOtracking marker word: '',I6)')
     &    (CHAINT(IW(JKRUN+LMHLEN+I)),I=JKRURT,JKRUFS-1),IPROKI,NOTRKI
      ENDIF
C
      RETURN
      END
