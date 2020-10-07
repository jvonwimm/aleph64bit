      SUBROUTINE ASREVE
C ----------------------------------------------------------------
C - F.Ranjard - 861015
C! Decode EVEH KEVH and ASEV banks
C - Called by : ASIEVE                                 from this .HLB
C ---------------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      PARAMETER(JKEVRN=1,JKEVNT=2,JKEVNV=3,JKEVPI=4,JKEVWT=5,JKEVSR=6,
     +          JKEVTR=7,LKEVHA=7)
      PARAMETER(JKHIHC=1,LKHISA=1)
      PARAMETER(JKJOJD=1,JKJOJT=2,JKJOAV=3,JKJODV=4,JKJODC=5,LKJOBA=5)
      PARAMETER(JKLIGN=1,LKLINA=1)
      PARAMETER(JKPOKI=1,JKPOHX=2,JKPOHY=3,JKPOHZ=4,LKPOLA=4)
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
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
      DIMENSION JRND(LRND,LPRO)
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
C ----------------------------------------------------------------
C - save random #s set by data cards
      IF (NEVTJO .EQ. 1) CALL UCOPY (IRNDJO,JRND,LPRO*LRND)
C
C - Decode EVEH bank
      KEVEH = IW(NAEVEH)
      IF (KEVEH .NE. 0) THEN
        ECMSKI = IW(KEVEH+JEVETE) * 1.E-6
        IF (IW(KEVEH+JEVEES).NE.1) THEN
          KERRJO = IW(KEVEH+JEVEES) - LOFFMC
          CALL ALTELL ('ASREVE: incomplete input ',KERRJO,'NEXT')
        ELSE
C         for complete events
C           Decode kine event header KEVH bank
          JKEVH = IW(NAKEVH)
          IF (JKEVH .NE. 0) THEN
            KKEVH = JKEVH + LMHLEN
            NITRKI    = IW(KKEVH+JKEVNT)
            NIVXKI    = IW(KKEVH+JKEVNV)
            IDEVKI    = IW(KKEVH+JKEVPI)
            WEITKI    = RW(KKEVH+JKEVWT)
            IF (IPROJO(1) .NE. 0) THEN
C                 KINE process is required, take data card RNDM/RINI
              IRNDJO(1,1) = JRND(1,1)
              IRNDJO(2,1) = JRND(2,1)
              IRNDJO(3,1) = JRND(3,1)
            ELSE
              IRNDJO(1,1) = IW(KKEVH+JKEVRN)
              IRNDJO(2,1) = IW(KKEVH+JKEVSR)
              IRNDJO(3,1) = IW(KKEVH+JKEVTR)
            ENDIF
          ENDIF
C        Decode ASEV bank
          JASEV = IW(NAASEV)
          IF (JASEV .NE. 0) THEN
            KASEV = JASEV + LMHLEN
            DO 6 I=2,LROWS(JASEV)
              IF (IPROJO(I) .NE. 0) THEN
C                    the process is requested, take data card rndm
                IRNDJO(1,I) = JRND(1,I)
                IRNDJO(2,I) = JRND(2,I)
                IRNDJO(3,I) = JRND(3,I)
              ELSE
                IRNDJO(1,I) = ITABL(JASEV,I,JASERG)
                IRNDJO(2,I) = ITABL(JASEV,I,JASERG+1)
                IRNDJO(3,I) = ITABL(JASEV,I,JASERG+2)
              ENDIF
    6       CONTINUE
          ENDIF
        ENDIF
      ENDIF
      CALL VZERO (JRND,LPRO*LRND)
C
C - debug
      IF (FDEBJO) THEN
        WRITE (LOUTIO,820) NEVTJO,IW(KEVEH+6),(IW(KEVEH+I),I=1,5)
     &                     ,(IW(KEVEH+I),I=11,12)
     &                     ,FLOAT(IW(KEVEH+13))*1.E-6
  820   FORMAT (/1X,'+++ASREVE+++ event# ',I5,', trigger# ',I5,
     &           ', EVEH ',7I10,F12.4)
        WRITE (LOUTIO,822) IPROKI,NIVXKI,NITRKI,WEITKI
  822   FORMAT (14X,'Process id. = ',I6,'  # of input vertices = ',I3,
     &               '  # of input tracks = ',I4,'  weight = ',E12.4)
        WRITE (LOUTIO,821) IRNDJO
  821   FORMAT (14X,'random generator roots used during this job '
     &              /(14X,3I15,10X,3I15))
        IF (JASEV .GT. 0) WRITE (LOUTIO,823) (IW(KASEV+M),M=1,IW(JASEV)
     &                                                         -LMHLEN)
  823   FORMAT (14X,'random generator roots used during previous job '
     &              /(14X,3I15,10X,3I15))
      ENDIF
      RETURN
      END
