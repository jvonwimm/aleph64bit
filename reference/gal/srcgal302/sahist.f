      SUBROUTINE SAHIST
C-----------------------------------------------------------------------
C!    Fill histograms for the SATR test run
C                                   H.Burkhardt   October 1986
C     called from SADIGI if SATR hist flag is set
C     fill track info scattered over three banks temporarily
C     in PP vector to avoid a lot of forward-backword pointing
C     the histogramming is of interest for the following questions:
C        in small angle BHABHA scattering
C           how many secondaries are produced by the beam pipe
C           what is their impact on the SATR Monitor
C           and by how much do they change the measurement of
C           cos theta
C-----------------------------------------------------------------------
      SAVE
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
      PARAMETER(LCSAHT=1,MXSAHT=500,LCSADI=1)
      COMMON/SANAMC/NASAHT,NASADI
      COMMON/SATRCO/NSECSA,NCRASA,NCARSA,NTDCSA,NSIDSA,NLAYSA,NBRASA,
     .      RMINSA,RMAXSA,BRTHSA,ZLUMSA,DEADSA,DGASSA,COSTSA,
     .      NHITSA,NLOSSA,NWIRSA,XHITSA,XLOSSA,XWIRSA,XEVTSA,
     .      NHLASA(9),NHLDSA(9),PHIBSA(9)
C
       COMMON /WRKSPC/ WSPACE(88320)
C     using the working space NPARM could be maximal 8000
      PARAMETER (NPARM=100)
      DIMENSION PP(10,NPARM)
      EQUIVALENCE (WSPACE(1),PP(1,1))
      CHARACTER NAME*4,CHAINT*4
      EXTERNAL NAMIND
      EXTERNAL CHAINT
      DATA ID/600/
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
C - index of the next vertex/track to be stored in KINE/VERT
C   bank known by its index JVK
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)
C - # of vertices/tracks which could be stored in KINE/VERT
C   bank known by its index JVK
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))
C - index of the 1st parameter of KINE/VERT bank known by its
C   index JVK
      KPARVK(JVK) = JVK + IW(JVK+1)
C - index of 1st vertex/track # contained into the list of
C   bank KINE/VERT known by its index JVK
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)
C - charge of ALEPH particle# JPA
      CHARGE(JPA) = RTABL(IW(NAPART),JPA,7)
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPART),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPART),JPA,8)
C - # of vertices on a track known by its BOS index /
C   # of outgoing tracks of a vertex known by its BOS index
      NOFVK(JVK)  = IW(JVK+3)
C - Particle type of a track known by its BOS index
      KINTYP(JVK) = IW(KPARVK(JVK)+5)
C - incoming track # of a vertex known by its BOS index
      INPTRK(JVK) = IW(KPARVK(JVK)+5)
C - origin vertex # of a track known by its BOS index
      INPVRT(JVK) = IW(KLISVK(JVK)+1)
C - momentum of a track known by its BOS index
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2
     &                   +RW(KPARVK(JVK)+3)**2)
C - mass of a track known by its BOS index
      PMASVK(JVK) = RW(KPARVK(JVK)+4)
C - energy of a track known by its BOS index
      ENERVK(JVK) = SQRT (PMODVK(JVK)**2 + PMASVK(JVK)**2)
C - time of flight of the icoming particle to the vertex known by its
C   BOS index
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)
C - radius of the vertex known by its BOS index
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)
C - mother track # of a track known by its BOS index
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))
C - alephlib version# used in KINGAL (NKJ is the name-index of KJOB)
      ALEKIN(NKJ) = ITABL(IW(NKJ),1,JKJOAV)
C
      LOUT=IW(6)
      JVER = NLINK ('VERT',1)
      DO 1 I=1,NPARM
C       loop over vertex bank
        IF(JVER.EQ.0) GOTO 2
        NVX=IW(JVER-2)
        LHDR=IW(JVER+1)
        LPVER=IW(JVER+2)
        MXTRK=IW(JVER+3)
        KVER=JVER+LHDR
C       store the vertex info
C       one vertex can belong to a sequence of tracks, so loop
        DO 9 ITRK=IW(KVER+LPVER+1),IW(KVER+LPVER+MXTRK)
          IF(ITRK.GT.NPARM) GOTO 9
          PP(7,ITRK)=RW(KVER+1)
          PP(8,ITRK)=RW(KVER+2)
          PP(9,ITRK)=RW(KVER+3)
          RORG=SQRT(RW(KVER+1)**2+RW(KVER+2)**2)
          PP(10,ITRK)=RORG
    9   CONTINUE
        JVER=IW(JVER-1)
    1 CONTINUE
    2 CONTINUE
      JKIN = NLINK ('KINE',1)
C     count total number of charged and neutral tracks
      NCHAR=0
      NNEUT=0
      COSTSA=0.
      DO 3 I=1,NPARM
C       loop over kine bank
        IF(JKIN.EQ.0) GOTO 4
        NTRK=IW(JKIN-2)
        LHKIN=IW(JKIN+1)
        LPKIN=IW(JKIN+2)
        MXVX=IW(JKIN+3)
        KIN=JKIN+LHKIN
C       now store also the momentum/energy
        PP(1,NTRK)=RW(KIN+1)
        PP(2,NTRK)=RW(KIN+2)
        PP(3,NTRK)=RW(KIN+3)
        PP(4,NTRK)=ENERVK(JKIN)
        IF(NTRK.EQ.1) THEN
C         store cos theta of the original particle
          PTOT=SQRT(PP(1,1)**2+PP(2,1)**2+PP(3,1)**2)
          IF(PTOT.GT.0.) COSTSA=PP(3,1)/PTOT
        ENDIF
        ITYP=IW(KIN+5)
        IF(ITYP.EQ.1) THEN
          PP(5,NTRK)=0.0
          PP(6,NTRK)=0.
          NNEUT=NNEUT+1
        ELSEIF(ITYP.EQ.2) THEN
          PP(5,NTRK)=0.00051
          PP(6,NTRK)=1.
          NCHAR=NCHAR+1
        ELSE
          PP(5,NTRK)=0.00051
          PP(6,NTRK)=-1.
          NCHAR=NCHAR+1
        ENDIF
        JKIN=IW(JKIN-1)
    3 CONTINUE
    4 CONTINUE
      CALL HFILL(ID+1,FLOAT(NCHAR),0.,1.)
      CALL HFILL(ID+2,FLOAT(NNEUT),0.,1.)
C     point to IMPA bank (since NAIMPA seems wrong)
      JIMPA=NAMIND('IMPA')+1
C     count number of charged and neutral tracks in SATR acceptance
      NCHAA=0
      NNEUA=0
      DO 5 I=1,NPARM
C       loop over impact bank
        JIMPA=IW(JIMPA-1)
        IF(JIMPA.EQ.0) GOTO 6
        KIMPA=JIMPA+LMHLEN
        DO 7 J=1,LROWS(JIMPA)
          NAME=CHAINT(IW(KIMPA+1))
          IF(NAME.EQ.'SATR') THEN
C           point to info in KINE and PART bank
C           overwrite momentum and vertex for tracks hitting SATR
C           in the PP vector
            NR=IW(JIMPA-2)
            JKIN=NLINK('KINE',NR)
            LHKIN=IW(JKIN+1)
            LPKIN=IW(JKIN+2)
            MXVX=IW(JKIN+3)
            KIN=JKIN+LHKIN
            ITYP=IW(KIN+5)
            KPART=IW(NAPART)
            CHARG=RTABL(KPART,ITYP,7)
C           WRITE(LOUTIO,'('' SAHIST POINTING BACK  NR,ITYP,CHARG='',2I6
C    .                  F12.4)') NR,ITYP,CHARG
C           use position and momentum/energy when hitting SATR
            PP(1,NR)=RW(KIMPA+5)*RW(KIMPA+8)
            PP(2,NR)=RW(KIMPA+6)*RW(KIMPA+8)
            PP(3,NR)=RW(KIMPA+7)*RW(KIMPA+8)
            PP(4,NR)=RW(KIMPA+8)
            PP(7,NR)=RW(KIMPA+2)
            PP(8,NR)=RW(KIMPA+3)
            PP(9,NR)=RW(KIMPA+4)
            PP(10,NR)=SQRT(PP(7,NR)**2+PP(8,NR)**2)
C           reverse sign of pp(10 to signal track as accepted
            IF(PP(4,NR).GT.0.1) THEN
              PP(10,NR)=-PP(10,NR)
              IF(PP(6,NR).EQ.0) THEN
                NNEUA=NNEUA+1
              ELSE
                NCHAA=NCHAA+1
              ENDIF
            ENDIF
            IF(FDEBJO.AND.IPRIJO(6).EQ.1)
     .        WRITE(LOUT,'('' +++SAHIST PVEC FOR TRACK'',I3,'':'',5F9.5,
     .           F4.0,2F9.5,F11.5,F9.5)') I,(PP(IT,I),IT=1,10)
          ENDIF
          KIMPA=KIMPA+LCOLS(JIMPA)
    7   CONTINUE
    5 CONTINUE
    6 CONTINUE
      IF(NCHAA.GT.0) NCHAA=NCHAA-1
      CALL HFILL(ID+3,FLOAT(NCHAA),0.,1.)
      IF(NCHAA.GT.0) CALL HFILL(ID+16,ABS(COSTSA),0.,FLOAT(NCHAA))
      CALL HFILL(ID+4,FLOAT(NNEUA),0.,1.)
      DO 10 I=1,9
        CALL HFILL(ID+30+I,FLOAT(NHLASA(I)),0.,1.)
        NHLASA(I)=0
   10 CONTINUE
      DO 20 I=1,9
        CALL HFILL(ID+40+I,FLOAT(NHLDSA(I)),0.,1.)
        NHLDSA(I)=0
   20 CONTINUE
C     get number of remaining wires after oring from SADI bank
      KSADI=IW(NASADI)
      IF(KSADI.EQ.0) GOTO 900
      NWIR=LROWS(KSADI)
      NORE=NWIRSA-NWIR
      CALL HFILL(ID+26,FLOAT(NHITSA),0.,1.)
      CALL HFILL(ID+28,FLOAT(NLOSSA),0.,1.)
      CALL HFILL(ID+27,FLOAT(NWIRSA),0.,1.)
      CALL HFILL(ID+29,FLOAT(NORE)  ,0.,1.)
      IF (IPRIJO(6) .EQ. 1) THEN
         IF(FDEBJO .OR. XEVTSA.LE.100. .OR. NORE.GE.10)
     &   WRITE(LOUT,'(/1X,''+++SAHIST #Evt='',I5,'' CosTh='',F8.6,
     &   ''    #Part. from Bpipe: #El(total/sec. accepted)='',2I3,
     &   ''  #Gam(total/acc)='',2I3,/'' +++SAHIST'',
     &   '' #Hits  total='',I3,'' #lost in dead regions='',I2,4X,
     &   '' #Wires total='',I3,'' #ored='',I2)') NEVTJO,COSTSA,
     &   NCHAR,NCHAA,NNEUT,NNEUA,NHITSA,NLOSSA,NWIRSA,NORE
      ENDIF
      NHITSA=0
      NLOSSA=0
      IEMAX=0
      EMAX=0.
      DO 100 I=1,NTRK
        IF(PP(6,I).NE.0.AND.PP(4,I).GT.EMAX.AND.PP(10,I).LT.0.) THEN
          EMAX=PP(4,I)
          IEMAX=I
        ENDIF
 100  CONTINUE
      CALL HFILL(ID+10,ABS(COSTSA),0.,1.)
      COSTM=0.
      IF(IEMAX.GT.0) THEN
        CALL HFILL(ID+5,EMAX,0.,1.)
        RAMI=SQRT(PP(7,IEMAX)**2+PP(8,IEMAX)**2+PP(9,IEMAX)**2)
        IF(RAMI.GT.0.) COSTM=PP(9,IEMAX)/RAMI
        CALL HFILL(ID+13,ACOS(COSTM)-ACOS(COSTSA),0.,1.)
      ENDIF
      DO 110 I=1,NTRK
        RADI=SQRT(PP(7,I)**2+PP(8,I)**2+PP(9,I)**2)
        IF(RADI.EQ.0) GOTO 110
        COSTR=PP(9,I)/RADI
        DIST=SQRT( (PP(7,IEMAX)-PP(7,I))**2+(PP(8,IEMAX)-PP(8,I))**2+
     .             (PP(9,IEMAX)-PP(9,I))**2 )
        DISTT=ABS(SQRT(PP(7,IEMAX)**2+PP(8,IEMAX)**2)-
     .        SQRT(PP(7,I)**2+PP(8,I)**2))
        IF(PP(6,I).EQ.0) THEN
          CALL HFILL(ID+8,PP(4,I),0.,1.)
          IF(PP(10,I).LT.0.) THEN
            CALL HFILL(ID+9,PP(4,I),0.,1.)
            CALL HFILL(ID+14,DIST,PP(4,I),1.)
            CALL HFILL(ID+15,DISTT,PP(4,I),1.)
          ENDIF
        ELSEIF(I.NE.IEMAX) THEN
          CALL HFILL(ID+6,PP(4,I),0.,1.)
          IF(PP(10,I).LT.0.) THEN
            CALL HFILL(ID+7,PP(4,I),0.,1.)
            CALL HFILL(ID+11,DIST,PP(4,I),1.)
            CALL HFILL(ID+12,DISTT,PP(4,I),1.)
          ENDIF
        ENDIF
  110 CONTINUE
  900 CONTINUE
      END
