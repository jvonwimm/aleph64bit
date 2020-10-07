      SUBROUTINE HCSTAT
C --------------------------------------------
C
C! Statistics on single event
C!
C!        Author    :G.Catanesi  87/07/10
C!
C!   -Called by : HCASIG
C!   -Calls     : HCVRTX
C ---------------------------------------------------
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
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
      CHARACTER*4 TVOLU,CHAINT,CTABL
      LOGICAL FMUON,FNUNO,FEGAM,FHADR
      FMUON(IJKL) = IJKL.EQ.5 .OR. IJKL.EQ.6
      FEGAM(IJKL) = IJKL.LE.3
      FNUNO(IJKL) = IJKL.EQ.4
      FHADR(IJKL) = IJKL.GT.6
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
      CTABL (ID,NRBOS,L) = CHAINT(ITABL(ID,NRBOS,L))
C ------------------------------------------------------------------
C
      NHCSE = LROWS(JDHCSE)
      NHCHI = LROWS(JDHCHI)
      KHCHI = JDHCHI + LMHLEN
      NSTRM = 0
      DO 10 I=1,NHCHI
         NSTRM = NSTRM + IW(KHCHI+4)
   10 KHCHI = KHCHI + LCOLS(JDHCHI)
      JHTHT = IW(NAHTHT)
      NHTHT = LROWS (JHTHT)
      ETOT = 0.
      KHTHT = JHTHT + LMHLEN
      DO 20 I=1,NHTHT
         ETOT = ETOT + RW(KHTHT+4)
   20 KHTHT = KHTHT + LCOLS(JHTHT)
      JHWHT = IW(NAHWHT)
      NHWHT = LROWS (JHWHT)
C
      NHCC02 = NHCC02+NHCSE
      NHCC03 = NHCC03+NHWHT
      NHCC04 = NHCC04+NHTHT
      NHCC05 = NHCC05+1
      HCEAVE = HCEAVE + ETOT
      HCANST = HCANST + FLOAT(NSTRM)
C
C? this option is active only in the debug case
C
      IF(FHCDB2) THEN
C
         NHCTH = LROWS(JDHCTH)
         JKVOL = IW(NAKVOL)
C
C? select entering particles from the total sample of tracks in HCAL
C
         DO 60 J=1,NHCTH
C
            NKINE = ITABL(JDHCTH,J,1)
   30       JKINE = NLINK ('KINE',NKINE)
            NVERT = INPVRT (JKINE)
            TVOLU = CHAINT (ITABL(JKVOL,NVERT,1))
            IF (TVOLU(1:1) .EQ. 'H') THEN
               JVERT = NLINK ('VERT',NVERT)
               NKINE = INPTRK(JVERT)
               GOTO 30
            ELSE
               DO 40 JROW = 1,LROWS(JDHCTH)
                  IF (NKINE .EQ. ITABL(JDHCTH,JROW,1)) THEN
                     KHCTH = KROW (JDHCTH,JROW)
                     GOTO 50
                  ENDIF
   40          CONTINUE
C?                add track# NKINE in JDHCTH
               IF (LFRROW(JDHCTH) .EQ. 0) THEN
                  CALL WBANK (IW,JDHCTH,IW(JDHCTH)*2,*90)
               ENDIF
               JROW = LROWS(JDHCTH) + 1
               KHCTH = KNEXT(JDHCTH)
               IW(KHCTH+1) = NKINE
               IW(JDHCTH+LMHROW) = JROW
C                 update the energy deposited
   50          CONTINUE
               IW(KHCTH+2) = KINTYP (JKINE)
               RW(KHCTH+4) = ENERVK (JKINE)
               IF (J .NE. JROW) RW(KHCTH+5) = RW(KHCTH+5) + RTABL
     +         (JDHCTH,J,5)
               IW(KHCTH+6) = 1
            ENDIF
   60    CONTINUE
C
C? Write statistical informations about this event
C
         WRITE(LOUTIO,500)
         WRITE(LOUTIO,510)NEVTJO
         WRITE(LOUTIO,520)NHCSE,NHWHT,NHTHT,NSTRM,ETOT
         WRITE(LOUTIO,530)(HCEPOR(J) ,J=1,3)
C
         NHCET = 0
         DO 70 I = 1,LROWS (JDHCTH)
            NHCET = NHCET + ITABL(JDHCTH,I,6)
   70    CONTINUE
         WRITE(LOUTIO,540)NHCET
         WRITE(LOUTIO,550)
C
C?  Loop on HCAL entering particles
C
         ETOT = 0.
         VETOT = 0.
         NEGAM = 0
         NNEUT = 0
         NMUON = 0
         NHADC = 0
         NHADN = 0
         EGAM = 0.
         ENEU = 0.
         EMUON = 0.
         EHADC = 0.
         EHADN = 0.
         VEGAM = 0.
         VENEU = 0.
         VMUON = 0.
         VHADC = 0.
         VHADN = 0.
         KHCTH = JDHCTH + LMHLEN
         DO 80 J=1,NHCTH
            IF (IW(KHCTH+6) .EQ. 0) GOTO 80
C
C? founds the particle type
C
            ITYPE = IW(KHCTH+2)
C
C?  Groups together similar kind of particles
C
            IF(FMUON(ITYPE) .AND. RW(KHCTH+4).GT.2.) THEN
               VETOT = VETOT + 4.
            ELSE
               VETOT = VETOT + RW(KHCTH+4) + PARMAS(ITYPE)* HPANTP
     +         (ITYPE)
            ENDIF
C
            ETOT = ETOT + RW(KHCTH+4)
C
            IF (FEGAM(ITYPE)) THEN
               NEGAM = NEGAM + 1
               EGAM = EGAM + RW(KHCTH+4)
               VEGAM = VEGAM + RW(KHCTH+5)
            ELSEIF(FNUNO(ITYPE))THEN
               NNEUT = NNEUT + 1
               ENEU = ENEU + RW(KHCTH+4)
               VENEU = VENEU + RW(KHCTH+5)
            ELSEIF(FMUON(ITYPE))THEN
               NMUON = NMUON + 1
               EMUON = EMUON + RW(KHCTH+4)
               VMUON = VMUON + RW(KHCTH+5)
            ELSEIF(FHADR(ITYPE))THEN
               IF(CHARGE(ITYPE).NE.0.)THEN
                  NHADC = NHADC + 1
                  EHADC = EHADC + RW(KHCTH+4)
                  VHADC = VHADC + RW(KHCTH+5)
               ELSE
                  NHADN = NHADN + 1
                  EHADN = EHADN + RW(KHCTH+4)
                  VHADN = VHADN + RW(KHCTH+5)
               ENDIF
            ENDIF
C
C print particle entering list if the level 1 of the printout in on
C
            IF(ICHCJO(1).GT.0)THEN
               JPART = IW(NAPART)
               WRITE(LOUTIO,560)J,IW(KHCTH+1),(RW(KHCTH+II),II=3,5) ,
     +         CTABL(JPART,ITYPE,2),CTABL(JPART,ITYPE,3),CTABL(JPART,
     +         ITYPE,4)
            ENDIF
C
            KHCTH = KHCTH + LCOLS(JDHCTH)
   80    CONTINUE
C
         VETOT = VETOT - ENEU
C
         WRITE(LOUTIO,570)ETOT,VETOT, EGAM,VEGAM,NEGAM, ENEU,VENEU,
     +   NNEUT, EMUON,VMUON,NMUON, EHADC,VHADC, NHADC, EHADN,VHADN,
     +   NHADN
C
      ENDIF
      GOTO 100
C
   90 CONTINUE
      WRITE (LOUTIO,'(/1X,''+++HCSTAT+++  not enough space for '',
     +                    ''JDHCTH : NO printout'')')
C
  100 CONTINUE
      CALL WDROP(IW,JDHCTH)
      RETURN
  500 FORMAT(3X,'  +++ HCSTAT +++ ')
  510 FORMAT(3X,'EVENT NUMBER ==>',I5)
  520 FORMAT(3X,'number of tube segments = ',I8/ 3X,
     +'number of fired tubes   = ',I8/ 3X,'number of fired storeys = ',
     +I8/ 3X,'number of streamers     = ',I8/ 3X,
     +'energy  seen in Had Cal = ',F10.2)
  530 FORMAT(3X,'End-Cap A ',5X,'  Barrel  ',5X, 'End-Cap B ',5X,  3
     +(F10.2,5X))
  540 FORMAT(3X,'Number of tracks entering in HC (',I6,')')
  550 FORMAT(3X,'         ',3X,' Track # ',3X,'E HC bord.', 4X,
     +'E at vertex',4X,'Energy meas.',3X, 'Particle type')
  560 FORMAT(1X,I9,3X,I9,5X,F9.4,4X,F9.4,4X,F9.4,9X,3A4)
  570 FORMAT(3X,'Total Energy of entering particles = ', F10. 2,
     +' GeV with ',F10.2,'GeV of visible energy '//, 3X,
     +' Energy    :        entering    measured  # of particles'/ 3X,
     +' e/gamma   :',7X,F10.2,4X,F10.2,4X,I8/ 3X,' neutrinos :',7X,F10.
     +2,4X,F10.2,4X,I8/ 3X,' muons     :',7X,F10.2,4X,F10.2,4X,I8/ 3X,
     +' Char. Hadr:',7X,F10.2,4X,F10.2,4X,I8/ 3X,' Neut. Hadr:',7X,F10.
     +2,4X,F10.2,4X,I8)
      END
