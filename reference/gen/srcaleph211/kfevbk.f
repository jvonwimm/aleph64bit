      SUBROUTINE KFEVBK (VMAIN,RPARA,IPARA,MTRAK,ISTAT)
C -----------------------------------------------------------
C - J.Boucrot - B.Bloch - F.Ranjard - 870515
C! Fill event banks KINE VERT KHIS
CKEY KINE KINGAL FILL BANK /  INTERNAL
C  first KINE and VERT banks are booked and filled with parameters
C        sent as arguments (all vertices at the same position).
C  then  depending on the decay length of secondary particles , the
C        secondary vertices are displaced from the main vertex . The
C        propagation follows a straight line for neutral generating a
C        secondary vertec, and a simple helix for charged particles.
C        In case of charge particles generating a secondary vertex,
C        swim Px and Py of all secondaries up to decay vertex. Then
C        store the time of flight.
C        The magnetic field is assumed to be 15.0 Kgauss.
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KFEVBK
C              External References: KBVERT/KBKINE/KGPART/KGDECL(ALEPHLIB
C              Comdecks referenced: BCS, ALCONS, KIPARA, BMACRO, KMACRO
C
C - Usage   : CALL KFEVBK (VMAIN,RPARA,IPARA,MTRAK,ISTAT)
C - Input   : VMAIN          = vx,vy,vz of the main vertex
C             RPARA (1-4,k)  = px,py,pz,(mass) of track(k)
C                              if RPARA(4,k)=0. then the mass is taken
C                              from the PART bank.
C             IPARA (1,k)    = vertex# of the origin of the track(k)
C                   (2,k)    = vertex# of the decay of the track(k)
C                                0 if there is no decay
C                   (3,k)    = ALEPH particle#
C             MTRAK          = # of tracks
C             ISTAT          = return code  ( 0 means OK)
C                              -1 means too many particles
C                              -2 means wrong KINE/VERT booking
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
CKEY KINE KINGAL DEFAULT
      PARAMETER (LHKIN=3, LPKIN=5, LKVX=2, LHVER=3, LPVER=5, LVKI=50)
      PARAMETER (LGDCA=32)
      PARAMETER (LRPART=200, LCKLIN=1)
      PARAMETER (LRECL=16020, LRUN=1, LEXP=1001, LRTYP=1000)
      CHARACTER*60 LTITL
      PARAMETER (LUCOD=0, LNOTRK=100, LTITL='KINGAL run')
      PARAMETER (LUTRK=350)
      PARAMETER (BFIEL=15., CFIEL=BFIEL*3.E-4)
      PARAMETER (CLITS = CLGHT * 1.E+9)
      INTEGER IPARA(3,*)
      REAL RPARA(4,*),VMAIN(3)
      REAL KGDECL
      LOGICAL FDECAY,FNEUTR
      DATA NAPAR /0/
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
      CHARGE(JPA) = RTABL(IW(NAPAR),JPA,7)
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)
C - # of vertices on a track known by its BOS index JVK /
C   # of outgoing tracks of a vertex known by its BOS index JVK
      NOFVK(JVK)  = IW(JVK+3)
C - Particle type of a track known by its BOS index JVK
      KINTYP(JVK) = IW(KPARVK(JVK)+5)
C - incoming track # of a vertex known by its BOS index JVK
      INPTRK(JVK) = IW(KPARVK(JVK)+5)
C - origin vertex # of a track known by its BOS index JVK
      INPVRT(JVK) = IW(KLISVK(JVK)+1)
C - momentum of a track known by its BOS index JVK
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2
     &                   +RW(KPARVK(JVK)+3)**2)
C - mass of a track known by its BOS index JVK
      PMASVK(JVK) = RW(KPARVK(JVK)+4)
C - energy of a track known by its BOS index JVK
      ENERVK(JVK) = SQRT (PMODVK(JVK)**2 + PMASVK(JVK)**2)
C - time of flight of the icoming particle to the vertex known by its
C   BOS index JVK
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)
C - radius of the vertex known by its BOS index
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)
C - mother track # of a track known by its BOS index
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))
C - alephlib version# used in KINGAL (NKJ is the name-index of KJOB)
C   the user should have called *CA KJOBJJ to use this function
      ALEKIN(NKJ) = ITABL(IW(NKJ),1,JKJOAV)
C
C
      FDECAY(JTR) = IPARA(2,JTR).GT.1 .AND. IPARA(2,JTR).NE.IPARA(1,JTR)
      FNEUTR(JPA) = ABS (CHARGE(JPA)) .LT. .1
C -------------------------------------------------------
      ISTAT = 0
C
C - Get 'PART' name-index at the 1st entry
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')
C
C - Create main vertex bank
      IVMAI = 1
      JVERT = KBVERT (IVMAI,VMAIN,0)
C
C - Fill VERT and KINE banks
      DO 1 NT = 1,MTRAK
         JKINE = KBKINE (NT,RPARA(1,NT),IPARA(3,NT),IPARA(1,NT))
         IF (JKINE.LE.0) GOTO 998
         IF (IPARA(2,NT).GT.0) THEN
            JVERT = KBVERT (IPARA(2,NT),VMAIN,NT)
            IF (JVERT.LE.0) GOTO 998
         ENDIF
 1    CONTINUE
C
C - Propagate secondary vertices if any
C
      DO 100 NT = 1,MTRAK
         IPART = IPARA(3,NT)
         PMOD = SQRT (RPARA(1,NT)**2+RPARA(2,NT)**2+RPARA(3,NT)**2)
         TLIF = TIMLIF (IPART)
         IF (RPARA(4,NT).EQ.0.) THEN
            ZMAS = PARMAS (IPART)
         ELSE
            ZMAS = RPARA(4,NT)
         ENDIF
C
         IF (FDECAY(NT)) THEN
            DCLEN = KGDECL (PMOD,ZMAS,TLIF)
            IF (DCLEN .LE. 0.) GOTO 100
C           get the origin vertex
            IVOR = IPARA(1,NT)
            JVOR = NLINK ('VERT',IVOR)
            KVO  = KPARVK (JVOR)
C           get the decay vertex
            IVOUT = IPARA(2,NT)
            JVERT = NLINK ('VERT',IVOUT)
            KVX   = KPARVK (JVERT)
            KVTR  = KLISVK (JVERT)
C
C           straight line for neutral generating a secondary vx
            IF (FNEUTR(IPART)) THEN
               DO 102 IX = 1,3
                  RW(KVX+IX) = RW(KVO+IX) + RPARA(IX,NT)*DCLEN/PMOD
 102           CONTINUE
            ELSE
C
C          propagation according to a simple helix for charged
C
               RAD = PMOD / (CFIEL*CHARGE(IPART))
               DPSI = DCLEN / RAD
               DXDS = RPARA(1,NT) / PMOD
               DYDS = RPARA(2,NT) / PMOD
               DZDS = RPARA(3,NT) / PMOD
               CPSI = COS (DPSI)
               SPSI = SIN (DPSI)
               DX = RAD * (DXDS*SPSI + DYDS*(1.-CPSI))
               DY = RAD * (DYDS*SPSI - DXDS*(1.-CPSI))
               DZ = DCLEN * DZDS
               RW(KVX+1)  = RW(KVO+1) + DX
               RW(KVX+2)  = RW(KVO+2) + DY
               RW(KVX+3)  = RW(KVO+3) + DZ
C           swim Px and Py of all secondaries up to decay vertex
               MTVX = IW(JVERT+3)
               IF (MTVX .GT. 0) THEN
                  DO 103 N=1,MTVX
                     NS = IW (KVTR+N)
                     JKINE = NLINK ('KINE',NS)
                     IF (JKINE.EQ.0) GOTO 998
                     KTR = KPARVK (JKINE)
                     RW(KTR+1) = RPARA(1,NS)*CPSI - RPARA(2,NS)*SPSI
                     RW(KTR+2) = RPARA(1,NS)*SPSI + RPARA(2,NS)*CPSI
 103              CONTINUE
               ENDIF
            ENDIF
C           Store the time of flight
            RW(KVX+4)  = RW(KVO+4) + DCLEN/CLITS
         ENDIF
C
 100   CONTINUE
C
       GOTO 999
C
C - Error
C      unsuccessfull booking of VERT or KINE
 998   ISTAT = -2
C
C - End
 999   CONTINUE
       END
