      SUBROUTINE KXL7AL (VMAIN,ISTAT,MVX,MTRK)
C ---------------------------------------------------------
C - B.Bloch-Devaux - J.Boucrot - F.Ranjard - 870516
C - Modified for Jetset 7.3 T.Medcalf - 900802
C                           B.Bloch   - 900930
C! Build the event interface LUND 7.3-Aleph
CKEY KINE KINGAL LUND FILL   /  USER
C - Fill    : PTRAK(ix,n)  = px,py,pz,mass of track(n)
C                            if mass=0. it will be filled by the system
C             IPVNU(1,n)   = origin vertex # of track(n)
C                  (2,n)   = decay vertex # of track(n)
C                             0 if no decay
C                  (3,n)   = ALEPH particle #
C             IPCOD(n)     = LUND history code of track(n)
C - Book    : KHIS bank filled with IPCOD(n)
C - Call    : KFEVBK (VMAIN,PTRAK,IPVNU,MTRK,JSTAT)
C             to book and propagate the decay and fill VERT and KINE
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXL7AL
C              External Regerences: NAMIND(BOS77)
C                                  ALTABL/KFEVBK/KGPART/KBKINE(ALEPHLIB)
C              Comdecks referenced: BCS, LUN7COM, ALCONS, KIPARA,BMACRO
C                                   KMACRO
C
C - usage   : CALL KXL7AL (VMAIN,ISTAT,MVX,MTRK)
C - Input   : VMAIN = vx,vy,vz,tof of the primary vertex
C - Output  : ISTAT = status word ( = 0 means OK)
C                     - 1 means VERT or KINE bank missing
C                     - 2 means not enough space for VERT or KINE
C                     - 3 means too many tracks
C                     - 4 electrons beams not stored as lines 1 and 2
C                     - 5 means Lund status code larger than 30 found
C                     > 0 means unknown LUND particle# ABS(ISTAT)
C             MVX   = # of vertices
C             MTRK  = # of tracks to be propagated ( no beam electrons )
C ---------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
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
      PARAMETER (ILUD=10000,TLIMI=1.E-15)
      REAL PTRAK(4,LUTRK),VMAIN(4)
      INTEGER IPVNU(3,LUTRK),IPCOD(LUTRK)
      INTEGER ALTABL
      DATA NAPAR/0/
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
C ------------------------------------------------------
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')
C
C - Check particle buffer length
      IF (N7LU .GT. LUTRK) THEN
         IF (IW(6).GT.0) THEN
           WRITE (IW(6),'(/1X,''+++KXL7AL+++ not enough space to save''
     &         ,'' the event :# of tracks = '',I2,2X,''allowed = '',I2
     &         /13X,''==>increase LUTRK in *CD KIPARA'')') N7LU,LUTRK
         ENDIF
         ISTAT = - 3
         GOTO 999
      ENDIF
C
C - Build array containing vertex # and particle # of each track
C
      IBEA=0
      NVER = 1
      DO 10 ITR=1,N7LU
C Look for "mother" particle
         ILUN  = K7LU(ITR,2)
         IPART = KGPART(ILUN)
         IF (IPART .LE. 0) GOTO 998
         KS=K7LU(ITR,1)
         IF (KS.EQ.21 .AND. ILUN.EQ.23 ) KS =11
         IMOTH=K7LU(ITR,3)
C
C Store now momentum components and codes of the track :
          DO 9 I=1,3
 9        PTRAK(I,ITR-IBEA)=P7LU(ITR,I)
C         store the current mass
          PTRAK(4,ITR-IBEA)=P7LU(ITR,5)
          IPVNU(3,ITR-IBEA)=IPART
          IPCOD(ITR-IBEA)=KS*ILUD+IMOTH
C
             IF (KS.LE.5) THEN
C            Particle not decayed in LUND
C            if stable particle created in initial state ,IMOTH=0
                 IF (IMOTH-IBEA.LE.0 ) THEN
                   IPVNU(1,ITR-IBEA)=1
                ELSE
                   IPVNU(1,ITR-IBEA)=IPVNU(2,IMOTH-IBEA)
                ENDIF
                IPVNU(2,ITR-IBEA)=0
             ELSE IF ((KS.GE.11).AND.(KS.LE.15)) THEN
C            Particle has decayed in LUND
                 IF (IMOTH-IBEA.LE.0 ) THEN
C               Primary parton
                   IPVNU(1,ITR-IBEA)=1
                ELSE
                   IPVNU(1,ITR-IBEA)=IPVNU(2,IMOTH-IBEA)
                ENDIF
C               Decay inside LUND and finite lifetime :
C               this track will be propagated in KFEVBK until its decay
                TLIF = TIMLIF (IPART)
                IF (TLIF.GT.TLIMI .AND. MDCY(LUCOMP(ILUN),1).GT.0) THEN
                   NVER=NVER+1
                   IPVNU(2,ITR-IBEA)=NVER
                ELSE
C   Decay is immediate ( will not be propagated)
                   IPVNU(2,ITR-IBEA)=IPVNU(1,ITR-IBEA)
                ENDIF
C   New convention for beam particles KS=21
             ELSE IF (KS.EQ.21) THEN
C            electron beams were stored as well
C            check that they appear only on lines 1 or 2
                ILUN=-4
                IF (ITR.GT.2) GO TO 998
                IST=KBKINE(-ITR,PTRAK(1,ITR-IBEA),IPART,0)
                IF (IST.LE.0) THEN
                   ILUN=-2
                   GO TO 998
                ENDIF
                IBEA=IBEA+1
             ELSE IF (KS.GE.30) THEN
                ILUN=-5
                GO TO 998
             ENDIF
C
C         Update history code
          IF ( IMOTH .GT. IBEA ) THEN
             IPCOD(ITR-IBEA) = IPCOD(ITR-IBEA) - IBEA
          ELSE
             IPCOD(ITR-IBEA) = IPCOD(ITR-IBEA) - IMOTH
          ENDIF
 10    CONTINUE
C
C - Propagate decays and fill KINE and VERT banks
       NPARL = N7LU-IBEA
       CALL KFEVBK(VMAIN,PTRAK,IPVNU,NPARL,IFAIL)
C - Fill history bank KHIS
       JKHIS = ALTABL ('KHIS',1,NPARL,IPCOD,'I','E')
C
       MVX = NVER
       MTRK = NPARL
       ISTAT = IFAIL
       GOTO 999
C
C - Error
C      unknown LUND particle
 998   ISTAT = ABS(ILUN )
C
 999   RETURN
       END