      INTEGER FUNCTION KBVERT (NVX,VXYZT,NTR)
C -------------------------------------------------------------
C - Y.Kariotakis - 860200    modified by F.Ranjard - 870504
C
C! fill VERT bank
CKEY KINE KINGAL FILL BANK / USER  INTERNAL
C  Book and fill VERT,NR=NVX bank with vertex position and time of
C       flight
C  Connect vertex# NVX to track# NTR (enlarge KINE,NR=NTR bank if
C       necessary)
C  Return VERT,NR=NVX bank index
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: KBVERT
C              External References: AUBOS(ALEPHLIB), NLINK(BOS77)
C              Comdecks referenced: BCS, KIPARA, BMACRO, KMACRO
C
C - usage   : JVERT  = KBVERT (NVX,VXYZT,NTR)
C - input   : VYXZT  = vx,vy,vz and time of flight
C             NTR    = track origin (KINE bank # of the origin)
C             NVX    = vertex# (VERT bank # to be filled)
C - output  : KBVERT = BOS index of the VERT bank just filled
C                      0 means not enough space to book it
C                     -1       bank KINE,NR=NTR does not exist
C                     -2       not enough space to increase KINE
      SAVE
      REAL VXYZT(*)
CKEY KINE KINGAL DEFAULT
      PARAMETER (LHKIN=3, LPKIN=5, LKVX=2, LHVER=3, LPVER=5, LVKI=50)
      PARAMETER (LGDCA=32)
      PARAMETER (LRPART=200, LCKLIN=1)
      PARAMETER (LRECL=16020, LRUN=1, LEXP=1001, LRTYP=1000)
      CHARACTER*60 LTITL
      PARAMETER (LUCOD=0, LNOTRK=100, LTITL='KINGAL run')
      PARAMETER (LUTRK=350)
      PARAMETER (BFIEL=15., CFIEL=BFIEL*3.E-4)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C -------------------------------------------------------------------
C
C - Connect vertex NVX to track NTR if NTR .ne. 0
C
      IF (NTR .EQ. 0) GOTO 100
C
      JKINE = NLINK ('KINE',NTR)
      IF (JKINE .LE. 0) THEN
         KBVERT = -1
         GOTO 999
      ELSE
C        check the space left
         IF (LFRVK(JKINE) .EQ. 0) THEN
            CALL AUBOS ('KINE',NTR,IW(JKINE)+LKVX,JKINE,IGARB)
            IF (JKINE.EQ.0) THEN
               KBVERT = -2
               GOTO 999
            ENDIF
         ENDIF
C        store the vertex NVX
         KNVX = KNEXVK(JKINE)
         IW(KNVX+1) = NVX
C        increase the # of vertices on this track
         IW(JKINE+3) = IW(JKINE+3) + 1
      ENDIF
C
 100  CONTINUE
C
C - Book VERT, NR = NVX , RETURN if the bank exists already
      JVERT = NLINK ('VERT',NVX)
      IF (JVERT .GT. 0) GOTO 998
      CALL AUBOS ('VERT',NVX,LHVER+LPVER+LVKI,JVERT,IGARB)
      IF (JVERT .EQ. 0) GOTO 998
C
C - Fill the bank
      IW(JVERT+1) = LHVER
      IW(JVERT+2) = LPVER
      IW(JVERT+3) = 0
C
      KVERT = JVERT + LHVER
      RW(KVERT+1) = VXYZT(1)
      RW(KVERT+2) = VXYZT(2)
      RW(KVERT+3) = VXYZT(3)
      RW(KVERT+4) = VXYZT(4)
      IW(KVERT+5) = NTR
C
C -   end
C
 998  KBVERT = JVERT
C
 999  RETURN
      END
