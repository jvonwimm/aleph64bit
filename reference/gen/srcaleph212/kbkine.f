      INTEGER FUNCTION KBKINE (NTR,PXYZM,IPART,NVX)
C -------------------------------------------------------------
C - Y.Kariotakis - 860200    modified by F.Ranjard - 870504
C
C!  fill KINE bank
CKEY KINE KINGAL FILL BANK /USER   INTERNAL
C   first connect track# NTR to vertex bank VERT,NR=NVX (enlarge
C         VERT,NR=NVX bank if necessary)
C         IF the track# NTR is .LE. 0 THEN fill the corresponding
C         bank KINE,NR=NTR without connecting the track to any
C         vertex
C   then  fill KINE,NR=NTR bank (use SBANK to see the content)
C         in case the energy is not given compute the energy of the
C         particle from its momemtum and its mass found in the particle
C         data bank PART.
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Names: KBKINE
C               External References: AUBOS(ALEPHLIB), NLINK (BOS77)
C               Comdecks referenced: KIPARA, BCS, BMACRO, RMACRO
C
C - usage   : JKINE  = KBKINE (NTR,PXYZM,IPART,NVX)
C - input   : PYXZM  = px,py,pz and mass of the track
C                      if PXYZM(4)=0. get the mass taken from the PART b
C             IPART  = particle# (row# in PART bank)
C             NVX    = vertex origin (VERT bank # of the origin)
C             NTR    = track # (KINE bank # to be filled)
C - output  : KBKINE = BOS index of the KINE bank just filled
C                      0 means not enough space to book it
C                     -1       bank VERT,NR=NVX does not exist
C                     -2       not enough space to increase VERT
C                     -3       PART bank does not exist
      SAVE
      REAL PXYZM(*)
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
C -------------------------------------------------------------------
CKEY KINE PART KLIN
      IF (NAPAR .EQ. 0) THEN
         NAPAR = NAMIND ('PART')
         NAKLI = NAMIND ('KLIN')
      ENDIF
C
C - Check track# : if <0 store the track without connecting to
C   any vertex
      IF (NTR .LE. 0) GOTO 10
C
C - Connect track NTR to vertex NVX
C
      JVERT = NLINK ('VERT',NVX)
      IF (JVERT .LE. 0) THEN
         KBKINE = -1
         GOTO 999
      ELSE
C        check the space left
         IF (LFRVK(JVERT) .EQ. 0) THEN
            CALL AUBOS ('VERT',NVX,IW(JVERT)+LVKI,JVERT,IGARB)
            IF (JVERT.EQ.0) THEN
               KBKINE = -2
               GOTO 999
            ENDIF
         ENDIF
C        store the track NTR
         KNTR = KNEXVK(JVERT)
         IW(KNTR+1) = NTR
C        increase the # of tracks coming from this vertex
         IW(JVERT+3) = IW(JVERT+3) + 1
      ENDIF
C
 10   CONTINUE
C
C - Book KINE, NR = NTR
      CALL AUBOS ('KINE',NTR,LHKIN+LPKIN+LKVX,JKINE,IGARB)
      IF (JKINE .EQ. 0) GOTO 998
C
C - Fill the bank
      IW(JKINE+1) = LHKIN
      IW(JKINE+2) = LPKIN
      IW(JKINE+3) = 1
C
      IF (PXYZM(4) .EQ. 0.) THEN
C     get the mass from PART bank
         JPART = IW(NAPAR)
         IF (JPART.EQ.0) THEN
            KBKINE = -3
            GOTO 999
         ELSE
            PXYZM(4) = PARMAS (IPART)
         ENDIF
      ENDIF
C
      KKINE = JKINE + LHKIN
      RW(KKINE+1) = PXYZM(1)
      RW(KKINE+2) = PXYZM(2)
      RW(KKINE+3) = PXYZM(3)
      RW(KKINE+4) = PXYZM(4)
      IW(KKINE+5) = IPART
      IW(KKINE+LPKIN+1) = NVX
C
C -   end
C
 998  KBKINE = JKINE
C
 999  RETURN
      END
