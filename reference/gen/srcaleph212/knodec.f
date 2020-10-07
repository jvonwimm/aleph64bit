      INTEGER FUNCTION KNODEC (NODEC,LPDEC)
C -----------------------------------------------------------
C - F.Ranjard -870423                    modified - 880831
C - B.Bloch                              modified - 920507
CKEY KINE KINGAL DECAY       /  USER  INTERNAL
C! Return in the array NODEC the user particle# of the part.
C  which have not to be decayed by the generator.
C  particles with time of life < 1.E-15 are still decayed by
C  the generator.
C  If a card KTMX exists , the life time is taken from the Data card
C  and supersedes the 1.E-15 value
C  they are KNODEC such particles
C  IF the bank KLIN does not exist THEN
C    KNODEC = - the # of nodecay particles
C  LPDEC is the dimension of the array NODEC
C  if KNODEC .gt. LPDEC it means that the buffer is too small
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: KNODEC
C              External References: NAMIND(BOS77)
C              Comdecks referenced: BCS, KIPARA, BMACRO
C
C - usage  : MXDEC = KNODEC (NODEC,LPDEC)
C - input  : NODEC = array to contain user generator particle #
C            LPDEC = length of the NODEC array
C - output : KNODEC= # of particles which should not be decayed
C                    by the generator (the #s of the 1st LPDEC
C                    are stored in NODEC)
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
CKEY KINE KINGAL DEFAULT
      PARAMETER (LHKIN=3, LPKIN=5, LKVX=2, LHVER=3, LPVER=5, LVKI=50)
      PARAMETER (LGDCA=32)
      PARAMETER (LRPART=200, LCKLIN=1)
      PARAMETER (LRECL=16020, LRUN=1, LEXP=1001, LRTYP=1000)
      CHARACTER*60 LTITL
      PARAMETER (LUCOD=0, LNOTRK=100, LTITL='KINGAL run')
      PARAMETER (LUTRK=350)
      PARAMETER (BFIEL=15., CFIEL=BFIEL*3.E-4)
      INTEGER NODEC(*)
      DATA NAPAR /0/,IFIR/0/
      DATA TIMLI / 1.E-15/
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
C -----------------------------------------------------------
C  Get user's TIMLI value from KTMX data card if any
      JKTMX = IW(NAMIND('KTMX'))
      IF ( JKTMX.GT.0) THEN
         TIMLI = RW(JKTMX+1)
      ENDIF
CKEY KINE PART KLIN
      IF (NAPAR .EQ. 0) THEN
         NAPAR = NAMIND ('PART')
         NAKLI = NAMIND ('KLIN')
      ENDIF
      IDEC = 0
      JPART = IW(NAPAR)
      IF (JPART.EQ.0) THEN
         IDEC = -LGDCA
      ELSE
         JKLIN = IW(NAKLI)
         IF (JKLIN.EQ.0) THEN
            IDEC = -LGDCA
         ELSE
            DO 1 I=1,LROWS(JKLIN)
               IGDCA = ITABL (JPART,I,1)
               IUSER = ITABL (JKLIN,I,1)
               IF (IGDCA.GT.0 .AND. IGDCA.LE.LGDCA .AND.
     &             TIMLIF(I).GT.TIMLI .AND. IUSER.NE.0)THEN
                  IDEC = IDEC + 1
                  IF (IDEC.LE.LPDEC) NODEC(IDEC) = IUSER
               ENDIF
 1          CONTINUE
         ENDIF
      ENDIF
      KNODEC = IDEC
      IF (IFIR.EQ.0) THEN
         WRITE(IW(6),100) LGDCA,TIMLI
         IFIR = 1
      ENDIF
      RETURN
 100  FORMAT(1X,110('*'),/,1X,'*  For this run , among the first ',I5,
     $' particles , those with life time smaller than ',E12.5,' will be'
     $,/,1X,'*  decayed at generator level ',/,1x,110('*'))
      END
