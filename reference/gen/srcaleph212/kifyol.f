      SUBROUTINE KIFYOL
C -------------------------------------------------------------------
C - F.Ranjard - 890202
CKEY KINE KINGAL BANK FORMAT  / USER  INTERNAL
C! return old format of KINE and/or FKIN
C  bank KINE and/or FKIN is modified in place :
C  KINE : word(4) will contain the energy of the track defined by KINE
C  FKIN : word(4) will contain the energy of the track defined by KINE
C  the routine can be called for any file (old or new format) :
C  a test is made on the creation date to determine the format.
C  the routine must be called before using the KINE/FKIN bank in order
C  to use the same code for old an new format.
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KIFYOL
C              External References: NAMIND/NLINK(BOS77)
C                                   KIAVER(ALEPHLIB)
C              Comdecks referenced: BCS, FKINJJ, BMACRO, KMACRO
C              Banks referenced: KINE, FKIN, PART
C
C - usage    : CALL KIFYOL
C
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFKIPX=1,JFKIPY=2,JFKIPZ=3,JFKIMA=4,JFKIPA=5,JFKIOV=6,
     +          JFKIEV=7,JFKIHC=8,LFKINA=8)
      DATA NKINE /0/
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
C ----------------------------------------------------------------------
C
C - get name-indices of KINE, FKIN and the creation date of the file
      IF (NKINE .EQ. 0) THEN
         NKINE = NAMIND ('KINE')
         NFKIN = NAMIND ('FKIN')
         NAPAR = NAMIND ('PART')
         CALL KIAVER (AVER,IPROG)
         WRITE (IW(6),'(/1X,''+++KIFYOL+++ KINGAL ALEPHLIB '',F5.2,I5)')
      ENDIF
C
C - RETURN if AVER .lt. 9.0 because the format is already the old one
      IF (AVER .LT. 9.0) RETURN
C
C - change mass to energy in KINE if KINE banks exist
      JKINE = NKINE+1
 1    JKINE = IW(JKINE-1)
      IF (JKINE .NE. 0) THEN
         ITYP = KINTYP(JKINE)
         RW(KPARVK(JKINE)+4) = SQRT (RW(KPARVK(JKINE)+1)**2+
     &        RW(KPARVK(JKINE)+2)**2+RW(KPARVK(JKINE)+3)**2+
     &        RW(KPARVK(JKINE)+4)**2)
         GOTO 1
      ENDIF
C
C - change energy to mass in FKIN if FKIN bank exists
      JFKIN = IW(NFKIN)
      IF (JFKIN .NE. 0) THEN
         DO 2 N=1,LROWS(JFKIN)
            ITYP  = ITABL(JFKIN,N,JFKIPA)
            KFKIN = KROW(JFKIN,N)
            RW(KFKIN+4) = SQRT (RW(KFKIN+1)**2+RW(KFKIN+2)**2+
     &                          RW(KFKIN+3)**2+RW(KFKIN+4)**2)
  2      CONTINUE
      ENDIF
C
      END
