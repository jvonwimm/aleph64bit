      SUBROUTINE FYTREE(IBFUL)
C ------------------------------------------------------------
CKEY FYXX  / INTERNAL
C - J. Hilgart 030987
C! Build arrays which allow conversion from KINE and VERT banks
C  to a true tree structure FVER,FKIN.
C  This means  all secondary tracks from a vertex asspecified by
C  FVER are all listed contiguously.
C
C - Input banks : KINE, VERT
C - Output bank : JDKNFO, JDKOFN, JDVOUT
C - Output
C     : IBFUL = -1 if not enough space to book a bank
C
C Called from  FYKINE                      from ALEPHLIB.HLB
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LWORDB = 65536)
      PARAMETER (MAXMCX = 1000, MAXMES = 500)
      COMMON /FYRELA/ IHTYPE,ICALTO
     &               ,ITRCAL(MAXMES),ICALTR(2,MAXMES)
     &               ,LTKNUM,LVXNUM,IMEATR,KINTRK,KINVER
     &               ,FSHDRO,FTKDRO,CUTRAC,BFLCLT,ENMINC
     &               ,INDKIN,INDVER,JDKEKS,JDVNFO,JDVOUT
     &               ,JDKOFN,JDKNFO
      LOGICAL FSHDRO,FTKDRO
      PARAMETER (LFXWBK = 7)
      INTEGER  JDFXWB(LFXWBK)
      EQUIVALENCE (JDFXWB(1),INDKIN)
      COMMON /FYCHAR/ ELISAD
      CHARACTER*48 ELISAD
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
C ====================================================================
C
C General initialization
      IBFUL = 0
C
      CALL WBANK(IW,JDKOFN,LMHLEN+LTKNUM,*980)
      IW(JDKOFN+LMHCOL) = 1
      IW(JDKOFN+LMHROW) = LTKNUM
      CALL WBANK(IW,JDKNFO,LMHLEN+IW(INDKIN),*980)
      IW(JDKNFO+LMHCOL) = 1
      IW(JDKNFO+LMHROW) = IW(INDKIN)
      CALL WBANK(IW,JDVOUT,LMHLEN+2*LVXNUM,*980)
      IW(JDVOUT+LMHCOL) = 2
      IW(JDVOUT+LMHROW) = LVXNUM
      ICO = 0
C
C Loop over VERT banks
      DO 1 IVO = 1,IW(INDVER)
         IVN = ITABL (JDVNFO,IVO,1)
         IF (IVN .EQ. 0) GOTO 1
C
C No. of outgoing tracks
         JVER = IW(INDVER+IVO)
         NOUT = NOFVK(JVER)
         ICOV = 0
         ISEC = 0
 2       CONTINUE
         IF (ICOV .LT. NOUT) THEN
            DO 3 IOLD = ISEC+1, IW(INDKIN)
C
C KINE bank index
               KK = IW(INDKIN+IOLD)
C - origin vertex # of a track known by its BOS index JVK
               IVOR = INPVRT(KK)
               IF ( IVOR .EQ. IVO) GOTO 4
 3          CONTINUE
 4          CONTINUE
            ICOV = ICOV + 1
            ISEC = IOLD
            INW = ITABL(JDKEKS,IOLD,1)
            IF (INW .EQ. 0)  GOTO 2
            ICO = ICO + 1
C
C Build relationships old |--> new and vica versa
            IW(KROW(JDKNFO,IOLD)+1) = ICO
            IW(KROW(JDKOFN,ICO)+1) = IOLD
            GOTO 2
         ENDIF
 1    CONTINUE
C
C - Get #s of outgoing tracks for new vertices
C
      DO 10 IVO = 1,IW(INDVER)
         IVN = ITABL (JDVNFO,IVO,1)
         IF (IVN .EQ. 0) GOTO 10
         JVER = IW(INDVER+IVO)
         NOUT = NOFVK(JVER)
         IF (NOUT .EQ. 0) GOTO 10
         NEWT = 0
         IFOUT = LTKNUM+1
         DO 11 N = 1,NOUT
            ITO = IW(KLISVK(JVER)+N)
            ITN = ITABL(JDKNFO,ITO,1)
            IF (ITN .EQ. 0) GOTO 11
            NEWT = NEWT+1
            IFOUT = MIN (IFOUT,ITN)
 11      CONTINUE
         IF (NEWT .EQ. 0) GOTO 10
         IW(KROW(JDVOUT,IVN)+1) = IFOUT-1
         IW(KROW(JDVOUT,IVN)+2) = NEWT
 10   CONTINUE
C
      GOTO 990
C
C sick return: not enough space to book bank...
 980  IBFUL = - 1
 990  CALL WDROP(IW,JDKEKS)
      END
