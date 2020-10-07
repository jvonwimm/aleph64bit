      SUBROUTINE KIAVER (AVER,IPROG)
C ----------------------------------------------------------------
C - F.Ranjard - 890201
C
C! Return the ALEPHLIB version # and the program origin
CKEY KINE KINGAL VERSION  /  USER INTERNAL
C  Return the ALEPHLIB version # used during KINGAL processing.
C  if no KINGAL processing return the ALEPHLIB version# used during
C  GALEPH processing.
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KIAVER
C              External References: NAMIND(BOS77)
C              Comdecks referenced: BCS, KGJJPAR, AJJPAR, BMACRO
C              Banks referenced: KJOB, AJOB
C
C - usage    : CALL KIAVER (AVER,IPROG)
C - output   : AVER  = ALEPHLIB version # used during the 1st step
C                      of the generation.
C                      0. means not a montecarlo file
C              IPROG = 1 means KINGAL generation
C                      2       GALEPH generation
C                      0       not a montecarlo file
      SAVE
      REAL AVER
      INTEGER IPROG
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JACUMN=1,JACUGC=2,JACUEC=3,JACUHC=4,JACUNC=5,JACUMC=6,
     +          LACUTA=6)
      PARAMETER(JAJOBM=1,JAJORM=2,JAJOGI=3,JAJOSO=4,JAJOSD=5,JAJOGC=6,
     +          JAJOJD=7,JAJOJT=8,JAJOGV=9,JAJOAV=10,JAJOFT=11,
     +          JAJOFS=12,JAJOFC=13,JAJODV=14,JAJODD=15,JAJOTV=16,
     +          JAJOCV=17,LAJOBA=17)
      PARAMETER(JAKIKT=1,JAKIKP=2,LAKINA=9)
      PARAMETER(JAPRPF=1,JAPRRG=2,LAPROA=4)
      PARAMETER(JARURC=1,LARUNA=10)
      PARAMETER(JASERG=1,LASEVA=3)
      PARAMETER(JATIRT=1,LATITA=1)
CKEY KINE KINGAL HAC
      PARAMETER(JKEVRN=1,JKEVNT=2,JKEVNV=3,JKEVPI=4,JKEVWT=5,JKEVSR=6,
     +          JKEVTR=7,LKEVHA=7)
      PARAMETER(JKHIHC=1,LKHISA=1)
      PARAMETER(JKJOJD=1,JKJOJT=2,JKJOAV=3,JKJODV=4,JKJODC=5,LKJOBA=5)
      PARAMETER(JKLIGN=1,LKLINA=1)
      PARAMETER(JKPOKI=1,JKPOHX=2,JKPOHY=3,JKPOHZ=4,LKPOLA=4)
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
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
C -------------------------------------------------------------------
C
C - If KJOB exists (KINGAL was run) then get alephlib version #
      JKJOB = IW(NAMIND('KJOB'))
      IF (JKJOB .NE. 0) THEN
         IPROG = 1
         AVER = REAL(ITABL(JKJOB,1,JKJOAV)) / 10.
      ELSE
C     if KJOB does there but AJOB exists then get alephlib version#
         JAJOB = IW(NAMIND('AJOB'))
         IF (JAJOB .NE. 0) THEN
            IPROG = 2
            AVER  = REAL(ITABL(JAJOB,1,JAJOAV)) / 10.
         ELSE
C        there is no KJOB nor AJOB ==> not a montecarlo file
            IPROG = 0
            AVER  = 0.
         ENDIF
      ENDIF
C
      END
