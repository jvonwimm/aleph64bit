      SUBROUTINE FYFKIN(IBFUL)
C---------------------------------------------------------------
CKEY FYXX  /INTERNAL
C! Fill FKIN and FVER monte carlo banks from KINE and VERT banks
C! Fill FPOL and FZFR monte carlo banks from KPOL and KZFR banks
C - J. Hilgart 05.07.87             B.Bloch 10/10/90
C - Input banks   : INDKIN, INDVER, JDKNFO, JDKOFN, JDVOUT
C - Output banks  : FKIN, FVER, FPOI ,FPOL,FZFR
C - Output argument : IBFUL = -1 if not enough space to book a bank
C Called by FYKINE
C Calls:  AUBOS                                    from  ALEPHLIB.HLB
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
      PARAMETER(JFKIPX=1,JFKIPY=2,JFKIPZ=3,JFKIMA=4,JFKIPA=5,JFKIOV=6,
     +          JFKIEV=7,JFKIHC=8,LFKINA=8)
      PARAMETER(JFVEVX=1,JFVEVY=2,JFVEVZ=3,JFVETO=4,JFVEIP=5,JFVEIS=6,
     +          JFVENS=7,JFVEVN=8,JFVEVM=9,LFVERA=9)
      PARAMETER(JFPOIP=1,JFPOIS=2,LFPOIA=2)
      PARAMETER(JFPOKI=1,JFPOHX=2,JFPOHY=3,JFPOHZ=4,LFPOLA=4)
CD FLCOJJ
      PARAMETER(JFLCIL=1,LFLCOA=1)
CD FLTRJJ
      PARAMETER(JFLTIL=1,LFLTRA=1)
CD FSCOJJ
      PARAMETER(JFSCEN=1,JFSCIH=2,JFSCE1=3,JFSCE2=4,JFSCE3=5,JFSCH1=6,
     +          JFSCH2=7,JFSCTH=8,JFSCPH=9,JFSCIP=10,LFSCOA=10)
CD FSTRJJ
      PARAMETER(JFSTPX=1,JFSTPY=2,JFSTPZ=3,JFSTIQ=4,JFSTIH=5,JFSTCH=6,
     +          JFSTNH=7,JFSTD0=8,JFSTPH=9,JFSTZ0=10,JFSTAV=11,
     +          JFSTS2=12,JFSTNW=13,JFSTM2=14,JFSTNM=15,JFSTNC=16,
     +          JFSTMC=17,JFSTIC=18,LFSTRA=18)
CD FTCMJJ
      PARAMETER(JFTCIP=1,LFTCMA=1)
CD FTOCJJ
      PARAMETER(JFTOIP=1,LFTOCA=1)
CD FTTMJJ
      PARAMETER(JFTTIP=1,LFTTMA=1)
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
      PARAMETER(JKZFEF=1,LKZFRA=1)
      PARAMETER(JFZFZF=1,LFZFRA=1)
C
      INTEGER FYNHIS
      DATA IONC /0/
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
C ---------------------------------------------------------------------
C Initializations:
      IF (IONC.EQ.0) THEN
         NFKIN = NAMIND('FKIN')
         NFVER = NAMIND('FVER')
         NFPOI = NAMIND('FPOI')
         NFPOL = NAMIND('FPOL')
         NFZFR = NAMIND('FZFR')
         NKZFR = NAMIND('KZFR')
         NKPOL = NAMIND('KPOL')
         NKVOL = NAMIND('KVOL')
         NKHIS = NAMIND('KHIS')
         IONC = 1
      ENDIF
      IBFUL = 0
C
C Create FKIN, FVER, FPOI banks
      LE = LMHLEN + LFKINA*LTKNUM
      CALL AUBOS('FKIN',0,LE,JFKIN,IGAR1)
      IF (JFKIN.EQ.0) GOTO 990
      IW(JFKIN+LMHCOL) = LFKINA
      IW(JFKIN+LMHROW) = LTKNUM
C
      LE = LMHLEN + LFVERA*LVXNUM
      CALL AUBOS('FVER',0,LE,JFVER,IGAR2)
      IF (JFVER.EQ.0) GOTO 990
      IW(JFVER+LMHCOL) = LFVERA
      IW(JFVER+LMHROW) = LVXNUM
C
      LE = LMHLEN + LFPOIA*IW(INDKIN)
      CALL AUBOS('FPOI',0,LE,JFPOI,IGAR4)
      IF (JFPOI.EQ.0) GOTO 990
      IW(JFPOI+1) = LFPOIA
      IW(JFPOI+2) = IW(INDKIN)
C
      IF (IGAR1+IGAR2+IGAR4 .GT. 0) THEN
         JFKIN = IW(NFKIN)
         JFVER = IW(NFVER)
         CALL BWIND (IW,'KINE',IW(INDKIN),INDKIN)
         CALL BWIND (IW,'VERT',IW(INDVER),INDVER)
         IF (INDKIN.EQ.0 .OR. INDVER.EQ.0) GOTO 990
      ENDIF
C
C - Fill FVER bank
C
      JKVOL = IW(NKVOL)
      DO 10 IVO =1,IW(INDVER)
         IVN = ITABL (JDVNFO,IVO,1)
         IF (IVN .EQ. 0) GOTO 10
         JVER = IW(INDVER+IVO)
         MOTHO = INPTRK(JVER)
         IF (MOTHO .EQ. 0) THEN
            MOTH = 0
         ELSE
            MOTH = ITABL(JDKNFO,MOTHO,1)
         ENDIF
         NOUT  = ITABL (JDVOUT,IVN,2)
         IFOUT = ITABL (JDVOUT,IVN,1)
         KFVER = KROW (JFVER,IVN)
         RW(KFVER+JFVEVX) = RW(KPARVK(JVER)+1)
         RW(KFVER+JFVEVY) = RW(KPARVK(JVER)+2)
         RW(KFVER+JFVEVZ) = RW(KPARVK(JVER)+3)
         RW(KFVER+JFVETO) = TOFLIT(JVER)
         IW(KFVER+JFVEIP) = MOTH
         IW(KFVER+JFVEIS) = IFOUT
         IW(KFVER+JFVENS) = NOUT
         IF (JKVOL .EQ. 0) THEN
            IW(KFVER+JFVEVN)=INTCHA ('    ')
            IW(KFVER+JFVEVM)=INTCHA ('    ')
         ELSE
            IW(KFVER+JFVEVN) = ITABL(JKVOL,IVO,JKVOVN)
            IF (LCOLS(JKVOL).GE.JKVOVM) THEN
               IW(KFVER+JFVEVM) = ITABL(JKVOL,IVO,JKVOVM)
            ENDIF
         ENDIF
  10  CONTINUE
C
C - Fill FKIN bank
C
      JKHIS = IW(NKHIS)
      KFKIN = JFKIN + LMHLEN
      DO 20 ITN = 1,LTKNUM
         ITO = ITABL (JDKOFN,ITN,1)
         JKIN = IW(INDKIN+ITO)
         RW(KFKIN+JFKIPX) = RW(KPARVK(JKIN)+1)
         RW(KFKIN+JFKIPY) = RW(KPARVK(JKIN)+2)
         RW(KFKIN+JFKIPZ) = RW(KPARVK(JKIN)+3)
         RW(KFKIN+JFKIMA) = PMASVK(JKIN)
         IW(KFKIN+JFKIPA) = KINTYP(JKIN)
         INPV = ITABL (JDVNFO,INPVRT(JKIN),1)
         IW(KFKIN+JFKIOV) = INPV
         IF (NOFVK(JKIN) .GT. 1) THEN
            DO 19 IV =2,NOFVK(JKIN)
               IVSO = IW(KLISVK(JKIN)+IV)
               IVSN = ITABL(JDVNFO,IVSO,1)
               IF(IVSN .NE. 0) IW(KFKIN+JFKIEV) = IVSN
 19         CONTINUE
         ENDIF
         IF (JKHIS.GT.0 .AND. ITO .LE. KINTRK) THEN
            IOHIS = ITABL(JKHIS,ITO,1)
            IF (IOHIS .NE. 0) IW(KFKIN+JFKIHC) = FYNHIS (IOHIS)
         ENDIF
  20  KFKIN = KFKIN + LCOLS(JFKIN)
C
C - Fill FPOI bank
C  Tracks which have been dropped are related to their first
C  first direct predecessor, in its new ordering in FKIN of course,
C  which has been kept.
C
      DO 40 ID = 1, IW(INDKIN)
         INWR = ITABL (JDKNFO,ID,1)
         IF (INWR .EQ. 0) THEN
C        Track was dropped. Go up the tree until a non-dropped parent
C        is found
            IW(KROW(JFPOI,ID)+JFPOIS) = 1
            IOLD = ID
 41         IOLD = MOTHVK(NLINK('KINE',IOLD))
            INWR = ITABL (JDKNFO,IOLD,1)
            IF (INWR .EQ. 0) GOTO 41
         ENDIF
         IW(KROW(JFPOI,ID)+JFPOIP) = INWR
 40   CONTINUE
C
C - create FPOL by swapping KPOL and FPOL, then update the track #
C
      JKPOL = IW(NKPOL)
      IF (JKPOL .GT. 0) THEN
         CALL BSWAP (IW,'KPOL','FPOL')
         JFPOL = IW(NFPOL)
         DO 50 I = 1,LROWS(JFPOL)
            ITO = ITABL (JFPOL,I,JFPOKI)
            IF (ITO .GT. 0) THEN
               ITN = ITABL (JDKNFO,ITO,1)
               IW(KROW(JFPOL,I)+JFPOKI) = ITN
            ENDIF
 50      CONTINUE
      ENDIF
C
C - create FZFR , then fill the fragmentation for row # (FKIN row #)
C
      JKZFR = IW(NKZFR)
      IF (JKZFR .GT. 0) THEN
         LE = LMHLEN + LFZFRA*LROWS(JKZFR)
         CALL AUBOS ('FZFR',0,LE,JFZFR,IGAR)
         IF (JFZFR .EQ. 0) GOTO 990
         JFZFR = IW(NFZFR)
         IW(JFZFR+LMHCOL)=LFZFRA
         IW(JFZFR+LMHROW)=LROWS(JKZFR)
         DO 51 I = 1,LROWS(JFZFR)
            ITO = I
            ZFR = RTABL(JKZFR,I,JKZFEF)
            IF (ZFR .GT. 0.) THEN
               ITN = ITABL (JDKNFO,ITO,1)
               RW(KROW(JFZFR,ITN)+JFZFZF) = ZFR
            ENDIF
 51      CONTINUE
      ENDIF
C
C normal, healthy return
      RETURN
C sick return: not enough space to book bank...
 990  CONTINUE
      IBFUL = -1
      END
