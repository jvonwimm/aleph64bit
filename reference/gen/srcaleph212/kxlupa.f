       SUBROUTINE KXLUPA (IPART,JKLIN)
C --------------------------------------------------
C - B.Bloch - 870300      modified by F.Ranjard - 870423
C -modified by B.Bloch 890510 for new PART bank
C! fill 'PART' bank with LUND particles
CKEY KINE KINGAL LUND PART  /  USER
C  Get  the NOtracking marker word NOTRK from KRUN bank
C  Fill KLIN bank with LUND particle# which correspond
C       to GEANT particles ( or ALEPH particles)
C  Fill Antilambda C when necessary
C  Get  LUND particles and transfer them to PART bank
C       if they can be produced at LEP energies,
C       with a GEANT# and a tracking type set to NOTRK
C       because they are not used by GEANT.
C  Reduce PART and KLIN banks to their normal size
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXLUPA
C              External References: NAMIND(BOS77)
C                                   KGPART/KBKLIN/KBPART/KXLUTO/AUBPRS
C                                   (ALEPHLIB)
C                                   LUCHGE/ULMASS/LUNAME(JETSET)
C                                   IUCOMP(CERNLIB)
C              Comdecks referenced: BCS, BMACRO,KMACRO
C
C - Usage    : CALL KXLUPA (IPART,JKLIN)
C - Output   : IPART   = KBPART return flag
C                        .gt. 0 means OK
C              JKLIN   = KBKLIN return flag
C                        .gt. 0 means OK
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,
     +          JPARMW=9,JPARAN=10,LPARTA=10)
C     ILUGE (LLUGE) are the LUND numbers corresponding to the first
C                   part of PART bank ( used by GEANT)
C     ILUAL (LLUAL) are the LUND numbers corresponding to the rest of
C                   the PART bank
      PARAMETER ( ELEP = 120.)
      PARAMETER ( IDBNW = 114)
      PARAMETER ( DMAS =0.  , IANTI = 0)
      PARAMETER ( LLUGE=52 ,   LLUAL =315)
      PARAMETER ( NOPA1=392, NOPA2=500, LASTP=588)
      INTEGER ILUGE(LLUGE),ILUAL(LLUAL)
      EXTERNAL NAMIND
      CHARACTER TNAM*12
      DATA ILUGE /1,-7,7,0,-9,9,23,17,-17,38,
     &           18,-18,42,41,-41,37,24,57,43,44,
     &           45,46,47,70,-42,-57,-43,-44,-45,-46,
     &          -47,-70,-11,11,21,-21,20,-20,22,-22,
     &           58,3,-3,2,0,0,0,0,0,0,0,0/
      DATA ILAM /58/
      DATA ILUAL/-58, 4,83,19,-19, 8,-8,10,-10, 12,-12, 94,-94, 95, 0,
     $ 0,25, 3*0,36,96,26, 27,-27, 28,-28, 29,-29,30,-30,31,-31,32,-32,
     $ 33,34,35, 3*0 , 87,  0, 84, 3*0,500,501,502,503,504,505,506,-501,
     $ -502,-503,-504,-505,-506,0,0,101,-101,102,-102,103,-103,104,-104,
     $123,-123,124,-124,125,-125,126,-126,105,-105,106,-106,107,-107,108
     $,-108,109,-109,127,-127,128,-128,129,-129,130,-130,131,-131,61,-61
     $,62,-62,63,-63,64,-64,65,-65,66,-66,67,-67,68,-68,69,-69,48,-48,49
     $,-49,50,-50,51,-51,52,-52,53,-53,59,-59,60,-60,71,-71,72,-72,73,
     $-73,74,-74,75,-75,76,-76,54,-54,55,-55,56,-56,77,-77,78,-78,79,-79
     $,80,-80,145,-145,146,-146,147,-147,148,-148,149,-149,150,-150,151,
     $-151,152,-152,153,-153,154,-154,155,-155,156,-156,157,-157,158,
     $-158,159,-159,160,-160,161,-161,162,-162,163,-163,164,-164,165,
     $-165,166,-166,167,-167,168,-168,241,-241,242,-242,243,-243,244,
     $-244,245,-245,246,-246,247,-247,248,-248,249,-249,250,-250,251,
     $-251,252,-252,293,-293,294,-294,295,-295,296,-296,297,-297,298,
     $-298,299,-299,300,-300,301,-301,302,-302,308,-308,309,-309,310,
     $-310,311,-311,312,-312,313,-313,314,-314,315,-315,316,-316,317,
     $-317,318,-318,319,-319,320,-320,321,-321,322,-322,169,-169,170,
     $-170,171,-171,172,-172,173,-173,253,-253,254,-254,255,-255,256,
     $-256,303,-303,304,-304,305,-305,306,-306,307,-307,-7,7,5*0/
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
C ------------------------------------------------------
C - Get NAPAR name-index of PART bank
      NAPAR = NAMIND ('PART')
C - Get number of columns in PART bank
      IDPAR = IW(NAPAR)
      LCOPA = LCOLS(IDPAR)
C - Get Data Base version number
      CALL ADBVER(IVERS,IDATE)
C
C - NOtrack marker word stored in KRUN bank
      KNOTRK = ITABL (IW(NAMIND('KRUN')),1,2)
C
C - Fill KLIN with LUGEN particle# for the GEANT particles
C   which are the 1st LLUGE particles of PART
C
      DO 1 IPART=1,LLUGE
         JKLIN = KBKLIN (IPART,ILUGE(IPART))
         IF (JKLIN .LE. 0) GOTO 998
 1    CONTINUE
C - if new PART bank format and content, extend the KLIN bank
      IF (LCOPA.EQ.LPARTA .AND. IVERS.GE.IDBNW) THEN
        DO 2 IPART = LLUGE+1,LLUAL+LLUGE
         JKLIN = KBKLIN (IPART,ILUAL(IPART-LLUGE))
         IF (JKLIN .LE. 0) GOTO 998
  2     CONTINUE
C If old content complete with antiLambdac and update new format
C if needed
      ELSEIF (IVERS.LT.IDBNW ) THEN
C
C - Fill Antilambda C
C
         IALAM = KGPART (ILAM)
         NAPAR = NAMIND ('PART')
         TLIF  = TIMLIF (IALAM)
         CHAR  = LUCHGE (ILAM) / 3.
         ZMAS  = ULMASS (0,ILAM)
         TNAM  = ' '
         CALL LUNAME (-ILAM,TNAM)
         IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,-CHAR,TLIF)
         IF (IPART .LE. 0) GOTO 998
         JKLIN = KBKLIN (IPART,-ILAM)
         IF (JKLIN .LE. 0) GOTO 998
         IF (LCOPA.EQ.LPARTA) THEN
            MPART = KMPART (IPART,DMAS,IALAM)
            IF (MPART.LE.0) GO TO 998
            MPART = KMPART (IALAM,DMAS,IPART)
            IF (MPART.LE.0) GO TO 998
         ENDIF
      ENDIF
C
C - Get LUGEN particles and transfer them to PART
C   if their mass is in ELEP energy range
C   these particles are not tracked so their GEANT#
C   and tracking type are set to KNOTRK
C
      DO 1000 MYP=1,LASTP
         IF (MYP.GT.NOPA1 .AND. MYP.LT.NOPA2) GOTO 1000
         IF (IUCOMP(MYP,ILUGE,LLUGE).GT.0) GOTO 1000
         IF((LCOPA.EQ.LPARTA).AND.(IVERS.GE.IDBNW) .AND.
     $   (IUCOMP(MYP,ILUAL,LLUAL).GT.0)) GOTO 1000
         TNAM = ' '
         CALL LUNAME (MYP,TNAM)
         IF (TNAM .EQ. ' ') GOTO 1000
         CHAR = LUCHGE(MYP)/3.
         CALL KXLUTO(MYP,TLIF)
         ZMAS = ULMASS (0,MYP)
C
         IF (MYP.GT.100 .AND. ZMAS.GT.ELEP ) GO TO 1000
C          store the new particle# IPART
         IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,CHAR,TLIF)
         IF (IPART.LE.0) GOTO 998
C          store the user generator particle# of the new particle
         JKLIN = KBKLIN (IPART,MYP)
         IF (JKLIN.LE.0) GOTO 998
C
C          do the same for the antiparticle except if identical
         JANTI = MOD(KTYPL2(MYP),10)
         IF (JANTI.EQ.0 .AND. MYP.LE.100 ) THEN
           IF (LCOPA.EQ.LPARTA ) THEN
              MPART = KMPART (IPART,DMAS,IPART)
              IF (MPART.LE.0) GO TO 998
           ENDIF
         ELSE
           CALL LUNAME (-MYP,TNAM)
           IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,-CHAR,TLIF)
           IF (IPART.LE.0) GOTO 998
           IF (LCOPA.EQ.LPARTA ) THEN
              MPART = KMPART (IPART,DMAS,IPART-1)
              IF (MPART.LE.0) GO TO 998
              MPART = KMPART (IPART-1,DMAS,IPART)
              IF (MPART.LE.0) GO TO 998
           ENDIF
           JKLIN = KBKLIN (IPART,-MYP)
           IF (JKLIN.LE.0) GOTO 998
         ENDIF
C
 1000 CONTINUE
      CALL AUBPRS ('PARTKLIN')
C
      GOTO 999
C - not enough space
 998  CONTINUE
C - End
 999  CONTINUE
C
      END
