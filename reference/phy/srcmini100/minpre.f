      SUBROUTINE MINPRE(WLIST, CLASS,IMINI,KEEP)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini code preparation of new event.
C
C     Author: Stephen Haywood      06-Jan-93
C
C     Input  : WLIST  = Wish-list of banks to be written on Mini
C     Output : CLASS  = logical array set according to class word
C              IMINI  = flag indicating whether Mini is being read
C                     = 0 if not Mini i/p
C                     = 1 if Mini i/p with no wish-list on MINI card
C                     = 2 if Mini i/p with wish-list on MINI card
C              KEEP   = flag indicating whether event should be kept
C                     =  0 do not keep event
C                     = -1 lumi ervent: keep limited set of banks
C                     = +1 interesting event: create all possible banks
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JREVDS=1,JREVFE=2,JREVNE=4,JREVSB=6,JREVTI=7,JREVRB=8,
     +          JREVEC=10,LREVHA=10)
      PARAMETER(JRHAPN=1,JRHAPD=3,JRHAPH=4,JRHAPV=5,JRHAAV=6,JRHADV=7,
     +          JRHADD=8,JRHANI=9,JRHANO=10,JRHACV=11,JRHANU=12,
     +          LRHAHA=12)
      PARAMETER (MAXBNK=200,MAXCHA=4*MAXBNK)
      CHARACTER*800 MLISTE,MLISTR
      LOGICAL MINIMC,NOFORM
      COMMON / MINCOM / MLISTE,MLISTR,MINIMC,NOFORM
C
      COMMON / SCALMO / ISCP93,SCPF93
      CHARACTER*800 WLIST
      CHARACTER*4 NAME,NLIST,CHAINT,PROGN
      LOGICAL BTEST,CL24,CLASS,RMINI
      DIMENSION CLASS(32)
      DIMENSION IPV(10),IAV(10)
      DATA ITYP,IAV,IYR,LRUN,RMINI / 0,10*0,0,-999,.FALSE. /
      DATA IFR93,ILR93,IMIVR,IAL94 / 20000 , 23546 , 90 , 157  /
      DATA IMD93 ,SCP93 / 0 , 0.996433 /
      SAVE ITYP,IAV,IYR,LRUN,RMINI
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
C
C++   Initialise bank list for this event.
C
      CALL MINLIS('0')
C
C++   See if input data is already Mini data.
C
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.NE.LRUN) THEN
         CALL ALVSN(ITYP,IPV,IAV,IYR)
         IF (ITYP.EQ.5) THEN
            RMINI = .TRUE.
         ELSE
            RMINI = .FALSE.
         ENDIF
         LRUN = IRUN
C For all MINIs of 1993 data made before a correct reprocessing in 1994
C with MINI  version 90 and ALEPHLIB version 15.6 and before ,
C all particle momenta must be scaled ( a wrong BFIELD was used ) :
         ISCP93 = 0
         SCPF93 = SCP93
         IF (RMINI) THEN
            IF (IRUN.LT.IFR93.OR.IRUN.GT.ILR93) GO TO 4
            JRHAH=IW(NAMIND('RHAH'))
            IF (JRHAH.GT.0) THEN
               DO 3  IRH = LROWS(JRHAH),1,-1
                  KRHAH = KROW(JRHAH,IRH)
                  PROGN = CHAINT(IW(KRHAH+JRHAPN))
                  IF (INDEX(PROGN,'MIN').EQ.0) GO TO 3
                  IF (IW(KRHAH+JRHAPV).NE.IMIVR) GO TO 3
                  IF (IW(KRHAH+JRHAAV).GE.IAL94) GO TO 3
                  ISCP93 = 1
                  GO TO 4
 3             CONTINUE
            ENDIF
 4          CONTINUE
         ENDIF
      ENDIF
C
C++   If input is POT or DST and it was made with old ALEPHLIB, then
C++   determine whether event is class 24, and if so, update classword.
C
      KREVH = IW(NAMIND('REVH'))
      IF ((ITYP.EQ.3 .OR. ITYP.EQ.4) .AND. IAV(3).LT.146) THEN
         CALL CLAS24(CL24)
         IF (CL24) CALL SBIT1(IW(KROW(KREVH,1)+JREVEC),24)
      ENDIF
C
C++   Determine which class bits are set.
C
      KWORD = 0
      IF (KREVH.GT.0) THEN
         IF (LCOLS(KREVH).GE.JREVEC) KWORD = ITABL(KREVH,1,JREVEC)
      ENDIF
      DO 5 I=1,32
    5 CLASS(I) = BTEST(KWORD,I-1)
C
C++   Consider the case where the input is aleady Mini-DST.
C++   Output list is defined by WLIST, or 'E' if WLIST is empty.
C
      IF (RMINI) THEN
         IF (WLIST.EQ.' ') THEN
            I = 0
   10       I = I + 1
            NAME = NLIST(IW,I,'E')
            IF (NAME.EQ.' ') GOTO 20
            CALL MINLIS(NAME)
            GOTO 10
   20       CONTINUE
            IMINI = 2
         ELSE
            MLISTE = WLIST
            IMINI = 1
         ENDIF
         GOTO 100
      ELSE
         IMINI = 0
      ENDIF
C
C++   MC data is inevitably in POT form, and the LUPA bank is not
C++   stripped from Z events. Therefore, drop it here.
C++   A test is made on the Fiducial Energy.
C
      IF (MINIMC) THEN
         KLUPA = NLINK('LUPA',0)
         IF (KLUPA.GT.0) THEN
            EFID = RTABL(KLUPA,1,9)
            IF (EFID.EQ.0.) KLUPA = NDROP('LUPA',0)
         ENDIF
      ENDIF
C
C++   Check if this is a reduced DST event: only EVEH, REVH and perhaps
C++   BCNT, SCLS and SPDA. If so, do not keep it.
C++   After 1993, do not keep LUPA events.
C++      KEEP= 0 => do not keep event,
C++      KEEP=-1 => luminosity event, create limited set of banks,
C++      KEEP=+1 => create all Mini banks.
C
      I = 1
      KEEP = 0
   30 NAME = NLIST(IW,I,'E')
      IF (NAME.EQ.' ') GOTO 40
      IF (NAME.NE.'EVEH' .AND. NAME.NE.'REVH' .AND. NAME.NE.'BCNT' .AND.
     &    NAME.NE.'SCLS' .AND. NAME.NE.'SPDA') THEN
         IF (IYR.LT.93 .OR. NAME.NE.'LUPA') THEN
            IF (NAME.EQ.'LUPA' .OR. NAME.EQ.'SILU' .OR. NAME.EQ.'SILH')
     &        THEN
               KEEP = -1
            ELSE
               KEEP = +1
               GOTO 40
            ENDIF
         ENDIF
      ENDIF
      I = I + 1
      GOTO 30
   40 CONTINUE
C
  100 CONTINUE
      RETURN
      END
