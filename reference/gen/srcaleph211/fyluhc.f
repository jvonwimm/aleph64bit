      INTEGER FUNCTION FYLUHC(IFKIN,MXDAU,LUDAU)
C -----------------------------------------------------------
CKEY FYXX MCARLO KINE HISTORY / USER
C - F.Ranjard - 880920
C! Decode LUND history code.
C  Return the # of daughters and the list of daughter#s of a track
C  known by its FKIN row #.
C  There are FYLUHC such daughters.
C  if FYLUHC .eq. 0 then NO daughter, or NOT a LUND history code
C  if FYLUHC .gt. MXDAU then the buffer LUDAU is too small
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: FYLUHC
C              External References: NAMIND(BOS77)
C              Comdecks referenced: BCS, BMACRO
C
C - usage  : NDAU  = FYLUHC (IHCOD,MXDAU,LUDAU)
C - input  : IFKIN = track# known by its row# in FKIN bank
C            MXDAU = length of LUDAU array
C - output : LUDAU = array which contains the list of daughters
C            FYLUHC= # of daughters ( the #s of the 1st MXDAU are
C                    stored in LUDAU)
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
      INTEGER LUDAU(*)
      DATA NFKIN /0/
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
C - history code
      IHCOD(IFLR) = ITABL(JFKIN,IFLR,JFKIHC)
C - mother track#
      MOTHER(IFLR)= MOD (IHCOD(IFLR),10000)
C --------------------------------------------------------------
      IF (NFKIN.EQ.0) THEN
         NFKIN = NAMIND('FKIN')
      ENDIF
C
      NDAU = 0
C
C - IF not FKIN bank RETURN with FYLUHC=0
      JFKIN = IW(NFKIN)
      IF (JFKIN.EQ.0) GOTO 900
C
C - IF not a valid track# THEN RETURN with FYLUHC=0
      IF (IFKIN.EQ.0 .OR. IFKIN.GT.LROWS(JFKIN)) GOTO 900
C
C - IF not a valid LUND history code THEN RETURN with FYLUHC=0
      IF (IHCOD(IFKIN) .EQ. 0) GOTO 900
C
C - Loop over all history codes which are .ne. 0 to find the
C   daughters of track# IFKIN
      DO 10 I=1,LROWS(JFKIN)
         IF (IHCOD(I) .EQ. 0) GOTO 10
         IF (MOTHER(I).EQ.IFKIN) THEN
            NDAU = NDAU + 1
            IF (NDAU.LE.MXDAU) LUDAU(NDAU) = I
         ENDIF
 10   CONTINUE
C
 900  FYLUHC = NDAU
      END
