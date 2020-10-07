      SUBROUTINE FYINEV(IBFUL)
C ----------------------------------------------------------
CKEY FYXX  /INTERNAL
C J.Hilgart - 871307          mods 310588
C! Fxxx DST format event initialization.
C If this routine has already been called in this event, do not
C repeat certain steps dealing w/ MC info.
C OUTPUT : IBFUL = -1 if not enough space to book a bank
C
C - Called from    USER
C - Calls          BLIST,BWIND,BDROP,WBANK,WDROP    from BOS.OLB
C                  FYKILL                           from ALEPHLIB
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
      DATA IONC /0/
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
C ====================================================================
C Name indices
      IF (IONC.EQ.0) THEN
         NKINE = NAMIND('KINE')
         NVERT = NAMIND('VERT')
         NCALO = NAMIND('CALO')
         NKEVH = NAMIND('KEVH')
         NKHIS = NAMIND('KHIS')
         IONC = 1
      ENDIF
      IBFUL = 0
C
C Drop any negative KINE banks to avoid any trouble
      KK = NKINE + 1
 2    KK = IW(KK-1)
      IF (KK .GT. 0) THEN
         NR = IW(KK-2)
         IF (NR .GT. 0) GOTO 3
         IDRP = NDROP('KINE',NR)
         GOTO 2
      ENDIF
 3    CONTINUE
C
C Get no. of tracks and vertices which came from event generator
      JKEVH = IW(NKEVH)
      IF (JKEVH .NE. 0) THEN
         KINTRK = ITABL(JKEVH,1,JKEVNT)
         KINVER = ITABL(JKEVH,1,JKEVNV)
      ELSE
         KINTRK = 0
         KINVER = 0
      ENDIF
C
C Drop previous Fxxx banks
      LEN = LNBLNK (ELISAD)
      CALL BDROP(IW,ELISAD(1:LEN))
C
C Add the Fxxx banks to E list.
      CALL BLIST(IW,'E+',ELISAD(1:LEN))
C
C
C - get all KINE and VERT indices
      CALL BWIND (IW,'KINE',MAXMCX,INDKIN)
      IF (INDKIN.EQ.0) GOTO 990
      LAST = IW(INDKIN)
      KLAST = IW(INDKIN+LAST)
      IF (IW(KLAST-1).NE.0) GOTO 989
      CALL BWIND (IW,'VERT',MAXMCX,INDVER)
      IF (INDVER.EQ.0) GOTO 990
      LAST = IW(INDVER)
      KLAST = IW(INDVER+LAST)
      IF (IW(KLAST-1).NE.0) GOTO 989
C
C Set work banks JDKEKS and JDVNFO with new track #s and new
C     vertex #s or 0 if the track or the vertex has to be dropped.
       CALL FYKILL (IBFUL)
       IF (IBFUL .EQ. -1) GOTO 990
C
C normal, healthy return
      RETURN
C sick return: not enough space to book bank...
 989  CONTINUE
      WRITE (IW(6),*) ' FYINEV MAXMCX in /FYRELA/ is too small '
 990  CONTINUE
      IBFUL = -1
      IW(1) = LFXWBK
      CALL WDROP (IW,JDFXWB)
      END
