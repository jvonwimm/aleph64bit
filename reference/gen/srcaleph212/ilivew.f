      SUBROUTINE ILIVEW(LIST,IER)
C-----------------------------------------------------------------------
CKEY ITC WIRE ILIV
C! Create 'Live' wire bank ILIV
C!
C!    Author     :- W. Atwood
C!    Modified   :- J. Sedgbeer  89/12/08
C!    Modified   :- J. Sedgbeer  91/01/07 Ensure correct IWST bank used.
C!
C!    Input:
C!      LIST   /C    : BOS list to which ILIV is to be added e.g. 'T+'
C!      commons:     /BCS/     dbase bank IWST
C!      params:      IWSTJJ
C!
C!    Output:
C!      IER    /I    : = 0 successful
C!                     = 1 input bank is empty or does not exist
C!                     = 2 not enough space (garbage collection may
C!                                  have been done).
C!                     =-1 O.K. but garbage collection
C!      ILIV bank
C!
C!   calls     : none
C!
C! ILIVEW:
C! Create bank ILIV of 'live' ITC wires for this event. ILIV gives
C! the status of a wire (channel) - see DDL.
C!
C?  If first then Set name indices and bank formats
C?  Create ILIV bank
C?  Get IWST bank
C?  Loop over wires in IWST
C?    fill ILIV
C?  End Loop
C?  Add ILIV to BOS list
C-----------------------------------------------------------------------
      SAVE
C I/O commons and parameters
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JIWSID=1,JIWSVR=2,JIWSIW=4,JIWSFL=5,JIWSIP=6,LIWSTA=6)
      CHARACTER*(*) LIST*(*)
      EXTERNAL NAMIND
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA NWMAX/960/
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        NIWST = NAMIND('IWST')
        NILIV = NAMIND('ILIV')
        CALL BKFMT('ILIV','I')
      ENDIF
      IER = 1
C
C Create ILIV bank. Use IGARB to note if garbage collection done.
C
      IGARB = 0
      CALL AUBOS('ILIV',0,LMHLEN+NWMAX,JILIV,IER)
      IF(IER.EQ.2) GOTO 999
      IF(IER.EQ.1) IGARB = 1
C
      IW(JILIV+LMHCOL) = 1
      IW(JILIV+LMHROW) = NWMAX
C
C-----------------------------------------------------------------------
C Check for validity of IWST bank.
C If valid IWST bank then loop over wires in IWST - fill ILIV
C
      JIWST = IW(NIWST)
      IF(JIWST.GT.0) THEN
        NWIR = LROWS(JIWST)
        DO 10 I=1,NWIR
          KK = KROW(JIWST,I)
          IWIR = IW(KK+JIWSIW)
          IFLG = IW(KK+JIWSFL)
          IW(JILIV+LMHLEN+IWIR) = IFLG
   10   CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C Add ILIV to list
C
      CALL BLIST (IW,LIST,'ILIV')
C
C If garbage collection then set error flag to -1
C
      IER = 0
      IF(IGARB.EQ.1) IER = -1
C
  999 CONTINUE
      END
