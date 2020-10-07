      SUBROUTINE MINLIS(BANK)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Add a bank to the list of Mini-DST banks.
C
C     Author: Stephen Haywood      15-Mar-90
C
C     Called by MINDST
C
C     INPUT  : BANK  = bank name to be added to Mini list
C                      '0   ' to initialise list for new event
C     The common block / MINCOM / is modified.
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
      PARAMETER (MAXBNK=200,MAXCHA=4*MAXBNK)
      CHARACTER*800 MLISTE,MLISTR
      LOGICAL MINIMC,NOFORM
      COMMON / MINCOM / MLISTE,MLISTR,MINIMC,NOFORM
      CHARACTER*4 BANK
      DATA LRUN,LEVT / -999,-999 /
      SAVE LRUN,LEVT
C
C++   If we have a new event, empty bank list.
C
      CALL ABRUEV(IRUN,IEVT)
      IF(IRUN.NE.LRUN .OR. IEVT.NE.LEVT) THEN
         LRUN = IRUN
         LEVT = IEVT
         MLISTE = ' '
      ENDIF
C
C++   Empty list.
C
      IF(BANK(1:1).EQ.'0') THEN
         MLISTE = ' '
         RETURN
      ENDIF
C
C++   Store bank name in list.
C
      LASTM = LENOCC(MLISTE)
      IF(LASTM+4.LE.MAXCHA) THEN
         MLISTE(LASTM+1:LASTM+4) = BANK
      ELSE
         WRITE(IW(6),'('' MINLIS: Cannot extend Mini list'',
     &     ''- following bank will be lost: '',A4)') BANK
      ENDIF
C
C++   Add bank to 'E' list which will be dropped by ABRSEL before new
C++   event is read in.
C
      CALL BLIST(IW,'E+',BANK)
C
      RETURN
      END
