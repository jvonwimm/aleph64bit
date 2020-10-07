      SUBROUTINE MINOUT
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Simple routine to write a Mini-DST.
C
C     Author: Stephen Haywood      15-Mar-90
C
C     Input  : List of Mini-DST banks in / MINCOM /
C
C     This routine is required for standalone output.
C     The data base should be opened and events read with ABRSEL.
C     The Mini o/p file should be closed with
C        CALL BWRITE(IW,LMINI,'0')
C     where LMINI is the o/p unit obtained from ABUNIT.
C     The event records are dropped for each event by adding Mini banks
C     to 'E' list; which is then dropped by ABRSEL.
C     Since the compression package uses BLIST, one of the BOS lists
C     must be used - we use the 'T' list.
C
C     This routine used to create the EDIR. This is now done from REVH
C     and the corresponding code has been removed.
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
C
      COMMON / NAMRHA / NARHAH
C
      CHARACTER*800 RLIST,ELIST
      CHARACTER*4 BANK,NLIST
      LOGICAL FIRST,COMP
      DATA FIRST,COMP,LMINI,LRUN / .TRUE.,.FALSE.,0,-999 /
      SAVE FIRST,COMP,LMINI,LRUN
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
C++   Initialisation.
C
      IF(FIRST) THEN
C
C++      Determine whether compression is required.
C
         KCOMP = IW(NAMIND('COMP'))
         IF(KCOMP.GT.0) THEN
            COMP = .TRUE.
            DO 10 I=1,IW(KCOMP)
   10       IF(IW(KCOMP+I).EQ.INTCHA('NONE')) COMP = .FALSE.
         ENDIF
C
C++      Determine the output units.
C
         CALL ABUNIT(LDST,LSEL,LSEL2,LMINI,LEDIR)
C
         FIRST = .FALSE.
      ENDIF
C
C++   See if we have a new run.
C
      IF(LMINI.GT.0) THEN
         CALL ABRUEV(IRUN,IEVT)
         IF(IRUN.NE.LRUN) THEN
C
C++         Update RHAH bank and write run record.
C
            NARHAH = NAMIND('RHAH')
            CALL BKRHAL('MINI    ',MINVSN(DUMMY),0,5, IFAIL)
C
            CALL BWRITE(IW,LMINI,MLISTR)
C
            LRUN = IRUN
         ENDIF
      ENDIF
C
C++   Handle the event record.
C
C++   If the Mini bank list is empty, do not write anything.
C
      IF (MLISTE.EQ.' ') THEN
         CALL BLIST(IW,'T=','0')
         RETURN
      ENDIF
C
C++   Compress bank.
C++   The 'T' list is subsequently used by MINNOF.
C
      ELIST = MLISTE
      CALL BLIST(IW,'T=',ELIST(1:LENOCC(ELIST)))
      IF(COMP) THEN
         CALL CMPLIS('T',NCOMP)
         DO 200 I=1,MAXCHA/4
            BANK = NLIST(IW,I,'T')
            IF(BANK.EQ.' ') GOTO 205
  200    ELIST(4*I-3:4*I) = BANK
  205    CONTINUE
      ELSE
         NCOMP = 0
      ENDIF
C
C++   Kill the bank formats for some banks from POT/DST.
C
      IF (NOFORM) CALL MINNOF(-1)
C
C++   Write Mini-DST event record.
C
      LASTM = LENOCC(ELIST)
      IF(LMINI.GT.0) CALL BWRITE(IW,LMINI,ELIST(1:LASTM))
C
C++   Restore the bank formats for some banks from POT/DST.
C
      IF (NOFORM) CALL MINNOF(+1)
C
C++   All banks on the Mini list are added to the 'E' list by MINLIS.
C++   Further action is required if some banks are compressed.
C
      IF(NCOMP.GT.0) CALL BLIST(IW,'E+',ELIST(1:LASTM))
C
      RETURN
      END
