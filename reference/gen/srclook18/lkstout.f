C*EI
C*DK LKSTOUT
C*DF UNIX
      SUBROUTINE LKSTOUT (LNEW)
C -----------------------------------------------------------
C! set LOUT logical unit : terminal output unit
C  the first call defines the default
CKEY LOOK OUTPUT LOUT
C - F.Ranjard - 911114
C - Input  : LNEW / I : terminal output unit
C - ENTRY LKRSOUT
C   reset LOUT to ULDEF
C -----------------------------------------------------------
C*IF .NOT.DOC
      INTEGER LNEW
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
      DATA IFI /0/
C -----------------------------------------------------------
C - 1st entry
      IF (IFI.EQ.0) THEN
         IFI = 1
         LDEF = LNEW
      ENDIF
C
C - next entry
      LOUT = LNEW
      RETURN
C -----------------------------------------------------------
C
C - entry
      ENTRY LKRSOUT
      LOUT = LDEF
      RETURN
C -----------------------------------------------------------
      END
