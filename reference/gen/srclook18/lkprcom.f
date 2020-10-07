C*EI
C*DK LKPRCOM
C*DF UNIX
      SUBROUTINE LKPRCOM(PRECOM,COMNUM)
C --------------------------------------------------
C ---------------------------------------------------
C*IF .NOT.DOC
      CHARACTER*80 PRECOM(20)
      INTEGER COMNUM
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
C --------------------------------------------------
      DO 10 I=1,COMNUM
      L=LNBLNK(PRECOM(I))
 10   WRITE(LOUT,FMT='(I3,2A)') I,'] ',PRECOM(I)(1:L)
      RETURN
      END
