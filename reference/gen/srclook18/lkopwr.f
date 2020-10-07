C*EI
C*DK LKOPWR
C*DF UNIX
      SUBROUTINE LKOPWR(COMAND,ATYPE,LTYPE)
C ------------------------------------------------------------
C! Open a new print file in 72, 132 characters or 32040 bytes
C - Input:    ATYPE  / A  = ALEPH file type
C             LTYPE  / I  = record length
C                           72 or 132 characters
C                           3600 or 32040 bytes
C ------------------------------------------------------------
C*IF .NOT.DOC
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
      CHARACTER FILENM*80, FTYPE*6, FDUM*6
      CHARACTER*(*) ATYPE, COMAND
      LOGICAL LEX,LKOPTI
C ----------------------------------------------------------
         IF (LKOPTI(COMAND,'OUTPUT')) THEN
C         Write/Out list file
            NPOS = 2
         ELSE
C         Write name nr file
            NPOS = 3
         ENDIF
         CALL LKGTPAR(COMAND,NPOS,'_Output file: ',FILENM,LF)
         IF (ATYPE.EQ.'   ') THEN
          IF (LKOPTI(COMAND,'EPIO')) THEN
              FTYPE='EPIO'
          ELSEIF (LKOPTI(COMAND,'NATIVE')) THEN
              FTYPE='NATIVE'
          ELSEIF (LKOPTI(COMAND,'CARD')) THEN
              FTYPE='CARD'
          ELSE
              FTYPE = '    '
          ENDIF
         ELSE
          FTYPE = ATYPE
         ENDIF
C
         IF (LTYPE.LE.72) THEN
            LF = 1
            FTYPE = 'CARD'
         ELSEIF (LTYPE.LE.132) THEN
            LF =2
            FTYPE = 'CARD'
         ELSE
            LF = 3
         ENDIF
         LL=LNBLNK(FILENM)
         IF (FILENM(1:LL).NE.'*') FILNAM(LF) =FILENM(1:LL)
         LL=LNBLNK(FILNAM(LF))
         FDUM = FTYPE
         IF (FTYPE.EQ.'   ' .AND. LF.EQ.3) FDUM = 'EPIO'
         LREC = KALREC (FDUM)
         LDUM = MALREC (FDUM,LTYPE)
         CALL AOPENW(LFILE(LF),FILNAM(LF)(1:LL),FTYPE,'    ',IRC)
         LDUM = MALREC (FDUM,LREC)
         IF (IRC.NE.0) THEN
              WRITE(LOUT,*)' [Failed to open file ',FILNAM(LF)(1:LL),
     +                  ' with write access]'
              FLGFIL(LF)=.FALSE.
              RETURN
          ELSE
              WRITE(LOUT,*)' [File ',FILNAM(LF)(1:LL),
     +           ' opened with write access on unit ',LFILE(LF),']'
              FLGFIL(LF)=.TRUE.
          ENDIF
      END
