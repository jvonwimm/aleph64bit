C*EI
C*DK LKNICE
C*DF UNIX
      SUBROUTINE LKNICE(COMAND)
C ----------------------------------------------------------------
C ----------------------------------------------------------------
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
C*CA LKLAST
      COMMON/LKLAST/LBNKNM,LBNKNR,LROWNM,LPRNUM
      CHARACTER*4 LBNKNM
      INTEGER LBNKNR,LROWNM,LPRNUM
C*CC LKLAST
      CHARACTER COMAND*80,RANG*80,STR*25,CH,CHNR,FMT*80
      INTEGER LROW,HROW
      CHARACTER*4 NMBK,CHAINT
      LOGICAL LKOPTI
      EXTERNAL LKOPTI
      INTEGER IFMT(30)
      LOGICAL FFOR,FHEX
      DATA NUFMT /0/
C ----------------------------------------------------------------
      IF (NUFMT.EQ.0) NUFMT = NAMIND('UFMT')
      IF(.NOT.LKOPTI(COMAND,'KINE').AND..NOT.LKOPTI(COMAND,'PART')) THEN
          LROW=0
          HROW=0
          FFOR = LKOPTI(COMAND,'FORMAT')
          FHEX = LKOPTI(COMAND,'HEX')
          IF (FFOR .OR. FHEX) THEN
C Look/Format= or Look/Hex
C
C   get bank format
C
            IF (FHEX) THEN
              FMT = '2I,(K)'
              LFMT = 6
            ELSE
              CALL LKCOMPA(COMAND,'FORMAT',FMT)
            ENDIF
            LFMT = LNBLNK(FMT)
C   if format is ' ' then
C      erase the existing one for this bank if any
C      and then use the standard one
C   else create a 'UFMT' bank to contain the format
C   if not enough space then keep the standard format
C
            IF (LFMT.EQ.0) THEN
             JUFMT = IW(NUFMT)
               IF (JUFMT.GT.0) THEN
                 IF (NMBK.EQ.CHAINT(IW(JUFMT+1))) JUFMT=NDROP('UFMT',0)
               ENDIF
             ELSE
               CALL TRFMT(FMT,IFMT,NFMT)
               JUFMT = NBANK ('UFMT',0,NFMT)
               IF (JUFMT.NE.0) THEN
                 IW(JUFMT+1) = INTCHA(NMBK)
                 DO 10 I=2,NFMT
 10                IW(JUFMT+I) = IFMT(I)
               ENDIF
             ENDIF
C
          ELSEIF (LKOPTI(COMAND,'ROWS')) THEN
             CALL LKCOMPA(COMAND,'ROWS',RANG)
             N=INDEX(RANG,'-')
             CALL LKTRINT(RANG(1:25),I,CH)
             IF (RANG(1:1).EQ.'*') CH='*'
             IF (CH.EQ.' '.OR.CH.EQ.'A') GOTO 998
             IF (CH.EQ.'*') THEN
                LROW=1
             ELSE
                LROW=I
             ENDIF
             IF (N.NE.0) THEN
                 CALL LKTRINT(RANG(N+1:N+24),I,CH)
                 IF (RANG(N+1:N+1).EQ.'*') CH='*'
                 IF (CH.EQ.' '.OR.CH.EQ.'A') GOTO 998
                 IF (CH.EQ.'*') THEN
                     HROW=-1
                 ELSE
                     HROW=I
                     IF (HROW.LT.LROW) GOTO 998
                 ENDIF
             ELSE
                 HROW=LROW
             ENDIF
          ENDIF
          CALL LKGTNAM(COMAND,NMBK,*999)
          IF (NMBK.EQ.'*') THEN
              WRITE(LOUT,*)
     +        '[Wild card for filename only with "/BOS" parameter]'
              RETURN
          ENDIF
          CALL LKGTINT(COMAND,2,' ',NR,CHNR)
      ENDIF
      IF (LKOPTI(COMAND,'PART')) THEN
            CALL PRPART
      ELSEIF (LKOPTI(COMAND,'KINE')) THEN
        IF(IW(NAMIND('KINE')).NE.0) THEN
            CALL PRKINE
        ELSEIF(IW(NAMIND('FKIN')).NE.0) THEN
            CALL PRFKIN
        ELSEIF(IW(NAMIND('DTMC')).NE.0) THEN
            CALL PRDTMC
        ELSE
            WRITE(LOUT,*)' [KINE and FKIN banks do not exist]'
        ENDIF
      ELSEIF (.NOT.FFOR.AND..NOT.FHEX .AND. NMBK.EQ.'HLWD') THEN
        CALL HCPRIND (1)
      ELSEIF (.NOT.FFOR.AND..NOT.FHEX .AND. NMBK.EQ.'HWDI') THEN
        CALL HCPRIND (2)
      ELSE
              CALL LKPRROW(NMBK,NR,LROW,HROW,CHNR)
              LBNKNM=NMBK
              LBNKNR=NR
              LROWNM=HROW
              LPRNUM=HROW-LROW+1
      ENDIF
      RETURN
 998  WRITE(LOUT,*) '[Use form number-number (eg =3-8) for range, ',
     +         'wild cards accepted (eg =5-*) or single digit (eg =5)]'
      RETURN
 999  WRITE(LOUT,*) '[Four character BOS bank name required, and ',
     +            'optional bank number]'
      RETURN
      END
