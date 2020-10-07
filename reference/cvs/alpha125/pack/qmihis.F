      SUBROUTINE QMIHIS
CKEY INIT HIST /INTERNAL
C----------------------------------------------------------------------
C! initialize histogram package
C!called from QUIHIS
C!                                                  H.Albrecht 20.09.88
C!                  Options NREC and RECL added     O.Schneider 1.06.94
C----------------------------------------------------------------------
#ifndef DOC
      SAVE CTYPE
#include "qcdesh.h"
      COMMON / QUEST / IQUEST(100)
      LOGICAL LOV,LUP,LEX
      INTEGER ACARD1,ACARD2,ACARD3
      CHARACTER CH2*2, CWRK*24, CTYPE*4
      PARAMETER (LRDEF = 1024)
#if defined(UNIX)
      DATA CTYPE /'EXCH'/
#else
      DATA CTYPE /'HIS'/
#endif
C
C       HBOOK title
C
      IF (ACARD1 ('HTIT') .NE. 0)  I = ACARD2 (1,CQHTIT(41:80))
      IF (I .GE. 40)  I =  ACARD2 (1,CQHTIT)
      CALL HTITLE (CQHTIT)
C
C       get name of histogram file
C
      CALL ACDARG ('HIST',CTYPE,'A',CQFHIS,CWRK,CQDWRK,IER)
      IF (IER .LT. 0)  THEN
        CALL QWMESS ('_QMIHIS_ No HIST card given')
        GO TO 90
      ENDIF
#if defined(UNIX)
      CALL CUTOL (CQFHIS)
#endif
      CALL QWMESS ('_QMIHIS_ Histogram file : ' // CQFHIS)
      IF (IER .GT. 0)  CALL QWERRC (IER,'QMIHIS',CTYPE)
      IF (CQDWRK(1:4) .NE. 'DISK')  CALL QWERRC (7,'QMIHIS',CTYPE)
#if defined(UNIX)
      IF (INDEX(CQDWRK,'DISP').NE.0) THEN
         CALL ADSPOS (KUHIST,CQFHIS,CWRK(1:3),CQDWRK,IER)
      ENDIF
#endif
C
C       get parameters UPDA, NOOV, NREC, RECL
C
      LUP = ACARD3 (2, 'UPDA', CWRK) .GT. 0
      LOV = ACARD3 (2, 'NOOV', CWRK) .GT. 0
      IF (LUP .AND. LOV)  CALL QWSYNT
     +   ('_QMIHIS_ UPDA and NOOV are mutually exclusive')
      NREC = 16000
      IF (LUP) THEN
        LRECL = 0
      ELSE
        LRECL = LRDEF
      ENDIF
      IF (ACARD3(2,'NREC ',CWRK) .NE. 0) THEN
        IF (LUP) CALL QWSYNT
     +   ('_QMIHIS_ UPDA and NREC are mutually exclusive')
        READ(CWRK(6:),*,ERR=10) ITMP
        IF (ITMP.GE.100) NREC=MIN0(MAX0(ITMP,100),65000)
   10   CONTINUE
      ENDIF
      IF (ACARD3(2,'RECL ',CWRK) .NE. 0) THEN
        READ(CWRK(6:),*,ERR=20) ITMP
        IF(ITMP.GT.0.AND.ITMP.LT.1000000) LRECL=ITMP
   20   CONTINUE
      ENDIF
C
C       does file already exist ?
C
      CALL AINQUI (0, CQFHIS, LEX)
      IF (LEX)  THEN
        IF (LOV)  THEN
          CALL QWSYNT
     +      ('_QMIHIS_ File exists. NOOV specified !')
        ELSE
          IF (LUP)  THEN
            CALL QWMESS  ('_QMIHIS_ File exists and will be updated')
          ELSE
            CALL QWMESS
     +       ('_QMIHIS_ File exists. On VAX : create new version')
#if defined(UNIX)
C           For unix machines: remove file first
            CALL SYSTEM ('rm '//CQFHIS(1:LNBLNK(CQFHIS)))
#endif
            CALL QWMESS
     +       ('           On other machines : OVERWRITE')
          ENDIF
        ENDIF
      ELSE
        IF (LUP)  THEN
          CALL QWMESS
     +      ('_QMIHIS_ File does not yet exist : UPDATE ignored')
          LUP = .FALSE.
          IF (LRECL .EQ. 0) LRECL = LRDEF
        ENDIF
      ENDIF
C
C       syntax flag ?
C
      IF (XSYNTX)  THEN
        CALL QWMESS
     +    ('_QMIHIS_ Syntax flag --> histogram file NOT opened')
        GO TO 90
      ENDIF
C
C       open histogram file
C
      CWRK = ' '
      IF (LUP) THEN
        CH2 = 'U'
      ELSE
        CH2 = 'NQ'
        IQUEST(10) = NREC
        WRITE(CWRK(14:),'(A,I6)') 'NREC=', NREC
      ENDIF
   30 ITMP=LRECL
      CALL HROPEN (KUHIST, 'ALPHA', CQFHIS, CH2, LRECL, IER)
      WRITE(CWRK(:11),'(A,I6)') 'RECL=', LRECL
      IF (IER .EQ. 0)  THEN
        CALL QWMESS ('_QMIHIS_ Open successful: '//CWRK)
        XHISTO = .TRUE.
      ELSE
        IF (LUP .AND. ITMP .NE. LRDEF) THEN
C       this code is needed because RZOPEN fails on UNIX/OSF1 when LRECL=0
          CALL QWMESS
     +    ('_QMIHIS_ ZEBRA not able to determine RECL from file.')
          CALL QWMESS ('_QMIHIS_ Making best guess for RECL'//
     +                         ' and trying to open again ...')
          LRECL = LRDEF
          GOTO 30
        ENDIF
        CALL QWSYNT ('_QMIHIS_ Open failed: '//CWRK)
        LUP = .FALSE.
      ENDIF
C
C       read histogram file for updating
C
      IF (LUP)  THEN
        I = 9999
        CALL HRIN (0,I,0)
        CALL QWMESS
     +    ('_QMIHIS_ Read existing histos/ntuples for updating')
        CALL HINDEX
      ENDIF
C
   90 END
#endif
