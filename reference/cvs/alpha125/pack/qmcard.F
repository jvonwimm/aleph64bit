      SUBROUTINE QMCARD
CKEY INIT CARDS /INTERNAL
C----------------------------------------------------------------------
C! read and interpret cards
C called from QMINIT
C                                                   H.Albrecht 06.11.88
C                                         Mod       E. Blucher 02.02.92
C                                         Mod       F. Ranjard 01.07.92
C                                         Mod       G. Graefe  10.02.95
C                                         Mod       T.Oest     26.07.95
C                                         Mod       J.Boucrot  23.10.98
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      CHARACTER CHAR*4
      LOGICAL LEX,LOV
      INTEGER ACARD1,ACARD2
C
C       read data cards
C
#if defined(UNIX)
CLatB get name of file from the environment
      CALL GETENV('ALPHACARDS',CQFWRK)
C_PVM
C     get name of data card file from the environment
      CALL QPVMCD(CQFWRK)
C_PVM
      IF (CQFWRK .NE. ' ') OPEN(IW(5),FILE=CQFWRK,STATUS='OLD')
#endif
      IDUM = MRESET (KUCARD)
      CALL BREADC      
C
C No KUPTER output if ALPHA called from DALI :
C
      IF (IW(NAMIND('DALI')).GT.0)  KUPTER = 0
      INQUIRE (KUCARD, NAME = CQFWRK)
      CALL QWMESS ('_QMCARD_ Card file : ' // CQFWRK)
C
C        switch card unit to KUCAR2
C       loop over READ cards
C
   20 CALL ACDARG ('READ','CARDS','*',CQFWRK,CHAR,CQDWRK,IER)
      IF (IER .NE.-1) THEN
         IF (IER.NE.0)    CALL QWERRC (IER,'QMCARD','CARDS')
         CALL AOPEN (KUCAR2,CQFWRK,'CARDS',CQDWRK,IER)
         IF (IER.NE.0) THEN
            CALL QWSYNT (' _QMCARD_ Card file cannot be opened')
         ELSE
            CALL QWMESS ('_QMCARD_ next card file : ' // CQFWRK)
            IDUM = MRESET (KUCAR2)
            CALL BREADC
         ENDIF
         GOTO 20
      ENDIF
C 
C_MINI   For official MINI productions, set up the data cards,
C        especially for QSELEP cuts:
C
      CALL QMININC(IER)
      IF (IER.NE.0) CALL QMTERM('_QMCARD_ Error in QMININC')
C
C       special cards :
C
C       SYNTAX
C
   30 IF (ACARD1 ('SYNT') .NE. 0)  CALL QWSYNT
     +  ('_QMCARD_ SYNTAX card given')
C
C       TIME
C
      N = IW (NAMIND ('TIME'))
      IF (N .NE. 0)  QTIME = FLOAT (IW(N+1))
C
C       COPY
C
      IF (ACARD1 ('COPY') .NE. 0)  THEN
        XCOPYJ = .TRUE.
        CALL QWMESS ('------- ALPHA Copy Job -------------------')
        CQUNPK = 'NODE'
      ENDIF
C
C       DEBU
C
      N = IW (NAMIND ('DEBU'))
      IF (N .NE. 0)  THEN
        KDEBUG = 0
        IF(IW(N).GE.1) KDEBUG = IW(N+1)
        IF(IW(N).GE.2) KDEBU1 = IW(N+2)
        IF(IW(N).GE.3) KDEBU2 = IW(N+3)
        IF (IW(N-1) .NE. 0)  CALL QWMESS
     +      ('_QMCARD_ 1st DEBU card accepted; next ones ignored')
      ENDIF
C
C       NWRT
C
      N = IW (NAMIND ('NWRT'))
      IF (N .NE. 0)  THEN
        KNWRLM = IW(N+1)
        IF (IW(N-1) .NE. 0)  CALL QWMESS
     +      ('_QMCARD_ 1st NWRT card accepted; next ones ignored')
      ENDIF
C
C       FILO
C
      CALL AFILOU (CQFOUT, CQFWRK, CQDWRK, KFEOUT)
      IF (KFEOUT .GT. 0)  THEN
        CALL QWMESS ('_QMCARD_ event output file: ' // CQFOUT)
        CALL QWERRC  (KFEOUT,'QMCARD','NATIVE or EPIO or EDIR')
        KFEOUT = -1
      ENDIF
C
      IF (KFEOUT .EQ. 0)  THEN
        LOV = .FALSE.
        NP = 2
C
   50   IF (ACARD2 (NP,CHAR) .EQ. 0)  GO TO 60
        IF (CHAR .EQ. 'NOOV')  LOV = .TRUE.
        IF (CHAR .EQ. 'NORU')  KFEOUT = 1
        IF (CHAR .EQ. 'SELR')  KFEOUT = 2
        IF (CHAR .EQ. 'SREC')  XSREC = .TRUE.
        NP = NP + 1
        GO TO 50
C
C++   See if Mini-DST o/p is required.
C++   If so, force SELR option.
C
   60   CONTINUE
        IF(NLINK('MINI',0).GT.0.OR.NLINK('MINA',0).GT.0) THEN
           XWMINI = .TRUE.
           KFEOUT = 2
           IF(XCOPYJ) CQUNPK = '    '
        ELSE
           XWMINI = .FALSE.
        ENDIF
C
C
C++   See if Nano-DST o/p is required.
C++   If so, force SELR option.
C
        IF(NLINK('NANO',0).GT.0) THEN
           XWNANO = .TRUE.
           KFEOUT = 2
           IF(XCOPYJ) CQUNPK = '    '
        ELSE
           XWNANO = .FALSE.
        ENDIF
C
        IF(CQFOUT(1:1) .NE. ' ')  THEN
          CALL QWMESS('_QMCARD_ Event output file: ' // CQFOUT)
        ELSE
          CALL QWMESS
     &      ('_QMCARD_ Output file name not given on FILO card')
        ENDIF
        IF (CQDWRK(1:4) .EQ. 'DISK')  THEN
          CALL AINQUI (0, CQFOUT, LEX)
          IF (LEX)  THEN
            IF (LOV)  THEN
              CALL QWSYNT
     +          ('0_QMCARD_ File exists. NOOV specified!')
            ELSE
              CALL QWMESS
     +          ('_QMCARD_ File exists. On VAX: create new version')
              CALL QWMESS
     +          ('           On other machines: OVERWRITE')
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C---Histogram printing card
C
      IF (ACARD1 ('NOPH') .NE. 0)  XPRHIS = .FALSE.
C
C--- QFILL control cards
C
      IF (ACARD1 ('NONE') .NE. 0) THEN
        XFILL = .FALSE.
        XFILMC = .FALSE.
        XFILCO = .FALSE.
        XFILCH = .FALSE.
        XFILV0 = .FALSE.
        XFILPC = .FALSE.
        XFILGA = .FALSE.
        XFILJE = .FALSE.
        XFILEM = .FALSE.
      ENDIF
      XFILMC=ACARD1 ('NOMC').EQ.0
      XFILV0=ACARD1 ('NOV0').EQ.0
      IF (ACARD1 ('NOCH') .NE. 0)  THEN
        XFILCH = .FALSE.
        XFILV0 = .FALSE.
      ENDIF
      XFILCO=ACARD1 ('NOCO').EQ.0
      XFILPC=ACARD1 ('NOPC').EQ.0
      XFILGA=ACARD1 ('NOGA').EQ.0
      XFILEM=ACARD1 ('NOEM').EQ.0
C
C       EFLW
C
      N = IW (NAMIND ('EFLW'))
      IF (N .NE. 0)  THEN
        IF (IW(N).GT.0) KEFOPT = IW(N+1)
        IF(KEFOPT.EQ.2)CALL QWMESS
     +    ('_QMCARD_ EFLW option 2 selected (mask energy flow)')
        XFILEF = .TRUE.
        XFILCH = .TRUE.
        XFILCO = .TRUE.
        IF(KEFOPT.NE.2)XFILV0 = .TRUE.
        IF (IW(N-1) .NE. 0)  CALL QWMESS
     +      ('_QMCARD_ 1st EFLW card accepted; next ones ignored')
      ENDIF
C
C       XFILJE+XFILEF
C
C
C       EFLJ
C
      N = IW (NAMIND ('EFLJ'))
C EFLJ Card MUST be there if QFND has been asked :
      JQFND = IW(NAMIND('QFND'))
      IF (JQFND.GT.0.AND.N.EQ.0) THEN
         CALL AUBOS('EFLJ',0,1,JEFLJ,IGARB)
         N = IW(NAMIND('EFLJ'))
      ENDIF
      IF (N .NE. 0)  THEN
        IF (IW(N).GT.0) KEFOPT = IW(N+1)
        IF(KEFOPT.EQ.2)CALL QWMESS
     +    ('_QMCARD_ EFLJ option 2 selected (mask energy flow)')
        XFILEF = .TRUE.
        XFILJE = .TRUE.
        XFILCH = .TRUE.
        XFILCO = .TRUE.
        IF(KEFOPT.NE.2)XFILV0 = .TRUE.
        IF (IW(N-1) .NE. 0)  CALL QWMESS
     +      ('_QMCARD_ 1st EFLJ card accepted; next ones ignored')
      ENDIF
C
C       FRxy data cards :
C
      CALL QMCFRF                    
C
C        NSEQ
C
      XNSEQ=.TRUE.
      CALL AUBOS('NSEQ',0,0,JNSEQ,IGARB)
C
C       UNPK
C
      IF (ACARD1 ('UNPK') .NE. 0 .AND. .NOT. XCOPYJ)
     +    I = ACARD2 (1, CQUNPK)
C
C       drop special card banks (only those which are not yet dropped)
C
      CALL BLIST (IW,'T=',
     &'SYNTDEBUUNPKCOPYNWRTNOEFNOPH')
      CALL BDROP (IW,'T')
C
#if defined(UNIX)
C       properly close input file
      CLOSE(IW(5))
#endif
      END
#endif
