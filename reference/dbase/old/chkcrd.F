*DK CHKCRD
      PROGRAM CHKCRD
C -------------------------------------------------------------
C! Check BOS data cards file structure
C  Author:- F.Jacot - 891023
C                    Revised    23/10/89
C                               11/07/90  by F.Ranjard
C  No library required
C
C  Description
C  ===========
C read a BOS data card file on unit 7
C check that columns 73-80 are empty
C stop at the 1st line with data in columns 73-80 with a message
C ------------------------------------------------------------
#ifndef DOC
      CHARACTER*132 LINE
      CHARACTER*10  LLREC,TEMP
C ------------------------------------------------------------
#if    defined(IBM)
C
      IRC = IOSCLR()
C
C     Get LRECL of Data Cards file
      CALL VMREXX('F','LLREC',TEMP,IRC)
      LL=LENOCC(TEMP)
      READ(TEMP(1:LL),'(I5)') NL
C
C     Read one line on IBM
      NCRD=0
  120 CONTINUE
      READ(7,'(A)',END=150) LINE(1:NL)
#else
C
C     Read one line on VAX
      NCRD=0
  120 CONTINUE
      LINE=' '
      READ(7,'(Q,A)',END=150) NL,LINE
#endif
C
C     Check the line
      NCRD=NCRD+1
      IF (NL .GT. 72) THEN
         DO 130 IJ=73,NL
            IF(LINE(IJ:IJ) .NE. ' ') GOTO 140
  130    CONTINUE
      ENDIF
      GO TO 120
C
  140 WRITE(6,'(//2A,I5,2X,3A/)')
     +         ' *** CHKCRD ***  there are values',
     +         ' after column 72 on card no. ',NCRD,LINE(IJ-5:IJ-1),
     +         '|',LINE(IJ:NL)
      WRITE(6,'(A/)') ' Check your CARDS File from this card no. !!!'
C
#if    defined(IBM)
      STOP 999
C
C     E.O.F on data card file found: no error
  150 STOP
#else
      CALL EXIT (999)
C
C     E.O.F on data card found: no error
 150  CALL EXIT
#endif
      END
#endif
