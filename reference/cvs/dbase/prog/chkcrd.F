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
      NCRD=0
 120  CONTINUE
      READ(7,'(A)',END=150) LINE
      nl = lnblnk(line)
      if (nl.le.72) goto 120
C
  140 WRITE(6,'(//2A,I5,2X,3A/)')
     +         ' *** CHKCRD ***  there are values',
     +         ' after column 72 on card no. ',NCRD,LINE(73:NL)
      WRITE(6,'(A/)') ' Check your CARDS File from this card no. !!!'
C
      CALL EXIT (999)
C
C     E.O.F on data card found: no error
 150  CALL EXIT

      END
#endif
