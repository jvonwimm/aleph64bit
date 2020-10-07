      SUBROUTINE CRFILM
C-----------------------------------------------------------------------
CKEY ALREAD CREATE FILM BANK
C  Author:       M. Talby               May 90
C
C! create and write FILM bank
C
C  Input : none
C
C  Output:  none
*-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alrcom.h"
      CHARACTER*80 CHFILM
      CHARACTER*4 CHAINT,NAME
C --
C   Write data filename to select file: FILM data card
C   ==================================================
C
      LUTSEL = JUNIAL ('OUT',2,0)
C   get name + nr of the bank which contains the filename
C --
      IF(IFWITH.EQ.0) THEN
C --
C   Edir for input data:
C   Filename comes from a FILI card or a FILM card
C --
C   Look for EDIR output unit
C --
        JND = NAMIND('$PTS')+1
   10   JND = IW(JND-1)
        IF(JND.EQ.0) GOTO 100
        LUNA = IW(JND-2)
        IF (LUNA.NE.LUTSEL) GOTO 10
C --
        NAME  = 'FILI'
        NR    = NRFILI(1)
        JFILM = NLINK ('FILM',NRFILM)
        IF (JFILM.NE.0) THEN
C --
C   If FILM exists then swap to nr=0 if not already 0
C   and then write it onto select file
C --
          IF (NRFILM.NE.0)ISWP = NSWAP('FILM',NRFILM,'FILM',0)
          GOTO 30
        ENDIF
      ELSE
C --
C   Edir for output data:
C   filname comes from NAFILO(IO) card
C --
        IO    = 1
        NAME  = NAFILO(IO)
        NR    = NRFILO(IO)
      ENDIF
C --
C   Get bank which contains the filename
C --
      IND = NLINK (NAME,NR)
      IF(IND.EQ.0) THEN
        IF(IW(6).GT.0) WRITE(IW(6),*)
     &    ' CRFILM_ No master file given for new select file:'
      ENDIF
C --
      IF(IFWITH.NE.0) THEN
        CALL ALSTIN (IW(IND+1),IW(IND),CHFILM)
        ILEN1 = INDEX(CHFILM,'WITH')
        IBAR = 0
        DO 33 L = ILEN1-1,1,-1
          IF (INDEX(CHFILM(L:L),'|') .NE. 0) THEN
             LFILM = ((L-1)+3) / 4
             DO 32 J = L,LFILM*4
                CHFILM(J:J) = ' '
   32        CONTINUE
             GOTO 34
          ENDIF
   33   CONTINUE
   34   JFILM = NBANK('FILM',0,LFILM)
C --
C   Create, Fill FILM, nr=0 with the filename
C --
        DO 35 II = 1,LFILM
          IW(JFILM+II) = INTCHA(CHFILM(4*II-3:4*II))
   35   CONTINUE
C
      ELSE
C
        JFILM = NBANK ('FILM',0,IW(IND))
        CALL UCOPY (IW(IND+1),IW(JFILM+1),IW(IND))
      ENDIF
C --
C   Write the FILM bank with the filename
C --
  30  CALL BWRSB(IW,LUTSEL,'FILM',0)
C --
C   Drop FILM bank(s) which are no longer necessary
C   next time if a FILM bank exist it is a new input file read
C   through en event directory
C --
  100 IDRP = NDROP ('FILM',0)
      IF (NRFILM.NE.0) IDRP = NDROP('FILM',NRFILM)
C --
      RETURN
      END
#endif
