      SUBROUTINE MINLIS(BANK)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Add a bank to the list of Mini-DST banks.
C
C     Author: Stephen Haywood      15-Mar-90
C
C     Called by MINDST
C
C     INPUT  : BANK  = bank name to be added to Mini list
C                      '0   ' to initialise list for new event
C     The common block / MINCOM / is modified.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "mincom.h"
      CHARACTER*4 BANK
      SAVE LRUN,LEVT
      DATA LRUN,LEVT / -999,-999 /
C
C++   If we have a new event, empty bank list.
C
      CALL ABRUEV(IRUN,IEVT)
      IF(IRUN.NE.LRUN .OR. IEVT.NE.LEVT) THEN
         LRUN = IRUN
         LEVT = IEVT
         MLISTE = ' '
      ENDIF
C
C++   Empty list.
C
      IF(BANK(1:1).EQ.'0') THEN
         MLISTE = ' '
         RETURN
      ENDIF
C
C++   Store bank name in list.
C
      LASTM = LENOCC(MLISTE)
      IF(LASTM+4.LE.MAXCHA) THEN
         MLISTE(LASTM+1:LASTM+4) = BANK
      ELSE
         WRITE(IW(6),*)' MINLIS: Cannot extend Mini list',
     &     '- following bank will be lost: ', BANK
      ENDIF
C
C++   Add bank to 'E' list which will be dropped by ABRSEL before new
C++   event is read in.
C
      CALL BLIST(IW,'E+',BANK)
C
      RETURN
      END
#endif
