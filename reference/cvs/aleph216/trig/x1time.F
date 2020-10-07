      SUBROUTINE X1TIME(IPTIM,IDTIM)
C----------------------------------------------------------------------
C!  - Unpack the event time
C.
C.   Author   :- Alois Putzer           4-AUG-1989
C.               Martin Wunsch          4-AUG-1989
C.               Yves A. Maumary       20-DEC-1989 Adapted for ALEPHLIB
C.
C.   Inputs:
C.        - IPTIM : INTEGER  Array of dimension 2 containing the
C.                           packed time
C.
C.   Outputs:
C.        - IDTIM : INTEGER  Array of dimension 7 containing the event
C.                           time in the following order:
C.                           day, month, year, hour, minute, second,
C.                           1/100 second
C.
C.   Libraries required: CERNLIB
C.
C.   Calls: IAND, ISHFT
C.
C?   Description
C?   ===========
C?   The time when the event occured is contained in the trigger bank
C?   XTCN, but in a packed form. This routine decodes the time into
C?   an array of 7 integers with, in order:
C?   day, month, year, hour, minute, second, 1/100 second
C?
C.======================================================================
#ifndef DOC
      SAVE
      DIMENSION IPTIM(*),IDTIM(*)
      INTEGER MASK
      DATA MASK/255/
C - Get Day
      IDTIM(1) = IAND(IPTIM(1),MASK)
C - Get Month
      IDTIM(2) = IAND(ISHFT(IPTIM(1),-8),MASK)
C - Get Year
      IDTIM(3) = ISHFT(IPTIM(1),-16)
C - Get Hour
      IDTIM(4) = ISHFT(IPTIM(2),-24)
C - Get Minute
      IDTIM(5) = IAND(ISHFT(IPTIM(2),-16),MASK)
C - Get Second
      IDTIM(6) = IAND(ISHFT(IPTIM(2),- 8),MASK)
C - Get 1/100 Second
      IDTIM(7) = IAND(IPTIM(2),MASK)
      RETURN
      END
#endif
