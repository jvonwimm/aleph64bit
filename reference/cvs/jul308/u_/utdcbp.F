      SUBROUTINE UTDCBP(IWORD,NBITS,IBITP)
C----------------------------------------------------------------------
C!  - Unpack bitstrings and fills the bits into arrays
C.
C.   Author   :- Alois Putzer           4-AUG-1989
C.               Martin Wunsch          4-AUG-1989
C.               Yves A. Maumary       20-DEC-1989 Adapted for JULIA
C.
C.   Inputs:
C.        - IWORD : INTEGER : bitstring to be unpacked
C.        - NBITS : INTEGER : length of the bitstring
C.
C.   Outputs:
C.        - IBITP : INTEGER : array of dimension NBITS containing the
C.                            unpacked bitstring
C.
C.   Libraries required: CERNLIB
C.
C.   Calls: BTEST, VZERO
C.
C?   Description
C?   ===========
C?   Unpacks a bitstring of length NBITS into an INTEGER array of
C?   dimension NBITS
C?
C?
C!======================================================================
#ifndef DOC
      LOGICAL BTEST
      DIMENSION IWORD(*),IBITP(*)
C
C - Zero the output array
      CALL VZERO(IBITP,NBITS)
C - Get decoded bits
      DO 10 J = 1,NBITS
        K = J-1
        JND = 1 + INT(K/32)
        IBIT = MOD(K,32)
        IF(BTEST(IWORD(JND),IBIT))THEN
          IBITP(J) = 1
        ENDIF
   10 CONTINUE
      RETURN
      END
#endif
