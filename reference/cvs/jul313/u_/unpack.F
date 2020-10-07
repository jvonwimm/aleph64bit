      SUBROUTINE UNPACK(IWORD,NVAL,IUNP)
C----------------------------------------------------------------------
C!  - Unpack 16 bit words and fill them into arrays
C.
C.   Author   :- Alois Putzer           4-AUG-1989
C.               Martin Wunsch          4-AUG-1989
C.               Yves A. Maumary       20-DEC-1989 Adapted for JULIA
C.
C.   Inputs:
C.        - IWORD : INTEGER : array of values packed in 16 bit words
C.        - NVAL  : INTEGER : number of values (MUST BE EVEN)
C.
C.   Outputs:
C.        - IUNP  : INTEGER : array of dimension NVAL with the unpacked
C.                            values
C.
C.   Libraries required: CERNLIB
C.
C.   Calls: IAND, ISHFT, VZERO
C.
C?   Description
C?   ===========
C?   NVAL values packed in 16 bit words into NVAL/2 INTEGERs are
C?   unpacked into NVAL INTEGERs
C?
C?
C!======================================================================
#ifndef DOC
      DIMENSION IWORD(*),IUNP(*)
      INTEGER IT16M
      DATA IT16M/65535/
C
      CALL VZERO(IUNP,NVAL)
      MADC = NVAL/2
      DO 10 I = 1,MADC
        ITEST = IWORD(I)
        IUNP(2*I  ) = ISHFT(ITEST,-16)
        IUNP(2*I-1) = IAND(ITEST,IT16M)
   10 CONTINUE
      RETURN
      END
#endif
