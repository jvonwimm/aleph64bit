      SUBROUTINE RDCARD  (IERD)
C----------------------------------------------------------------------
C!  - Read JULIA data cards
C!
C!   Author   :- E. Lancon             10-MAR-1992
C!
C!   Inputs:
C!        - None
C!
C!   Outputs:
C!        - IERD         I/      0 = OK
C!                               else NOT OK
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rflags.h"
      PARAMETER (IFIRST=0,ILAST=999999)
#include "bmacro.h"
      IERD  = 0
      IDUM  = MRESET (LCOMRL)
      CALL BREADC
C
C?   Look at possible existing JCAR bank, decode it
C?   save it as ARJC , it will be renamed to JCAR in RWRUNH
C?   when the run record will be read in and the number of JCAR banks
C?   already present on the file (previous processing) is determined.
C
      NRJCAR = 0
      NAJCAR = NAMIND('JCAR')
      KJCAR = IW(NAJCAR)
      IF ( KJCAR.GT.0 ) NRJCAR = IW(KJCAR-2)
      CALL BCALLC(IW,'JCAR',NRJCAR)
      CALL BSWAP (IW, 'JCAR', 'ARJC')
C
C?   Is there a READ data card ?
C?   first define new input unit
C
   20 CALL AGTFIL ('READ','READ',LCOMRL-1,IER)
      IF (IER.NE.-1) THEN
        IF(IER.NE.0) THEN
          WRITE (6,*) ' RDCARD : cannot open extra data card file'
          IERD = 1
        ELSE
          IDUM = MRESET(LCOMRL-1)
          CALL BREADC
        ENDIF
        GOTO 20
      ENDIF
C
C?   Case of POT card make it a FILO card
C
      IF (  IW(NAMIND('POT ')).NE.0 ) THEN
        CALL BSWAP (IW, 'POT ', 'FILO')
      ENDIF
C
C?   LOOK for DEBU card
C
      KDEBU = IW(NAMIND('DEBU'))
      IF (KDEBU.NE.0) THEN
        IF (IW(KDEBU).EQ.1) THEN
          IDEBBC = IFIRST
          IDEBTC = ABS(IW(KDEBU+1))
        ELSE
          IDEBBC = ABS(IW(KDEBU+1))
          IDEBTC = ABS(IW(KDEBU+2))
        ENDIF
      ELSE
        IDEBBC = IFIRST
        IDEBTC = ILAST
      ENDIF
C
C?   Get Input/Output units and formats
C
      CALL RDEFIO (IERIO)
      IF (IERIO.NE.0) IERD = 2
C
  999 RETURN
      END
#endif
