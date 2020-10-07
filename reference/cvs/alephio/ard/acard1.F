      INTEGER FUNCTION ACARD1 (CLAB)
C----------------------------------------------------------------------
C!  Return index for the next card bank
CKEY ALREAD CARD
C   Author     :Hartwig Albrecht         4-NOV-88
C   modified by D.Schlatter                APR-89
C
C        get bank index of next CLAB bank number. start at
C        CLAB,NR=0.
C        The next call will get CLAB,NR=1 ect..
C        To restart from NR=0 call ACARD1 with CLAB=' '
C
C!   Inputs: CLAB : bank label (character * 4)
C!                  = 1st...4th character on the input card
C                   = '    ' reset counter and start with 1. card
C!
C!   Outputs: ACARD1 > 0 : card ( = BOS bank with label CLAB) exists
C!                   = 0 : no (more) cards with label CLAB given.
C!
C    Calls:              AIOCAR
C!   Libraries required: BOS77
C
C    ENTRY:      ACARD0(NR)       returns name-index and number of
C                                 last used card
C!
C!   Description
C!   ===========
C!
C?   Simple minded card interpreter.
C?   ACARD1 checks the existence of the first (next) BOS bank CLAB
C?          which is assumed to be created by an input card.
C?          For every bank label CLAB, the cards are interpreted in
C?          their proper order. Bank numbers are not allowed to be
C?          given on input cards.
C?          The content of this bank can be interpreted by subsequent
C?          calls to ACARD2.
C?   Example : Fortran program :
C?          CHARACTER * 20 CPARA          ; must be sufficiently long !
C?      10  IF (ACARD1 ('ABCD') .EQ. 0)  GO TO 30       ; get next card
C?          NPARA = 1                       ; init loop over parameters
C?      20  IF (ACARD2 (NPARA,CPARA) .EQ. 0)  GO TO 10  ; more parm's ?
C?          ...                           ;  interprete parameter CPARA
C?          NPARA = NPARA + 1                          ; next parameter
C?          GO TO 20
C?      30  CONTINUE ...                 ; all 'ABCD' cards interpreted
C?   Example : card syntax (text starts in column 1 !) :
C?   ABCD 'PARM1 | XYZ   | PARM3 | | PARM5|PARM6   '
C?   The syntactical separator | may be surrounded by blank spaces.
C?   For the time being, numbers on input cards are NOT supported by
C?   ACARD1/2 : This would require major changes in some BOS routines.
C?======================================================================
C
#ifndef DOC
#include "bcs.h"
      CHARACTER *(*) CLAB
      CHARACTER*4   CLABS
      CHARACTER * 1 CDUM
      INTEGER ACARD2,ACARD0
C
      PARAMETER (LCARD=40 )
      INTEGER INEXT(LCARD)
      CHARACTER*4 CARD(LCARD)
      SAVE
      DATA INEXT/LCARD*0/
      DATA NCARD/0/
C --------------------------------------------------------------
      ACARD1 = 0
C                    reset INEXT counter?
      IF(CLAB.EQ.' ') THEN
        DO 1 I=1,LCARD
    1   INEXT(I) = 0
        RETURN
      ENDIF
C                    new card name?
      DO 2 I=1,NCARD
        JCARD=I
        IF(CLAB.EQ.CARD(I)) GOTO 4
    2 CONTINUE
      NCARD=NCARD+1
      CARD(NCARD) = CLAB
      JCARD=NCARD
    4 CONTINUE
C                    get bank index for card JCARD
      ACARD1 = NLINK( CLAB, INEXT(JCARD) )
      IF( ACARD1.EQ.0) RETURN
      INEXT(JCARD) = INEXT(JCARD) + 1
C
      IF(CLAB.EQ.'FILM' .AND. IW(ACARD1-1).EQ.0) INEXT(JCARD) = 0

C                    init ACARD2 :
      IDUM = ACARD2 (-ACARD1, CDUM)
C
C               Save arguments for ACARD0 (APOLLO compiler)
      ICARD1 = ACARD1
      CLABS = CLAB
C
      RETURN
C                    return last NAME-index, without INEXT change
      ENTRY  ACARD0( NR )
      NR     = IW( ICARD1 - 2 )
      ACARD0 = NAMIND( CLABS )
      END
#endif
