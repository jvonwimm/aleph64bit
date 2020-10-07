      SUBROUTINE EAUTOP ( NAUTO , NAUTOW )
C----------------------------------------------------------------------
CKEY E_SET EHWI EHPA RUNQ
C!  - Decode banks EHWI and EHPA and summarize (condense) into
C!    arrays NAUTO and NAUTOW. These arrays are subsequently packed
C!    into bank ECRQ with the routine ECRUNQ.
C!
C!     Lee F. Thompson. 29th April 1994.
C
C ======================================================================
#ifndef DOC
#include "bcs.h"
      INTEGER ECXMOD
      INTEGER NAUTO  ( 36 )
      INTEGER NAUTOW ( 36 )
#include "bmacro.h"
C*---------------------------------------------------------------------
C*
C*    OK, first unpack the EHPA bank for this event if it exists.
C*
      KEHPA  = IW ( NAMIND ( 'EHPA' ) )
      IF ( KEHPA .NE. 0 ) THEN
C*        Decode this way as the bank is unformatted
          IF ( IW ( KEHPA ) .GT. 0 ) THEN
              DO IIEHPA = 1 , IW ( KEHPA )
                  MODULE = ECXMOD ( IW ( KEHPA + IIEHPA ) )
                  IF ( MODULE .GE. 1 .AND. MODULE .LE. 36 ) THEN
                      NAUTO ( MODULE ) = NAUTO ( MODULE ) + 1
                  ELSE
                      CALL RERROR ( 'EAUTOP' , 1 ,
     *                'Invalid Module number from EHPA' )
                  END IF
              END DO
          ELSE
              CALL RERROR ( 'EAUTOP' , 2 , 'EHPA exists but empty' )
          END IF
      END IF
C*
C*    next unpack the EHWI bank in a similar way
C*
      KEHWI  = IW ( NAMIND ( 'EHWI' ) )
      IF ( KEHWI .NE. 0 ) THEN
C*        Decode this way as the bank is unformatted
          IF ( IW ( KEHWI ) .GT. 0 ) THEN
              DO IIEHWI = 1 , IW ( KEHWI )
                  MODULE = IBITS ( IW ( KEHWI + IIEHWI ) , 20 , 12 )
                  IF ( MODULE .GE. 1 .AND. MODULE .LE. 36 ) THEN
                      NAUTOW ( MODULE ) = NAUTOW ( MODULE ) + 1
                  ELSE
                      CALL RERROR ( 'EAUTOP' , 3 ,
     *                'Invalid Module number from EHWI' )
                  END IF
              END DO
          ELSE
              CALL RERROR ( 'EAUTOP' , 4 , 'EHWI exists but empty' )
          END IF
      END IF
      RETURN
      END
#endif
