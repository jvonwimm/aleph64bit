      SUBROUTINE JPCOMB( SCHEME, PP, IP1, IP2 )
C----------------------------------------------------------------------
CKEY PHYSICS JET JADE
C   Author      : C. Bowdery     18-APR-1991  Does particle combining
C
C   Description
C   ===========
C!   Two particles are combined to a form a 'jet' using the specified
C!   scheme.
C
C
C  input   : SCHEME      CHAR*2, combination scheme: 'E', 'E0', 'P'
C            PP          array of n-vectors (to be modified)
C            IP1         pointer to particle 1 ("winner")
C            IP2         pointer to particle 2 ("loser")
C======================================================================
#ifndef DOC
      SAVE
C
      INTEGER     IP1, IP2
C
      REAL        PP(5,*), PX, PY, PZ, E, PTOT
C
      CHARACTER   SCHEME*2
C
C-----------------------------------------------------------------------
C
C                 Merge the "loser" with the "winner"
C
      PX    = PP(1,IP1) + PP(1,IP2)
      PY    = PP(2,IP1) + PP(2,IP2)
      PZ    = PP(3,IP1) + PP(3,IP2)
      E     = PP(4,IP1) + PP(4,IP2)
C
      PTOT  = SQRT( PX**2 + PY**2 + PZ**2 )
C
      IF( SCHEME .EQ. 'E' ) THEN
        PP(1,IP1) = PX
        PP(2,IP1) = PY
        PP(3,IP1) = PZ
        PP(4,IP1) = E
        PP(5,IP1) = PTOT
C
      ELSEIF( SCHEME .EQ. 'E0' ) THEN
        PP(1,IP1) = E*PX/PTOT
        PP(2,IP1) = E*PY/PTOT
        PP(3,IP1) = E*PZ/PTOT
        PP(4,IP1) = E
        PP(5,IP1) = E
C
      ELSEIF( SCHEME .EQ. 'P' ) THEN
        PP(1,IP1) = PX
        PP(2,IP1) = PY
        PP(3,IP1) = PZ
        PP(4,IP1) = PTOT
        PP(5,IP1) = PTOT
C
      ELSE
        PP(1,IP1) = 0.0
        PP(2,IP1) = 0.0
        PP(3,IP1) = 0.0
        PP(4,IP1) = 0.0
        PP(5,IP1) = 0.0
      ENDIF
C
C                      Kill the "loser"
C
      PP(1,IP2) =  0.0
      PP(2,IP2) =  0.0
      PP(3,IP2) =  0.0
      PP(4,IP2) =  0.0
      PP(5,IP2) = -1.0
C
      RETURN
      END
#endif
