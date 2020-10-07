      SUBROUTINE  QWHICH_BP(IFLAG)
C-----------------------------------------------------------------------
CKEY FILL USER
C
C   Author  J. Boucrot     28-9-98
C
C   Returns a code indicating the origin of the BEAM SPOT 
C   transmitted in the QVXNOM,QVYNOM,QVZNOM variables
C
C - Input:
C   None.
C
C - Output:
C   IFLAG   / I  Origin of the beam spot:
C        = 0  no beam position found (should never happen)
C        = 1  from the 'JSUM' bank - should be very rare,
C             only if getbp fails and neither RXYZ nor LFIL exist
C        = 2  from the 'LFIL' bank, if getbp fails and no RXYZ exists
C        = 3  from the 'RXYZ' bank (only for old LEP1 data processing
C             of 1989 -> 1995 if getbp fails )
C        = 4  from getbp (event-chunk beam position)
C-----------------------------------------------------------------------
#ifndef DOC
      COMMON / GETFLGS /  IGETEN , IGETXY
C ----------------------------------------------------------------------
      IFLAG = IGETXY
C
      RETURN
      END
#endif

