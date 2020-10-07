      SUBROUTINE  QWHICH_EN(IFLAG)
C-----------------------------------------------------------------------
CKEY FILL USER                 
C
C   Author  J. Boucrot     28-9-98
C
C   Returns a code indicating the origin of the LEP energy
C   transmitted in the QELEP variable
C
C - Input:
C   None.
C
C - Output:
C   IFLAG   / I  Origin of the LEP energy value QELEP :
C        = 0  no energy found
C        = 1  from the 'RLEP' bank (always the case for MCarlo)
C        = 2  from the 'LFIL' bank 
C             for 1989 -> 1992 real data 
C             or LEP2 data when neither time-slice nor run-averaged
C        = 3  from the run-averaged values of the LEP Working Group
C             for 1993 -> 1995 LEP 1 data, or LEP 2 data if 
C             or LEP 2 data if no time-slice available
C        = 4  from the 15' time-slices of the LEP Working Group
C             for LEP 2 data
C-----------------------------------------------------------------------
#ifndef DOC
      COMMON / GETFLGS /  IGETEN , IGETXY
C ----------------------------------------------------------------------
      IFLAG = IGETEN 
C
      RETURN
      END
#endif

