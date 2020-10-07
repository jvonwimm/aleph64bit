      SUBROUTINE QFMCSX (ISETUP, SIGMAX)
C-----------------------------------------------------------------------
CKEY FILL BEAMSPOT / INTERNAL
C
C   Author  S. Wasserbaech  2-5-95
C 
C   Returns the BE setup code (row index in ALRP) used in the previous
C   call to QFGET_BP, and the generated sigma_x (width of the luminous
C   region) for that call.  The outputs are -1 if the previous call to
C   QFGET_BP was unsuccessful, or if real data is being analyzed, or
C   if a BSIZ card is in use.
C
C - Input:
C   None.
C
C - Output:
C   ISETUP  / I  Selected/generated BE setup code
C   SIGMAX  / R  Generated sigma_x (cm)
C-----------------------------------------------------------------------
#ifndef DOC
      COMMON / GETBPCO / IFRUN0, IFEVT0, ILRUN0, ILEVT0,
     >                   ISETUP0, SIGMAX0
C-----------------------------------------------------------------------
      ISETUP = ISETUP0
      SIGMAX = SIGMAX0
C
      RETURN
      END
#endif

