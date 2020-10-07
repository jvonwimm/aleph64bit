      INTEGER FUNCTION VITEXI (TXTM,ILAY,IFAC,IMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates local layer, face, and module indices from text module
C - Joe Rothberg, March 1994
C
C - Input:
C   TXTM   / C*4  Text module name
C
C - Output:
C   VITEXI / I  = VDOK if successful
C               = VDERR if error occurred
C   IFAC   / I    Local face index
C   ILAY   / I    Local layer index
C   IMOD   / I    Local module index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER ILAY,IFAC, IMOD
      CHARACTER*4 TXTM
C
C local variables
      CHARACTER*2 CHFAC
      CHARACTER*1 CHMOD, CHLAY
C
C ----------------------------------------------------------------------
C
      VITEXI = VDERR
C
      CHMOD = TXTM(1:1)
      CHLAY = TXTM(2:2)
      CHFAC = TXTM(3:4)
C
      IMOD = 0
      ILAY = 0
      IFAC = 0
      READ(CHFAC,'(I2)',ERR=990)IFAC
      IF (CHMOD .EQ. 'B') IMOD = 1
      IF (CHMOD .EQ. 'A') IMOD = 2
      IF (CHLAY .EQ. 'I') ILAY = 1
      IF (CHLAY .EQ. 'O') ILAY = 2
C
      IF (ILAY .GT. 0 .AND. IMOD .GT. 0) THEN
        IF (IFAC .GT. 0 .AND. IFAC .LE. NFACEL(ILAY)) VITEXI=VDOK
      ENDIF
C
      RETURN
C ----------------------------------------------------------------------
 990  CONTINUE
      RETURN
      END
#endif
