      SUBROUTINE NVPCOB(IPCOB, ILINE, NLINE, NMAX, IERR)
C ----------------------------------------------------------------
CKEY PTOJ PCOB PCPA / INTERNAL
C     Author: J.Carr               Date : 20- 3-91
C!    Navigate from PCOB to PCPA
C     PCOB <- PCHY <- PCPA
C     IPCOB : number of the row in bank PCOB
C     ILINE (NLINE) : list of the rows corresponding to IPCOB in bank PC
C     IERR  =   0 : OK
C     IERR  =  98 : no solution
C     IERR  = -99 : NLINE > NMAX
C     IERR  = - 1 : error on banks
C -----------------------------------------------------------------------
#ifndef DOC
      LOGICAL FIRST
      SAVE FIRST, MPCOB, MPCHY, MPCPA
      DATA FIRST /.TRUE./
C     Common defining the names of the columns in Fortran
#include "pcobjj.h"
#include "pchyjj.h"
#include "pcpajj.h"
C
      DIMENSION ILINE (*)
#include "bcs.h"
CD BMACRO  reduced set of intrinsic functions to handle BOS banks
      LROWS(ID)         = IW(ID+2)
      KROW (ID,NRBOS)   = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C ----------------------------------------------------------------------
C     Initializations
C     Subroutine of type NAMIND
      IF (FIRST) THEN
         FIRST = .FALSE.
         MPCOB = NAMIND ('PCOB')
         MPCHY = NAMIND ('PCHY')
         MPCPA = NAMIND ('PCPA')
      END IF
      IERR  =  98
      NLINE = 0
C     Look for banks
      KPCOB  = IW (MPCOB)
      KPCHY  = IW (MPCHY)
      KPCPA  = IW (MPCPA)
      IF(KPCOB.EQ.0.OR.KPCHY.EQ.0.OR.KPCPA.EQ.0) THEN
         IERR = -  1
         GO TO 999
      ENDIF
      LPCOB = LROWS (KPCOB)
      LPCHY = LROWS (KPCHY)
      LPCPA = LROWS (KPCPA)
C     *** Reverse path from PCOB to PCHY
      DO 10, JPCHY = 1, LPCHY
         JCOUR = ITABL (KPCHY ,JPCHY, JPCHPC)
         IF (IPCOB .EQ. JCOUR) THEN
            IPCHY = JPCHY
C           *** Reverse path from PCHY to PCPA
            DO 11, JPCPA = 1, LPCPA
               JCOUR = ITABL (KPCPA ,JPCPA, JPCPPC)
               IF (IPCHY .EQ. JCOUR) THEN
                  IPCPA = JPCPA
C                 Suppression of multiple paths
                  DO 5, J = 1, NLINE
                     IF(IPCPA.EQ.ILINE(J))GOTO 7
    5                CONTINUE
                  IF(NLINE.EQ.NMAX) THEN
                       IERR = -99
                       GOTO 999
                  ENDIF
                  NLINE = NLINE + 1
                  ILINE (NLINE) = IPCPA
                  IERR = 0
    7             CONTINUE
               END IF
   11          CONTINUE
         END IF
   10    CONTINUE
  999 RETURN
C Errors
      END
#endif
