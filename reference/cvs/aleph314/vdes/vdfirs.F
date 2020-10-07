      INTEGER FUNCTION VDFIRS
     >     (XCOOR,SIGMA,IVIEW,ISTF,NSF,ESTRI,MXSTR,IER)
C ----------------------------------------------------------------------
CKEY VDETDES STRIP / USER
C!  Compute the distribution of charge on the physical strips
C - Alain Bonissent, Joe Rothberg, and Manoj Thulasidas, 1994
C
C   Computes the distribution of charge into the physical strips, given
C   the position and sigma of a gaussian charge distribution.  Only
C   strips which lie within +/- 3 sigma receive charge.
C
C   It is assumed that strips are numbered starting from the minimum
C   end of the local (abc) coordinate system.
C
C - Input:
C   XCOOR  / R  Position of the center of the electron cloud
C               (local coords a or b, cm)
C   SIGMA  / R  Sigma of the charge distribution
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   MXSTR  / I  Dimension of ESTRI in the calling routine
C
C - Output:
C   VDFIRS / I  = VDOK if successful
C               = VDERR if an error occurred
C   ISTF   / I  Address of the first fired strip
C   NSF    / I  Number of fired strips
C   ESTRI  / R  Array, for each strip the fraction of charge released
C   IER    / I  Return code: 0 = OK
C                            1 = no strips
C                            2 = more than MXSTR strips
C                            3 = IVIEW is out of range
C                            4 = SIGMA is too small
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
C
      INTEGER IVIEW,ISTF,NSF,IER
      INTEGER MXSTR
      REAL XCOOR(*)
      REAL SIGMA
      REAL ESTRI(*)
C
C  functions:
C
C     FREQ      CERNLIB error function
      REAL FREQ
C
C     Local variables
C
C     PITCH       pitch
C     NSTRP       number of strips
C     ALEDG       distance from low edge of active region to center
C     GCEDG       dist from gaussian center to low edge active region
C     LSTRP       current strip number
C     ISTL        last fired strip
C     XLO, XHI    lower and upper interval coordinates in coordinate
C                 system of the Gaussian charge distribution
C     QPREV, QHI  Value of ERF at low and high end of strip region
C     S           scale factor in arg of ERF
C     EPS         minimum sigma allowed
C
      REAL EPS
      PARAMETER (EPS = 0.00001)
C
      REAL PITCH, ALEDG, GCEDG
      REAL XLO, XHI
      REAL QPREV, QHI
      INTEGER NSTRP, LSTRP, ISTL
C
C ----------------------------------------------------------------------
C check validity of arguments
C
      VDFIRS = VDERR
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW. NE. VVIEWP)) THEN
        IER = 2
C
      ELSEIF (SIGMA .LT. EPS) THEN
        IER = 3
C
      ELSE
C
C ----------------------------------------------------------------------
        IF (IVIEW.EQ.VVIEWZ) THEN
C   z-side
          PITCH = STPITZ
          NSTRP = NZSTRP
          ALEDG = AMNSRZ
        ELSE
C   rphi-side
          PITCH = STPITP
          NSTRP = NPSTRP
          ALEDG = BMNSRP
C
        ENDIF
C ----------------------------------------------------------------------
C  gaussian center to edge of active region
        GCEDG = XCOOR(IVIEW) - ALEDG
C
C  find first and last fired strip
        ISTF = MAX(IFIX((GCEDG - 3.*SIGMA)/PITCH)+1,1)
        ISTL = MIN(IFIX((GCEDG + 3.*SIGMA)/PITCH)+1,NSTRP)
C
C   number of fired strips
        NSF = (ISTL-ISTF)+1
C
        IF (ISTF.GT.NSTRP.OR.ISTL.LT.1) THEN
C ----------------------------------------------------------------------
C    the charge distribution is outside active region of wafer
C
          NSF = 0
          IER = 1
C ----------------------------------------------------------------------
        ELSE IF (NSF.GT.MXSTR) THEN
          IER = 2
        ELSE
C
C charge around strip LSTRP is accumulated within
C a range of +/- 0.5*pitch. transform the upper and lower
C bounds into coordinates relative to gaussian center. Scale to use ERF
C
C            XLO = (ISTF-1)*PITCH - GCEDG
          XLO = (ISTF+NSF-1)*PITCH - GCEDG
          XLO=-XLO
          IF (XLO/SIGMA.GT.3.)THEN
            QPREV=1.
          ELSEIF (XLO/SIGMA.LT.-3.)THEN
            QPREV=0.
          ELSE
            QPREV = FREQ(XLO/SIGMA)
          ENDIF
C
C            DO LSTRP = 1, NSF
          DO LSTRP = NSF,1,-1
C               XHI = XLO+LSTRP*PITCH
            XHI = (ISTF+LSTRP-2)*PITCH - GCEDG
            XHI=-XHI
            IF (XHI/SIGMA.GT.3.)THEN
              QHI=1.
            ELSEIF (XHI/SIGMA.LT.-3.)THEN
              QHI=0.
            ELSE
              QHI = FREQ(XHI/SIGMA)
            ENDIF
            ESTRI(LSTRP) = (QHI-QPREV)
            QPREV = QHI
          ENDDO
          IER = 0
          VDFIRS = VDOK
C ----------------------------------------------------------------------
C  valid number of strips
        ENDIF
C ----------------------------------------------------------------------
C  valid input arguments
      ENDIF
C ----------------------------------------------------------------------
      RETURN
      END
#endif
