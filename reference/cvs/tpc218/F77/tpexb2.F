*DK tpexb2
      SUBROUTINE TPEXB2(XEX,YWIR,WSTEP,XX1,XX3,TT1,TT3)
C
C! Fast sim : Routine to determine ExB shift of the two points
C  parameterizing a super-broken segment. This routine is
C  used only for the first or the last segment of a given
C  track element.
C
C  Called from:   T2TRAN
C  Calls:         none
C
C
C=======================================================================
C  The results are valid only in the following assumptions:
C
C  Drift Field 110 V/cm
C  Gas Mix     9% CH4 91% Argon
C  SW voltage of about 1300 V
C
C  Grids geometry as in Aleph TPC proposal i.e.
C  sw pitch 0.4 cm, 0grid pitch 0.1 cm, gating grid pitch 0.2 cm.
C  distances of the three grids from the pad plane 0.4,0.8,1.3 cm.
C
C  Magnetic field and gating grid voltages according to ITYP
C
C=======================================================================
C
C  Inputs:   PASSED     : --WSTEP, ditance between two wires in cm
C                         --YWIR, Y coord of the wire
C                         --LDEB, transport debug level
C            FASTER    :  --XPROP,  proportion of collected electrons
C                         --XSHFT,  shift along the wire in cm as
C                                   a function of the distance to
C                                   the wire in Y
C                         --XEX, segment extremities coordinates
C            TPCOND.INC   --CFIELD, magnetic field value
C
C  Outputs:  PASSED    :  --XXX1,XXX3, positions of the two points
C                                      along the wire
C                         --TTT1,TTT3, time of the two points
C=====================================================================
#include "tpcond.h"
#include "faster.h"
      DATA BNOM/15./
      HWIR  = WSTEP/2.
      COEFF = CFIELD/BNOM
C
C  Determine the step number which corresponds to the
C  length of the super broken segment along Y axis (Max=50).
C  In other words, each super broken segment is cut in NY
C  steps in order to determine the mean ExB shift by numeric
C  integration. Note that if the Y-coordinates of the segment
C  extremities are YWIR-WSTEP/2.,YWIR+WSTEP/2. (whole segment)
C  this mean ExB has already been calculated as a function of
C  the segment slope in XY plan. See SUBROUTINE TPEXB3.
C          1st half :
C
      SY1   = SIGN(.5 , XEX(3,2)-XEX(2,2) ) - .49
      NY1   = ( XEX(3,2)-XEX(2,2) )/HWIR*50. + SY1
      IF(XEX(3,2).LT.YWIR .AND. XEX(3,2).GT.XEX(1,2)) NY1 = 50
      IF(XEX(3,2).GT.YWIR .AND. XEX(3,2).LT.XEX(1,2)) NY1 =-50
C
C          2nd half :
C
      SY3   = SIGN(.5 , XEX(2,2)-XEX(1,2) ) - .49
      NY3   = ( XEX(2,2)-XEX(1,2) )/HWIR*50. + SY3
      IF(XEX(1,2).LT.YWIR .AND. XEX(3,2).LT.XEX(1,2)) NY3 =-50
      IF(XEX(1,2).GT.YWIR .AND. XEX(3,2).GT.XEX(1,2)) NY3 = 50
C
C          1st and last step number
C
      MY    = ABS( XEX(2,2) - YWIR )/HWIR*50. + 1.
      NSTEP = MAX(IABS(NY1),IABS(NY3))
      IF(MY.LE.0 .OR. NSTEP.GT.50) THEN
         WRITE(6,100) MY,NSTEP,
     .              ((XEX(I,J),J=1,4),I=1,3),XEX(4,1),XEX(4,2)
         XX1 = XEX(2,1)
         XX3 = XEX(2,1)
         TT1 = 0.
         TT3 = 0.
         RETURN
      ENDIF
C
C  Compute segment slope and origin in XY plan (in term of steps)
C
      AX1   = ( XEX(1,1)-XEX(2,1) )/49.
      BX1   =   XEX(2,1) - AX1
C
      AX3   = ( XEX(3,1)-XEX(2,1) )/49.
      BX3   =   XEX(2,1) - AX3
C
C  Loop over steps
C
      XM1   = 0.
      XX1   = 0.
      TT1   = 0.
      XM3   = 0.
      XX3   = 0.
      TT3   = 0.
      DO 1 IY=MY,NSTEP
C
C      1st half : X1   = X-Shift Value for this step
C                 XM1  = Mean Value
C                 XX1  = Quadratic Mean Value
C                 TT1  = Shift delay
C
        IF ( IY.GT.IABS(NY1) ) GOTO 2
        X1  = AX1*IY + BX1 - XSHFT(IY)*COEFF*SIGN(1.,FLOAT(NY1))
        XM1 = XM1 + X1
        XX1 = XX1 + (X1-XEX(2,1))**2
        TT1 = TT1 + ABS(XSHFT(IY))*COEFF
C
C      2nd half :
C
2       IF ( IY.GT.IABS(NY3) ) GOTO 1
        X3  = AX3*IY + BX3 + XSHFT(IY)*COEFF*SIGN(1.,FLOAT(NY3))
        XM3 = XM3 + X3
        XX3 = XX3 + (X3-XEX(2,1))**2
        TT3 = TT3 + ABS(XSHFT(IY))*COEFF
1     CONTINUE
C
C  Determine the right values of TT1 and XX1, TT3 and XX3.
C
      N1    = IABS(NY1)-MY+1
      IF (N1.GT.0) THEN
        XM1 = XM1/N1
        XX1 = SQRT( XX1/N1 ) * SIGN(1.,XM1-XEX(2,1)) + XEX(2,1)
        TT1 = TT1/N1
      ELSE
        XX1 = XEX(2,1)
      ENDIF
C
      N3    = IABS(NY3)-MY+1
      IF (N3.GT.0) THEN
        XM3 = XM3/N3
        XX3 = SQRT( XX3/N3 ) * SIGN(1.,XM3-XEX(2,1)) + XEX(2,1)
        TT3 = TT3/N3
      ELSE
        XX3 = XEX(2,1)
      ENDIF
C
C  Charge renormalization according to the grid configuration
C
      XEX(4,1) = XEX(4,1)*XPROP
      XEX(4,2) = XEX(4,2)*XPROP
C--------------------------------------------------------------
100      FORMAT(1X,' -- TPEXB2 Warning -- Call expert '/
     .          1X,'1st step (',I5,') <0 or last step (',I5,') >50'/
     .          1X,'beg. coords  : ',4F10.4/
     .          1X,'mid. coords  : ',4F10.4/
     .          1X,'end  coords  : ',4F10.4/
     .          1X,'total charge : ',2F5.0)
C--------------------------------------------------------------
      RETURN
      END
