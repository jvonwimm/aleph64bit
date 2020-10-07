*DK tpexbs
      SUBROUTINE TPEXBS(ITIP,XFRWIR,YALWIR,IRET)
C-----------------------------------------------------------------------
C!  This subroutine gives the ExB shift along the wire in the
C!  amplification region, parametrized from Montecarlo results
C!  done tracking the electrons in the grid region.
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
C  Magnetic field and gating grid voltages according to ITIP
C
C=======================================================================
C
C  CALLED FROM:  TSTRAN
C  CALLS:        DIVDIF (E105), CERN LIBRARY
C
C  INPUT PARAMETERS :
C
C   ITIP  =  1   B = 0
C   ITIP  =  2   B = 15 KG  gating grid full transparent
C   ITIP  =  3   B = 15 KG  gating grid in DIODE mode
C   ITIP  =  4   B = 15 KG  gating grid in 75% DIODE
C
C   XFRWIR  = difference between the x coordinate of the sense
C             wire where the electron will end and the x of the
C             "undeflected" trajectory of the electron projected
C             on the pad plane ( "undeflected" means without taking
C             into account the fields in the grids region)
C             range : -0.02 cm ,0.02 cm
C
C  OUTPUT PARAMETERS :
C
C   YALWIR  = shift along the sense wire in cm
C
C   IRET    = 0  O.K.
C   IRET    = 1  The electron does not reach the sense wire plane
C                    (ends on the gating grid)
C   IRET    = 10 XFRWIR out of the range
C   IRET    = 11 Wrong ITIP
C                              Gigi Rolandi  Trieste 20/11/84
C
C  M. Mermikides 10-Sep-87.  - For mag. field other than nominal 15 KG,
C                              we can scale the amount of shift
C                              accordingly.
C-----------------------------------------------------------------------
#include "tpcond.h"
       DIMENSION F2(8),X2(8),F3(8),X3(8),F4(8),X4(8)
C
       DATA F2/-0.04,-0.004,0.042,0.134,-0.056,0.018,0.088,0.184/
       DATA X2/0.,0.026,0.082,0.09999,0.10001,0.114,0.184,0.2/
C
       DATA F3/-0.196,-0.05,0.0,0.1,-0.1,-0.024,-0.004,0.0/
       DATA X3/0.0,0.034,0.07,0.08999,0.09001,0.106,0.134,0.15/
       DATA XMAX3/0.15/
C
       DATA F4/-0.156,-0.04,0.01,0.1,-0.08,-0.01,0.03,999999./
       DATA X4/0.0,0.024,0.072,0.08999,0.09001,0.114,0.16,999999./
       DATA XMAX4/0.16/
C
       DATA HAPISW/.2/
       DATA BNOM/15./
C
C   HAPISW IS HALF PITCH OF SENSE GRID
C
       IF(ITIP.LT.1.OR.ITIP.GT.4) THEN
       IRET =11
       RETURN
       ENDIF
C
       IF(XFRWIR.LT.-HAPISW.OR.XFRWIR.GT.HAPISW) THEN
       IRET = 10
       RETURN
       ENDIF
C  If zero field, no shift.
       IRET = 0
       IF (ABS(CFIELD).LT.0.0001) GO TO 10
C
       GO TO (10,20,30,40),ITIP
C
C       ITIP = 1
C
  10   YALWIR = 0.
       RETURN
C
C       ITIP = 2
C
  20   ABX=ABS(XFRWIR)
       YY= DIVDIF(F2,X2,8,ABX,1)
       GO TO 100
C
C       ITIP = 3
C
  30   ABX=ABS(XFRWIR)
       IF( ABX. GT. XMAX3) THEN
       IRET = 1
       RETURN
       ENDIF
       YY=DIVDIF(F3,X3,8,ABX,1)
       GO TO 100
C
C      ITIP = 4
C
  40   ABX=ABS(XFRWIR)
       IF(ABX. GT. XMAX4) THEN
       IRET = 1
       RETURN
       ENDIF
       YY=DIVDIF(F4,X4,7,ABX,1)
       GO TO 100
C
 100  CONTINUE
      YALWIR=-YY*SIGN(1.,XFRWIR)
C Scale y-shift according to magnetic field
      YALWIR = CFIELD*YALWIR/BNOM
C
      RETURN
      END
