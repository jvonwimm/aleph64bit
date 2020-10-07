C!  BOM calibration constants
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
#if defined(DOC)
C
C   DSCALE  Scale factor used for storing data in BOMR bank
C   CSCALE  Scale factor used for storing currents in BOMC bank
C   PCALMM  Calibration constant for position at BOM
C   XIPOFF  Offsets for beam position and angle at IP
C   GAINA0  Beam calibration parameter: constant term
C   GAINA1  Beam calibration parameter: linear term
C   GAINA2  Beam calibration parameter: quadratic term
C   CURCOR  Current of corrector magnet (Amps)
C   CALCOR  Corrector magnet calibration (mrad/A at 20 GeV)
C   DEFCOR  Deflection induced by corrector magnet (mrad)
C   IADCOR  Corrector magnet address
C   IXYCOR  Corrector magnet type: 1 = x, 2 = y
C
#endif
