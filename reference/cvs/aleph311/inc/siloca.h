C! transform to ARS system from local system indices
C                  x y z     <-----------   IMD IST IPH IRD
      IROW = ISINUM(IST,IMD)
      IXTAL = (IPH-1)/2 +1
      JSINT = IW(NASINT)
      IF ( JSINT.LE.0 ) GO TO 998
      ISIDE = 1
      IF ( IPH.GE.9 .AND. IPH.LE.24 ) ISIDE = -1
C GET RADIUS in local coordinates
      DR = RTABL(JSINT,IROW,JSINXR+IXTAL-1) - RDEF
      DXT= RTABL(JSINT,IROW,JSINTP)
      DYT= RTABL(JSINT,IROW,JSINTP+1) - YTDEF
      RAD = RMINSI(IMD)+ RADSTP*(IRD-1)+0.5*RADSTP + DR
C GET PHI in local coordinates
      ITP = IOR +1
      PHI = PHISTP*(IPH-1) + PHSHFT(ITP,IMD)+0.5*PHISTP
C GET Z   in local coordinates
      ZED = Z0SNSI(IMD)+ ZWIDSI*(IST-1)
C transform into ARS cartesian system
      X = RAD * COS(PHI) - DXT + DPOSSI(1,IMD) + ISIDE * GAPXSI(IMD)
      Y = RAD * SIN(PHI) - DYT + DPOSSI(2,IMD)
      Z = ZED  + DPOSSI(3,IMD)
      IF (IMD.EQ.2) Z = -Z
