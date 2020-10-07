C! transform from ARS system to local system indices
C                  x y z      ----------->  IMD IST IPH IRD
      JSINT = IW(NASINT)
      IF ( IW(JSINT).LE.0 ) GO TO 999
C Get STACK   bin
      IOK = -2
      IST = INT((Z+EPS-Z0SNSI(IMD)-DPOSSI(3,IMD))/ZWIDSI)+1
      IF ( IST.LE.0 .OR. IST.GT.NZBNSI) GO TO 999
      IROW = ISINUM(IST,IMD)
      DXT= RTABL(JSINT,IROW,JSINTP)
      DYT= RTABL(JSINT,IROW,JSINTP+1) - YTDEF
      X = X+DXT-DPOSSI(1,IMD)
      Y = Y+DYT-DPOSSI(2,IMD)
C Check for gap between halves of Calo
      X0 = ABS(X)
      ISIDE = 1
      IF ( X.LT.0. ) ISIDE =-1
      X = X - ISIDE * GAPXSI(IMD)
      RAD = SQRT(X*X+Y*Y)
C Get PHI bin
      IOK = -3
      PHI = ATG(Y,X)
      ITP = MOD(IST-1,3)+1
      PHIT= PHI-PHSHFT(ITP,IMD)
      IF (PHIT.LT.0.) PHIT= PHIT+TWOPI
      IF (PHIT.GT.TWOPI) PHIT= PHIT-TWOPI
      IPH = INT(PHIT/PHISTP)+1
      IF ( IPH.LE.0 .OR. IPH.GT.NPBNSI) GO TO 999
C GET RADIUS  bin
      IOK = -1
      IXTAL = (IPH-1)/2 +1
      DR = RTABL(JSINT,IROW,JSINXR+IXTAL-1) - RDEF
      RADP =  RAD-RMINSI(IMD)-DR
      IF ( RADP.LT.0.) GO TO 999
      IRD = INT(RADP/RADSTP)+1
      IF ( IRD.LE.0 .OR. IRD.GT.NRBNSI) GO TO 999
