      REAL FUNCTION DSTACK(THESTA,PHISTA,THEST2,PHIST2)
C----------------------------------------------------------------
C  M.Verderi                                             2-10-94
C!  Computes the angular distance expressed at pi/2 between the barycenters
C!  given by (thesta,phista) and (thest2,phist2)
C
C  Called by GHAFAK
C----------------------------------------------------------------
#ifndef DOC
      PARAMETER (COSBMX=0.76,SCALE1 = 1.356756 )
      DATA DEUXPI /6.283185307/
      DSTACK = -10.
      IF (THEST2.EQ.0.) RETURN
      DELTHE = THESTA-THEST2
      DELPHI = PHISTA-PHIST2
      ANGLE = DELPHI
      ANG2PI = ANGLE-DEUXPI*SIGN(1.,ANGLE)
      IF (ABS(ANG2PI).LT.ABS(ANGLE)) THEN
         PHM = ANG2PI
      ELSE
         PHM = ANGLE
      ENDIF
      DELPHI = PHM
      IF (ABS(COS(THEST2)).LT.0.76) THEN
         DELTHE = DELTHE/SIN(THEST2)
         SCALE  = 1.
      ELSE
         DELTHE = DELTHE/(ABS(COS(THEST2)))
         DELPHI = DELPHI*ABS(TAN(THEST2))
         SCALE  = SCALE1
      ENDIF

      DSTACK = SQRT( DELTHE**2+DELPHI**2 )
      DSTACK = SCALE*DSTACK
      RETURN
      END
#endif
