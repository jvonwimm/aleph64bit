      SUBROUTINE VHERR1(IVIEW,PVEC,PH,VERR)
C ---------------------------------------------------------------------
C! Compute the VDET hit error, based on Monte Carlo
C! returns the intrinsic VDET space resolution
CKEY VDET TRACK
C
C   Based on VHERR by D.Brown - 8-9-94 but with different 
C   error parametrisation
C      Manoj Thulasidas 10-June-1996
C   Modify the error parameterization (due to change in S/N in MC)
C      Manoj Thulasidas 16-July-1997
C
C  INPUT;  IVIEW   = View flag; 1 = W, 2 = U
C          PVEC(3) = track momentum vector at wafer in wafer frame
C          PH      = Pulseheight in normalized units
C OUTPUT;  VERR    = hit error
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  IO variables
C
      INTEGER IVIEW
      REAL PVEC(3),PH,VERR
C
C  Local variables
C
      REAL SINE
      REAL NORM(2), POLY1(2), POLY2(2)
      DATA NORM  /11.2E-04,   5.2E-04/
      DATA POLY1 / 0.0E-04,   0.0E-04/
      DATA POLY2 /33.8E-04,  29.4E-04/
C
C  Compute the sines
C
      IF (IVIEW .EQ. 1) THEN
        SINE = ABS(PVEC(3)) / SQRT(PVEC(1)**2+PVEC(3)**2)
      ELSE
        SINE = ABS(PVEC(2)) / SQRT(PVEC(1)**2+PVEC(2)**2)
      END IF
C
C  Error
C
      VERR = NORM(IVIEW) + POLY1(IVIEW)*SINE + POLY2(IVIEW)*SINE**2

      RETURN
      END
#endif
