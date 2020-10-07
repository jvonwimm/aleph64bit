      SUBROUTINE ETRDEV ( ICL , IELEM )
C----------------------------------------------------------------------
C!  - Test transverse shower developpement
C-     Author  : M.N. Minard  M.Pepe         29/08/89
C      Input     ICL  cluster row
C      Output    IELEM = 0 , electromagnetic transverse shape
C                      = 1 , non electromagnetic shape
C      Banks used ECLU , ESDA
C?
C!======================================================================
#ifndef DOC
#include "esdajj.h"
#include "eclujj.h"
#include "bcs.h"
      DIMENSION COSI (4) , ECLU (4)
      REAL VV
      DATA DANGMX , ECLIM / 0.060, 0.85 /
#include "bmacro.h"
      NAECLU = NAMIND ('ECLU')
      KECLU = IW (NAECLU)
      JECLU = KROW(KECLU,ICL)
      NAESDA = NAMIND ('ESDA')
      KESDA = IW(NAESDA)
      IELEM = 1
      IF ( KESDA .EQ.0.OR.KECLU.EQ.0) GO TO 990
C
C-    Store energy vector of cluster
C
      ECLU (3) = RW(JECLU+JECLE4)*COS(RW(JECLU+JECLT4 ))
      ET        = RW(JECLU+JECLE4)*SIN(RW(JECLU+JECLT4))
      ECLU (1) = ET *COS (RW(JECLU+JECLF4))
      ECLU (2) = ET *SIN (RW(JECLU+JECLF4))
      ECLU (4) = RW(JECLU+JECLE4)
      IESDA = IW(JECLU+JECLES)
C
C-    Look if cluster is electromagnetic
C
C
C-    Loop on storeys in cluster
C
      ECENT = 0.
 50   JESDA = KROW(KESDA,IESDA)
      ITOW = IW(JESDA+JESDIO)
      IK = IW(JESDA+JESDDK)
      ERES = RW(JESDA+JESDME)
      CALL ESRBC('ALEPH',IW(JESDA+JESDTJ),IW(JESDA+JESDFI),IK,COSI)
      DR = 0.
      DO 55 I=1,3
       DR = DR + COSI(I)**2
 55   CONTINUE
      DR = SQRT (DR)
      COSI(4) = DR
C
C-    Calculate angle / cluster barycenter
C
      VV=VDOTN(COSI(1),ECLU(1),3)
      IF(ABS(VV).GT.1.) VV=SIGN(1.,VV)
      DANG = ACOS ( VV)
      IF (ABS(DANG) .LT. DANGMX ) ECENT = ECENT + ERES
 70   IESDA = IW( JESDA+JESDES )
      IF ( IESDA.NE.0 ) GO TO 50
C
C-    Check if energy concentration match with eleectromagnetic
C
      FRAC = ECENT / ECLU(4)
      IF ( FRAC .GE. ECLIM ) IELEM = 0
 990  CONTINUE
      RETURN
      END
#endif
