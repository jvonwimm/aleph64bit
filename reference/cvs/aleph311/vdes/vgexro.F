      SUBROUTINE VGEXRO(ITFY,TF,TEXP)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Expand transformation structure from DAF into a full matrix
C - Francesco Forti, 18 August 1990.
C
C   The output form is the same as the input but has the rotation
C   matrix explicitily calculated.
C
C   Here is the structure of the two types of transformation forms:
C
C    TFORM =   (bank VAGB or VALC)
C        (TRanslat(3)  = Length [-1.,1.]   : 'Global transl. vector',
C         ROtation(3)  = Angle  [-4.,4.]   : 'Global rotat. angles'
C         EcovarM(21)  = REAL   [*,*]      : 'Triangular covariance
C                                             matrix')
C
C    TEXPD =    ( COMMON /VGPAAL/ )
C        (TRanslat(3)  = Length [-1.,1.]   : 'Total  transl. vector',
C         ROtation(9)  = Angle  [-4.,4.]   : 'Total  rotat. matrix'
C         EcovarM(21)  = REAL   [*,*]      : 'Triangular covariance
C                                             matrix')
C
C - Input:
C   TF(27)   / R  Transformation structure
C   ITFY     / I  Flags the transformation TF to be local or global:
C                   1 -> global transformation
C                   2 -> local tranformation
C                 In this way it is possible to have different
C                 conventions for global and local transformations.
C
C - Output:
C   TEXP(33) / R  Same transformation in expanded form TEXP
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgpaal.h"
      REAL TF(*), TEXP(*)
      INTEGER I,ITFY
      DOUBLE PRECISION PSI, THETA, PHI, C1, C2, C3, S1, S2, S3
      DOUBLE PRECISION PHIV, PHIU, PHIW, CV, CU, CW, SV, SU, SW
#include "vgvmcr.h"
#include "vgmacr.h"
C ----------------------------------------------------------------------
C
C     Copy translation vector:
C
      DO I=0,2
        TEXP(JVTETR+I) = TF(JVTFTR+I)
      ENDDO
C
C     Copy error matrix:
C
      DO I=0,LVEMLN-1
        TEXP(JVTEEM+I) = TF(JVTFEM+I)
      ENDDO
C
C     Rotation matrix construction:
C
      IF (ITFY .EQ. 1) THEN
C
C     Global transformation:
C
        PSI   = TF(JVTFRO+0)
        THETA = TF(JVTFRO+1)
        PHI   = TF(JVTFRO+2)
        C1 = COS(PSI)
        C2 = COS(THETA)
        C3 = COS(PHI+PSI)
        S1 = SIN(PSI)
        S2 = SIN(THETA)
        S3 = SIN(PHI+PSI)
C
C Lauber-Brownd convention.
C
        TEXP( KVINDX(1,1)) =   C3*C2*C1 + S3*S1
        TEXP( KVINDX(1,2)) =   C3*S1*C2 - C1*S3
        TEXP( KVINDX(1,3)) = - S2*C3
        TEXP( KVINDX(2,1)) =   S3*C2*C1 - S1*C3
        TEXP( KVINDX(2,2)) =   S3*S1*C2 + C3*C1
        TEXP( KVINDX(2,3)) = - S3*S2
        TEXP( KVINDX(3,1)) =   S2*C1
        TEXP( KVINDX(3,2)) =   S1*S2
        TEXP( KVINDX(3,3)) =   C2
C
      ELSEIF (ITFY .EQ. 2) THEN
C
C       Local alignment has a different convention,
C       three rotations around vuw axes in the order:
C                          w, u, v.
C       The angles are stored in the "standard" order vuw.
C
        PHIV = TF(JVTFRO+0)
        PHIU = TF(JVTFRO+1)
        PHIW = TF(JVTFRO+2)
        CV = COS(PHIV)
        CU = COS(PHIU)
        CW = COS(PHIW)
        SV = SIN(PHIV)
        SU = SIN(PHIU)
        SW = SIN(PHIW)
        TEXP( KVINDX(1,1)) =   CU*CW
        TEXP( KVINDX(1,2)) = - CU*SW
        TEXP( KVINDX(1,3)) =   SU
        TEXP( KVINDX(2,1)) =   CV*SW + SV*SU*CW
        TEXP( KVINDX(2,2)) =   CV*CW - SV*SU*SW
        TEXP( KVINDX(2,3)) = - SV*CU
        TEXP( KVINDX(3,1)) =   SV*SW - CV*SU*CW
        TEXP( KVINDX(3,2)) =   SV*CW + CV*SU*SW
        TEXP( KVINDX(3,3)) =   CV*CU
      ENDIF
C
      RETURN
      END
#endif
