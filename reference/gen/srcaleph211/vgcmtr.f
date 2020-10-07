      SUBROUTINE VGCMTR (TE1,TE2,TECM)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Combine two transformation structures
C - Francesco Forti, 17 August 1990
C
C   Combines two transformation structures to give a single
C   transformation.  TE1 is applied FIRST and TE2 is applied
C   afterwards.  Calling A the rotation matrix and T the translation
C   vector, the first transformation is XP = A*X + T. The second is
C   XPP = B*XP + S where TE1=(A,T) and TE2=(B,S).  The complete
C   transformation will be XPP = B*(A*X+T) + S = B*A*X + (B*T + S),
C   i.e., TECM = (B*A,B*T+S).
C
C - Input:
C   TE1(33)  / R  First transformation structure
C   TE2(33)  / R  Second transformation structure
C
C - Output:
C   TECM(33) / R  Output transformation structure
C
C   Libraries required: CERNLIBS
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
      INTEGER VDERR, VDOK
      PARAMETER (VDERR = -1)
      PARAMETER (VDOK  = 1)
C
C     Labels for views:
C
      INTEGER VVIEWZ, VVIEWP
      PARAMETER (VVIEWZ = 1)
      PARAMETER (VVIEWP = 2)
C
C     Fixed VDET geometry parameters:
C
      INTEGER NVLAYR, NVMODF, NVVIEW, NPROMM, IROMAX
      PARAMETER (NVLAYR = 2)
      PARAMETER (NVMODF = 2)
      PARAMETER (NVVIEW = 2)
      PARAMETER (NPROMM = 1)
      PARAMETER (IROMAX = 4)
C
C     Array dimensions:
C
      INTEGER NVWMMX, NVWFMX, NVFLMX, NVFMAX, NVMMAX, NVWMAX
      INTEGER NVZRMX, NVPRMX
      PARAMETER (NVWMMX = 3)
      PARAMETER (NVWFMX = NVWMMX*NVMODF)
      PARAMETER (NVFLMX = 15)
      PARAMETER (NVFMAX = 24)
      PARAMETER (NVMMAX = NVFMAX*NVMODF)
      PARAMETER (NVWMAX = NVFMAX*NVWFMX)
      PARAMETER (NVZRMX = NVFMAX*IROMAX)
      PARAMETER (NVPRMX = NVMMAX*NPROMM)
C
C! Parameters used in the alignment geometrical package
      INTEGER LVEMLN, LVTFOR, LVTEXP, JVTFTR, JVTFRO, JVTFEM,
     &        JVTETR, JVTERO, JVTEEM
      PARAMETER( JVTFTR=1, JVTFRO=4, JVTFEM=7,
     &           JVTETR=1, JVTERO=4, JVTEEM=13,
     &           LVEMLN=21, LVTFOR=LVEMLN+JVTFEM-1,
     &           LVTEXP=LVEMLN+JVTEEM-1 )
C
C     (Other dimension parameters are defined in VGLOBL.)
C
      REAL VTEXPD, VTEXPI, RFDIST
      COMMON /VGPAAL/ VTEXPD(LVTEXP,NVFLMX,NVWFMX,NVLAYR),
     &                VTEXPI(LVTEXP,NVFLMX,NVWFMX,NVLAYR),
     &                RFDIST(NVFLMX,NVWFMX,NVLAYR)
      REAL TE1(*), TE2(*), TECM(*)
      REAL DUMMY
C! Definition of variables used in VGMACR.
      INTEGER III, JJJ, KVINDX
      REAL VDOT
      REAL VFXGWC, VFNLVV, VFNLWW, VFNLUU
      INTEGER IIXYZ, IIWFF, IIFAC, IILAY
C! macros to be used in geometry package.
C
C Matrix to array index for the rotation matrix
      KVINDX(III,JJJ) = JVTERO + (JJJ-1)*3 + III-1
C
C Geometrical quantities for track extrapolation
C VFXGWC : Gives the position in xyz of the wafer center
      VFXGWC(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       JVTETR+IIXYZ-1 , IIFAC, IIWFF, IILAY )
C VFNLxx : Gives the three xyz coordinates of basis vector in the local
C          coordinate. xx can be VV, UU, or WW for the three vectors.
      VFNLVV(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,1), IIFAC, IIWFF, IILAY )
      VFNLUU(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,2), IIFAC, IIWFF, IILAY )
      VFNLWW(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,3), IIFAC, IIWFF, IILAY )
C ----------------------------------------------------------------------
C
C     Initialize TECM = TE2:
C
      CALL UCOPY(TE2, TECM, LVTEXP)
C
C     Matrix multiplication (see CERNLIB F004):
C
      CALL RMMLT( 3,3,3,
     &  TE2(KVINDX(1,1)),TE2(KVINDX(1,2)),TE2(KVINDX(2,1)),
     &  TE1(KVINDX(1,1)),TE1(KVINDX(1,2)),TE1(KVINDX(2,1)),
     &  TECM(KVINDX(1,1)),TECM(KVINDX(1,2)),TECM(KVINDX(2,1)),
     &  DUMMY)
C
C     Calculate the new translation vector.
C     The translation vector in TECM is currently .EQ. S
C
      CALL RMMPA( 3,3,
     &  TE2(KVINDX(1,1)),TE2(KVINDX(1,2)),TE2(KVINDX(2,1)),
     &  TE1(JVTETR), TE1(JVTETR+1),
     &  TECM(JVTETR), TECM(JVTETR+1)  )
C
      RETURN
      END
