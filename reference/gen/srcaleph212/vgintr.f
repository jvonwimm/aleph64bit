      SUBROUTINE VGINTR (TE,TEINV,IFAIL)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Invert a transformation structure
C - Francesco Forti, 17 August 1990
C
C   Inverts a transformation structure. The direct transformation is
C   defined as XP = A*X + T.  The inverse will be (B = A**-1)
C   X = B*(XP-T) = B*XP - B*T.
C   If the matrix A is singular, the IFAIL flag will be negative and
C   the TEINV structure will be undefined.
C
C - Input:
C   TE(33)    / R  Transformation structure
C
C - Output:
C   TEINV(33) / R  Output transformation structure
C   IFAIL     / I  =0 if OK;
C                  =-1 if singular matrix
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
      REAL TE(*), TEINV(*)
      REAL WORK(3)
      INTEGER I, IFAIL
C
C ----------------------------------------------------------------------
C
C     Initialize output to form to input form:
C
      CALL UCOPY(TE,TEINV,LVTEXP)
C
C     Proceed with transformation.
C     Change sign of translation:
C
      DO I=0,2
        TEINV(JVTETR+I) = -TEINV(JVTETR+I)
      ENDDO
C
C     Now perform inversion (see CERNLIB F010):
C
      CALL REQINV(3, TEINV(JVTERO), 3, WORK,
     &            IFAIL, 1, TEINV(JVTETR) )
C
      RETURN
      END
