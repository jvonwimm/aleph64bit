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
#if defined(DOC)
   LVEMLN = length of the error matrix on the transformation params.
   LVTFOR = total length of the transformation vector in normal form
   LVTEXP = total length of the transformation vector in expanded form
   The Jxxyy parameters are offsets in the transformation vector with
   xx  = TF -> refer to the normal form (TFORM) as stored in the banks.
   xx  = TE -> refer to the expanded form (TEXPD) as stored in the
               common blocks.
   yy  = TR -> translation vector
   yy  = RO -> rotation : three angles for TFORM,
                          3x3 matrix for TEXPD. The matrix is stored
                          as a FORTRAN matrix (1st index is fastest).
                          The inline function KVINDX is available in
                          VGMACR to access elements of the matrix.
   yy  = EM -> Error matrix.
  VTEXPD(1,IFAC,IWFF,ILAY) : Direct transformation for a given wafer,
              in the sense that brings the point in the wafer coords
              vuw to the aleph main coords xyz.
  VTEXPI(1,IFAC,IWFF,ILAY) : Inverse of VTEXPD. Brings a point in xyz
              coordinates to the wafer local coordinate system.
  RFDIST(IFAC,IWFF,ILAY) : Distance from wafer center to the origin
#endif
