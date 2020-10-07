*DK agsatt
      SUBROUTINE AGSATT
C-----------------------------------------------------------------------
C  B. BLOCH-DEVAUX - 850910
CKEY GEOM DRAW
C! set GEANT drawing attributes
C Called by  AGEOME
C Calls GSATT                 from GEANT3
C
C ---------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "alfgeo.h"
#include "gcdraw.h"
#include "gckine.h"
C ----------------------------------------------------------------------
C  Set user defaults
C
      IGKINE = 1
C
C Computes initial scaling factor according to fiducial volume
C to allow space for a header
C
      SCU=8.5/ALRMAX
      SCV=9.8/ALZMAX
      GSCU=MIN(SCU,SCV)
      GSCV=GSCU
C
C    Set artificial volumes to 'unseen'
C
      CALL GSATT('ALEF','SEEN',0)
C
      IF (IGEOJO(1)+IGEOJO(2)+IGEOJO(3).GT.0)
     1      CALL GSATT('CDET','SEEN',0)
      IF (IGEOJO(3).GT.0) THEN
         CALL GSATT('TPCR','SEEN',0)
         CALL GSATT('TPC ','SEEN',0)
      ENDIF
      IF (IGEOJO(4).GT.0) THEN
         CALL GSATT('ECBL','SEEN',0)
         CALL GSATT('EBAR','SEEN',0)
         CALL GSATT('EBAL','SEEN',0)
         CALL GSATT('ECMA','SEEN',0)
         CALL GSATT('ECMB','SEEN',0)
         CALL GSATT('ECPA','SEEN',0)
         CALL GSATT('ECPB','SEEN',0)
      ENDIF
      IF (IGEOJO(4)+IGEOJO(5)+IGEOJO(6).GT.0) THEN
         CALL GSATT('ECEA','SEEN',0)
         CALL GSATT('ECEB','SEEN',0)
      ENDIF
      IF (IGEOJO(5).GT.0) THEN
         CALL GSATT('LCEA','SEEN',0)
         CALL GSATT('LCEB','SEEN',0)
      ENDIF
      IF (IGEOJO(6).EQ.1) THEN
         CALL GSATT('SATC','SEEN',0)
         CALL GSATT('SALY','SEEN',0)
         CALL GSATT('SACY','SEEN',0)
         CALL GSATT('SATA','SEEN',0)
         CALL GSATT('SATB','SEEN',0)
         CALL GSATT('SACT','SEEN',0)
         CALL GSATT('SASP','SEEN',0)
         CALL GSATT('SASU','SEEN',0)
      ENDIF
      IF (IGEOJO(13).GT.0) THEN
         CALL GSATT('COIL','SEEN',0)
         CALL GSATT('COIN','SEEN',0)
         CALL GSATT('COUT','SEEN',0)
         CALL GSATT('COEN','SEEN',0)
         CALL GSATT('COBY','SEEN',0)
      ENDIF
      IF (IGEOJO(11).GT.0) THEN
         CALL GSATT('QUEA','SEEN',0)
         CALL GSATT('QUEB','SEEN',0)
      ENDIF
      IF (IGEOJO(7).GT.0)  THEN
         CALL GSATT('HCBL','SEEN',0)
         CALL GSATT('HCEA','SEEN',0)
         CALL GSATT('HCEB','SEEN',0)
         CALL GSATT('HBAR','SEEN',0)
         CALL GSATT('HBAL','SEEN',0)
         CALL GSATT('HCMA','SEEN',0)
         CALL GSATT('HCMB','SEEN',0)
         CALL GSATT('HCPA','SEEN',0)
         CALL GSATT('HCPB','SEEN',0)
      ENDIF
      IF (IGEOJO(8).GT.0)  THEN
         CALL GSATT('MUON','SEEN',0)
         CALL GSATT('MUEA','SEEN',0)
         CALL GSATT('MUEB','SEEN',0)
         CALL GSATT('MMIA','SEEN',0)
         CALL GSATT('MMIB','SEEN',0)
         CALL GSATT('MMOA','SEEN',0)
         CALL GSATT('MMOB','SEEN',0)
         CALL GSATT('MCIA','SEEN',0)
         CALL GSATT('MCIB','SEEN',0)
         CALL GSATT('MMBA','SEEN',0)
         CALL GSATT('MMBB','SEEN',0)
         CALL GSATT('MCOA','SEEN',0)
         CALL GSATT('MCOB','SEEN',0)
         CALL GSATT('MUBO','SEEN',0)
      ENDIF
      IF (IGEOJO(12).GT.0) THEN
         CALL GSATT('EPBA','SEEN',0)
         CALL GSATT('EPBB','SEEN',0)
      ENDIF
      RETURN
      END
#endif
