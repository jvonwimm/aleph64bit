*DK astdeb
      SUBROUTINE ASTDEB
C ---------------------------------------------------------
C - F.Ranjard - 870813
C! debug after a tracking step
C  this routine is called by GUSTEP after every step when
C  FGALJO is true to perform various tasks such as:
C  - IMPAct banks if required
C  - large debug printout if required
C  - specific output to retreive energy deposited in ALEPH
C - called by GUSTEP                         from this .HLB
C - calls     ASTIMP                         from this .HLB
C             GPCXYZ, GSXYZ, GDCXYZ          from GEANT3 lib
C ---------------------------------------------
#ifndef DOC
      SAVE
#include "agcons.h"
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "trkcom.h"
#include "gcking.h"
      LOGICAL FTPC,FLAST
#include "gmacro.h"
C
C ----------------------- GALEPH IMPA banks
      IF (NAGIMP.GT.0) THEN
C
C - IF it is a primary track THEN
C      IF it is the entrance point in a detector but TPC gas OR
C         it is the exit point of a charged part. from the TPC gas THEN
C         store the track parameters in bank IMPA, NR=IGTRA
         FTPC  = IGSVOL.GT.0 .AND. IGSVOL.EQ.IDETJO(3)
         FLAST = IGNWVO.EQ.2
         IF (IGSTAK .EQ. 0) THEN
            IF ( (FIRST .AND. .NOT.FTPC) .OR.
     &           (FLAST .AND. FCHRG .AND. FTPC) ) THEN
               CALL ASTIMP
            ENDIF
         ENDIF
C
      ENDIF
C ----------------------- end GALEPH IMPA
C
C - debug
      IF(FDEBJO.AND.IPRIJO(17).EQ.1) THEN
       CALL GPCXYZ
       IF(NGKINE.GT.0) THEN
        WRITE(LOUTIO,801)NGLEVE,(CHAHOL(NGAMES(I)),NGUMBR(I),I=1,NGLEVE)
        WRITE(LOUTIO,802) CHAHOL(KGCASE),NGKINE,
     &                   ((GKIN(J,I),J=1,5),I=1,NGKINE)
       ENDIF
      ENDIF
 801  FORMAT(/1X,'+++ASTDEB+++ ',I2,10(2X,A4,I3))
 802  FORMAT(14X,A4,3X,I3,:,5F10.4,:/(24X,5F10.4))
C
C - If display of the event is required then
C - Save 1st and last point of the track + points when leaving a volume
C - or points every 20cm about
      IF(FDISJO) THEN
         IF(GSLENG.EQ.0. .OR. IGSTOP.NE.0 .OR. IGNWVO.EQ.2
     &      .OR. MOD(GSLENG,20.).EQ.0.) CALL GSXYZ
      ENDIF
C
#if defined(DRAW)
C - Online display of the track trajectory if required
      IF(FDISJO.AND.IPRIJO(18).EQ.1) THEN
         CALL GDCXYZ
      ENDIF
#endif
      END
#endif
