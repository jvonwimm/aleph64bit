*DK catino
        SUBROUTINE CATINO
C.----------------------------------------------------------------
C J.Boucrot   March 87    F.Ranjard - 870815
C                                     880316
C! Create Geantino Part type LTYPEL or next (48-52)
C  Called by ECHIT, HCHIT, CALHIT                from this .HLB
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "capano.h"
#include "trkcom.h"
      REAL HPANTP
#include "gcking.h"
C ---------------------------------------------------------
C Build the "shower particle" which will have :
C        - GEANT particle code = LTYPEL/LTYPME/LTYPBA/LTYPAN/LTYPGA
C        - GEANT tracking type =  6 ( no interactions )
C
C     select geantino type  electron / photon / hadron
      IF (ITRKEL(11).EQ.2) THEN
         JTYP = LTYPEL
      ELSE IF (ITRKEL(11).EQ.1) THEN
         JTYP = LTYPGA
      ELSE IF (ITRKEL(11).EQ.4) THEN
         IBAR = NINT(HPANTP (ITRKEL(4)))
         IF (IBAR .EQ. -1) THEN
            JTYP = LTYPBA
         ELSE IF (IBAR .EQ. 0) THEN
            JTYP = LTYPME
         ELSE IF (IBAR .EQ. 1) THEN
            JTYP = LTYPAN
         ENDIF
      ELSE
         WRITE (LOUTIO,*)' +++CATINO+++ this tracking type is ',
     &    'not considered - RETURN', ITRKEL
         RETURN
      ENDIF
C
C set shower interaction mechanism
      KGCASE = JHOCHA ('SHOW')
      NGKINE = 1
C
      DO I=1,3
        GKIN(I,1) = TRKELE(7)*TRKNXT(I+3)
        GPOS(I,1) = TRKNXT(I)
      ENDDO
      GKIN(4,1) = TRKELE(8)
      GKIN(5,1) = REAL(JTYP)
      GTOFD(1) = 0.
C
   10 RETURN
      END
#endif
