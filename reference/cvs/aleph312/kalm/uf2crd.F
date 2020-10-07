      SUBROUTINE UF2CRD(N_COORD, R_IN, RPHI_IN, Z_IN,
     +           RPHI_SIG, Z_SIG, RPHI_Z_COR,
     +           N_PT, R_PT, PHI_PT, Z_PT,
     +           WT_RPHI, WT_Z, COR_RPHI_Z, I_COORD, *)
C! Translate coordinates to internal values and sort by decreasing radius
#ifndef DOC

      REAL R_IN(*), RPHI_IN(*), Z_IN(*)
      REAL RPHI_SIG(*), Z_SIG(*), RPHI_Z_COR(*)
      DOUBLE PRECISION R_PT(*), PHI_PT(*), Z_PT(*)
      DOUBLE PRECISION WT_RPHI(*), WT_Z(*), COR_RPHI_Z(*)
      INTEGER N_COORD, N_PT, I_COORD(*)
#include "vdmsjj.h"
#include "uftcom.h"
#include "bcs.h"
#include "vrldcom.h"

C  Local variables
      PARAMETER (MAX_PTS=40)
      INTEGER SORTARR(MAX_PTS)
      REAL R_SORT(MAX_PTS)
      INTEGER IMATCH(MAX_PTS)
      LOGICAL FIRST
      DATA NVDMS /0/

#include "bmacro.h"

C Link to VDMS bank, if present

      IF (NVDMS.EQ.0) NVDMS = NAMIND('VDMS')

C Check number of coordinates

      IF(N_COORD.GT.MAX_PTS) RETURN 1
      IF(N_COORD.LT.3) RETURN 1

C Include material crossings without coordinates

      CALL UCOPY(R_IN, R_SORT, N_COORD)
      R_SORT(N_COORD+1) = UKRVAC
      R_SORT(N_COORD+2) = UKRITC
      R_SORT(N_COORD+3) = UKRTPC
      N_PT = N_COORD+3

C Identify VD crossings without coordinates and add to list

      KVDMS = IW(NVDMS)
      IF (KVDMS .GT. 0)THEN
        CALL UZERO(IMATCH,1,MAX_PTS)
        DO IC = 1, N_COORD
          IF (R_IN(IC).LT.UKRITC) THEN
            D_VD_MATCH = 1.
            I_VD_MATCH = 0
            DO IVD = 1, LROWS(KVDMS)
              IF (IMATCH(IVD).EQ.0)THEN
                RVD = RTABL(KVDMS,IVD,JVDMRA)
                DVD = ABS(R_IN(IC)-RVD)
                IF (DVD.LE.0.2 .AND. DVD.LT.D_VD_MATCH)THEN
                  D_VD_MATCH = DVD
                  I_VD_MATCH = IVD
                ENDIF
              ENDIF
            ENDDO
            IF (I_VD_MATCH .NE. 0)THEN
              IMATCH(I_VD_MATCH) = 1
            ENDIF
          ENDIF
        ENDDO
        DO IVD = 1, LROWS(KVDMS)
          IF (IMATCH(IVD).EQ.0)THEN
            N_PT = N_PT+1
            R_SORT(N_PT) = RTABL(KVDMS,IVD,JVDMRA)
          ENDIF
        ENDDO
      ENDIF

C Sort in decreasing order

      CALL SORTZV(R_SORT, SORTARR, N_PT, 1, 1, 0)

      DO I = 1, N_PT
        R_PT(I) = DBLE(R_SORT( SORTARR(I) ))
        IF ( R_PT(I) .LE. 0.D0 ) RETURN 1
        IF (SORTARR(I).LE.N_COORD) THEN
          PHI_PT(I) = DBLE(RPHI_IN( SORTARR(I) )) / R_PT(I)
          CALL UBA2PI(PHI_PT(I))
          IF ( PHI_PT(I) .LT. 0.D0 ) RETURN 1
          IF ( PHI_PT(I) .GT. TWO_PI ) RETURN 1
          Z_PT(I)     = DBLE(Z_IN( SORTARR(I) ))
          WT_RPHI(I)     = 1.D0/(RPHI_SIG( SORTARR(I) ) + .00000001D0)
          WT_Z(I)    = 1.D0/(Z_SIG( SORTARR(I) ) + .00000001D0)
          COR_RPHI_Z(I) = RPHI_Z_COR( SORTARR(I) )
          I_COORD(I) = SORTARR(I)
        ELSE
          I_COORD(I) = 0
        ENDIF
      ENDDO

      RETURN
      END

#endif
