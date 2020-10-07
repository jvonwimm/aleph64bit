*DK siirun
      SUBROUTINE SIIRUN
C.----------------------------------------------------------------
C  B.Bloch-Devaux  October 91
C! SCAL run initialisation
C     - Called by ASIMOD
C     - Calls     SIRDAF,SIDFGO                      from this HLB
C                 ALTELL, GTSTUP                     from ALEPHLIB
C.----------------------------------------------------------------
#ifndef DOC
#include "iocom.h"
#include "jobcom.h"
#include "bcs.h"
#include "sinamc.h"
#include "sistat.h"
#include "siparm.h"
      INTEGER GTSTUP,AGETDB
      EXTERNAL GTSTUP,AGETDB
C ----------------------------------------------------------------
      ISITP = GTSTUP ('SI',1)
C
C Define formats and name-indices of SI BOS banks
C
C---BANKS created by GALEPH or on line
      NASIHT = NAMIND('SIHT')
      CALL BKFMT('SIHT','I')
      NASIDI = NAMIND('SIDI')
      CALL BKFMT('SIDI','I')
      NASIX2 = NAMIND('SIX2')
      CALL BKFMT('SIX2','I')
      NASIXA = NAMIND('SIXA')
      CALL BKFMT('SIXA','I')
      NASIFO = NAMIND('SIFO')
      CALL BKFMT('SIFO','I')
      NASIHI = NAMIND('SIHI')
      CALL BKFMT('SIHI','I')
C
C Initialize Statistics for SCAL
C
      DO 10 I = 1,NSIST
         NSICOU(I) = 0
         ESICOU(I) = 0.
 10   CONTINUE
C
C No need for SICAL Geometry if not there
      IF ( ISITP.LE.0) GO TO 900
C---BANKS taken from data base
      JND = AGETDB('SZTHSFTHSRCO',ISITP)
      IF (JND.EQ.0) WRITE(IW(6),990)
C
C get Zero suppression scheme
C
      CALL SIZINI
C
C Define Readout Geometry for SCAL
C
      CALL SIRDAF (LRDBIO,IRUNJO,IERR)
      IF (IERR .NE. 0) THEN
         CALL ALTELL ('SIIRUN: missing data base bank(s)',0,'STOP')
      ENDIF
      IF (IPRIJO(9).GT.0) CALL SIPRGO
C
C Fill some geometry quantities
C
      CALL SIDFGO
C
C set parametrization flag defined by ICSIJO(2)
C
      SIPARF = ICSIJO(2).EQ.1
C
 900  CONTINUE
 990  FORMAT(////,'  Warning No SICAL SRCO found for this run  ',
     $          /,'  Defaults will be applied')
      END
#endif
