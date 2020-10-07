*DK tsfini
      SUBROUTINE TSFINI
C-----------------------------------------------------------------------
C!  Close files, flush buffers, output histograms, drop work banks
C
C  Called from: TSMAIN (main program)
C  Calls:       WDROP, BWRITE, BOSTA, HISTDO, HSTORE
C
C  Inputs:   /TPCOND/  --Condition flags
C            /TPCBOS/  --work bank id's
C
C  Outputs:  WRITE:       --BOS buffer, unit 25
C                         --Histograms, units 6 and 15
C  D. DeMille
C  D. Cowen   12 Feb 88  --Use BWRITE instead of BOSWR so we can
C                          specify EPIO vs. FORT (native) formats.
C                          An earlier BUNIT call sets these formats.
C
C                        --Don't do OPENs or CLOSEs on units 25 and
C                          33 if using EPIO.
C-----------------------------------------------------------------------
#include "bcs.h"
#include "tpcbos.h"
#include "tpcond.h"
#include "hiscom.h"
#include "faster.h"
C
C  Close track element file, but only if we're not using EPIO
C
#if defined(VAX)
         IF (.NOT.REPIO) CLOSE(UNIT=33)
#endif
C
C  Drop Landau Fluctuation Banks
C
       CALL BDROP(IW,'TLAN')
       CALL BDROP(IW,'TIND')
      CALL WDROP(IW,INTWRD)
C
C  Drop analog signal reference banks and single-pulse bank
C
      DO 1 KCHAN = 1, NCHAN
         IF ( INDREF(KCHAN) .NE. 0 ) CALL WDROP(IW,INDREF(KCHAN))
    1 CONTINUE
         CALL WDROP(IW,ITPULS)
C
C  If we did electronics simulation, drop banks used for it, flush
C  the BOS buffer for the digitizations file, and close the file if
C  we're not writing in EPIO format.
C
      IF ( LTWDIG .OR. LTPDIG .OR. LTTDIG ) THEN
C
         CALL WDROP(IW,ITSNOI)
         CALL WDROP(IW,ITPNOI)
         CALL WDROP(IW,ITSHAP)
         CALL WDROP(IW,ITMADC)
C
         IF(LWRITE) CALL BWRITE(IW,25,'0')
#if defined(VAX)
         IF (.NOT.WEPIO) CLOSE(UNIT=25)
#endif
C
      ENDIF
C
C  Output final status of BOS
C
      CALL BOSTA
C
C  Write out histograms
C
      NDEBSM = NTPCDE+NTPCDD+NTPCDT+NTPCDA+NTPCDC+NTPCDR+NTPCDI
       IF (NDEBSM .GT. 0) CALL HISTDO
C
      RETURN
      END
