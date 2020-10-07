*DK tpensc
      SUBROUTINE TPENSC
C-----------------------------------------------------------------------
C!  Clear out sector-dependent banks and counters
C
C  Called from:  TSTEER
C  Calls:        WDROP, VZERO
C
C  Inputs:   /TPCBOS/    --INDSIG, signal bank id
C                        --INDREF, reference bank id's
C                        --NTSGHT, number of blocks open in signal bank
C            /TPCOND/    --steering flags
C
C  Outputs:  None
C
C  D. DeMille
C  Modifications:
C            1. P. Janot  03 Mar 1989 -- Drop work bank IDCLUS if
C                         it has not been already (especially
C                         when there was not enough space for
C                         extending it)
C-----------------------------------------------------------------------
#include "bcs.h"
#include "tpcbos.h"
#include "tpcond.h"
C
C  Drop the analog signal bank and clear the counter for total number
C  of channels hit
C
      CALL WDROP(IW,INDSIG)
      NTSGHT = 0
C
C  For each active channel type, clean out the reference bank and clear
C  the counter for number of channels hit
C
      DO 1 KCHAN = 1,NCHAN
C
         IF ( .NOT. LTDIGT(KCHAN) ) GOTO 1
         NCLEAR = IW(INDREF(KCHAN)+1)*IW(INDREF(KCHAN)+2)
         CALL VZERO( IW(INDREF(KCHAN) + 3), NCLEAR )
C
 1    CONTINUE
C
C  Drop work bank IDCLUS if it hasn't been dropped before
C
      CALL WDROP(IW,IDCLUS)
C
      RETURN
      END
