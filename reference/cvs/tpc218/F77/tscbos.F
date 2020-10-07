*DK tscbos
      SUBROUTINE TSCBOS
C--------------------------------------------------------------------
C! Define BOS bank names, default lengths, extensions and formats
C   Called from TSINIT
C  M. Mermikides 4/12/86   (Derived from previous BLOCK DATA)
C
C  P. Janot     11 oct 88   Change LRECL in BUNIT to save space in
C                           EPIO format.
C--------------------------------------------------------------------
#include "bcs.h"
#include "tpcbos.h"
#include "tpcond.h"
C
C Look at REPIO and WEPIO to set the BOS read and write
C flags, respectively, to the appropriate formats for the
C appropriate units.
C
      IF (REPIO) THEN
         CALL BUNIT(33,'EPIO',16020)
      ELSE
         CALL BUNIT(33,'FORT',16020)
      ENDIF
C
      IF(LWRITE) THEN
        IF(WEPIO) THEN
         CALL BUNIT(25,'EPIO',16020)
        ELSE
         CALL BUNIT(25,'FORT',16020)
        ENDIF
      ENDIF
C
C
      MAXNCH(1) = 200
      MAXNCH(2) = 1800
      MAXNCH(3) = 44
      NDEFCH = 150
      NEXTCH = 100
      NTSGHT = 0
C
C Clear pointers to WORK banks initially
C
      INDREF(1) = 0
      INDREF(2) = 0
      INDREF(3) = 0
      INDSIG = 0
      IDCLUS = 0
      ITSHAP = 0
      ITDSHP = 0
      ITPNOI = 0
      ITSNOI = 0
      ITPULS = 0
      ITMADC = 0
      INDBRT = 0
      INDHL = 0
      INDDI = 0
C
C /DIGBNK/ Default sizes and increments
C
      NDIDEF(1) = 500
      NDIDEF(2) = 3000
      NDIDEF(3) = 300
      NDIEXT(1) = 200
      NDIEXT(2) = 1000
      NDIEXT(3) = 100
C
C /TPBNAM/ Bank names (Trigger pads are provisional)
C
      DIGNAM(1) = 'TWIR'
      DIGNAM(2) = 'TWDI'
      DIGNAM(3) = 'TPAD'
      DIGNAM(4) = 'TPDI'
      DIGNAM(5) = 'TTRI'
      DIGNAM(6) = 'TTDI'
      DO 10 I = 1,6
         CALL BKFMT(DIGNAM(I),'B32')
   10 CONTINUE
C  TPTE name index  /TPNAMI/
      NATPTE = NAMIND('TPTE')
C
      RETURN
      END
