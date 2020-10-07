*DK tginit
      SUBROUTINE TGINIT (GFIEL,ICTP)
C--------------------------------------------------------------------
C!  Initialize geometry, I-O, histograms, and work banks
C
C  Called from:  GALEPH/TPIRUN
C  Calls:        TSCDEF, TSCNST, TGCBOS, TSRCON
C                TPINEL, TOBKHS, TSAVCN, TPFILL
C
C  Input: GFIEL / R  = GALEPH magnetic field
C         ICTP  / I  = GALEPH run conditions
C
C  A. Caldwell, D. DeMille, M. Mermikides
C  Modifications
C     1. D. Cowen    24 Jun 88 --This routine originally was TSINIT,
C                                modified here for use with GALEPH.
C     2. F.Ranjard   28 Mar 89 --add arguments in the calling sequence
C                                and suppress references to GALEPH *CD
C--------------------------------------------------------------------
      INTEGER ICTP(*)
#include "alcons.h"
#include "tpcond.h"
#include "bcs.h"
C
C TPCSIM version number
C
      CALL TPCVER(VERS)
      WRITE(IW(6),10) VERS
C
C  Set default indices for running TPCSIM from GALEPH
C
C   Create TPTE and TPHT bank
      ICTP(4) = 1
C   Get rid of TPCO, TCRL and TPCH
      ICTP(6) = 0
C
C  Set up default condititions
C
      CALL TSCDEF
C
C  Assign to CFIELD (in Comdeck TPCOND) the value GFIEL
C
      CFIELD = GFIEL
      BCFGEV = CLGHT*CFIELD*1.E-5
      BCFMEV = BCFGEV*1000.
C
C  Physical constants for TPC simulation
C
      CALL TSCNST
C
C Initialise BOS bank names, formats and default sizes
C
      CALL TGCBOS
C
C  Calculate parameters for breaking track elements at sector
C  boundaries
C
      CALL TPBGEO
C
C  If we want to do electronics, initialize appropriate banks and
C  open the output file
C
      CALL TPINRF
      CALL TPINEL
C
C  Make histograms
C
C     CALL TOBKHS(1,0)
C
C  Save conditions in BOS bank (TSIM,NR=0) and add to C list
C
      CALL TSAVCN(INTCHA('GAL '))
      CALL BLIST(IW,'C+','TSIM')
C   print TSIM written on output
      IF (ICTP(5).GT.0) THEN
         CALL PRTABL('TSIM',0)
      ENDIF
C
C  Read Landau Fluctuation and ExB effect constants for TPCSIM
C
      CALL TPFILL(IRET)
      IF (IRET .EQ. 1) STOP 99
C
C  Fill lookup table of Gaussian random numbers
C
      INDX = 0
      DO WHILE (INDX.LT.1000)
         CALL RANNOR(G1,G2)
         IF(ABS(G1).LE.3.)THEN
            GRANNO(INDX+1) = G1
            GRANNO(INDX+2) = -G1
            INDX = INDX + 2
         ENDIF
         IF(INDX.LT.1000 .AND. ABS(G2).LE.3.)THEN
            GRANNO(INDX+1) = G2
            GRANNO(INDX+2) = -G2
            INDX = INDX + 2
         ENDIF
      ENDDO
C
10    FORMAT(//9X,' TPC digitizations will be done with',
     &         1X,'TPCSIM version ',F4.2/)
      RETURN
      END
