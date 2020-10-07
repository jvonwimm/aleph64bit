*DK tsinit
      SUBROUTINE TSINIT
C--------------------------------------------------------------------
C!  Initialize geometry, I-O, histograms, and work banks
C
C  Called from:   TPMAIN (Main program)
C  Calls:         TSCDEF,TSRCON,TSCNST,TSCBOS,TPINRF,TPINEL,
C                 TOBKHS,TSAVCN,TPFILL,OPENDB
C
C  Inputs:   READ:  --Run conditions, unit 12
C  Outputs:  See individual subroutines
C
C  A. Caldwell, D. DeMille, M. Mermikides
C  Modifications
C     1. F. Ranjard  05 May 88 --Load TPC geometry using the TPCDES
C                                package from ALEPHLIB.
C     2. F. Ranjard  05 May 88 --Open database using OPENDB (ALEPHLIB)
C     3. P. Janot    06 May 88 --Do NOT load TPC geometry because
C                                this has to be done for each run.
C                                Now done in SUBROUTINE TOEVIN.
C     4. M. Rumpf    08 Nov 89
C -For Apollo version : provide 2 links in your home directory :
C   TRKFIL :to your 'Track Element Input File'
C   DIGFIL :to your 'Track Element Output File'
C  You will be prompted for card file pathname.
C--------------------------------------------------------------------
#include "bcs.h"
#include "alcons.h"
#include "tpcond.h"
#include "faster.h"
      LOGICAL LEX, OPENDB
      CHARACTER*4 TFORM
      DATA LBASE/4/,LINP/33/,ILNDB/1024/
C
C  Set up default condititions
C
      CALL TSCDEF
C
C  Read run cards to update conditions
C
#if defined(APOLLO)
      LUCAR=IW(5)
      OPEN(UNIT=LUCAR,FILE='* TPCSIM CARDS:',STATUS='OLD')
#endif
      CALL TSRCON
C
C  Physical constants for TPC simulation
C
      CALL TSCNST
C
C Initialise BOS bank names, formats and default sizes
C
      CALL TSCBOS
C
C  Open Database file
C
      LEX = OPENDB (LBASE)
#if defined(VAX)
C
C  Open the input file, but not if reading in EPIO format
C
      IF (.NOT.REPIO) OPEN(UNIT=33,FILE=TRKFIL,FORM='UNFORMATTED',
     &        STATUS='OLD',READONLY)
#endif
#if defined(APOLLO)
C
C  Open the input file, but not if reading in EPIO format
C
       OPEN(UNIT=33,FILE='TRKFIL',FORM='UNFORMATTED',
     &        STATUS='OLD')
#endif
C
C  If we want to do electronics, initialize appropriate banks and
C  open the output file
C
      CALL TPINRF
C
      IF ( LTWDIG .OR. LTPDIG .OR. LTTDIG ) THEN
         CALL TPINEL
#if defined(VAX)
         IF (.NOT.WEPIO) OPEN(UNIT=25,FILE=DIGFIL,FORM='UNFORMATTED',
     &                        STATUS='NEW')
#endif
#if defined(APOLLO)
          OPEN(UNIT=25,FILE='DIGFIL',FORM='UNFORMATTED',
     &                        STATUS='NEW')
#endif
      ENDIF
C
C  Make histograms
C
      NDEBSM = NTPCDE+NTPCDD+NTPCDT+NTPCDA+NTPCDC+NTPCDR+NTPCDI
      IF(NDEBSM .GT. 0) CALL TOBKHS(0)
C
C  Save conditions in BOS bank (TSIM) and add to C list
C
      CALL TSAVCN(INTCHA('TPC '))
C
C  Read Landau Fluctuation and ExB effect constants for FAST
C  simulation
C
      IF(ILEVEL .EQ. 'FAST') THEN
         CALL TPFILL(IRET)
         IF(IRET.EQ.1) STOP 99
      ENDIF
      RETURN
      END
