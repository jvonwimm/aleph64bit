C ------------------------------------------------------------------------------
      SUBROUTINE TPSMGT(NDIM,BANK,IBANK,ARRAY,IRET)
C ------------------------------------------------------------------------------
C! Read in the given row IBANK of BANK (upper triangular matrix) 
C! into ARRAY(NDIM,NDIM).  NR of the BANK is the TPC setup code
C  Author  Manoj Thulasidas  Dec. 8, 1997
C
C  Input 
C            NDIM  = dimenstion of ARRAY  (21 for TPSM)
C            BANK  = the bank name        (TPSM)
C            IBANK = the row number       (= view number here)
C  Output
C            ARRAY = the square matrix into which the bank is unpacked
C            IRET  = 0 if success, -1 if failed
C ------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
C  Arguments
      INTEGER NDIM, IBANK, IRET
      CHARACTER*4 BANK
      REAL ARRAY(NDIM,*)
C  Functions
      INTEGER  ALGTDB, GTSTUP, NAMIND, JUNIDB
      EXTERNAL ALGTDB, GTSTUP, NAMIND, JUNIDB
C  Local variables
      INTEGER ITPC, NABANK, LUNDB, KBANK, IRUN, IND, IEVT
#include "bmacrod.h"
#include "bmacro.h"
C ------------------------------------------------------------------------------
      IRET = -1
C  get the run number
      CALL ABRUEV(IRUN,IEVT)
C Get database Calibration bank
      ITPC  = GTSTUP ('TP',IRUN)
      LUNDB = JUNIDB()
      IRET = ALGTDB(LUNDB,BANK,ITPC)
      NABANK = NAMIND(BANK)
      KBANK = IW(NABANK)
      IF(KBANK .GT. 0)THEN
        WRITE (IW(6),*)'TPSMGT: reading the database bank ',BANK
C-- load the alignment error matrix
        IND = KROW(KBANK,IBANK)+1
C-- unpack the triangular matrix.  
        CALL TRUPCK(RW(IND),ARRAY,NDIM)
        IRET = 0
      ELSE
        WRITE (IW(6),*)'TPSMGT: Can not read database bank ', BANK
      END IF
      RETURN
      END
#endif
