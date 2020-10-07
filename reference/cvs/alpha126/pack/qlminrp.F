      SUBROUTINE QLMINRP(XLUMRMP,NBHAMR)
CKEY RUN /INTERNAL
C----------------------------------------------------------------------
C! gives the lumi with MiniramP for the current run
C called from QMNEWR
C returns XLUMRMP = Lumi with Mini-Ramp
C                 = -1. if not relevant  
C         NBHAMR  = Number of Bhabhas 
C                                              J.Boucrot  07.06.2000 
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "lumijj.h"
      PARAMETER ( METMR = 11 , NRUMI = 53151 )
#include "qmacro.h"
C----------------------------------------------------------------------
      XLUMRMP=-1.
      NBHAMR=0
C Continue only if the data card MRMP was present:
      IF (IW(NAMIND('MRMP')).EQ.0) GO TO 999
C Stop if the current run cannot have Mini Ramps :
      IF (KRUN.LT.NRUMI) GO TO 999 
C Stop if the LUMI bank doesn't have the right number of rows: 
      JLUMI=IW(NAMIND('LUMI'))
      IF (LROWS(JLUMI).LT.METMR) GO TO 999
C Get the Mini-Ramp lumi from bank 'LUMI', row 11:
      KLUMI=KROW(JLUMI,METMR)
      XLUMRMP=RW(KLUMI+JLUMLU)
      NBHAMR=NINT(RW(KLUMI+JLUMB2))      
C
C
 999  return
      END
#endif

