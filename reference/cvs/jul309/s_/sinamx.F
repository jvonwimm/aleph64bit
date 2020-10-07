      SUBROUTINE SINAMX
C----------------------------------------------------------------------
C!  - Set Scal name indices
C!
C!   Author   : B.Bloch-Devaux 910129
C!              Modified for Bunch train mode October 1994
C!
C!   Description : Call NAMIND for all relevant SICAL banks
C!   ===========
C?
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
      EXTERNAL NAMIND
C----------------------------------------------------------------------
C Input banks (S list)
      NASIDI = NAMIND('SIDI')
      NASIFO = NAMIND('SIFO')
      NASIX2 = NAMIND('SIX2')
      NASCHU = NAMIND('SCHU')
C JULIA banks (T list)
      NASPDA = NAMIND('SPDA')
      NASCLS = NAMIND('SCLS')
C Temporary banks
      NASMAP = NAMIND('SMAP')
C POT banks (E list)
                NASILH = NAMIND('SILH')
                NASILU = NAMIND('SILU')
C Run end record  (permanent, but reset each run)
                NASLUM = NAMIND('SLUM')
C Job end record  (permanent)
      NASSTA = NAMIND('SSTA')
C Reconstruction constants (permanent)
      NASREC = NAMIND('SREC')
                NASBHA = NAMIND('SBHA')
      NASMPD = NAMIND('SMPD')
      NASECA = NAMIND('SECA')
      RETURN
      END
#endif
