      SUBROUTINE TNHLFP(ISLOT,IPAD,KTS,NHALF)
C-----------------------------------------
C   Author   :- D.Cinabro             11-MAR-1990
C=========================================
C! Calculate the number of halfpads in a sub cluster
C  Code stolen from Andreas Roth
C
C   Inputs    : ISLOT=  sector slot number
C               IPAD =  sector padrow number
C               KTS/I = pointer to row in TSCL bank
C   Ouput     : NHALF/I = nuumber of half pads in this sub cluster
C=========================================
#ifndef DOC
C
#include "bcs.h"
#include "tscljj.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
C
C Number of sub pulses
      NSUB = IW(KTS+JTSCNS)
C Number of the first pad
      NPAD = IW(KTS+JTSCFP)
C Sector type
      ITYP = ITPTYP(ISLOT)
C Number of equivalent full pads in this row
      NROW = NTPDPR(IPAD,ITYP)
C Number of halfpads here
      IF (NPAD.EQ.1) THEN
        NHALF = 2
      ELSEIF (NPAD.EQ.2) THEN
        NHALF = 1
      ELSEIF ((NPAD-1+NSUB).EQ.(NROW+1)) THEN
        NHALF = 1
      ELSEIF ((NPAD-1+NSUB).EQ.(NROW+2)) THEN
        NHALF = 2
      ELSE
        NHALF = 0
      ENDIF
C
      RETURN
      END
#endif
