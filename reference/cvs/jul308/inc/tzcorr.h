      PARAMETER (LTZCOR=9)
      COMMON/TZCORR/ TZOFFS(LTZCOR),TZDEP1(LTZCOR),
     &               TZDEP2(LTZCOR)
#if defined(DOC)
C----------------------------------------------------------------------
C! Constants for correcting TPC z coordinates for pulse shape & offset
C The first index is the sector number, and the second is
C    1--3  Time algorithm choice as specified by TimeAlg of bank TLCT
C    9     for wire pulses (used by TWIRES)
C
C  TZOFFS =  Constant offset in z
C  TZDEP1 =  Not yet defined
C  TZDEP2 =  Not yet defined
C---------------------------------------------------------------------
#endif
