      SUBROUTINE BABEND(TEXT)
C ---------------------------------------------------------------
C - V.Blobel - from BOS77                        F.Ranjard - 870203
CKEY IO BOS ABEND
C! Call trace back in case of BOS fatal error
C - Print 'TEXT' then call traceback routine
C - called by several BOS77 routines
C -----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      CHARACTER*(*) TEXT
C -----------------------------------------------------------------
      LOUT = IW(6)
      IF (LOUT.GT.0) WRITE(LOUT,'(1X,''+++BABEND+++ '',A)') TEXT
C
CFLR      CALL BOSBK(IW)
#include "tracbck.h"
      END
#endif
