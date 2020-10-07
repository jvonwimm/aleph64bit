      SUBROUTINE MPRSUM
C----------------------------------------------------------------------
C!Print run statistics for MUON
C!   G.Capon 13-aug-87
C?
C!======================================================================
#ifndef DOC
#include "rstatr.h"
#include "rlunit.h"
#include "mujosu.h"
      CHARACTER*1 BAR(80)
      DATA BAR/80*'-'/
C--------------------------------------------------------------------
      WRITE(LOUTRL,'(''0'',80A)') BAR
C
      WRITE(LOUTRL,777) NRUNRS(4),NNMUDI,NCLTOT,NCLMAX,NTRTST,NTRASS
 777  FORMAT(' Muon Job Summary :',/,
     +' number of analysed events ................... ',I5,/,
     +' number of events with muon hits ............. ',I5,/,
     +' total nb of muon ch clusters found .......... ',I5,/,
     +' max nb of muon ch clusters in an event ...... ',I5,/,
     +' total nb of extrap tracks tested for assoc .. ',I5,/,
     +' total nb of extrap tracks associated ........ ',I5,/)
      RETURN
      END
#endif
