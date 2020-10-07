*DK muwsum
      SUBROUTINE MUWSUM
C*******************************************************************
C
C F.Bossi -870818
C
C! print general statistics of MUON
C
C      Called from ASCRUN       in this .HLB
C      Calls       none
C
C*******************************************************************
#ifndef DOC
#include "mustat.h"
#include "iocom.h"
C -------------------------------------------------------------------
      IF (NEVMUH.LE.0)THEN
         WRITE(LOUTIO,'(/3X,''+++MUWSUM+++ No hits in Muon chambers'')')
      ELSE
         WRITE(LOUTIO,50) NEVMUH,NTMCLU,NTMCLU/NEVMUH,NMXCLU
      ENDIF
C
 50   FORMAT(//1X,'+++MUWSUM+++',10X,'MUON RUN SUMMARY'/3X
     +,'Number of events with muon hits',T60,I5/ 3X
     +,'total number of muon ch. clusters found',T60,I5/ 3X
     +,'average number of clusters per event with muon hits',T60,I5/3X
     +,'Max. number of muon ch. clusters in an event',T60,I5/)
C
      END
#endif
