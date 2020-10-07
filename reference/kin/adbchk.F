      subroutine adbchk(ivers)
C -------------------------------------------
C  B.Bloch-Devaux May 2001
C  this routine checks the current database version number 
C  against the value indicated in a possible DBVS card 
C  if OK or no DBVS card just return 
C  if not OK exit after an error message 
C       ivers: current version number ( output of ADBVER for example)
C ------------------------------------------------
#include "bcs.h"
      jname = nlink('DBVS',0)
      if (jname.ne.0) then
         ioffvs = iw(jname+1)
         if ( ioffvs.ne.ivers) then
            write(iw(6),*) ' +++ ADBCHK +++ current dbase version is ',
     &      ivers,ioffvs,' was required .... EXIT !'
            call exit(1)
         endif
      endif
      return
      end
