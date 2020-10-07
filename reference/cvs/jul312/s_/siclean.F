      SUBROUTINE SICLEAN(kcl,iflag)
C----------------------------------------------------------------------
C!  - Checks for quality flag of cluster
C!
C!
C!    Author:     B. Bloch-Devaux   June 1998
C!                modified April 99 to extend to more backgrounds types
C!                modified March 2000 to use Alephlib routine SIBKCL
C!
C!    Input:      - kcl = cluster number in SCLS, if 0 = initialisation
C!                _ banks 'SCLS' from Sical clusters
C!    Output:
C!                - iflag quality flag 0 = OK,1= oscillating,2= beam loss
C!                                     3=backwards,4=scraping,5=offmom
C!                - creates bank SIID with flag and pointer to SCLS
C!                  pointers to SILU and PECO are filled later in
C!                  SILUMI and SICRPOT
C?
C!======================================================================
#ifndef DOC
      SAVE nclu,iruno,ifclu
#include "alcons.h"
#include "bcs.h"
#include "sclsjj.h"
#include "rcurnt.h"
#include "sinamc.h"
#include "siidjj.h"
      PARAMETER( LMAXC = 20)
      DIMENSION IFCLU(LMAXC)
C      
C==================================================================
C
#include "bmacro.h"
C----------------------------------------------------------------------
      iflag = 0
      If ( kCL.le.0) THEN
C loop on clusters found to identify possible background sources
        CALL VZERO(IFCLU,LMAXC)
        JSCLS = IW(NASCLS)
        IF (JSCLS .LE. 0) GO TO 999
        NCLU = LROWS(JSCLS)
        IF ( NCLU.LE.0) GO TO 999
C book SIID bank with same length as SCLS
        LEN = NCLU*LSIIDA + LMHLEN
        CALL AUBOS('SIID',0,LEN,KSIID,IGARB)
        IF (IGARB.EQ.2) GO TO 998
        IW(KSIID+LMHROW) = NCLU
        IW(KSIID+LMHCOL) = LSIIDA
        DO 320 ICL = 1, NCLU
          CALL SIBKCL(IRUNRC,icl,iflag)
          IFCLU(ICL) = iflag
          KID = KROW(KSIID,ICL)
          IW(KID+JSIICI) = iflag
          IW(KID+JSIISC) = ICL
  320 CONTINUE
C
      ELSE IF ( (KCL.gt.0).and.(KCL.le.nclu)) then
           iflag = ifclu(kcl)
      ENDIF
C----------------------------------------------------------------------
      GO TO 999
  998 CALL RERROR('SICLEAN', -2, ' no room for SIID')
  999 CONTINUE
      RETURN
      END
#endif
