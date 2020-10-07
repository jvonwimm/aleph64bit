      SUBROUTINE SRPATC(IER)
C----------------------------------------------------------------------
C! Find patches of clusters
C!
C!    Author:     H. Meinhard       06-Jan-1988
C!    Modified:   H. Meinhard       06-Feb-1990 (2)
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    Find patches of clusters by first combining three clusters in
C!    different spacial orientations, then by combining two clusters
C!    and - eventually - a single fired wire in the third orientation
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "spatjj.h"
#include "spcpjj.h"
#include "ssppjj.h"
C MPT = preliminary maximal number of patches
      PARAMETER (MPT=200)
#include "bosext.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C look whether bank SPAT exists; if yes, return
      IF (IW(NASPAT) .NE. 0)                                GOTO 999
C
C look whether banks SCOO, SSCP, SCLU, SSBP exist; if not, return
C with error status
      IF (IW(NASCOO).EQ.0 .OR. IW(NASSCP).EQ.0 .OR. IW(NASCLU).EQ.0 .OR.
     +    IW(NASSBP).EQ.0)                                  GOTO 901
C
C create banks for patches, for pointers from patches to coordinates
C and for pointers from sides to patches
      CALL AUBOS('SPAT',0,LMHLEN+MPT*LSPATA,KSPAT,IGARB)
      IF (KSPAT.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) KSPAT = IW(NASPAT)
      IW(KSPAT+LMHCOL) = LSPATA
C
      CALL AUBOS('SPCP',0,LMHLEN+MPT*LSPCPA,KSPCP,IGARB)
      IF (KSPCP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSPAT = IW(NASPAT)
        KSPCP = IW(NASPCP)
      ENDIF
      IW(KSPCP+LMHCOL) = LSPCPA
C
      CALL AUBOS('SSPP',0,LMHLEN+2*LSSPPA,KSSPP,IGARB)
      IF (KSSPP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSPAT = IW(NASPAT)
        KSPCP = IW(NASPCP)
        KSSPP = IW(NASSPP)
      ENDIF
      IW(KSSPP+LMHCOL) = LSSPPA
      IW(KSSPP+LMHROW) = 2
C
C top of loop over sides
      DO 300 ISIDE = 1, 2
        IW(KROW(KSSPP,ISIDE)+JSSPPB) = LROWS(KSPAT)+1
C
C look for 3-patches
        CALL SR3PAT(ISIDE,IER)
        IF (IER .NE. 0)                                     GOTO 903
        N3TOT = LROWS(KSPAT)
C
C look for 2-patches (clusters in orientations 1,2)
        CALL SR2PAT(ISIDE,1,2,3,N3TOT,IER)
        IF (IER .NE. 0)                                     GOTO 903
C
C look for 2-patches (clusters in orientations 2,3)
        CALL SR2PAT(ISIDE,2,3,1,N3TOT,IER)
        IF (IER .NE. 0)                                     GOTO 903
C
C look for 2-patches (clusters in orientations 3,1)
        CALL SR2PAT(ISIDE,3,1,2,N3TOT,IER)
        IF (IER .NE. 0)                                     GOTO 903
C
C bottom of loop over sides
        IW(KROW(KSSPP,ISIDE)+JSSPPE) = LROWS(KSPAT)
  300 CONTINUE
C
C compress SPAT and SPCP banks to actual size
      CALL AUBPRS('SPATSPCP')
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRPATC',-IER,
     +  'No space for new bank SPAT, SPCP, or SSPP')
      GOTO 999
  903 IER = 3
      CALL RERROR('SRPATC',-IER,'Maximum number of patches exceeded')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
