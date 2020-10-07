      SUBROUTINE SRCLUS(IER)
C----------------------------------------------------------------------
C! Find clusters of fired wires
C!
C!    Author:     H. Meinhard       05-Jan-1988
C!    Modified:   H. Meinhard       06-Feb-1990  (3)
C!
C!    Output:     - IER       /I    error flag (.ne. 0 if error)
C!
C!    Description
C!    ===========
C!    For each sector in each orientation, look from the interaction
C!    point towards the fired wires and search for overlays of fired
C!    wires consisting of three or two fired wires. Hits in clusters
C!    with three fired wires are not used for clusters with two fired
C!    wires
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "sccpjj.h"
#include "sclujj.h"
#include "sgeomc.h"
#include "sparam.h"
#include "ssbpjj.h"
C     MCL = preliminary maximal number of clusters
      PARAMETER (MCL=200)
      INTEGER IBUFF(3,MGRSG*MSCSG,MADSP)
#include "bosext.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C look whether bank SCLU exists; if yes, return
      IF (IW(NASCLU) .NE. 0)                                GOTO 999
C
C look whether bank SCOO exists; if no, return with error sts.
      IF (IW(NASCOO) .EQ. 0)                                GOTO 901
C
C if bank SSCP does not exist, create it
      IF (IW(NASSCP) .EQ. 0) CALL SSCPCR(IER)
      IF (IER .EQ. 2)                                       GOTO 902
C
C create banks for clusters, for pointers from clusters to coordinates
C and for pointers from orientations to clusters
      CALL AUBOS('SCLU',0,LMHLEN+MCL*LSCLUA,KSCLU,IGARB)
      IF (KSCLU .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) KSCLU = IW(NASCLU)
      IW(KSCLU+LMHCOL) = LSCLUA
C
      CALL AUBOS('SCCP',0,LMHLEN+MCL*LSCCPA,KSCCP,IGARB)
      IF (KSCCP .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCLU = IW(NASCLU)
        KSCCP = IW(NASCCP)
      ENDIF
      IW(KSCCP+LMHCOL) = LSCCPA
C
      CALL AUBOS('SSBP',0,LMHLEN+2*MLAYSG/3*LSSBPA,KSSBP,IGARB)
      IF (KSSBP .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCLU = IW(NASCLU)
        KSCCP = IW(NASCCP)
        KSSBP = IW(NASSBP)
      ENDIF
      IW (KSSBP+LMHCOL) = LSSBPA
      IW (KSSBP+LMHROW) = 2*MLAYSG/3
C
C top of loop over sides
      DO 310 ISIDE = 1, 2
C
C top of loop over orientations
        DO 300 IORIE = 1, 3
          IND = (ISIDE-1)*MLAYSG/3+IORIE
          IW(KROW(KSSBP,IND)+JSSBPB) = LROWS(KSCLU)+1
C
C fill address buffer
          CALL SRADDR(ISIDE,IORIE,IBUFF,IER)
          IF (IER .EQ. 1)                                   GOTO 904
C
C look for clusters of three fired wires
          CALL SR3CLU(ISIDE,IORIE,IBUFF,IER)
          IF (IER .NE. 0)                                   GOTO 903
          N3TOT = LROWS(KSCLU)
C
C look for clusters of two fired wires (layers 1, 2)
          CALL SR2CLU(ISIDE,IORIE,IBUFF,1,2,N3TOT,IER)
          IF (IER .NE. 0)                                   GOTO 903
C
C look for clusters of two fired wires (layers 2, 3)
          CALL SR2CLU(ISIDE,IORIE,IBUFF,2,3,N3TOT,IER)
          IF (IER .NE. 0)                                   GOTO 903
C
C look for clusters of two fired wires (layers 1, 3)
          CALL SR2CLU(ISIDE,IORIE,IBUFF,1,3,N3TOT,IER)
          IF (IER .NE. 0)                                   GOTO 903
C
C bottom of loop over orientations
          IW(KROW(KSSBP,IND)+JSSBPE) = LROWS(KSCLU)
  300   CONTINUE
C
C bottom of loop over sides
  310 CONTINUE
C
C compress SCLU and SCCP banks to actual size
      CALL AUBPRS('SCLUSCCP')
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRCLUS',-IER,
     +  'No space for new bank SCLU, SCCP, SSBP, or SSCP')
      GOTO 999
  903 IER = 3
      CALL RERROR('SRCLUS',-IER,'Maximum number of clusters exceeded')
      GOTO 999
  904 IER = 4
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
