      INTEGER FUNCTION EQEDGE(XINE,DISTAN,PHIEDG)
C -------------------------------------------------------------------
CKEY ECALDES CLUSTER CRACK OVERLAP / USER
C     S.Loucatos     creation February 89
C                                  H. Videau modification 14 mars 90
C! Check position  of one point wrt cracks, overlap, fwd hole
C  Can be used to check energy measurement of a cluster given by
C  its barycentre coordinates.
C   Input  :
C            XINE(3) Point.
C   Output: Quality flag for Energy meas/t:
C            EQEDGE:      0     =  OK
C                     bit 0 set = point in the phi crack
C                     bit 1 set =   "    "  "  Overlap BL-EC
C                     bit 2 set =   "    "  "  EC edge towards beam hole
C                     bit 3 set =   "  outside EMCAL theta,phi range
C
C            DISTAN: distance to the closest lateral edge   REAL
C            PHIEDG: -1 if closest is phi inf               REAL
C                     1 if closest is phi sup.
C  This can be combined with the sign of the track and the sign of the
C  field to know if the track is entering the module or getting out.
C   Calls: EINTST,EFNDTW,EVOLPL
C   Called by USER
C   ---------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Input
         REAL XINE(3)
C  Output
         REAL DISTAN,PHIEDG
C  External
         LOGICAL EINTST
C  Locales
         INTEGER IBQUAL,CL,RW,ST,SC,MD,PL,BARIL,LEPLAN,IPL
         REAL PLANES(4,8),DISTINF,DISTSUP,DIST
         CHARACTER*12 WRONG,VOLNAM
         INTEGER PHIINF(ECALST,ECALSC),PHISUP(ECALST,ECALSC),LEDGE
         REAL LOWCUT,HIGCUT
C  Initialisations
         DATA BARIL/2/,LEDGE/1/
        DATA PHIINF/3,3,3,3,3,7,4,4,4/
        DATA PHISUP/4,4,4,4,4,8,3,3,3/
C To be taken from cards or from the data base !!!!!!!!!!!!!!!!!
         DATA LOWCUT,HIGCUT/1.5,4.5/
C  Execution
C     Good energy measurement
         IBQUAL = 0
          DISTAN=100000.
      CALL EFNDTW(XINE,'ALEPH',CL,RW,ST,SC,MD,PL,WRONG)
C
C  Out of theta,phi,stack range
         IF(.NOT.EINTST(RW,CL,ST))THEN
                   IBQUAL= IBQUAL+8
                   GO TO 777
                                  ENDIF
C
C  Check of the distance to the edges of the sensitive region
         IF(SC.EQ.BARIL) THEN
               VOLNAM='B sensitive'
                         ELSE
               VOLNAM='E sensitive'
                         END IF
         CALL EVOLPL(VOLNAM,SC,MD,LEPLAN,PLANES)
C
C     Lower EC edge
         IF(SC.NE.BARIL) THEN
         DISTAN=PLANES(1,LEDGE)*XINE(1)+PLANES(2,LEDGE)*XINE(2)+
     &          PLANES(3,LEDGE)*XINE(3)+PLANES(4,LEDGE)
         IF(DISTAN.LT.HIGCUT)IBQUAL=IBQUAL+4
                         END IF
C
C     Cracks in phi
         IPL=PHIINF(ST,SC)
         DISTINF=PLANES(1,IPL)*XINE(1)+PLANES(2,IPL)*XINE(2)+
     &           PLANES(3,IPL)*XINE(3)+PLANES(4,IPL)
         IPL=PHISUP(ST,SC)
         DISTSUP=PLANES(1,IPL)*XINE(1)+PLANES(2,IPL)*XINE(2)+
     &           PLANES(3,IPL)*XINE(3)+PLANES(4,IPL)
         IF(ABS(DISTSUP).LT.ABS(DISTINF))THEN
                       DIST=DISTSUP
                       PHIEDG=1.
                                         ELSE
                       DIST=DISTINF
                       PHIEDG=-1.
                                         END IF
         IF(DIST.LT.HIGCUT)IBQUAL=IBQUAL+1
C
C     Overlap EC-Barrel
      IF (RW.GE.ESCOFR(BARIL)-1-ESCORL(BARIL).AND.
     &    RW.LT.ESCOFR(BARIL)-1+ESCORL(BARIL).OR.
     &    RW.GE.ESCOLS(BARIL)-2-ESCORL(BARIL).AND.
     &    RW.LT.ESCOLS(BARIL)-2+ESCORL(BARIL)) THEN
                                IBQUAL=IBQUAL+2
                                        ENDIF
         IF(ABS(DIST).LT.ABS(DISTAN))DISTAN=DIST
 777     CONTINUE
         EQEDGE = IBQUAL
         END
#endif
