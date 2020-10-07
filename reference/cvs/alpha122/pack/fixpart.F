      SUBROUTINE FIXPART
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! Fix the syntax of some antiparticles as given by KINGAL
C  to make them understandable by ALPHA
C  Called from QMNEWR
C                                                   J.Boucrot  01.02.94
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
#include "partjj.h"
      CHARACTER CNOLD*12,CNNEW*12,PREVL*1
#include "bmacro.h"
C----------------------------------------------------------------------
C Depending on which version of LUND/HVFL  was run , and if one reads
C a KINGAL/RAW or a POT/MINI tape , some neutral particles
C added in the 'PART' bank which are not standard ALPHA particles
C have an antiparticle name which is not the ALPHA standard :
C    e.g.  D*_20 0   instead of D*_2#0
C          B*_2s~0   instead of B*_2s#0
C   This routine modifies the extended 'PART' bank when running ALPHA ,
C   to have an antiparticle name which follows the ALPHA conventions .
C
      JPART = IW(NAPART)
      IF (JPART.EQ.0) GO TO 999
      NPART = 0
      IF (JPART.GT.0) NPART = LROWS(JPART)
      IF (NPART.LE.KNPAST) GO TO 999
      DO 500 IPART = KNPAST+1,NPART
         KPART = KROW(JPART,IPART)
         ICODE=KMCCOD(IPART)
         CNOLD = ' '
         CALL ALSTIN(IW(KPART+JPARNA),3,CNOLD)
         J0 = ICFILA('0',CNOLD,1,12)
         IF (J0.LE.0.OR.J0.GT.12) GO TO 500
         PREVL = CNOLD(J0-1:J0-1)
         IF (PREVL.NE.' '.AND.PREVL.NE.'~') GO TO 500
         CNNEW = CNOLD
         CNNEW(J0-1:J0-1) = '#'
         CALL ALINST(CNNEW,IW(KPART+JPARNA),NDUM)
         ICODE=KMCCOD(IPART)
 500  CONTINUE
 999  RETURN
      END
#endif
