      SUBROUTINE CTRTOC(ITR,IECO,NECO,ICLUH,NCLUH,MXCLU,IER)
C***********************************************************************
C! Go from Trk to Ecob & Hclu                                          *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : KINE,VERT                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Itr     : track number                                            *
C!   IECO    : Table of EcObs                                          *
C!   NECO    : Number of Ecobs                                         *
C!   ICLUH   : Table of HCLUS                                          *
C!   NCLUH   : Number of HCLUS                                         *
C!                                                                     *
C***********************************************************************
#ifndef DOC
      EXTERNAL CEGCLU,CHGCLU
#include "rlunit.h"
#include "bcs.h"
      DIMENSION ITRAC(500)
      DIMENSION ICLUH(*),IECO(*)
      NTRMX=500
      LHEV3=3
      NTRAC=1
C
C?    Loop on tracks
C
      ITRAC(1)=ITR
      IT=0
   15 IT=IT+1
      IF(IT.LE.NTRAC)THEN
         ITK=ITRAC(IT)
C
C?    get vertices
C
         KKINE=NLINK('KINE',ITK)
         LPK=IW(KKINE+2)
         NVX=IW(KKINE+3)
C
C   loop on vertices
C
         DO 10 I=2,NVX
         IVT=IW(KKINE+LHEV3+LPK+I)
         KVERT=NLINK('VERT',IVT)
         LPV=IW(KVERT+2)
         NOUTK=IW(KVERT+3)
         DO 11 ITK=1,NOUTK
         IF(NTRAC.GE.NTRMX)GO TO 998
         NTRAC=NTRAC+1
         ITRAC(NTRAC)=IW(KVERT+LHEV3+LPV+ITK)
   11    CONTINUE
   10    CONTINUE
         GO TO 15
      ENDIF
C
C?  Now, table itrac contains all tracks issued from original track
C       we loop on these tracks
C
      NECO=0
      NCLUH=0
      DO 200 IT=1,NTRAC
      ITK=ITRAC(IT)
      CALL CEGCLU(ITK,IECO,NECO,MXCLU,IER2)
      IF(IER2.NE.0)GO TO 998
      CALL CHGCLU(ITK,ICLUH,NCLUH,MXCLU,IER2)
      IF(IER2.NE.0)GO TO 998
  200 CONTINUE
      IER=0
      RETURN
  998 CALL RERROR('CTRTOC',1,' MORE THAN NTRMX TRACKS IN A SHOWER')
      IER=1
      RETURN
      END
#endif
