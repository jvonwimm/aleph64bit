      SUBROUTINE FIDHEA
C========================================================================
C!    Fills DHEA bank for quantities relative to E-Flow
C     Authors: M.N.Minard M.Pepe        12/05/89
C
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "dheajj.h"
#include "pjetef.h"
#include "bcs.h"
      DIMENSION TVEC (3 , 2 )
#include "bmacro.h"
      KDHEA = IW(NAMIND('DHEA'))
      IF ( KDHEA.EQ.0) THEN
         CALL AUBOS('DHEA',0,LMHLEN+LDHEAA,KDHEA,IGARB)
         IF ( IGARB.EQ.2) GO TO 999
         CALL BLIST(IW,'E+','DHEA')
      ENDIF
      JDHEA = KDHEA+LMHLEN
      IW(JDHEA+JDHENJ) = NFITJT(2)
      RW(JDHEA+JDHEEC) = ETRACK
C
C-    In case E-Flow is not performed
C-    calculate track sum
C
      IF ( NFITJT(2).LT.0) THEN
         CALL FLOWTR ( XTRACK)
         RW (JDHEA+JDHEEC) = XTRACK
         CALL FLOWEH ( EFLOW)
         RW (JDHEA+JDHEEF) = EFLOW
         RW (JDHEA+JDHEEL) = EFLOW
         GO TO 990
      ENDIF
      CALL UCOPY ( THRTEF(1,2,2) ,TVEC ( 1,1) , 3)
      CALL UCOPY ( THRTEF(1,3,2) , TVEC(1,2) , 3)
      RW(JDHEA+JDHEEL) = VRECNF(4,1)
      RW(JDHEA+JDHEPF) = SQRT(VRECNF(1,2)**2+VRECNF(2,2)**2
     &+VRECNF(3,2)**2)
      IF ( RW(JDHEA+JDHEPF).GT.0.001) THEN
      RW(JDHEA+JDHETH) = ACOS(VRECNF(3,2)/RW(JDHEA+JDHEPF))
      RW(JDHEA+JDHEPH) = 0.
      IF ( SQRT(VRECNF(1,2)**2+VRECNF(2,2)**2).GT.0.001) THEN
        RW(JDHEA+JDHEPH) = ATAN2(VRECNF(2,2),VRECNF(1,2))
      ENDIF
      IF (RW(JDHEA+JDHEPH).LT.0.)
     &      RW(JDHEA+JDHEPH)=RW(JDHEA+JDHEPH)+TWOPI
      RW(JDHEA+JDHEEF) = VRECNF(4,2)
      RW(JDHEA+JDHEET) = VRECNF(5,2)
      TNORM=SQRT(THRTEF(1,1,2)**2+THRTEF(2,1,2)**2+THRTEF(3,1,2)**2)
      IF (TNORM.NE.0.)THEN
         RW(JDHEA+JDHET1) = ACOS(THRTEF(3,1,2)/TNORM)
      ELSE
         CALL RERROR('FIDHEA',2,'TNORM = 0')
         RW(JDHEA+JDHET1) = 0.
      ENDIF
      RW(JDHEA+JDHEP1) = 0.
      IF (SQRT(THRTEF(1,1,2)**2+THRTEF(2,1,2)**2).GT.0.001)THEN
      RW(JDHEA+JDHEP1) = ATAN2(THRTEF(2,1,2),THRTEF(1,1,2))
      IF (RW(JDHEA+JDHEP1).LT.0.)
     &      RW(JDHEA+JDHEP1)=RW(JDHEA+JDHEP1)+TWOPI
      ENDIF
      CALL FIDROT ( TVEC , RW(JDHEA+JDHET1),RW(JDHEA+JDHEP1))
      TNORM = SQRT(TVEC(1,1)**2+TVEC(2,1)**2+TVEC(3,1)**2)
      IF (TNORM.NE.0.)THEN
         RW(JDHEA+JDHET2) = ACOS(TVEC(3,1)/TNORM)
      ELSE
         CALL RERROR('FIDHEA',2,'TNORM = 0')
         RW(JDHEA+JDHET2) = 0.
      ENDIF
      RW (JDHEA+JDHEP2) = 0.
      IF (SQRT(TVEC(1,1)**2+TVEC(2,1)**2).GT.0.001)THEN
      RW(JDHEA+JDHEP2) = ATAN2(TVEC(2,1),TVEC(1,1))
      IF (RW(JDHEA+JDHEP2).LT.0.)
     &      RW(JDHEA+JDHEP2)=RW(JDHEA+JDHEP2)+TWOPI
      ENDIF
      RW(JDHEA+JDHEE1) = THRTEF(4,1,2)
      RW(JDHEA+JDHEE2) = THRTEF(4,2,2)
      RW(JDHEA+JDHEE3) = THRTEF(4,3,2)
      ENDIF
 990  CONTINUE
      RETURN
 999   CALL RERROR('FIDHEA',1,'Array too small')
      RETURN
      END
#endif
