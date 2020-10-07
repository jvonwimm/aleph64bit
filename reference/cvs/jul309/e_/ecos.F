      SUBROUTINE ECOS ( EN,IPHI,ITHE,IST,EVECT)
C=======================================================================
C! Given a storey energy in Ecal, it creates the corresponding
C! energy vector
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C  Input : EN     storey energy
C          IPHI   column index of storey
C          ITHE   row index of storey
C          IST    stack #
C  Output: EVECT  energy vector
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "yvxljj.h"
C
      DIMENSION EVECT(4) , POINT(3) , VERT (3)
#include "bmacro.h"
C
C  Get fitted vertex position
C
      NAYVXL = NAMIND ('YVXL')
      KYVXL  = IW (NAYVXL)
C
        VERT ( 1 ) = 0.
        VERT ( 2 ) = 0.
        VERT ( 3 ) = 0.
C
      IF(KYVXL.NE.0) THEN
C
        NVX    = LROWS(KYVXL)
        IF(NVX.GT.0) THEN
        DO 5 IVX = 1,NVX
          JYVXL = KROW ( KYVXL,IVX)
          IVT   = IW(JYVXL+JYVXVT)
          IF( IVT.EQ.0) THEN
           VERT ( 1  ) = RW ( JYVXL + JYVXXP )
           VERT ( 2  ) = RW ( JYVXL + JYVXYP )
           VERT ( 3  ) = RW ( JYVXL + JYVXZP )
          ENDIF
  5     CONTINUE
        ENDIF
      ENDIF
C
C
C Get storey baricenter
C
      CALL ESRBC ('ALEPH',ITHE,IPHI,IST,POINT)
      RHO = 0.
      DO 10 I =1,3
      RHO = RHO + ( POINT(I)-VERT(I))**2
 10   CONTINUE
C
C Calculates direction cosines
C
      RHO = SQRT (RHO)
      DO 20 I =1,3
      EVECT (I) = ((POINT(I)-VERT(I))/RHO)*EN
 20   CONTINUE
      EVECT (4) = EN
      RETURN
      END
#endif
