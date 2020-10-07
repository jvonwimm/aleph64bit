      SUBROUTINE PMSKFI
C========================================================================
C!    Books & fills PMSK bank
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C?
C!======================================================================
#ifndef DOC
#include "pmskjj.h"
#include "emskjj.h"
#include "prtmjj.h"
#include "alcons.h"
#include "bcs.h"
      DIMENSION EVECT (4) , EVTE(4,3)
#include "bmacro.h"
C
C -     Define pointers
C
      NAEMSK = NAMIND('EMSK')
      NAPRTM = NAMIND('PRTM')
      KEMSK = IW (NAEMSK)
      IF ( KEMSK.EQ.0) THEN
        CALL RERROR('PMSKFI',1,'EMSK does not exist -> no PMSK built')
        GO TO 900
      ENDIF
C
C-    Create PMSK bank
C
      NEMSK = LROWS(KEMSK)
      NLENG = LMHLEN + NEMSK*LPMSKA
      CALL AUBOS ( 'PMSK',0,NLENG,KPMSK,IGARB)
      IF ( IGARB.EQ.2) GO TO 990
      CALL BLIST(IW,'E+','PMSK')
      IW (KPMSK+LMHROW) = NEMSK
      IW (KPMSK+LMHCOL) = LPMSKA
C
C-    Update pointers if needed
C
      IF ( IGARB.NE.0) THEN
           KEMSK = IW(NAEMSK)
           KPRTM = IW(NAPRTM)
      ENDIF
C
C-    Fill PMSK bank
C
      KPRTM = IW (NAPRTM)
      NTRAK = IW (KPRTM+LMHROW)
      IF (NTRAK.GT.0) THEN
C
C-    Loop on tracks
C
        DO 10 ITRAK = 1,NTRAK
        JPRTM = KROW(KPRTM,ITRAK)
        IF ( IW( JPRTM + JPRTMA ) .GT.0 ) THEN
          IEMSK = IW   ( JPRTM + JPRTMA )
          JEMSK = KROW ( KEMSK , IEMSK  )
          JPMSK = KROW ( KPMSK , IEMSK  )
C
C-    Loop on stacks
C
          DO 20 IK = 1,5
          IF(IK.EQ.1.OR.IK.EQ.3.OR.IK.EQ.4)THEN
            DO 8 IV = 1,4
            EVECT(IV) = 0.
 8          CONTINUE
          ENDIF
C
C-      Loop on regions
C
          DO 15 IL = 1,2
          IND = ( IL-1)*15+(IK-1)*3
          IF ( RW(JEMSK+IND+1) .GT. 0.) THEN
            ET = RW(JEMSK+IND+1)*SIN(RW(JEMSK+IND+2))
            EZ = RW(JEMSK+IND+1)*COS(RW(JEMSK+IND+2))
            EY = ET*SIN(RW(JEMSK+IND+3))
            EX = ET*COS(RW(JEMSK+IND+3))
            EVECT ( 1 ) =  EVECT(1) + EX
            EVECT ( 3 ) =  EVECT(3) + EZ
            EVECT ( 2 ) =  EVECT(2) + EY
            EVECT ( 4 ) =  EVECT(4) + RW(JEMSK+IND+1)
          ENDIF
 15     CONTINUE
C
        IF ( IK .EQ. 2) CALL UCOPY(EVECT(1),EVTE(1,1),4)
        IF ( IK .EQ. 3) CALL UCOPY(EVECT(1),EVTE(1,2),4)
        IF ( IK .EQ. 5) CALL UCOPY(EVECT(1),EVTE(1,3),4)
 20     CONTINUE
C
C-     Fill PMSK  , end track loop
C
C-     Fill total energy in mask
        IW ( JPMSK+1) = IW( JPRTM + JPRTCT)
        RW ( JPMSK+2) = EVTE(4,1)+EVTE(4,2)+EVTE(4,3)
        DO 30 IL =1,3
        IF (IL.NE.3) THEN
          IF(RW(JPMSK+2).GT.0.) THEN
C
C-     Fill Energy Fractions
            RW ( JPMSK+2+IL) = EVTE(4,IL)/RW(JPMSK+2)
          ENDIF
        ENDIF
        ET = SQRT (EVTE(1,IL)**2+EVTE(2,IL)**2)
        IF (ET .GT. 0.) THEN
C-     Fill theta & phi
          PHI = ATAN2(EVTE(2,IL),EVTE(1,IL))
          IF(PHI.LT.0.)    PHI =  PHI  + TWOPI
          IF(PHI.GT.TWOPI) PHI =  PHI  - TWOPI
          RW (JPMSK+4+(IL-1)*2+1) = ATAN2(ET,EVTE(3,IL))
          RW (JPMSK+4+(IL-1)*2+2) = PHI
        ENDIF
 30     CONTINUE
C
C-     Fill reference pointer to FRFT
C
        IW (JPMSK +11) = ITRAK
        ENDIF
 10     CONTINUE
C
C-     End of loop on tracks
C
      ENDIF
 900  RETURN
 990  CALL RERROR ('PMSKFI',2,'Problem in creating PMSK')
      RETURN
      END
#endif
