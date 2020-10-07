      SUBROUTINE ECRTR ( NETP1 , ISTAC )
C=======================================================================
C!    Calculates storeys crossed by extrapolated track
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C-    Input  :   NETP1        = Track number
C-    Output :   ISTAC ( 1 )  = Number of storeys crossed for the stack
C-                      2->5  = Encoded storey address
C-
C?
C!======================================================================
#ifndef DOC
#include "etp1jj.h"
#include "bcs.h"
#include "ectcom.h"
      DIMENSION ISTAC (5,5), XXX(8) , XXXM(8)
      DATA NPST / 73728 /  , NTMAX / 4 /
#include "bmacro.h"
C
C-    Initialize pointers
C
      CALL VZERO ( ISTAC,15 )
      IDEAD = 0
      IFLAG = 0
      ISTPR = 0
C
C-    Track initialization
C
      CALL EINITK ( NETP1 , IFLG , XXX(1))
C
C-    XXX = last TPC point
C-    Keep coordinates from extrapolated point
C
      DO 20 IDATA = 1,8
 20   XXXM ( IDATA ) = XXX(IDATA)
      IF ( IFLG.EQ.0 ) GO TO 70
      STEP = STPKEC
C
C-     Initialize helix parameters
C
      CALL EINHLX(STEP,1,XXX)
C
 30   CONTINUE
C
C-      Get next point on helix
C
      CALL EHLIX (XXX)
      IFLPR = IFLAG
C
C-      Find present storey address ISTAD
C
      CALL ECDIGI ( XXX , IFLAG , ISTAD , IROSE )
C
C-      Test if in active region
C-      IFLAG:  2 if in ECAL
C-              #2 not in Ecal sensitive region
C
      IF ( IFLAG.EQ. 2 ) THEN
C
C-      Test if storey idem to previous step
C
         IF ( ISTAD.NE.ISTPR ) THEN
            ISTPR = ISTAD
C
C-       Find stack number
C
            IK = ( ISTAD-1 ) /NPST  +1
            IF ( ISTAC (1,IK).LT.NTMAX ) THEN
                 ISTAC (1,IK) = ISTAC (1,IK) + 1
                 IL = ISTAC(1,IK) + 1
                 ISTAC (IL,IK ) = ISTAD
            ENDIF
         ENDIF
      ELSEIF ( IFLAG.EQ.1.AND.IDEAD.EQ.0) THEN
C-        In the dead region of the overlap
         IF ( IFLPR.EQ.0 ) THEN
            IDEAD = 1
 50         STEP = 0.5*STEP
            CALL EINHLX ( STEP , 1, XXXM )
 51         CALL EHLIX (XXX)
            CALL ECDIGI ( XXX,IFLAG,ISTAD,IROSE )
            IF ( IFLAG.EQ.0 ) THEN
               GO TO 51
            ELSEIF ( IFLAG.EQ.1) THEN
               IF(STEP.GT.STMNEC ) GO TO 50
               STEP = STPKEC
               CALL EINHLX (STEP,1,XXX)
            ENDIF
         ENDIF
       ENDIF
C-       Out of Ecal
         IF ( IFLAG.EQ.4 ) GO TO 70
C
         IF (IFLAG.EQ.0.AND.IFLPR.EQ.2) THEN
            GO TO 70
         ENDIF
         GO TO 30
 70      CONTINUE
C
        RETURN
C
        END
#endif
