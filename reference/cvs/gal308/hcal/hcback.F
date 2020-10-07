*DK hcback
      SUBROUTINE HCBACK(POS,KLEAV,VIN,XX,YY)
C-----------------------------------------------------
C
C! Determine spot coordinate in the plane which containing the spot
C! Transform spot coordinates from local frame to the Aleph frame
C!
C!  Author    :F.Romano   86/7/20
C!  Modified  :G.Catanesi 86/11/1,87/10/21,88/07/06
C!  Modified  :A.Sciaba`  98/07/01 : bug fix in the determination of xx
C!
C!          INPUT:
C!               - POS/R  : spot coordinates in the local frame
C!               - KLEAV/I: flag of the type spot type [0,2]
C!
C!          OUTPUT:
C!                - VIN/R   : spot coordinates in the Aleph frame
C!                - XX/R    : spot coordinate in the tube plane
C!                                                 along wires
C!                - YY/R    : spot coordinate  in the tube plane
C!                                  normal to the wires
C!       Called by : HCSHOW
C!    Calls     :HCRINV
C!
C ---------------------------------------------
#ifndef DOC
      SAVE
#include "agcons.h"
#include "jobcom.h"
#include "hcgega.h"
#include "hccong.h"
#include "hcloc.h"
C
      DIMENSION POS(*),VIN(*),SPOT(3)
C -----------------------------------------------------------
      UNO=1.
      IF(IHCPOR.EQ.LPBAR) THEN
C.....
C    Shower started in the barrel
C.....
         IF(MOD(IHCMOD,2).EQ.1) UNO=-1.
C....Transform coordinates from parametrization frame to module frame
         SPOT(1)=UNO*POS(2)
         SPOT(2)= POS(3)+RHBAMN
         SPOT(3)=UNO*POS(1)
         NROT = IHCMOD
         IF(KLEAV.EQ.1) THEN
            IUNO=1.
            IF(POS(2).GT.0.) IUNO=-1.
C.....
C    If spot is in a contiguous module, rotate coordinates in the
C    parametrization frame before the calculation of XX and YY.
C.....
            POS(3)=SPOT(2)
            YY = POS(2)*COS(PIBY6) + IUNO*POS(3)*SIN(PIBY6)
            ZZ = -IUNO*POS(2)*SIN(PIBY6) + POS(3)*COS(PIBY6)
            POS(2) = YY
            POS(3) = ZZ - RHBAMN
            IHCMOD= IHCMOD+2*IUNO
         ELSE
C...
Check if the spot is in the endcap
C...
            IF(KLEAV.EQ.2) GO TO 10
         ENDIF
C
         XX=POS(1)
         if (pos(2).le.0) xx = -pos(1)
         YY=POS(2)*UNO
         IF(YY.LE.0.)IHCMOD=IHCMOD+INT(UNO)
         IF(IHCMOD.GT.LPHCBM)IHCMOD=IHCMOD-LPHCBM
         IF(IHCMOD.LT.1)IHCMOD=IHCMOD+LPHCBM
         YY = ABS(YY)
         ZZ = POS(3)
      ELSE
C.....
C    Shower started in the endcap
C.....
         NROT = LPHCBM+1
         IF(IHCPOR.EQ.LPECB) THEN
            NROT = NROT+1
            UNO = -1
         ENDIF
C....In the endcap parametrization frame and module frame are equivalent
C....Only translate z-coordinate origin
         SPOT(1)= POS(1)
         SPOT(2)= POS(2)
         SPOT(3)= POS(3)+ZHECMN
      ENDIF
C
   10 CONTINUE
C
C.....
C    Transform from the module frame to the Aleph frame
C.....
      CALL HCRINV(NROT,SPOT,VIN)
C
      IF(IHCPOR.EQ.LPBAR.AND.KLEAV.NE.2) GOTO 20
C....
Calculate module number in the endcap and x-y coordinate in the tube pla
Care is taken for modules 2 and 5 where orientation of tubes depends on
C endcap
C....
      ALPHA=ATG(VIN(2),VIN(1))
      IHCMOD = INT(ALPHA/PIBY3)+1
      UX = 1.
      UY = 1.
      IF(IHCMOD.GT.3) UY=-UY
      IF(IHCMOD.GT. 1.AND. IHCMOD.LT.5) UX=-UX
C
      IF(IHCMOD.EQ. 2.OR. IHCMOD.EQ.5) THEN
         XX = -UNO*VIN(1)*COS(PIBY3)-VIN(2)*SIN(PIBY3)
         YY = -UNO*VIN(1)*SIN(PIBY3)+VIN(2)*COS(PIBY3)
      ELSE
         XX = VIN(1)
         YY = VIN(2)
      ENDIF
C
      XX = UX*XX
      YY = UY*YY
C
      IF(IHCPOR.NE.LPBAR) GOTO 20
      IF(VIN(3).GT.0.) THEN
         IHCPOR = LPECA
      ELSE
         IHCPOR = LPECB
      ENDIF
C
   20 CONTINUE
C....
C    Fill ILAY with the tube plane number
C....
      ZZ = POS(3) - HCTIRF(IHCPOR)
      IHCIPL = INT(ZZ/HCSMTH)+1
C
   30 CONTINUE
C
      RETURN
      END
#endif
