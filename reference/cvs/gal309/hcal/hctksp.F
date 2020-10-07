*DK hctksp
      SUBROUTINE HCTKSP(IPOR,IPL,YY,DY,NSP,NTUSP,IDEAD)
C-------------------------------------------------------
C
C! Take into account the spacers inside the module
C! in the barrel , and the iron frames in the
C! end-caps
C!
C!        Author       : G.Catanesi   86/06/07
C!        Modified by  : F.Ranjard    88/12/12
C!        Modified by  : A.Venturi    98/07/01
C!                       consistency checks added
C!        input :
C!             - YY/R   :coordinate in the tube plane normal
C!                       to the wires
C!             - DY/R   :projection of the track element
C!                       normal to the wires
C!             - IPOR/I :portion number
C!             - IPL/I  :layer number
C!        output :
C!              - YY/R     : coordinate in the tube plane normal
C!                           minus the spacer width
C!              - DY/R     : width of the track element outside spacer
C!              - NTUSP/I  : Flag (if=I track element passed trougth
C!                                      the spacer I)
C!              - NSP/I    : # of spacers found
C!              - IDEAD/I  : =0  sensitive zone
C!                           =1  dead zone
C!
C!
C!        called by :HCFITU
C!        calls     : none
C ------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "hcpara.h"
      DIMENSION YEDGE(2)
C --------------------------------------------------------------------
C
C - set lower and upper edges of the track element
      YMIN = YY-DY/2.
      YMAX = YY+DY/2.
C
      IND = 0
      IDEAD = 0
      NSP = 0
C.
      IF(IPOR.EQ.LPBAR)THEN
         LOOP = LHCSP
      ELSE
         LOOP = LPHCT
      ENDIF
C
      DO 10 N=1,LOOP
C
         NSP = N
C - set spacer width WSPAC, upper edge of the 1st spacer DOW, lower
C   edge of the 2nd spacer SUP
         WSPAC = HCDPSP(IPOR,IPL,NSP)
         CALL HCDWSP (IPOR,IPL,NSP,YEDGE)
         DOW = YEDGE(1)
         SUP = YEDGE(2)
C
         IF (YMIN.GT.SUP .AND. YMAX.GE.(SUP+WSPAC)) GOTO 10
C
C .... track element inside the spacer: element is killed
C
         IF(YMIN.GT.SUP .AND. YMAX.LT.(SUP+WSPAC))THEN
            IDEAD = 1
            GOTO 999
         ENDIF
C
C .....track element between the spacers
C
         IF (YMIN.GE.DOW .AND. YMAX.LE.SUP) GOTO 20
C
C .... track element passing trough the spacer
C
         IF (YMIN.LE.SUP .AND. YMAX.GE.(SUP+WSPAC)) THEN
            NTUSP = INT((SUP-YMIN)/HCSAMP(IPOR))+2
            GOTO 20
         ENDIF
C
C ....lower edge of the track element is inside the spacer
C     reduce the track element length to the one outside the spacer
C
         IF (YMIN.LT.DOW .AND. YMAX.LE.SUP) THEN
            DY = YMAX - DOW
            YY = DOW + DY/2.
            GOTO 20
         ENDIF
C
C....upper edge of the track element is inside the spacer
C    reduce the track element length to the one outside the spacer
C
         IF (YMIN.GE.DOW .AND. YMAX.GT.SUP) THEN
            DY = SUP - YMIN
            YY = SUP - DY/2.
            GOTO 20
         ENDIF
C
   10 CONTINUE
   20 CONTINUE
C
C - Substract one spacer width from the YY position
C
C - Check if it is beyond all the spacers
      IF(N.EQ.LOOP+1) THEN
        IF(NSP.EQ.N) 
     +  WRITE(LOUTIO,*) ' HCTKSP: spacers inconsistency #1'
        IF(NTUSP.NE.0)
     +  WRITE(LOUTIO,*) ' HCTKSP: spacers inconsistency #2'
        NSP=N
      ENDIF
C
      DO 30 J=1,N-1
         YY = YY - HCDPSP(IPOR,IPL,J)
   30 CONTINUE
      IF(YY.LE.0.)IDEAD=1
C
  999 RETURN
      END
#endif
