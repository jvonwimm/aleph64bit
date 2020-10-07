      SUBROUTINE HCFITU(IPOR,IPL,YY,DY,ITUBF,NTUB,YPART,NTUSP,WSPAC)
C------------------------------------------------------------------
C
C! find the tubes intersected by the track element
C!       is usable also with DY=0. (shower)
C!
C!          Author   : G.Catanesi  86/12/05
C!          Modified : G.Catanesi  87/10/21,88/05/01
C!                     F.Ranjard   88/12/12
C!                     A.Venturi   98/7/1
C!             spacers taken into account in the determination
C!             of YMAX and of YY
C!
C!          input  :
C!                 - YY/R           : coordinate normal to the wire dir.
C!                                   (Plane R.S.)
C!                 - DY/R           : track element projection          ion
C!                 - IPOR/I         : portion number
C!                 - IPL/I          : plane number
C!          output :
C!                 - ITUBF/R        :  # of first tube hit
C!                 - NTUB/I         : # of tubes hit
C!                 - YPART(1:NTUB)  :  % of the element track in
C!                                      the tube  j
C!                 - NTUSP/I        : if=I the Ith tube hit (1:NTUB)is
C!                                    a spacer
C!                 - WSPAC/R        : width of the spacer found (if any)
C!
C!    calls     : HCTKSP  from this .HLB
C!
C -------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
      REAL YPART(*)
      INTEGER HCLATU
C --------------------------------------------------------------------
      NTUB = 0
      IDEAD = 0
      ITUBF = 0
      NTUSP = 0
      YPART(1) = 1.
C - set HCBM thickness of outer wall, HCSM tube width
      HCBM = HCDRBM(IPOR)
      HCSM = HCSAMP(IPOR)
C
C - return if track element in the outer wall
      IF (YY.LE.HCBM) RETURN
C
C verify if the boundaries are out of range
C
      IF ((YY-DY/2.).LT.0.) THEN
         DY = DY/2. + YY
         YY = DY/2.
      ENDIF
C
C Take in account spacers in barrel and iron frames in End-Cap
      IF (ICHCJO(3).EQ.0) THEN
         YYY = YY
         DYY = DY
         CALL HCTKSP(IPOR,IPL,YYY,DYY,NSP,NTUSP,IDEAD)
         YY = YYY
         DY = DYY
         IF(IDEAD.EQ.1) GOTO 50
      ENDIF
C - set lower and upper edge of the redefined track element
C   (YY and DY can be modified by HCTKSP)
      YMIN = MAX (YY-DY/2. - HCBM , 0.)
      YMAX = MAX (YY+DY/2. - HCBM , 0.)
C.
C - compute 1st tube hit, RETURN if out of range
      LASTU = HCLATU(IPOR,IPL)
      ITUBF = INT(YMIN/HCSM)+1
      IF (ITUBF .GT. LASTU) GOTO 50
C
C - compute last tube hit, take care of spacer if any, limit last tube
C   to the maximum # of tubes in the layer
      IF (NTUSP.EQ.0)THEN
         ITUBL = INT(YMAX/HCSM)+1
      ELSE
         WSPAC = HCDPSP(IPOR,IPL,NSP)
         ITUBL = INT((YMAX - WSPAC)/HCSM)+1
         DY = DY - WSPAC
         YMAX=YMAX-WSPAC
         YY = YY -WSPAC/2.
      ENDIF
      ITUBL = MIN (ITUBL,LASTU)
C.
C - set total # of tubes hit
      NTUB = ITUBL-ITUBF+1
C
C - RETURN if only 1 tube hit or if point element (shower)
      IF (NTUB.LE.1 .OR. DY.LE.1.E-6) GOTO 50
C
C - determine which part of the track element goes through each tube
      YMOD = MOD(YMAX,HCSM)
      IF(YMOD.GT.DY.AND.NTUB.EQ.2) THEN
       YMOD=0.
      ENDIF
      YPERC = YMOD/DY
      YPART(NTUB)=YPERC
      YPART(1) = 1. - YPERC
      IF (NTUB.GT.2) THEN
         DO 10 LL=2,NTUB-1
            YPART(LL)=HCSM/DY
   10    CONTINUE
         YPART(1) = 1.-YPART(NTUB)-(NTUB-2)*HCSM/DY
      ENDIF
      DO I=1,NTUB
       IF(YPART(I).LT.0..OR.YPART(I).GT.1.) THEN
        DO J=1,NTUB
          YPART(J)=0.
        ENDDO
        YPART(1)=1.
       ENDIF
      ENDDO
C     add the width of the spacers to the local coordinate 
C     (it was removed by HCTKSP to make the tube determination
C      easier)
 50   CONTINUE
C
      DO 60 I=1,NSP-1
        YY=YY+HCDPSP(IPOR,IPL,I)
 60   CONTINUE
C
      END
#endif
