       SUBROUTINE VCORMP(IVIEW,JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  use bonding error list to correct electronics channels list
C
C - Joe Rothberg, August 1995
C                             mod 18 July 1996  for case 11 rphi
C                             mod 14 August 1996 for case 10 Z
C                             mod 11 June 1997   for case 8 rphi 
C
C - Input:
C         IVIEW  /I   view
C         JMOD   /I   Global module number
C - Output: IELCHP in COMMON     Channel map
C--------------------------------------------------------------
c        code = 0   No fault , strip is OK. Comment may be meaningful
c               1   Saturated
c               2   Dead
c               3   Noisy/hot
c               4   Pinhole disconnected
c               5   Pinhole alive
c               6   Shorted  (nb stop contains the addresses of other strip)
c               7   Unbonded
c               8   Dislocation +n (in units of 50 microns)
c               9   Dislocation -n
c              10   Dislocation odd +n
c              11   Dislocation odd -n
c              12   Dislocation even +n
c              13   Dislocation even -n
c                   Dislocation sign is sign of :
c                  (Readout channel to which the strip should be attached)
c                  - (readout channel to which the strip is actually attached)
c -------------------------------------------------------------------------
c     bond numbers :
c      view 1 : bond 1 between capacitance and first kapton
c                    2 between first kapton and first Si strip
c                    3 between first Si strip and second kapton strip
c                    4 between second kapton strip and second Si stripc
c
c      view 2 : bond 1 between capacitance and first wafer strip
c                    2 between wafer 1 and 2
c                    3 between wafer 2 and 3')
c ------------------------------------------------------------------------
C bonding error list
cc      INTEGER maxerr
cc      PARAMETER(maxerr=100)
cc      INTEGER numerr(2), ibnerr(2,maxerr,5)
cc      COMMON/bonder/numerr,ibnerr
C -----------------------------------------------------------------
c      ibnerr(iv,ie,1) = ivad1
c      ibnerr(iv,ie,2) = ivad2
c      ibnerr(iv,ie,3) = ivbond
c      ibnerr(iv,ie,4) = ivfault
c      ibnerr(iv,ie,5) = ivparam
C -----------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C ------------------------------------------------------------------
#include "velchn.h"
#include "vduefc.h"
#include "vbnder.h"
#include "vglobl.h"
C ---------------------------------------------------------------------
C Arguments
       INTEGER IVIEW, JMOD
C
C Local variables
       INTEGER IRET, ISMOD
       INTEGER ivad1, ivad2, ivview, ivbond, ivfault, ivparam
       INTEGER i,j, iv, ie, ia, iia, iw
       INTEGER ietemp1(3),ietemp2(3)
C
C Functions
       INTEGER VSMJMD
C -------------------------------------------
C
       DO ie = 1, numerr(JMOD,IVIEW)
         ivfault = ibnerr(JMOD,IVIEW,ie,4)
         ivbond  = ibnerr(JMOD,IVIEW,ie,3)
C -------------------------------------
C faults with no change to mapping
C -------------------------------------
         IF(ivfault.GE.1.AND.ivfault.LE.5 .OR. ivfault.EQ.7) THEN

C   r phi view
           IF(IVIEW .EQ. vviewp) THEN
C address range
            DO ia =ibnerr(JMOD,IVIEW,ie,1),ibnerr(JMOD,IVIEW,ie,2)
              IELCHP(ia,eflag) = 1
C     loop over affected wafers
              DO iw = ivbond,3
                 IELCHP(ia,3+iw) = ivfault
              ENDDO
C
            ENDDO
C  z view
           ELSE IF (IVIEW .EQ. vviewz) THEN
C address range
             DO ia =ibnerr(JMOD,IVIEW,ie,1),ibnerr(JMOD,IVIEW,ie,2)
              IELCHZ(ia,eflag) = 1
              IF(ivbond .EQ. 1 .OR. ivbond .EQ. 2) THEN
                DO iw = 1,3
                 IF(IELCHZ(ia,iw) .GE. 0)IELCHZ(ia,iw+3)=ivfault
                ENDDO
              ELSEIF(ivbond .EQ. 3 .OR. ivbond .EQ. 4) THEN
                DO iw =3,2,-1
                 IF(IELCHZ(ia,iw) .GE. 0) THEN
                   IELCHZ(ia,iw+3)=ivfault
                   GOTO 30
                 ENDIF
                ENDDO
 30             CONTINUE
              ENDIF
C
            ENDDO
C  views
          ENDIF
C -----------------------------------------
C fault is short
C -----------------------------------------
        ELSE IF(ivfault .EQ. 6) THEN
C   r phi view
           IF(IVIEW .EQ. vviewp) THEN
C two addresses are shorted
            DO iia =1,2
              ia = ibnerr(JMOD,IVIEW,ie,iia)
              IELCHP(ia,eflag) = 1
C
C     loop over affected wafers
              DO iw = ivbond,3
                 IELCHP(ia,iw)=IELCHP(ibnerr(JMOD,IVIEW,ie,1),iw)
                 IELCHP(ia,3+iw) = ivfault
              ENDDO
C
            ENDDO
C z view
           ELSEIF(IVIEW .EQ. vviewz) THEN
C address range
            DO iia = 1,2
              ia = ibnerr(JMOD,IVIEW,ie,iia)
              IELCHZ(ia,eflag) = 1
C
              IF(ivbond .EQ. 1 .OR. ivbond .EQ. 2) THEN
                DO iw = 1,3
                 IF(IELCHZ(ia,iw) .GE. 0) THEN
                    IELCHZ(ia,iw)=IELCHZ(ibnerr(JMOD,IVIEW,ie,1),iw)
                    IELCHZ(ia,iw+3)=ivfault
                 ENDIF
                ENDDO
              ELSEIF(ivbond .EQ. 3 .OR. ivbond .EQ. 4) THEN
                DO iw =3,2,-1
                 IF(IELCHZ(ia,iw) .GE. 0) THEN
                    IELCHZ(ia,iw)=IELCHZ(ibnerr(JMOD,IVIEW,ie,1),iw)
                   IELCHZ(ia,iw+3)=ivfault
                   GOTO 31
                 ENDIF
                ENDDO
 31             CONTINUE
              ENDIF
C
            ENDDO
C  view
          ENDIF
C ------------------------------------------------------
C added  11 June 1997
C ----------------------------------------------------
C fault is dislocation,  +n
C ----------------------------------------------------
        ELSE IF(ivfault .EQ. 8) THEN
        ivparam = ibnerr(JMOD,IVIEW,ie,5)
C -------------------
C   set ivparam=1   bank VMBE (mod 54) has a zero but should be 1
        ivparam = 1
C -------------------
C   r phi view
           IF(IVIEW .EQ. vviewp) THEN
C address range
            DO ia=ibnerr(JMOD,IVIEW,ie,1),ibnerr(JMOD,IVIEW,ie,2)
              IELCHP(ia,eflag) = 1
C
C     loop over affected wafers
              DO iw = ivbond,3
                 IELCHP(ia,iw) = IELCHP(ia+ivparam,iw)
                 IELCHP(ia,3+iw) = ivfault
              ENDDO
C
            ENDDO
           ENDIF
C ----------------------------------------------------
C ----------------------------------------------------
C fault is dislocation, odd +n
C ----------------------------------------------------
        ELSE IF(ivfault .EQ. 10) THEN
        ivparam = ibnerr(JMOD,IVIEW,ie,5)
C   r phi view
           IF(IVIEW .EQ. vviewp) THEN
C address range
            DO ia=ibnerr(JMOD,IVIEW,ie,1),ibnerr(JMOD,IVIEW,ie,2),2
              IELCHP(ia,eflag) = 1
C
C     loop over affected wafers
              DO iw = ivbond,3
                 IELCHP(ia,iw) = IELCHP(ia+ivparam,iw)
                 IELCHP(ia,3+iw) = ivfault
              ENDDO
C
            ENDDO
C
C z view, modified 14 August 1996, case 10 Z
           ELSEIF (IVIEW .EQ. vviewz .AND. ivbond .EQ. 1) THEN
C address range
            DO ia=ibnerr(JMOD,IVIEW,ie,1),ibnerr(JMOD,IVIEW,ie,2),2
              IELCHZ(ia,eflag) = 1
C
C     loop over affected wafers
              DO iw = ivbond,3
                 IF(IELCHZ(ia,iw) .GE. 0) THEN
                   IELCHZ(ia,iw) = IELCHZ(ia+ivparam,iw)
                   IELCHZ(ia,3+iw) = ivfault
                 ENDIF
              ENDDO
C
            ENDDO
C view
           ENDIF
C
C ---------------------------------------------------
C  modified 17 July 1996 for case 11
        ELSEIF(ivfault .EQ. 11) THEN
          ivparam = ibnerr(JMOD,IVIEW,ie,5)
C
C   r phi view
           IF(IVIEW .EQ. vviewp) THEN
C address range
           DO iw=1,3
            ietemp1(iw) = 0
           ENDDO
C
            DO ia=ibnerr(JMOD,IVIEW,ie,1),ibnerr(JMOD,IVIEW,ie,2),2
               IELCHP(ia,eflag) = 1
C
              IF(ia .LE. 1024) THEN
                DO iw = ivbond,3
                 ietemp2(iw) = IELCHP(ia,iw)
                 IELCHP(ia,iw) = ietemp1(iw)
                 ietemp1(iw) =  ietemp2(iw)

                 IELCHP(ia,3+iw) = ivfault
               ENDDO
C
              ENDIF
C
            ENDDO
C
         ELSEIF (IVIEW .EQ. vviewz) THEN
C
         ENDIF
C
C fault type
        ENDIF
C error
       ENDDO
C ---------------------------------------------------------------
      RETURN
      END
#endif
