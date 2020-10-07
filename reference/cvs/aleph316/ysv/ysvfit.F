        SUBROUTINE YSVFIT(FULL_FIT,NR,IER,IGARB)
C-----------------------------------------------------------------------------
C!  Perform fit using vertex, track, and V0 indices in work banks.
C
C       Author: D.Casper    04/07/96
C
C       Modified:           08/08/97  Do not check for V0s or ITC tracks
C                                     in the kink veto
C
C       FULL_FIT = sets YTOP mode
C       NR  = number of YSVX bank to put fit in
C       IER = -1 out of space in BOS
C           = 0 fit succeeded
C           = 1 fit failed, or vertex/track overlap
C       IGARB = 1 if garbage collection occurred (quelle horreur...)
C
C       Input banks:    YSVX, YSTL, YSCL, YS0L, /ysvwrk/
C
C       Ouput banks:    YSVX    Extends YSVX if no more rows, writes a
C                               new row into the appropriate YSVX/NR if
C                               fit is successful:
C                               NR = - (vertex multiplicity)
C-----------------------------------------------------------------------------
#ifndef DOC
        PARAMETER (N_ROW_ALLOC = 10)
        PARAMETER (R_ITC = 12.80)
        PARAMETER (R_TPC = 28.50)
#include "ysvxjj.h"
#include "yscljj.h"
#include "ystljj.h"
#include "ys0ljj.h"
#include "ysvtjj.h"
#include "ysvwrk.h"
#include "bcs.h"
C
        LOGICAL VTX_GUESS,FULL_FIT
        DATA NYSVX /0/
        SAVE NYSVX, NYSTL, NYS0L, NYSCL, NYKNK
C
#include "bmacro.h"
C
        IF (NYSVX.LE.0) THEN
            NYSVX = NAMIND('YSVX')
            NYSTL = NAMIND('YSTL')
            NYS0L = NAMIND('YS0L')
            NYSCL = NAMIND('YSCL')
            NYKNK = NAMIND('YKNK')
        ENDIF
        IGARB = 0
C
C+ Check whether any of the tracks in the list to fit are also included
C+ in the vertices
C
        IF (LROWS(INDYSTM).GT.0 .AND. LROWS(INDYSTW).GT.0) THEN
          DO IWRD = 1, 3
            IF (IAND(IW(KROW(INDYSTM,LROWS(INDYSTM))+IWRD),
     &              IW(KROW(INDYSTW,LROWS(INDYSTW))+IWRD)) .NE. 0) THEN
                IER = 1
                RETURN
            ENDIF
          ENDDO
        ENDIF
C
C+  And ITC tracks
C
        IF (LROWS(INDYSCM).GT.0 .AND. LROWS(INDYSTW).GT.0) THEN
            IF (IAND(IW(KROW(INDYSCM,LROWS(INDYSCM))+1),
     &              IW(KROW(INDYSCW,LROWS(INDYSCW))+1)) .NE. 0) THEN
                IER = 1
                RETURN
            ENDIF
        ENDIF
C
C+ Do similar check for V0s
C
        IF (LROWS(INDYS0M).GT.0 .AND. LROWS(INDYS0W).GT.0) THEN
            IF (IAND(IW(KROW(INDYS0M,LROWS(INDYS0M))+1),
     &              IW(KROW(INDYS0W,LROWS(INDYS0W))+1)).NE.0) THEN
                IER = 1
                RETURN
            ENDIF
        ENDIF
C
C+  Check whether this vertex includes a kink
C
        KYKNK = IW(NYKNK)
        IF (KYKNK.NE.0 .AND. INDYSKL.NE.0) THEN
            DO IK = 1, LROWS(INDYSKL)
                NTKNK = 0
                DO IWRD = 1, 3
                    NTKNK = NTKNK + NUMBIT(
     &                  IAND(IW(KROW(INDYSKL,IK)+IWRD),
     &                  IW(KROW(INDYSTM,LROWS(INDYSTM))+IWRD)))
                ENDDO
                IF (NTKNK.GE.2) THEN
                    IER = 1
                    RETURN
                ENDIF
            ENDDO 
        ENDIF
C
C+ Lookup input banks
C
        KYSTL = IW(NYSTL)
        KYSCL = IW(NYSCL)
        KYS0L = IW(NYS0L)
C
C+ Find output bank YSVX and extend if needed
C
        KYSVX = NLINK('YSVX',NR)
        IF (KYSVX.EQ.0) THEN
            CALL AUBOS('YSVX',NR,LMHLEN+N_ROW_ALLOC*LYSVXA,
     &              KYSVX,IGARB)
            IF (IGARB.EQ.2) THEN
                IER = -1
                GOTO 999
            ENDIF
            IW(KYSVX+LMHCOL) = LYSVXA
            IW(KYSVX+LMHROW) = 0
        ELSE IF (LFRROW(KYSVX).LE.0) THEN
C
C+ If we extend, all hell breaks loose, because the bank is moved; since this
C+ is almost as difficult to deal with as a full garbage collection, make a
C+ virtue of necessity by doing one to clean up the old bank.
C
            CALL AUBOS('YSVX',NR,IW(KYSVX)+N_ROW_ALLOC*LYSVXA,
     &              KYSVX,IGARB)
            IF (IGARB.EQ.2) THEN
                IER = -1
                GOTO 999
            ENDIF
            IF (IGARB.NE.1) THEN
                CALL BGARB(IW)
                IGARB = 1
            ENDIF
        ENDIF
        IF (IGARB.EQ.1) THEN
            KYSTL = IW(NYSTL)
            KYSCL = IW(NYSCL)
            KYS0L = IW(NYS0L)
            KYSVX = NLINK('YSVX',NR)
        ENDIF
C
C+  Fit
C
        IYSVX = KNEXT(KYSVX)
        IF (LROWS(INDYSVW).GT.0)THEN
            VTX_GUESS =.FALSE.
        ELSE
            VTX_GUESS =.TRUE.
        ENDIF
        CALL YFTVTC(LROWS(INDYSVW),LROWS(INDYSTL),LROWS(INDYS0L),
     &          LROWS(INDYSCL),
     &          VTX_GUESS,RW(INDYSVW+LMHLEN+1),RW(INDYSEW+LMHLEN+1),
     &          IW(INDYSTL+LMHLEN+1),LCOLS(KYSTL),LCOLS(KYSTL),
     &          RW(KROW(KYSTL,1)+JYSTH0),RW(KROW(KYSTL,1)+JYSTE0),
     &          IW(INDYS0L+LMHLEN+1),LCOLS(KYS0L),LCOLS(KYS0L),
     &          RW(KROW(KYS0L,1)+JYS0H0),RW(KROW(KYS0L,1)+JYS0E0),
     &          IW(INDYSCL+LMHLEN+1),LCOLS(KYSCL),LCOLS(KYSCL),
     &          RW(KROW(KYSCL,1)+JYSCH0),RW(KROW(KYSCL,1)+JYSCE0),
     &          RW(IYSVX+JYSVVX),RW(IYSVX+JYSVVM),RW(IYSVX+JYSVC2),
     &          IFAIL)
        IF (IFAIL.EQ.0) THEN
          RVX = RW(IYSVX+JYSVVX)**2 + RW(IYSVX+JYSVVY)**2
          IF (RVX.LE.R_ITC**2) THEN
              JHELIX = JYSTH0
              JERROR = JYSTE0
          ELSE IF (RVX.LE.R_TPC**2) THEN
              JHELIX = JYSTH1
              JERROR = JYSTE1
          ELSE
              JHELIX = JYSTH2
              JERROR = JYSTE2
          ENDIF
          IF (FULL_FIT .AND. LROWS(INDYSTL)+LROWS(INDYS0L).LE.10) THEN
            CALL YFMVTR(LROWS(INDYSVW),LROWS(INDYSTL),LROWS(INDYS0L),
     &          VTX_GUESS,.FALSE.,.FALSE.,.FALSE.,
     &          RW(INDYSVW+LMHLEN+1),RW(INDYSEW+LMHLEN+1),
     &          IW(INDYSTL+LMHLEN+1),LCOLS(KYSTL),LCOLS(KYSTL),
     &          RW(KROW(KYSTL,1)+JHELIX),RW(KROW(KYSTL,1)+JERROR),
     &          IW(INDYS0L+LMHLEN+1),LCOLS(KYS0L),LCOLS(KYS0L),
     &          RW(KROW(KYS0L,1)+JYS0H0),RW(KROW(KYS0L,1)+JYS0E0),
     &          0,AMPC,
     &          RW(IYSVX+JYSVVX),RW(IYSVX+JYSVVM),RW(INDHXOU+LMHLEN+1),
     &          RW(INDVHOU+LMHLEN+1),RW(INDHXNU+LMHLEN+1),
     &          RW(INDVHNU+LMHLEN+1),PSUM,VPSUM,VPSVX,AMASS,DMASS,VMVX,
     &          VMPS,RW(IYSVX+JYSVC2),IFAIL)
          ELSE
            CALL YFTVTC(LROWS(INDYSVW),LROWS(INDYSTL),LROWS(INDYS0L),
     &          LROWS(INDYSCL),
     &          VTX_GUESS,RW(INDYSVW+LMHLEN+1),RW(INDYSEW+LMHLEN+1),
     &          IW(INDYSTL+LMHLEN+1),LCOLS(KYSTL),LCOLS(KYSTL),
     &          RW(KROW(KYSTL,1)+JHELIX),RW(KROW(KYSTL,1)+JERROR),
     &          IW(INDYS0L+LMHLEN+1),LCOLS(KYS0L),LCOLS(KYS0L),
     &          RW(KROW(KYS0L,1)+JYS0H0),RW(KROW(KYS0L,1)+JYS0E0),
     &          IW(INDYSCL+LMHLEN+1),LCOLS(KYSCL),LCOLS(KYSCL),
     &          RW(KROW(KYSCL,1)+JYSCH0),RW(KROW(KYSCL,1)+JYSCE0),
     &          RW(IYSVX+JYSVVX),RW(IYSVX+JYSVVM),RW(IYSVX+JYSVC2),
     &          IFAIL)
          ENDIF
        ENDIF
        IF (IFAIL.NE.0) THEN
          IER = 1
          GOTO 999
        ENDIF
C
C+  Cuts here?
C
        CONTINUE
C
C+  Add vertex to list
C
        IW(KYSVX+LMHROW) = IW(KYSVX+LMHROW) + 1
C
C+  Get track masks
C
        CALL VZERO(IW(IYSVX+JYSVTM),5)
C+  Tracks in the vertices fit
        IF (LROWS(INDYSTW).GT.0) THEN
            DO IWRD = 1, 4
                IW(IYSVX+JYSVTM+IWRD-1) =
     &              IOR(IW(IYSVX+JYSVTM+IWRD-1),
     &                  IW(KROW(INDYSTW,LROWS(INDYSTW))+IWRD))
            ENDDO
        ENDIF
C+  Tracks in the track fit list
        IF (LROWS(INDYSTM).GT.0) THEN
            DO IWRD = 1, 3
                IW(IYSVX+JYSVTM+IWRD-1) =
     &              IOR(IW(IYSVX+JYSVTM+IWRD-1),
     &                  IW(KROW(INDYSTM,LROWS(INDYSTM))+IWRD))
            ENDDO
        ENDIF
C+  ITC tracks in the vertices fit
        IF (LROWS(INDYSCW).GT.0) THEN
            IW(IYSVX+JYSVCM) =
     &              IOR(IW(IYSVX+JYSVCM),
     &                  IW(KROW(INDYSCW,LROWS(INDYSCW))+1))
        ENDIF
C+  ITC track in the circle fit list
        IF (LROWS(INDYSCM).GT.0) THEN
            IW(IYSVX+JYSVCM) =
     &              IOR(IW(IYSVX+JYSVCM),
     &                  IW(KROW(INDYSCM,LROWS(INDYSCM))+1))
        ENDIF
C+  V0s in the vertices fit
        IF (LROWS(INDYS0W).GT.0) IW(IYSVX+JYSV0M) = 
     &              IOR(IW(IYSVX+JYSV0M),
     &                  IW(KROW(INDYS0W,LROWS(INDYS0W))+1))
C+  V0s in the fit list
        IF (LROWS(INDYS0M).GT.0) IW(IYSVX+JYSV0M) =
     &              IOR(IW(IYSVX+JYSV0M),
     &                  IW(KROW(INDYS0M,LROWS(INDYS0M))+1))
C+  Clear vertex quality flag!!!!
        IW(IYSVX+JYSVQU) = 0
C
        IER = 0
 999    CONTINUE
        RETURN
        END
#endif
