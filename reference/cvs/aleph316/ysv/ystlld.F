      SUBROUTINE YSTLLD(IER)
C--------------------------------------------------------------------
C! Create YSTL bank and store information about tracks
C
C       Author: D. Casper  1-jul-1996
C   
C       Returns:  IER = 0 if tracks loaded
C                 IER = -1 if input bank(s) missing
C                 IER = 1 if no room in BOS
C
C       Input banks:    FRFT    Track fits in ITC/VDET
C                       FRTL    Coordinate information used to identify
C                               ITC-only tracks
C                       TGFT    Track parameters in TPC
C                       FTCL    Coordinate lists in TPC
C                       TPCO    Coordinates in TPC
C                       FICL    Coordinate lists in ITC
C                       ICCO    Coordinates in ITC
C                       FVCL    Coordinate lists in VDET
C                       VDCO    Coordinates in VDET
C
C       Output banks:   YSTL    TPC track list in internal format
C                       YSFT    Pointer from FRFT->YSTL (if positive)
C                               and YSCL (if negative).  
C
C--------------------------------------------------------------------
#ifndef DOC
C
C+  Maximum number of helices constrained by size of the bit mask
C
      PARAMETER (MAX_TRACKS = 96)
C
#include "bcs.h"
#include "tgftjj.h"
#include "frftjj.h"
#include "frtljj.h"
#include "ftcljj.h"
#include "tpcojj.h"
#include "ficljj.h"
#include "iccojj.h"
#include "fvcljj.h"
#include "vdcojj.h"
#include "ystljj.h"
#include "ysftjj.h"
C
      DIMENSION XC_IN(3),XC_OUT(3),P_IN(3),P_OUT(3)
      DATA NFRFT/0/
      SAVE NFRFT, NFRTL, NFTCL, NTPCO, NFICL, NICCO, NFVCL, NVDCO
      SAVE NTGFT
C
#include "bmacro.h"
C
      IER = -1
      IF (NFRFT .EQ. 0) THEN
        NFRFT = NAMIND('FRFT')
        NTGFT = NAMIND('TGFT')
        NFRTL = NAMIND('FRTL')
        NFVCL = NAMIND('FVCL')
        NVDCO = NAMIND('VDCO')
        NFICL = NAMIND('FICL')
        NICCO = NAMIND('ICCO')
        NFTCL = NAMIND('FTCL')
        NTPCO = NAMIND('TPCO')
        NYSTL = NAMIND('YSTL')
      ENDIF
C
C+  Drop old output banks
C
      CALL BDROP(IW,'YSTLYSFT')
C
C+  Find input banks
C
      KFRFT = IW(NFRFT)
      KFRTL = IW(NFRTL)
      KTGFT = IW(NTGFT)
      IF (KFRFT .LE. 0 .OR. KFRTL.LE.0) GOTO 999
C
C+  Create YSTL bank with maximum size
C
      CALL AUBOS('YSTL',0,LMHLEN+LYSTLA*LROWS(KFRFT),KYSTL,IGARB)
      IF (IGARB.EQ.1) THEN
        KFRFT = IW(NFRFT)
        KFRTL = IW(NFRTL)
        KTGFT = IW(NTGFT)
      ELSE IF (IGARB.EQ.2) THEN
        IER = 1
        GOTO 999
      ENDIF
      IW(KYSTL+LMHCOL) = LYSTLA
      IW(KYSTL+LMHROW) = 0
      CALL BLIST(IW,'T+','YSTL')
C
C+  Create YSFT bank to translate from FRFT to YSTL number
C
      CALL AUBOS('YSFT',0,LMHLEN+LYSFTA*LROWS(KFRFT),KYSFT,IGARB)
      IF (IGARB.EQ.1) THEN
        KFRFT = IW(NFRFT)
        KFRTL = IW(NFRTL)
        KTGFT = IW(NTGFT)
        KYSTL = IW(NYSTL)
      ELSE IF (IGARB.EQ.2) THEN
        IER = 1
        GOTO 999
      ENDIF
      IW(KYSFT+LMHCOL) = LYSFTA
      IW(KYSFT+LMHROW) = LROWS(KFRFT)
      CALL BLIST(IW,'T+','YSFT')
C
C+  If we have FRFT/2, also find FRFT/0 (which will temporarily be
C+  FRFT/3...the next bank)
C
      IF (IW(KFRFT-2).EQ.2 .AND. IW(KFRFT-1).GT.0) THEN
        KFRFT0 = IW(KFRFT-1)
      ELSE
        KFRFT0 = KFRFT
      ENDIF
C
C+ TGFT bank contains fit in TPC
C
      IF (KTGFT.LE.0) KTGFT = KFRFT0
C
      IER = 0
C
C+  Loop over tracks in FRFT
C
      DO I = 1, LROWS(KFRFT)
          IFRTL  = KROW(KFRTL,I)
C
C+  Is this a bonafide helix?
C
          NTPC = IW(IFRTL+JFRTNT)
          IF (NTPC.GE.4)THEN
            IYSTL = KNEXT(KYSTL)
C
C+  Clear quality flag for good track
C
            IW(IYSTL+JYSTQF) = 0
C
C+  Possible cuts here
C
            CONTINUE
C
C+  Check for maximum number of charged tracks
C
            IF (IW(KYSTL+LMHROW) .GE. MAX_TRACKS)
     &          IW(IYSTL+JYSTQF) = IOR(IW(IYSTL+JYSTQF),1)
C
C+  Lookup track in fit banks
C
            IFRFT  = KROW(KFRFT,I)
            IFRFT0 = KROW(KFRFT0,I)
            IF (I.LE.LROWS(KTGFT)) THEN
              ITGFT = KROW(KTGFT,I)
            ELSE
              ITGFT = 0
            ENDIF
C
C+  Find the radii of the inner- and outer-most coordinate on this track
C+  Make no assumptions about the ordering of hits.  First find the right
C+  places to look
C
            K_IN = 0
            K_OUT = 0
            IF (IW(IFRTL+JFRTNV).GT.0) THEN
              IF (IW(NFVCL).EQ.0 .OR. IW(NVDCO).EQ.0) GOTO 999
              J_IN = JVDCR0
              JP_IN = JVDCPH
              JZ_IN = JVDCZ0
              JSZ_IN = JVDCSZ
              K_IN = IW(NVDCO)
              L_IN = IW(NFVCL) + LMHLEN + IW(IFRTL+JFRTIV) + JFVCIV
              N_IN = IW(IFRTL+JFRTNV)
            ENDIF
            IF (IW(IFRTL+JFRTNI).GT.0) THEN
              IF (IW(NFICL).EQ.0 .OR. IW(NICCO).EQ.0) GOTO 999
              IF (K_IN .EQ. 0) THEN
                  J_IN = JICCRV
                  JP_IN = JICCPH
                  JZ_IN = JICCZV
                  JSZ_IN = JICCSZ
                  K_IN = IW(NICCO)
                  L_IN = IW(NFICL) + LMHLEN + IW(IFRTL+JFRTII) + JFICII
                  N_IN = IW(IFRTL+JFRTNI)
              ENDIF
              J_OUT = JICCRV
              JP_OUT = JICCPH
              JZ_OUT = JICCZV
              JSZ_OUT = JICCSZ
              K_OUT = IW(NICCO)
              L_OUT = IW(NFICL) + LMHLEN + IW(IFRTL+JFRTII) + JFICII
              N_OUT = IW(IFRTL+JFRTNI)
            ENDIF
            IF (IW(IFRTL+JFRTNT).GT.0) THEN
              IF (IW(NFTCL).EQ.0 .OR. IW(NTPCO).EQ.0) GOTO 999
              IF (K_IN .EQ. 0) THEN
                  J_IN = JTPCRV
                  JP_IN = JTPCPH
                  JZ_IN = JTPCZV
                  JSZ_IN = JTPCSZ
                  K_IN = IW(NTPCO)
                  L_IN = IW(NFTCL) + LMHLEN + IW(IFRTL+JFRTIT) + JFTCIT
                  N_IN = IW(IFRTL+JFRTNT)
              ENDIF
              J_OUT = JTPCRV
              JP_OUT = JTPCPH
              JZ_OUT = JTPCZV
              JSZ_OUT= JTPCSZ
              K_OUT = IW(NTPCO)
              L_OUT = IW(NFTCL) + LMHLEN + IW(IFRTL+JFRTIT) + JFTCIT
              N_OUT = IW(IFRTL+JFRTNT)
            ENDIF
            IF (K_IN.EQ.0 .OR. K_OUT.EQ.0) THEN
              R_IN  = 0.
              R_OUT = 0.
              IW(IYSTL+JYSTQF) = IOR(IW(IYSTL+JYSTQF),2)
            ELSE
C
C+  Find innermost coordinate
C
              R_IN = RTABL(K_IN,IABS(IW(L_IN)),J_IN)
              M_IN = IABS(IW(L_IN))
              DO I_IN = 2, N_IN
                ICOORD = IABS(IW(L_IN + I_IN - 1))
                RCOORD = RTABL(K_IN,ICOORD,J_IN)
                IF (RCOORD.LT.R_IN) THEN
                  R_IN = RCOORD
                  M_IN = IABS(ICOORD)
                ENDIF
              ENDDO
              XC_IN(1) = R_IN*COS(RTABL(K_IN,M_IN,JP_IN))
              XC_IN(2) = R_IN*SIN(RTABL(K_IN,M_IN,JP_IN))
              XC_IN(3) = RTABL(K_IN,M_IN,JZ_IN)
              SZ_IN    = RTABL(K_IN,M_IN,JSZ_IN)
C
C+  Find outermost coordinate
C
              R_OUT = RTABL(K_OUT,IABS(IW(L_OUT)),J_OUT)
              M_OUT = IABS(IW(L_OUT))
              DO I_OUT = 2, N_OUT
                ICOORD = IABS(IW(L_OUT + I_OUT - 1))
                RCOORD = RTABL(K_OUT,ICOORD,J_OUT)
                IF (RCOORD.GT.R_OUT) THEN
                  R_OUT = RCOORD
                  M_OUT = ICOORD
                ENDIF
              ENDDO
              XC_OUT(1) = R_OUT*COS(RTABL(K_OUT,M_OUT,JP_OUT))
              XC_OUT(2) = R_OUT*SIN(RTABL(K_OUT,M_OUT,JP_OUT))
              XC_OUT(3) = RTABL(K_OUT,M_OUT,JZ_OUT)
              SZ_OUT    = RTABL(K_OUT,M_OUT,JSZ_OUT)
            ENDIF
C
C+ Possible cuts here
C
            CONTINUE
C
C+ Add track to list
C
            IW(KYSTL+LMHROW) = IW(KYSTL+LMHROW) + 1
            IW(KROW(KYSFT,I)+JYSFTN) = IW(KYSTL+LMHROW)
C
C+  Set FRFT track number
C
            IW(IYSTL+JYSTTN) = I
C
C+ Helix parameters and errors at origin (FRFT/2)
C
            CALL UCOPY(RW(IFRFT+JFRFIR),RW(IYSTL+JYSTH0),5)
            CALL UCOPY(RW(IFRFT+JFRFEM),RW(IYSTL+JYSTE0),15)
            RW(IYSTL+JYSTC0) = RW(IFRFT+JFRFC2)
            IW(IYSTL+JYSTN0) = IW(IFRFT+JFRFDF)
C+ Helix parameters and errors in ITC (FRFT/0)
            CALL UCOPY(RW(IFRFT0+JFRFIR),RW(IYSTL+JYSTH1),5)
            CALL UCOPY(RW(IFRFT0+JFRFEM),RW(IYSTL+JYSTE1),15)
            RW(IYSTL+JYSTC1) = RW(IFRFT0+JFRFC2)
            IW(IYSTL+JYSTN1) = IW(IFRFT0+JFRFDF)
C+ Helix parameters in TPC (TGFT/0)
            IF (ITGFT.NE.0) THEN
              CALL UCOPY(RW(ITGFT+JTGFIR),RW(IYSTL+JYSTH2),5)
              CALL UCOPY(RW(ITGFT+JTGFEM),RW(IYSTL+JYSTE2),15)
              RW(IYSTL+JYSTC2) = RW(ITGFT+JTGFCD)
              IW(IYSTL+JYSTN2) = IW(ITGFT+JTGFDF)
            ELSE
              CALL UCOPY(RW(IFRFT0+JFRFIR),RW(IYSTL+JYSTH2),5)
              CALL UCOPY(RW(IFRFT0+JFRFEM),RW(IYSTL+JYSTE2),15)
              RW(IYSTL+JYSTC2) = RW(IFRFT0+JFRFC2)
              IW(IYSTL+JYSTN2) = IW(IFRFT0+JFRFDF)
            ENDIF
C+ Number of hits in each subdetector 
            IW(IYSTL+JYSTNV) = IW(IFRTL+JFRTNV)
            IW(IYSTL+JYSTNI) = IW(IFRTL+JFRTNI)
            IW(IYSTL+JYSTNT) = IW(IFRTL+JFRTNT)
C+ Turning angle to min and max hits
            IF (IW(IYSTL+JYSTQF).EQ.0) THEN
              CALL YHELIX(I,XC_IN,SZ_IN,PSI_IN,P_IN,IER)
              IF (IER.NE.0) RETURN
              CALL YHELIX(I,XC_OUT,SZ_OUT,PSI_OUT,P_OUT,IER)
              IF (IER.NE.0) RETURN
              RW(IYSTL+JYSTRI) = PSI_IN+ABS(RW(IFRFT0+JFRFIR))
              RW(IYSTL+JYSTRO) = PSI_OUT-ABS(RW(IFRFT0+JFRFIR))
            ENDIF
          ENDIF
      ENDDO
C
 999  CONTINUE
C  Resize the output bank, making sure it exists even if it has zero rows
      IF (LROWS(KYSTL).GT.0) THEN
        CALL AUBPRS('YSTL')
      ELSE
        CALL AUBOS('YSTL',0,LMHLEN,KYSTL,IGARB)
      ENDIF
      RETURN
      END
#endif
