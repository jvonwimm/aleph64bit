      SUBROUTINE YSCLLD(IER)
C--------------------------------------------------------------------
C! Create YSCL bank and store information about ITC tracks
C
C       Author: D. Casper  10-jul-1996
C   
C       Returns:  IER = 0 if tracks loaded
C                 IER = -1 if input bank(s) missing
C                 IER = 1 if no room in BOS
C
C       Input banks:    FRFT    Track list
C                       FRTL    Coordinate information used to identify
C                               ITC-only tracks ("circles")
C                       FICL    ITC coordinate list
C                       ICCO    ITC coordinates
C                       FTCL    TPC coordinate list
C                       TPCO    TPC coordinates
C
C       Output bank:    YSCL    Internal list of ITC-only tracks
C
C       Modifies:       YSFT    Lookup from FRFT row to YSCL row;
C                               ITC-only tracks are negative numbers.
C
C--------------------------------------------------------------------
#ifndef DOC
C
C+  Maximum number of helices constrained by size of the bit mask
C
      PARAMETER (MAX_TRACKS = 32)
C
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "ficljj.h"
#include "iccojj.h"
#include "tpcojj.h"
#include "ftcljj.h"
#include "yscljj.h"
#include "ysftjj.h"
C
      REAL P_IN(3),P_OUT(3),XC_IN(3),XC_OUT(3)
C
      DATA NFRFT/0/
      SAVE NFRFT, NFRTL, NFICL, NICCO, NFTCL, NTPCO, NYSFT
C
#include "bmacro.h"
C
      IER = -1
      IF (NFRFT .EQ. 0) THEN
        NFRFT = NAMIND('FRFT')
        NFRTL = NAMIND('FRTL')
        NFICL = NAMIND('FICL')
        NICCO = NAMIND('ICCO')
        NTPCO = NAMIND('TPCO')
        NFTCL = NAMIND('FTCL')
        NYSFT = NAMIND('YSFT')
      ENDIF
C
C+  Drop old output bank
C
      CALL BDROP(IW,'YSCL')
C
C+  Find input banks
C
      KFRFT = IW(NFRFT)
      KFRTL = IW(NFRTL)
      KYSFT = IW(NYSFT)
      IF (KFRFT .LE. 0 .OR. KFRTL.LE.0 .OR. KYSFT.EQ.0) GOTO 999
C
C+  Create YSCL bank with maximum size
C
      CALL AUBOS('YSCL',0,LMHLEN+LYSCLA*LROWS(KFRFT),KYSCL,IGARB)
      IF (IGARB.EQ.1) THEN
        KFRFT = IW(NFRFT)
        KFRTL = IW(NFRTL)
        KYSFT = IW(NYSFT)
      ELSE IF (IGARB.EQ.2) THEN
        IER = 1
        GOTO 999
      ENDIF
      IW(KYSCL+LMHCOL) = LYSCLA
      IW(KYSCL+LMHROW) = 0
      CALL BLIST(IW,'T+','YSCL')
      IER = 0
C
C+  Loop over tracks in FRFT
C
      DO I = 1, LROWS(KFRFT)
          IFRTL  = KROW(KFRTL,I)
C
C+  Is this an ITC track?
C
          NTPC = IW(IFRTL+JFRTNT)
          IF (NTPC.LT.4)THEN
            IYSCL = KNEXT(KYSCL)
C
C+  Clear quality flag for good track
C
            IW(IYSCL+JYSCQF) = 0
C
C+  Possible cuts here
C
            CONTINUE
C
C+  Check for maximum number of charged tracks
C
            IF (IW(KYSCL+LMHROW) .GE. MAX_TRACKS) THEN
                IW(IYSCL+JYSCQF) = IOR(IW(IYSCL+JYSCQF),1)
            ENDIF
C
C+  Lookup track in fit banks
C
            IFRFT  = KROW(KFRFT,I)
C
C+  Find the radii of the inner- and outer-most coordinate on this track
C+  Make no assumptions about the ordering of hits.  
C
            K_IN = 0
            K_OUT = 0
            IF (IW(IFRTL+JFRTNI).GT.0) THEN
              IF (IW(NFICL).EQ.0 .OR. IW(NICCO).EQ.0) THEN
                  GOTO 999
              ENDIF
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
              JSZ_IN = JICCSZ
              K_OUT = IW(NICCO)
              L_OUT = IW(NFICL) + LMHLEN + IW(IFRTL+JFRTII) + JFICII
              N_OUT = IW(IFRTL+JFRTNI)
            ENDIF
            IF (IW(IFRTL+JFRTNT).GT.0) THEN
              IF (IW(NFTCL).EQ.0 .OR. IW(NTPCO).EQ.0) THEN
                  GOTO 999
              ENDIF
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
              JSZ_OUT = JTPCSZ
              K_OUT = IW(NTPCO)
              L_OUT = IW(NFTCL) + LMHLEN + IW(IFRTL+JFRTIT) + JFTCIT
              N_OUT = IW(IFRTL+JFRTNT)
            ENDIF
            IF (K_IN.EQ.0 .OR. K_OUT.EQ.0) THEN
              R_IN  = 0.
              R_OUT = 0.
              N_IN = 0
              N_OUT = 0
              IW(IYSCL+JYSCQF) = IOR(IW(IYSCL+JYSCQF),2)
            ELSE
C
C+  Find innermost coordinate
C
              R_IN = RTABL(K_IN,IABS(IW(L_IN)),J_IN)
              PHI_IN = RTABL(K_IN,IABS(IW(L_IN)),JP_IN)
              Z_IN = RTABL(K_IN,IABS(IW(L_IN)),JZ_IN)
              SIGZ_IN = RTABL(K_IN,IABS(IW(L_IN)),JSZ_IN)
              DO I_IN = 2, N_IN
                ICOORD = IABS(IW(L_IN + I_IN - 1))
                RCOORD = RTABL(K_IN,ICOORD,J_IN)
                IF (RCOORD.LT.R_IN) THEN
                  R_IN = RCOORD
                  PHI_IN = RTABL(K_IN,ICOORD,JP_IN)
                  Z_IN = RTABL(K_IN,ICOORD,JZ_IN)
                  SIGZ_IN = RTABL(K_IN,ICOORD,JSZ_IN)
                ENDIF
              ENDDO
              XC_IN(1) = R_IN*COS(PHI_IN)
              XC_IN(2) = R_IN*SIN(PHI_IN)
              XC_IN(3) = Z_IN
C Bug fix M.Cattaneo 25-Aug-1997
C              XC_IN(3) = RTABL(K_IN,ICOORD,JZ_IN)
C
C+  Find outermost coordinate
C
              R_OUT = RTABL(K_OUT,IABS(IW(L_OUT)),J_OUT)
              PHI_OUT = RTABL(K_OUT,IABS(IW(L_OUT)),JP_OUT)
              Z_OUT = RTABL(K_OUT,IABS(IW(L_OUT)),JZ_OUT)
              SIGZ_OUT = RTABL(K_OUT,IABS(IW(L_OUT)),JSZ_OUT)
              DO I_OUT = 2, N_OUT
                ICOORD = IABS(IW(L_OUT + I_OUT - 1))
                RCOORD = RTABL(K_OUT,ICOORD,J_OUT)
                IF (RCOORD.GT.R_OUT) THEN
                  R_OUT = RCOORD
                  PHI_OUT = RTABL(K_OUT,ICOORD,JP_OUT)
                  Z_OUT = RTABL(K_OUT,ICOORD,JZ_OUT)
                  SIGZ_OUT = RTABL(K_OUT,ICOORD,JSZ_OUT)
                ENDIF
              ENDDO
              XC_OUT(1) = R_OUT*COS(PHI_OUT)
              XC_OUT(2) = R_OUT*SIN(PHI_OUT)
              XC_OUT(3) = Z_OUT
            ENDIF
C
C+ Possible cuts here
C
            CONTINUE
C
C+ Add track to list
C
            IW(KYSCL+LMHROW) = IW(KYSCL+LMHROW) + 1
C
C+  Set FRFT track number
C
            IW(IYSCL+JYSCTN) = I
            IW(KROW(KYSFT,I)+JYSFTN) = -IW(KYSCL+LMHROW)
C
C+ Helix parameters and errors 
C
            CALL UCOPY(RW(IFRFT+JFRFIR),RW(IYSCL+JYSCH0),5)
            CALL UCOPY(RW(IFRFT+JFRFEM),RW(IYSCL+JYSCE0),15)
            RW(IYSCL+JYSCC0) = RW(IFRFT+JFRFC2)
            IW(IYSCL+JYSCN0) = IW(IFRFT+JFRFDF)
C+ Number of hits in ITC
            IW(IYSCL+JYSCNI) = IW(IFRTL+JFRTNI)
C+ Turning angle to inner and outer coordinates
            IF (IW(IYSCL+JYSCQF).EQ.0) THEN
              CALL YHELIX(I,XC_IN,SIGZ_IN,PSI_IN,P_IN,IER)
              IF (IER.NE.0) RETURN
              CALL YHELIX(I,XC_OUT,SIGZ_OUT,PSI_OUT,P_OUT,IER)
              IF (IER.NE.0) RETURN
              RW(IYSCL+JYSCRI) = PSI_IN+ABS(RW(IFRFT+JFRFIR))
              RW(IYSCL+JYSCRO) = PSI_OUT-ABS(RW(IFRFT+JFRFIR))
            ENDIF
          ENDIF
      ENDDO
C
 999  CONTINUE
C  Resize the output bank, making sure it exists even if it has zero rows
      IF (LROWS(KYSCL).GT.0) THEN
        CALL AUBPRS('YSCL')
      ELSE
        CALL AUBOS('YSCL',0,LMHLEN,KYSCL,IGARB)
      ENDIF
      RETURN
      END
#endif
