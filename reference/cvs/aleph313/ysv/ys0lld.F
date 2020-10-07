      SUBROUTINE YS0LLD(IER)
C--------------------------------------------------------------------
C! Create YS0L bank and store information about V0S
C
C       Author: D. Casper  1-jul-1996
C   
C       Returns:  IER = 0 if V0'S loaded
C                 IER =-1 if missing bank
C                 IER = 1 if no room in BOS
C
C       Input banks:    YLV0 (if present)   List of high-purity V0's
C
C       Output bank:    YSV0                Internal V0 list
C
C       Modifies:       YSTL                Internal track list; tracks
C                                           on a V0 are flagged.
C
C--------------------------------------------------------------------
#ifndef DOC
C
C+  Maximum number of V0s constrained by size of bit mask
C
      PARAMETER (MAX_V0S = 32)
C
C+  Radii of various walls
C
      PARAMETER (R_TPC = 28.5, R_ITC = 12.8)
C
#include "bcs.h"
#include "ylv0jj.h"
#include "ys0ljj.h"
#include "ystljj.h"
#include "ysftjj.h"
C
      DIMENSION HXOUT(5,2), VHXOUT(15,2)
      DATA NYLV0/0/
      SAVE NYLV0, NYSTL, NYSFT
C
#include "bmacro.h"
C
      IF (NYLV0 .EQ. 0) THEN
        NYLV0 = NAMIND('YLV0')
        NYSTL = NAMIND('YSTL')
        NYSFT = NAMIND('YSFT')
      ENDIF
C
C+  Drop old output bank
C
      CALL BDROP(IW,'YS0L')
C
C+  Find input banks
C
      KYSTL = IW(NYSTL)
      KYSFT = IW(NYSFT)
      IF (KYSTL .LE. 0 .OR. KYSFT.LE.0) THEN
        IER = -1
        GOTO 999
      ENDIF
      KYLV0 = IW(NYLV0)
      IF (KYLV0 .EQ. 0) THEN
        LYS0L = LMHLEN
      ELSE
        LYS0L = LMHLEN + LYS0LA * LROWS(KYLV0)
      ENDIF
C
C+  Create output bank 
C
      CALL AUBOS('YS0L',0,LYS0L,KYS0L,IGARB)
      IF (IGARB.EQ.1) THEN
        KYSTL = IW(NYSTL)
        KYSFT = IW(NYSFT)
      ELSE IF (IGARB.EQ.2) THEN
        IER = 1
        GOTO 999
      ENDIF
      IW(KYS0L+LMHCOL) = LYS0LA
      IW(KYS0L+LMHROW) = 0
      CALL BLIST(IW,'T+','YS0L')
      IER = 0
C
C+  Return if no V0s exist
C
      IER = 0
      IF (KYLV0 .EQ. 0) RETURN
C
C+  Loop over V0s in YLV0
C
      DO I = 1, LROWS(KYLV0)
       IYLV0 = KROW(KYLV0,I)
       IYS0L = KNEXT(KYS0L)
       IW(IYS0L+JYS0K1) = ITABL(KYSFT,IW(IYLV0+JYLVK1),JYSFTN)
       IW(IYS0L+JYS0K2) = ITABL(KYSFT,IW(IYLV0+JYLVK2),JYSFTN)
       IF (IW(IYS0L+JYS0K1).GT.0 .AND. IW(IYS0L+JYS0K2).GT.0) THEN
        IW(KYS0L+LMHROW) = IW(KYS0L+LMHROW) + 1
C
C+  Check quality cuts
C
        IW(IYS0L+JYS0QF) = 0
        CONTINUE
C
C+  Check maximum number of V0s
C
        IF (IW(KYS0L+LMHROW).GE.MAX_V0S) THEN
            IW(IYS0L+JYS0QF) = IOR(IW(IYS0L+JYS0QF),1)
        ENDIF
C
C+  Find the radius of the vertex to determine which tracks to use
C
        R_VERTEX = SQRT(RTABL(KYLV0,I,JYLVVX)**2
     &                + RTABL(KYLV0,I,JYLVVY)**2)
        IF (R_VERTEX.GE.R_TPC) THEN
            JHXOFF = JYSTH2
            JHEOFF = JYSTE2
        ELSE IF (R_VERTEX.GE.R_ITC) THEN
            JHXOFF = JYSTH1
            JHEOFF = JYSTE1
        ELSE
            JHXOFF = JYSTH0
            JHEOFF = JYSTE0
        ENDIF
C
C+  Refit the V0 vertex and direction (ignore the mass)
C
        CALL YFMVTR(0,2,0,.FALSE.,.TRUE.,.FALSE.,.FALSE.,
     &      RW(IYLV0+JYLVVX),RW(IYLV0+JYLVVM),IW(IYS0L+JYS0K1),
     &      LCOLS(KYSTL),LCOLS(KYSTL),
     &      RW(KYSTL+LMHLEN+JHXOFF),RW(KYSTL+LMHLEN+JHEOFF),
     &      IXNU,NSNU,NSVNU,TNUI,VTNUI,0,AMPC,
     &      RW(IYS0L+JYS0VX),RW(IYS0L+JYS0VM),HXOUT,VHXOUT,
     &      TNUOUT,VTNUOUT,RW(IYS0L+JYS0PX),RW(IYS0L+JYS0PM),
     &      RW(IYS0L+JYS0PV),AMASS,DMASS,VMVX,VMPS,RW(IYS0L+JYS0C2),
     &      IFAIL)
        IF (IFAIL.NE.0) THEN
            IW(IYS0L+JYS0QF) = IOR(IW(IYS0L+JYS0QF),2)
        ELSE
C
C+  Get pseudo-track parameters and errors for V0
C
            CALL YTPAR(0,RW(IYS0L+JYS0VX),RW(IYS0L+JYS0VM),
     &          RW(IYS0L+JYS0PX),RW(IYS0L+JYS0PM),RW(IYS0L+JYS0PV),
     &          RW(IYS0L+JYS0H0),RW(IYS0L+JYS0E0),IFAIL)
            IF (IFAIL.NE.0) THEN
                IW(IYS0L+JYS0QF) = IOR(IW(IYS0L+JYS0QF),4)
            ENDIF
        ENDIF
C
C+  Set V0 flag on tracks
C
        IW(KROW(KYSTL,IW(IYS0L+JYS0K1))+JYSTQF) = 
     &      IOR(IW(KROW(KYSTL,IW(IYS0L+JYS0K1))+JYSTQF),4)
        IW(KROW(KYSTL,IW(IYS0L+JYS0K2))+JYSTQF) =
     &      IOR(IW(KROW(KYSTL,IW(IYS0L+JYS0K2))+JYSTQF),4)
       ENDIF
      ENDDO
C
      IER = 0
C
 999  CONTINUE
C  Resize the output bank, making sure it exists even if it has no rows
      IF (IW(KYS0L+LMHROW).GT.0) THEN
        CALL AUBPRS('YS0L')
      ELSE
        CALL AUBOS('YS0L',0,LMHLEN,KYS0L,IGARB)
      ENDIF
      RETURN
      END
#endif
