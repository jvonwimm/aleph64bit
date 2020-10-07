      SUBROUTINE VRECLU(IROMD,NR)
C
C----------------------------------------------------------------------
C!  - Select final clusters
C!
C!   Inputs:
C!        - IROMD       I/  cluster address from VHLS
C!
C!  Modified 16-1-94 by Dave Brown for the VDET upgrade
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
      SAVE FIRST, NAVCSG, NAVPLH, NAVTRS, NAVPHN
C
C      INTEGER JVFLFG,LVFLGA
#include "vflgjj.h"
C      INTEGER JVFPPH,LVFPHA
#include "vfphjj.h"
C      INTEGER JVCOCM,LVCOMA
#include "vcomjj.h"
C      INTEGER JVFHHA,LVFHLA
#include "vfhljj.h"
#include "vdflgs.h"
#include "vrecon.h"
#include "vprtnc.h"
#include "vcsgjj.h"
C
C PARAMETERS-
C MSUNB = mask for defining generic 'unbonded' strip
C MSGOOD = ANTI-mask for defining generic good strip
C These are designed so that a half bonded strip will be counted
C as good, but will still be skipped over if it's in the middle
C of a cluster, and it's PH is below threshold.
C
      INTEGER MGOOD,MSKIP
      PARAMETER (MSKIP = VBUNBD+VBUNUS)
      PARAMETER (MGOOD = VBSUPP+VBHOTC+VBNOIS+VBUNBD+VBUNUS)
C
C  Input variables
C
      INTEGER MODNUM
      INTEGER KVFHL,KVFPH,KVFLG,KVCOM
C
C  Output variables- none
C
C  Functions
C
      INTEGER NBANK,NLINK
      INTEGER VFWAFF,VIWFFW
C
C  Local variables
C
      INTEGER NAVCSG,NAVFHL,NAVCOM,NAVFLG,NR
      INTEGER NAVPLH,NAVTRS,NAVFPH,NAVPHN
      INTEGER NVCSG,KVCSG,IVCSG,ILOOP
      INTEGER IPH,IBEG,IADDR
      INTEGER IRET,IMOD
      INTEGER ISTRP,JSTRP,KSTRP,IFRS,ILS,NSIZ
      INTEGER IROMD,ILAY,IWFF,IWAF,IWMX,IPHI,IVIEW
      INTEGER PSUM, NUNBD
      INTEGER IDUM,JDUM
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      INTEGER IFLG,IFL,ISTRT,I
      LOGICAL SGOOD,SSKIP,IHIS1
      INTEGER NAMIND,VDYEAR
      INTEGER MSUNB
      PARAMETER (MSUNB = VBUNBD+VBUNUS)
      LOGICAL SUNBD, LOWTH
      INTEGER IWF,IVW,LYR
C
C  Inline functions
C
#include "bcs.h"
C      INTEGER LCOLS,LROWS,ID,KNEXT,KROW,NRBOS,LFRWRD,LFRROW,ITABL,L
C      REAL RTABL
#include "bmacro.h"
      SUNBD(IFLG) = IAND(IFLG,MSUNB).GT.0
      SGOOD(IFLG) = IFLG.GT.0.AND.IAND(IFLG,MGOOD).EQ.0
      SSKIP(IFLG) = IAND(IFLG,MSKIP).GT.0
      IHIS1(I) = IAND(I,1) .EQ. 1
      LOWTH(IWF,IVW,LYR) = IVW.EQ.1.AND.
     >   ((LYR.EQ.1.AND.IWF.NE.1).OR.(LYR.EQ.2.AND.IWF.EQ.3))
C
C Code starts here
C
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVCSG=NAMIND('VCSG')
      ENDIF
      KVFHL=NLINK('VFHL',NR)
      KVCOM=NLINK('VCOM',NR)
      KVFPH=NLINK('VFPH',NR)
      KVFLG=NLINK('VFLG',NR)
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
C
C  Unpack the address
C
      CALL VADEWA(IROMD,ILAY,IWFF,IPHI,IVIEW)
C
C Find the module (1 or 2) this is useful for new Vdet
C IWAF is useless here
C
      IRET = VFWAFF(IWFF,IMOD,IWAF)
C
C  If requested, histogram
C
      IF(IHIS1(HISTLV))THEN
        DO IVCSG=1,NVCSG
          IPH=RTABL(KVCSG,IVCSG,JVCSMP+IWAF-1)
          IFL=ITABL(KVCSG,IVCSG,JVCSMF+IWAF-1)
          IF(SGOOD(IFL))CALL HFILL(HISOFF+13,
     &    FLOAT(IPH),0.0,1.0)
        END DO
      END IF
      ISTRT=1
      NSIZ=0
      NUNBD=0
      PSUM=0
C
C For new Vdet, we clusterize separately on each wafer;
C This is not done for Vdet91
C
      IWMX=1
      IF(VDYEAR().EQ.95)IWMX=3
      DO 100 IWAF=1,IWMX
      IF(IWMX.EQ.3.AND.LOWTH(IWAF,IVIEW,ILAY))THEN
        CALL VRECLT(KVFHL,KVFPH,KVFLG,KVCOM,
     >             ILAY,IPHI,IVIEW,IMOD,IWAF)
        GO TO 100
      ENDIF
      DO 101 ILOOP=1,NVCSG
        IVCSG=ILOOP+ISTRT-1
        IF(IVCSG.GT.NVCSG)IVCSG=IVCSG-NVCSG
        IPH=RTABL(KVCSG,IVCSG,JVCSMP+IWAF-1)
        IFL=ITABL(KVCSG,IVCSG,JVCSMF+IWAF-1)
        IF(SGOOD(IFL).AND.IPH.GE.MNPULS)THEN
          IF(NSIZ.EQ.0)THEN
             IBEG = IVCSG
             NUNBD=0
          ENDIF
          NSIZ = NSIZ + 1
          PSUM=PSUM+IPH
        ENDIF
        IF(NSIZ.GT.0)THEN
C
C-- one skips strips only if one has already started building
C-- a cluster
C
          IF(SSKIP(IFL))THEN
            NUNBD=NUNBD+1
            NSIZ = NSIZ+1
          ENDIF
C
C-- if (pulsht < 0 and good strip) or
C--    (end of module) or
C--    (too many unbounded strips) or
C--    (a bad strip)
C--    [ a bad strip is one that is not good AND can't be skipped ]
C-- then
C--    a cluster is found
C
          IF ((IPH.LT.MNPULS.AND.SGOOD(IFL)) .OR.
     $       IVCSG.EQ.NVCSG .OR.
     $       NUNBD.GT.MXUNBD .OR.
     $       (.NOT.SSKIP(IFL).AND..NOT.SGOOD(IFL))) THEN

C
C             HERE a cluster has been found
C
C Fix the bug for VDET95 only
C
             IF(VDYEAR().EQ.95)THEN
                IFRS=IBEG
                ILS=IFRS+NSIZ-1
             ELSE
C
C  VDET91
C
C Add one strip at each extremity
C and accumulate the pulseheight if strip is good
C
C
C******************** Here a bug is introduced to reproduce the  ********
C******************** results of the (bugged) official code      ********
C******************** January 1995                               ********
C  This will be studied in more details, and fixed as soon as possible
C  May have lateral effects like for example cluster separation
C
               IFRS=IBEG-1
               IF(IFRS.EQ.0)IFRS=NVCSG
               IFL=ITABL(KVCSG,IFRS,JVCSMF+IWAF-1)
C               IF(SGOOD(IFL))THEN     !   this would be right
               IF(SUNBD(IFL))THEN      !   BUG ******
                  PSUM=PSUM+RTABL(KVCSG,IFRS,JVCSMP+IWAF-1)
                  NSIZ=NSIZ+1
               ELSE
                  IFRS=IBEG
               ENDIF
               ILS=IFRS+NSIZ
               IFL=ITABL(KVCSG,ILS,JVCSMF+IWAF-1)
C               IF(SGOOD(IFL))THEN     !   this would be right
               IF(SUNBD(IFL))THEN      !   BUG ******
                   PSUM=PSUM+RTABL(KVCSG,ILS,JVCSMP+IWAF-1)
                   NSIZ=NSIZ+1
               ELSE
                   ILS=IFRS+NSIZ-1
               ENDIF
C
             ENDIF
             IF(IHIS1(HISTLV))THEN
               CALL HFILL(HISOFF+20,FLOAT(PSUM),0.0,1.0)
             END IF
C
C  Final sum pulseheight cut
C
             IF(PSUM .GE. FNLSUM)THEN
C
C  For Vdet95, we need to change IWAF, wafer in module, into
C  IWFF, wafer in face
C
               IF(VDYEAR().EQ.95)IRET = VIWFFW(IMOD,IWAF,IWFF)
C
C Put cluster in banks
C
               CALL VCLBNK(KVFHL,KVFPH,KVFLG,KVCOM,IFRS,ILS,
     >                     ILAY,IPHI,IVIEW,IWAF,IWFF)
C
C
C  Increment counter for run summary
C
               NFNLHT(IVIEW) = NFNLHT(IVIEW) + 1
             ENDIF
C
C Reset cluster size
C
             NSIZ=0
             PSUM=0
          ENDIF
        ENDIF
  101 CONTINUE
  100 CONTINUE
      RETURN
      END
#endif
