      SUBROUTINE XTGENB(IRUN,XTGQOK,XTGLOK,XTGSOK)  
CKEY XLUMOK TRIG /INTERNAL  
C---------------------------------------------------------------------- 
C!  - Check which triggers are enabled. 
C!    Called from XLSLUM
C!    Author   :- J.Wear                  22-MAR-1990   
C!    Modified:   H. Meinhard       27-Apr-1993  (1) - Add Sical, return
C!                different flags for LCAL and SICAL
C!    Modified:   H. Meinhard       24-May-1993  (2) - Introduce changes
C!                due to 1993 trigger setup 
C!    Modified:   B.Bloch           11-May-1994  (1) - Use X1RG instead 
C!                of XTOP dropped in the trigger upgrade of 1994
C!    Modified:   B.Bloch           15-Jan-1997  (1) - Use LUPA or SILH 
C!                if X1RG missing in Luminosity only triggers   
C!                Update LCAL trigger name for High energy running>oct95
C!    Modified:   B.Bloch,G.Taylor   5-Jun-1998
C!                Bug fix, NAXTBN,NAXTOP were incorrectly initialised
C!  
C!   Inputs:  IRUN -- Run number
C!        - 
C!   Outputs: XTGQOK  /L    all essential large-angle triggers enabled  
C!            XTGLOK  /L    all essential LCAL triggers enabled 
C!            XTGSOK  /L    all essential SICAL triggers enabled
C!        - 
C!   Libraries required:
C!  
C!   Description
C!   ===========  If principal physics triggers and bhabha  
C!   triggers are enabled XTGENB returns true.  For use in determining  
C!   whether to count runs/events in the "ideal case" luminosity
C!   determination.  Each running period has distinct trigger names for 
C!   these required triggers, so each period must have its own list.
C?  
C!======================================================================
#ifndef DOC
      SAVE MXBRN,MXBMK,TTREQ,TTREL,TTRES,MHDRQ,MHDRL,MHDRS  
      SAVE NAXTBN,NAXTOP,NAX1RG,NALUPA,NASILH
      LOGICAL XTGQOK, XTGLOK, XTGSOK
      LOGICAL FIRST / .TRUE. /
#include "bcs.h" 
#include "xtbnjj.h"
#include "xtcnjj.h"
#include "xtopjj.h"
#include "x1rgjj.h"
#include "lupajj.h"
#include "silhjj.h"
      PARAMETER (MXBIT=6, MXBIL=2, MXBIS=1) 
      PARAMETER (NXBAD=19)  
      PARAMETER (JSEP89=1,JFAL89=2,JMAR90=3,JMAR92=4,JSEP92=5,JMAY93=6, 
     +  JNOV95=7, NPER=7)   
      CHARACTER*4 CHAINT
      CHARACTER*8 MNM   
      CHARACTER*8 TTREQ(MXBIT,NPER),TTREL(MXBIL,NPER),TTRES(MXBIS,NPER) 
      INTEGER     MXBRN(NXBAD),MXBMK(NXBAD),
     +            MHDRQ(NPER),MHDRL(NPER),MHDRS(NPER)   
      LOGICAL     XTGQ, XTGL, XTGS  
C.....Required triggers: large angle
      DATA TTREQ /  
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB',
     +'SNG_MUON','SNG_C_EM','ETT_EWBA','ETT_EWE*','ETT_EWEA','ETT_EWEB'/
C.....Required triggers: LCAL   
      DATA TTREL /  
     +  'LC_HI_LO','LC_LO_HI',  
     +  'LC_HI_LO','LC_LO_HI',  
     +  'LT_HI_LO','LT_LO_HI',  
     +  'LW_ET_HI','        ',  
     +  'LW_ET_HI','        ',  
     +  'LW_ET_HI','        ',  
     +  'LW_LO_HI','        '/  
C.....Required triggers: SiCAL  
      DATA TTRES /  
     +  '        ', 
     +  '        ', 
     +  '        ', 
     +  '        ', 
     +  'SICAL_ME', 
     +  'SICAL_ME', 
     +  'SICAL_ME'/ 
C.....Required trigger mask (hardwired): large angle
C     51512064=Z03120300, 1966848=Z001E0300 
      DATA MHDRQ / 51512064, 51512064,  1966848,  1966848,  1966848,
     +             1966848, 1966848 /   
C.....Required trigger mask (hardwired): LCAL   
C     49152=Z0000C000, 65536=Z00010000, 64=Z00000040
      DATA MHDRL /    49152,    49152,    49152,    65536,    65536,
     +                   64,       32 / 
C.....Required trigger mask (hardwired): SiCAL  
C     16=Z00000010  
      DATA MHDRS /        0,        0,        0,        0,       16,
     +                   16,       16 / 
C.....Hardwired trigger enable masks (XTOP corrupted)   
      DATA MXBRN /       4017,       4067,       4351,       4387,  
     &                   4476,       4525,  
     &                   4530,       5112,       5329,       5373,  
     &                   5852,       7240,       8419,       8420,  
     &                   8423,       8424,      12528,      12536,  
     &                12718/
      DATA MXBMK /-2083339265,-2083339265,-2083339274,-2083339266,  
     &            -2083339266,-2083339266,  
     &            -2083339266,-2095922177,-2095922177,-2095922177,  
     &               51569663,-2145393668,-2011175940,-2011175940,  
     &            -2011175940,-2011175940,-2009078786,-2009078786,  
     &            -2009078786/  
#include "bmacro.h"
C---------------------------------------------------------------------- 
C For MC, all triggers are always enabled   
      IF (IRUN.LE.2000) THEN
        XTGQOK = .TRUE. 
        XTGLOK = .TRUE. 
        XTGSOK = .TRUE. 
        GOTO 999
      ENDIF 
C
C Save the name indices
      IF( FIRST ) THEN
        NAXTBN = NAMIND('XTBN')
        NAXTOP = NAMIND('XTOP')
        NAX1RG = NAMIND('X1RG')
        NALUPA = NAMIND('LUPA')
        NASILH = NAMIND('SILH')
        FIRST = .FALSE.
      ENDIF
C   
C link to trigger banks 
      IERR = 0  
      KXTBN=IW(NAXTBN)  
      KXTOP=IW(NAXTOP)  
      KX1RG=IW(NAX1RG)  
      KLUPA=IW(NALUPA)  
      KSILH=IW(NASILH)  
C   
C Find which period this run belongs to 
      IF (IRUN.LE.4535) THEN
        KPER = JSEP89   
      ELSEIF (IRUN.LE.5909) THEN
        KPER = JFAL89   
      ELSEIF (IRUN.LT.14000) THEN   
        KPER = JMAR90   
      ELSEIF (IRUN .LT. 16500) THEN 
        KPER = JMAR92   
      ELSEIF (IRUN .LT. 20000) THEN 
        KPER = JSEP92   
      ELSEIF (IRUN .LT. 40000) THEN 
        KPER = JMAY93   
      ELSE  
        KPER = JNOV95   
      ENDIF 
C   
C reset the required bit masks  
      MKREQ = 0 
      MKREL = 0 
      MKRES = 0 
C   
C scan the XTBN bank for the required triggers  
      IF (KXTBN .NE. 0) THEN
        DO 330 ITBIT = 1, LROWS(KXTBN)  
          IDEF = ITABL(KXTBN,ITBIT,JXTBTN)  
          INUM = ITABL(KXTBN,ITBIT,JXTBTB)  
C XTBN corrupted?   
          IF (INUM .LT. 0 .OR. INUM .GT. 31) THEN   
            IERR = 1
            GO TO 888   
          END IF
C get the name of this trigger; convert it to upper case; check whether 
C non-blank 
          IF (IDEF .NE. 0 .AND. INUM .NE. 31) THEN  
            MNM(1:4) = CHAINT(ITABL(KXTBN,ITBIT,JXTBBM))
            MNM(5:8) = CHAINT(ITABL(KXTBN,ITBIT,JXTBBM+1))  
            CALL CLTOU(MNM) 
            IF (MNM .NE. '        ') THEN   
C compare with the required trigger names   
              DO 300 IREQ = 1, MXBIT
                IF (MNM .EQ. TTREQ(IREQ,KPER)) MKREQ = IBSET(MKREQ,INUM)
  300         CONTINUE  
              DO 310 IREQ = 1, MXBIL
                IF (MNM .EQ. TTREL(IREQ,KPER)) MKREL = IBSET(MKREL,INUM)
  310         CONTINUE  
              DO 320 IREQ = 1, MXBIS
                IF (MNM .EQ. TTRES(IREQ,KPER)) MKRES = IBSET(MKRES,INUM)
  320         CONTINUE  
            END IF  
          END IF
  330   CONTINUE
C if required bit mask empty, take it from hardwired mask   
        IF (MKREQ .EQ. 0) MKREQ = MHDRQ(KPER)   
        IF (MKREL .EQ. 0) MKREL = MHDRL(KPER)   
        IF (MKRES .EQ. 0) MKRES = MHDRS(KPER)   
      ELSE  
C XTBN is missing!  
        IERR = 1
        GO TO 888   
      END IF
C   
C Error handling: XTBN missing or corrupted - use hardwired masks   
  888 CONTINUE  
      IF (IERR .NE. 0) THEN 
        MKREQ = MHDRQ(KPER) 
        MKREL = MHDRL(KPER) 
        MKRES = MHDRS(KPER) 
      END IF
C   
C==== We have the required mask. Get the mask of enabled triggers now.  
C reset the mask first  
      MKENB = 0 
C   
C Use XTOP, word 6 of run header to get the trigger enabled mask.   
C or 3rd word of constants stored in 1st row of X1RG from 1994 onwards  
C If this run was missing or had upt_corr trigger banks in the run heade
C hardwire the enabled mask.
C or word 5 of LUPA bank if none already there ( LCAL only triggers)
C or word 5 of SILH bank if none already there ( SiCAL only triggers)   
      IF (KXTOP.NE.0) THEN  
         MKENB = ITABL(KXTOP,1,JXTOTR)  
      ELSEIF (KX1RG.NE.0) THEN  
         MKENB = ITABL(KX1RG,1,JX1RCO+2)
      ELSEIF (KLUPA.NE.0) THEN  
         MKENB = ITABL(KLUPA,1,JLUPTE)   
      ELSEIF (KSILH.NE.0) THEN  
         MKENB = ITABL(KSILH,1,JSILTE)   
      ENDIF 
      DO 20 IXBAD = 1,NXBAD 
   20  IF (IRUN.EQ.MXBRN(IXBAD)) MKENB = MXBMK(IXBAD)   
C   
C==== Compare required mask with enabled mask to find whether all re-   
C==== quired triggers are enabled.  
C Do bit-wise AND of Trigger Enable mask and Required Physics mask  
C Check if physics triggers enabled equal the required physics mask.
      XTGQ = .FALSE.
      XTGL = .FALSE.
      XTGS = .FALSE.
      IF (MKREQ .NE. 0) XTGQ = MKREQ .EQ. IAND(MKREQ,MKENB) 
      IF (MKREL .NE. 0) XTGL = MKREL .EQ. IAND(MKREL,MKENB) 
      IF (MKRES .NE. 0) XTGS = MKRES .EQ. IAND(MKRES,MKENB) 
C   
C Luminosity trigger patch for runs 13191-13199.
C Here triggers LT_A+BVH and LT_LO_HI were disabled due to noisy towers 
C Subsequent analysis showed that LW_ET_HI found all Bhabhas, therefore 
C these runs are allowed to pass.   
      IF (IRUN .GE. 13191 .AND. IRUN .LE. 13199) XTGL = .TRUE.  
C   
C give the final answer now 
      XTGQOK = XTGQ 
      XTGLOK = XTGL 
      XTGSOK = XTGS 
  999 RETURN
      END   
#endif
