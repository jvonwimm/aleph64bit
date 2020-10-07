      SUBROUTINE RECEVT(IREJ)
C----------------------------------------------------------------------
C!  - Process a single event through the reconstruction
C!   Modified :- E. Lancon             14-JUN-1991
C!              process the VDET (VPREDA and VDLINK) only is at least 2
C!              TPC tracks
C!   Modified :- P. Coyle              24-OCT-1991
C!              keep if a VDET laser event (trigger bit 21 set)
C!              and LASER was working (>=run 11869)
C!   Modified :- E. Lancon              8-NOV-1991
C!              SICAL added
C!   Modified :- E. Lancon              7-Apr-1993
C!              Swap FRFT 0 <-> FRFT 3 after tracking and back
C!              at the end of reconstruction
C!   Modified :- D. Rousseau            23-APR-1993
C!             process VDET if random trigger (trigger bit 31 set)
C!   Modified :- E. Lancon             13-SEP-1993
C!             process VDET if VREC data card is present
C!             - F.Ranjard             10-May-1994
C!             remove call to TDXMON
C!   Modified :- P. Comas              24-Oct-1995
C!             process VDET regardless the number of tracks if not in
C!             LEP1
C!
C!
C!  Output   IREJ = 0      Event OK
C!                <>0      Event rejected
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "frftjj.h"
      LOGICAL RMOD,FKAL
      LOGICAL BTEST
      INTEGER ALTRIG
      LOGICAL VTRLAS
      LOGICAL ALLEP1,LEP1
      SAVE LEP1,IROLD
#include "bmacro.h"
      DATA FKAL/.TRUE./
      DATA IROLD/0/
C--------------------------------------------------------------------
C
      IREJ = 0
      IER=0

C?   To check if we are in LEP1 just once per run

      CALL ABRUEV(IRUN,IEVT)

      IF (IRUN.NE.IROLD) THEN
        IROLD=IRUN
        LEP1=ALLEP1(IROLD)
      END IF
C
C?   The tracking part
C

C
C?   No Kallman Filter for PASS0
C
      IF ( FPASS0 ) FKAL = .FALSE.

      IF ( FDETRF(JULTP) ) THEN
        CALL RNXMOD(MODTPT,RMOD)
        IF ( FREPRO ) THEN
        ELSE
          CALL TPCREC
        ENDIF
      ENDIF

      IF ( FDETRF(JULIT) ) THEN
        CALL RNXMOD(MODITC,RMOD)
        KFRFT = IW(NAMIND('FRFT'))
        NFRFTB = LROWS(KFRFT)
        IF ( FREPRO ) THEN
          CALL ITCREP
          KFRFT = IW(NAMIND('FRFT'))
          NFRFTA = LROWS(KFRFT)
          IF ( NFRFTA.NE.NFRFTB ) THEN
            CALL RERROR ('RECEVT', -1,
     &        ' Uncompatible Number of Tracks bef./after ITCREP ')
            IF ( JDBDRF(JULSK).GE.2 )
     &        WRITE (LOUTRL,'(a,2i6)')
     &        ' - RECEVT - Number of tracks bef./aft. ITCREP '
     &        , NFRFTB, NFRFTA
            IREJ = -1
            GOTO 999
          ELSE
          ENDIF
        ELSE
          CALL ITCREC
        ENDIF
      ENDIF
C
      IF ( FDETRF(JULTP) .OR. FDETRF(JULIT) .OR. FREPRO ) THEN
        CALL RNXMOD(MODFIT,RMOD)
        IF( .NOT.FREPRO )THEN
          CALL FITALL (0,FKAL)
        ELSE
          CALL FREFIT(IERF)
C    Set bank numbers of fit banks to 0 (for V0-package)
          KTPCO=IW(NAMIND('TPCO'))
          IF(KTPCO.NE.0)IW(KTPCO-2)=0
          KFRFT=IW(NAMIND('FRFT'))
          IF(KFRFT.NE.0)IW(KFRFT-2)=0
          KFRTL=IW(NAMIND('FRTL'))
          IF(KFRTL.NE.0)IW(KFRTL-2)=0
        ENDIF
C
C?   Relate JULIA to true MC tracks in the ITC and TPC
C
        IF ( .NOT.FREPRO ) THEN
          CALL IATTMA(IER)
          IF ( IER.NE.0 )   WRITE (LOUTRL,'(a,i3)')
     &' *** Matching MC true - reconstructed tracks NOT done, IER ', IER
        ENDIF
      ENDIF
C
C?   And Now The VDET
C
C..............  Temporary !!!!!!!!!!!!!!
C
C   keep VDET only if > 2 TPC tracks near vertex OR a LASER event
C   Add requirement that TPC was reconstructed this processing DNB 17-3-
C
C
C      are there TPC tracks???
C
      IF (FDETRF(JULVD)) THEN
        NTRT=0
        DLIM=2.
        ZLIM=20.
        NTRTG=0
        KFRFT=IW(NAMIND('FRFT'))
        IF(KFRFT.NE.0) NTRT=LROWS(KFRFT)
        DO 1 I=1,NTRT
          DZER=RTABL(KFRFT,I,JFRFD0)
          ZZER=RTABL(KFRFT,I,JFRFZ0)
          IF(ABS(DZER).LT.DLIM.AND.ABS(ZZER).LT.ZLIM)NTRTG=NTRTG+1
    1   CONTINUE
        IF(   FDETRF(JULTP)         .AND.
     &      (LEP1.AND.NTRTG.LT.2.AND.IW(NAMIND('VREC')).EQ.0) )THEN
          ITRIG=ALTRIG(JLVL1,JLVL2,JLVL3)
          IF (ITRIG.NE.0) THEN
            IF ( VTRLAS(DUM) ) THEN
              CALL RNXMOD(MODPRV,RMOD)
              CALL VPREDA
            END IF
          ELSE
            JEBIRF(1)=IBSET(JEBIRF(1),31)
          END IF
        ELSE
          CALL RNXMOD(MODPRV,RMOD)
          CALL VPREDA
          IF (IW(NAMIND('VFIT')).NE.0) THEN
            CALL RNXMOD(MODVDE,RMOD)
            CALL VDLINK
          ENDIF
        ENDIF
      ENDIF
C..............  Temporary !!!!!!!!!!!!!!
C
C?   Swap FRFT 0 <-> FRFT 2
C
      IND = MSWAP (IW,'FRFT',0,'FRFT',3)
C
C?   Extrapolate tracks to Ecal, Hcal
C
      IF ( FDETRF(JULEC).OR.FDETRF(JULHC).OR.FDETRF(JULMU) ) THEN
        CALL RNXMOD(MODFTR,RMOD)
        CALL FTRACK
      ENDIF
C
C?   Form HCAL clusters
C
      IF (FDETRF(JULHC)) THEN
        CALL RNXMOD(MODHCL,RMOD)
        CALL HCFCLU(NHCLU)
      ENDIF
C
C?   Muons in HCAL
C
      IF (FDETRF(JULHC)) THEN
        CALL RNXMOD(MODMUF,RMOD)
C for monte carlo data mask the hcal tube efficiency
        IF( FMCRUN ) CALL HRHTUB(IER)
        CALL HMFIND
      ENDIF
C
C?   Muons in muons chambers
C
      IF (FDETRF(JULMU)) THEN
        CALL RNXMOD(MODMUA,RMOD)
C for monte carlo data mask the muon chamber hit efficiency
        IF( FMCRUN ) CALL MRMHIT(IER)
        CALL MUASS
      ENDIF
C
C?   Muons global HCAL + muons-Chambers association
C?   now combine the muon chamber and hcal information into
C?   a final muon ID and treat shadowing
C
      IF(FDETRF(JULHC).OR.FDETRF(JULMU)) THEN
        CALL RNXMOD(MODMUO,RMOD)
        CALL MUIDO(IER)
      ENDIF
C
C?   ECAL and HCAL Analysis (electrons, gammas, etc..)
C
      IF  (FDETRF(JULEC).OR.FDETRF(JULHC)) CALL CALREC
C
C?   LCAL and SATR and SICAL
C
      IF (FDETRF(JULSI)) THEN
        CALL RNXMOD(MODSIC,RMOD)
        CALL SICREC
      ENDIF
      IF (FDETRF(JULLC)) THEN
        CALL RNXMOD(MODLCA,RMOD)
        CALL LCAREC
      ENDIF
      IF (FDETRF(JULSA)) THEN
        CALL RNXMOD(MODSAT,RMOD)
        CALL SATREC(0,.TRUE.)
      ENDIF
      IF (FDETRF(JULLC)) THEN
        CALL RNXMOD(MODLTR,RMOD)
        CALL LTRACK
      ENDIF
C
C?   Fill FRID Bank (Part. Hypo. from tracks and calo.)
C
      IF (FDETRF(JULTP) .AND. .NOT.FPASS0 ) THEN
        CALL RNXMOD(MODPID,RMOD)
        CALL FPIDEN
      ENDIF
C
C?   The V0's  and Vertex Finding
C
      IF (FDETRF(JULYR)) THEN
        CALL RNXMOD(MODYMA,RMOD)
        CALL YFMAIN
      ENDIF
      IF (FDETRF(JULYT)) THEN
        CALL RNXMOD(MODYTO,RMOD)
        CALL YTOPOL
        IF (JDBDRF(JULYT).GE.1) CALL YTOPRT(JDBDRF(JULYT))
      ENDIF
      IF ((FDETRF(JULYR).OR.FDETRF(JULYT))) THEN
        CALL RNXMOD(MODYV0,RMOD)
        CALL YMFV0S
      ENDIF
C
C?   Energy Flow
C
      IF (FDETRF(JULEC).AND.FDETRF(JULHC).AND.FDETRF(JULEF)) THEN
        CALL RNXMOD(MODENF,RMOD)
        CALL EMSKEV
      ENDIF
C
C?   The BOMs
C
      IF (FDETRF(JULBO)) THEN
        CALL RNXMOD(MODBOM,RMOD)
        CALL OMBREC
      ENDIF
C
C?   Swap back FRFT 0 <-> FRFT 2
C
      IND = MSWAP (IW,'FRFT',0,'FRFT',3)
C
  999 RETURN
      END
#endif
