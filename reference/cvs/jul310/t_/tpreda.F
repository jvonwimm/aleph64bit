      SUBROUTINE TPREDA
C
C----------------------------------------------------------------
C! Prepare TPC data
C!
C!    Author:    R. Johnson    28-04-87
C!    Modified:  R. Johnson    21-01-91
C!    Modified:  W. Wiedenmann  3-02-92  handle TSIR,TSDI banks
C!    Modified:  W. Wiedenmann 19-05-93  call TPADFX
C!    Modified:  I. Tomalin     3-04-95  Correct T0 for bunch trains.
C!    Modified:  I. Tomalin     3-10-97  Correct 1997 T0 for problems.
C!    Modified:  M. Cattaneo   24-04-98  Force use of TWIR/TWDI in 
C!                                       run range 10000-11800
C!
C!    Description
C!    -----------
C?      1.  find pad-clusters
C?      2.  calculate coordinates
C?      3.  associate coordinates with Monte Carlo true hits
C?      4.  find wire-pulses
C!
C!---------------------------------------------------------------
#ifndef DOC
C
#include "ajobjj.h"
#include "tpgpar.h"
#include "tpccon.h"
#include "rparac.h"
#include "tparac.h"
#include "bcs.h"
#include "rcurnt.h"
#include "rflags.h"
#include "tflags.h"
#include "rlunit.h"
#include "tptime.h"
#include "tstate.h"
#include "tmonit.h"
C
      CHARACTER TEXT*100
      LOGICAL FIRST,FDROP
      LOGICAL FSMART
      CHARACTER NAM1*4,NAM2*4
      DATA FIRST/.TRUE./,FDROP/.TRUE./
C
C-----------------------------------------------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPAD=NAMIND('TPAD')
        NTSIR=NAMIND('TSIR')
        NTRIR=NAMIND('TRIR')
        NTWIR=NAMIND('TWIR')
        NTPCO=NAMIND('TPCO')
        NTCLU=NAMIND('TCLU')
        NTPUL=NAMIND('TPUL')
        NTSPU=NAMIND('TSPU')
        NTSCL=NAMIND('TSCL')
        NTWRR=NAMIND('TWRR')
        NTDRP=NAMIND('TDRP')
        NTSIM=NAMIND('TSIM')
      ENDIF
C
      ICNTER(LCPRED)=ICNTER(LCPRED)+1
C
        CALL ALTIME(TIME1)
      IER=0
C
C If we are in bunch train mode, T0 needs correcting.
       CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
       DO 10 I = 1,LTSECT
         IF (IBUN.GT.1) THEN
           TVOFFS(I) = TVOFS0(I) + FLOAT((IBUN - 1)*INBU)/TPCCLK
         ELSE
           TVOFFS(I) = TVOFS0(I)
         END IF
   10  CONTINUE
C
C Correct for 1997 hardware problem which shifted T0 by several ns.
C during periods of high luminosity at start of fill.
       TOFFST = TFIXT0(IRUNRC,IEVTRC)
       DO 20 I = 1,LTSECT
         TVOFFS(I) = TVOFFS(I) + TOFFST/TPCCLK
   20  CONTINUE
C
C++   Drop unwanted sectors
C
      KTDRP=IW(NTDRP)
      IF (KTDRP.NE.0) THEN
        LEN=IW(KTDRP)
        DO 472 I=1,LEN
          ISLOT=IW(KTDRP+I)
          IF (FDROP) THEN
            WRITE(LOUTRL,473) ISLOT
  473       FORMAT(' TPREDA:  dropping raw data for slot ',I2,
     &             ' as requested in user''s TDRP card.')
          ENDIF
          IND=NDROP('TPAD',ISLOT)
          IND=NDROP('TPDI',ISLOT)
          IND=NDROP('TWIR',ISLOT)
          IND=NDROP('TWDI',ISLOT)
          IND=NDROP('TRIR',ISLOT)
          IND=NDROP('TRDI',ISLOT)
          IND=NDROP('TSIR',ISLOT)
          IND=NDROP('TSDI',ISLOT)
          IND=NDROP('TISL',ISLOT)
  472   CONTINUE
        FDROP=.FALSE.
      ENDIF
C
C++   Check if any raw or simulated data are available
C
      KTSIR=IW(NTSIR)
      IF (KTSIR.NE.0) THEN
         FSMART=.TRUE.
         NAM1='TSIR'
         NAM2='TSDI'
         KTRIR=KTSIR
      ELSE
         FSMART=.FALSE.
         NAM1='TRIR'
         NAM2='TRDI'
         KTRIR=IW(NTRIR)
      ENDIF
      KTPAD=IW(NTPAD)
      KTPCO=IW(NTPCO)
      KTWIR=IW(NTWIR)
      IF (KTPAD.EQ.0 .AND. KTRIR.EQ.0 .AND. KTPCO.EQ.0
     &                                      .AND. KTWIR.EQ.0) THEN
        GO TO 999
      ENDIF
C
C++   Handle the unusual case where both reduced and unreduced wire
C++   data are available on input.
C++   In early 1991 data, force use of unreduced wire data.
C
      IF (KTWIR.NE.0 .AND. KTRIR.NE.0) THEN
        IF (FOLDWR .OR. (IRUNRC.GE.10000 .AND. IRUNRC.LE.11800) ) THEN
          CALL BDROP(IW,NAM1//NAM2)
        ELSE
          CALL BDROP(IW,'TWIRTWDI')
        ENDIF
      ENDIF
C
C++   Create the PTST status bank for the POT
C
      CALL TCPTST(IERP)
C
C++   Correct Hitlist for bug in TPP code
C
      CALL TPADFX
C
C++   Drop data from out-of-sequence sectors
C
      CALL TSEQUE
C
C++   Fix truncated raw data banks (overflow DAQ buffers)
C
      CALL TTRUNK
C
C++   Reduce the wire data if they are not already reduced by the TPP
C
      KTSIM=IW(NTSIM)
      IF (KTSIM.NE.0) THEN
C
C++     Old Monte Carlo files have reduced data in the TWIR and TWDI
C++     banks, so these banks must be renamed.
C
        CALL TWIRFX
      ENDIF
      IF (.NOT.FNOWIR) THEN
        CALL TWRRED(IER)
        CALL BLIST(IW,'T+',NAM1//NAM2)
        IF (IER.NE.0) THEN
          WRITE(TEXT,3716) IER
 3716     FORMAT('Error code ',I3,' from wire reduction.')
          CALL RERROR('TPREDA',3,TEXT(1:35))
        ENDIF
        IF (FCLEAN) CALL BDROP(IW,'TWIRTWDI')
      ENDIF
C
C++   For Monte Carlo, kill known bad pads and wires
C
      KTSIM=IW(NTSIM)
      IF (KTSIM.NE.0) CALL TPKILL
C
C++   Get the magnetic field from the HALL-probe if requested
C
      IF (MAGNET .AND. FTPC90) THEN
        ICNTER(LCMAGN)=ICNTER(LCMAGN)+1
        CALL TMAGNT(IER)
        IF (IER.NE.0) THEN
          IER=1000+IER
          GO TO 998
        ENDIF
      ENDIF
C
C++   Remove garbage hits from the TPAD bank
C
      IF (IW(NTSIM).EQ.0) CALL TFTPAD
C
C++   Reconstruct pad clusters
C
      IF (KTPAD.GT.0 .AND. .NOT.NOPADS) THEN
        CALL ALTIME(TMTP1)
        CALL TPADS(IER)
        CALL ALTIME(TMTP2)
        TPTSUM(3)=TPTSUM(3) + (TMTP2-TMTP1)
        ICNTER(LCPADS)=ICNTER(LCPADS)+1
        IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
          MTCLU=0
          MTPUL=0
          MTSPU=0
          MTSCL=0
          KTCLU=IW(NTCLU)
          KTPUL=IW(NTPUL)
          KTSPU=IW(NTSPU)
          KTSCL=IW(NTSCL)
 1842     IF (KTCLU.EQ.0) GO TO 1059
            MTCLU=MTCLU+IW(KTCLU+LMHROW)
            MTPUL=MTPUL+IW(KTPUL+LMHROW)
            KTCLU=IW(KTCLU-1)
            KTPUL=IW(KTPUL-1)
            IF (KTSPU.GT.0) THEN
              MTSPU=MTSPU+IW(KTSPU+LMHROW)
              KTSPU=IW(KTSPU-1)
            ENDIF
            IF (KTSCL.GT.0) THEN
              MTSCL=MTSCL+IW(KTSCL+LMHROW)
              KTSCL=IW(KTSCL-1)
            ENDIF
            GO TO 1842
 1059     CONTINUE
          WRITE(LDEBRL,1660) IRUNRC,IEVTRC,MTPUL,MTCLU,MTSPU,MTSCL
 1660     FORMAT(' TPADS  for run ',I5,' event ',I6,' found ',I5,
     &           ' pulses, ',I5,' clusters, ',I5,' subpulses, ',
     &           I5,' subclusters')
        ENDIF
      ENDIF
      IF (FTSTAT) CALL TPSTAT
      IF (FCLEAN) CALL BDROP(IW,'TLRLTPULTPRL')
      IF (IER.NE.0) THEN
        IER=IER+2000
        GO TO 998
      ENDIF
C
C++   Get TPC coordinates for all sectors
C
      KTSCL=IW(NTSCL)
      IF (KTSCL.GT.0 .AND. .NOT.NOPADS) THEN
        CALL ALTIME(TMTC1)
        CALL TCODRV(IER)
        CALL ALTIME(TMTC2)
        TPTSUM(5)=TPTSUM(5) + (TMTC2-TMTC1)
        ICNTER(LCTPCO)=ICNTER(LCTPCO)+1
        IF (FDEBRF) THEN
          KTPCO=IW(NTPCO)
          IF (JDBDRF(JULTP).GE.1) THEN
            WRITE(LDEBRL,1956) IRUNRC,IEVTRC,IW(KTPCO+LMHROW)
 1956       FORMAT(' TCODRV for run ',I5,' event ',I6,' finds ',
     &             I4,' coordinates')
          ENDIF
          IF (JDBDRF(JULTP).GE.2) THEN
            CALL TPCODP(LDEBRL)
            CALL TBCODP(LDEBRL)
          ENDIF
        ENDIF
      ENDIF
      IF (FTSTAT) THEN
        KTPCO=IW(NTPCO)
        IF (KTPCO.GT.0) ITSTAT(LTPCTS)=ITSTAT(LTPCTS)+LROWS(KTPCO)
      ENDIF
      IF (IER.NE.0) THEN
        IER=IER+3000
        GO TO 998
      ENDIF
C
C++   Get wire pulses
C
      KTSIR=IW(NTSIR)
      IF (KTSIR.NE.0) THEN
         FSMART=.TRUE.
         NAM1='TSIR'
         NAM2='TSDI'
         KTRIR=KTSIR
      ELSE
         FSMART=.FALSE.
         NAM1='TRIR'
         NAM2='TRDI'
         KTRIR=IW(NTRIR)
      ENDIF
      IF (KTRIR.GT.0 .AND. .NOT.FNOWIR) THEN
        CALL ALTIME(TMTW1)
        CALL TWIRES(IER)
        CALL ALTIME(TMTW2)
        TPTSUM(4)=TPTSUM(4) + (TMTW2-TMTW1)
        ICNTER(LCWIRE)=ICNTER(LCWIRE)+1
        IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
          MTWRR=0
          KTWRR=IW(NTWRR)
 1000     IF (KTWRR.LE.0) GO TO 1050
            MTWRR=MTWRR+LROWS(KTWRR)
            KTWRR=IW(KTWRR-1)
            GO TO 1000
 1050     CONTINUE
          WRITE(LDEBRL,1060) IRUNRC,IEVTRC,MTWRR
 1060     FORMAT(' TWIRES for run ',I5,' event ',I6,' finds ',I5,
     &           ' wire pulses')
        ENDIF
        IF (FTSTAT) THEN
          KTWRR=IW(NTWRR)
   25     IF (KTWRR.EQ.0) GO TO 26
            ITSTAT(LTWPTS)=ITSTAT(LTWPTS)+LROWS(KTWRR)
            KTWRR=IW(KTWRR-1)
            GO TO 25
   26     CONTINUE
        ENDIF
        IF (.NOT.FWIRPU) CALL BDROP(IW,NAM1//NAM2)
      ENDIF
      IF (IER.NE.0) THEN
        IER=IER+4000
        GO TO 998
      ENDIF
C
C++   Dump the clusters if requested
C
      IF (FDEBRF .AND. JDBDRF(JULTP).GE.5) CALL TCDUMP
C
      GO TO 999
C
  998 CONTINUE
      WRITE(TEXT,101) IER
      CALL RERROR('TPREDA',2,TEXT(1:88))
  101 FORMAT('ERROR RETURN=',I4,' FROM TPC PREPARE DATA.&',
     &       'Reconstruction is not complete for this event.')
  999 CONTINUE
        CALL ALTIME(TIME2)
      TPTSUM(1)=TPTSUM(1) + (TIME2-TIME1)
      RETURN
      END
#endif
