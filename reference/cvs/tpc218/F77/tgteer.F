*DK tgteer
      SUBROUTINE TGTEER (NEVT,ICTP)
C
C! Main steering routine for semi-detailed TPC simulation, to
C  produce analog and/or digitized signals on the pads, wires,
C  and/or trigger-pads. This routine is called from GALEPH
C  itself !
C
C  Called from: GALEPH/TPDIGI
C
C  Calls:  TPBRTP, TORAWI, TPBRTK, TPGETR, T2DEDX,
C          T2TRAN, TPAVAL, TSPCPL, TPSGNL, TPOPSG,
C          TSRESP, TPENSC
C
C  Input:  NEVT / I  = GALEPH event number
C          ICTP / I  = GALEPH run conditions
C
C  P. Janot  -- 24 June 1988
C
C  Modifications:    1. D. Cowen 10-JUL-88 -- On error return due
C                       to space constraints on the sector level,
C                       be sure to go to next sector.  Clean out
C                       banks related to sector beforehand.
C                    2. F.Ranjard 28-MAR-89 -- suppress references
C                       to GALEPH *CD and introduce arguments in the
C                       calling sequence.
C                    2. P. Janot 04-MAR-88 -- Do a garbage
C                       collection after each sector.
C                    3. P. Janot 05-MAR-88 -- Set to zero
C                       NLPHIT and NTPHIT (otherwise these
C                       variables are not initialized and
C                       lead to an infinite loop in TPSGNL)
C                    4. Z.Feng 28-APR-91 -- replace TSWRED with TWRRED
C                    5. W.Wiedenman 10-MAR-92 --replace old TRIR TRDI
C                       banks by new TSIR TSDI banks
C                    6. D. Casper -- treat pad response separately
C                       for each cluster.
C                    7. D. Casper -- add ~3% smearing of dE/dx event by
C                       event to simulate pressure variations and other
C                       systematics.
C
C ----------------------------------------------------------------------
      INTEGER ICTP(*)
#include "bcs.h"
#include "tpcbos.h"
#include "tpcond.h"
#include "tpcons.h"
#include "trakel.h"
#include "hiscom.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "faster.h"
#include "channl.h"
#include "tpstat.h"
      DATA NLPHIT,NTPHIT/0,0/
C
C  Determine time used for digitizing TPC analog signal
C
      CALL TIMAX(T1)
C
#if defined(VAX) || defined(IBM) || defined(APOLLO)
C  Check for debug flag; write out RUNH, EVEH, and TPTE banks
C  for each event if debug flag is on.
C
      IF (ICTP(5).GE.3) CALL TDEBUG (NEVT)
C
#endif
C
C  add smearing of ionization in quadrature event by event
C
      CALL RANNOR(FL1,FL2)
      POIMIN = POICON * (1.+0.03*FL1)
C
C  Setup work bank for broken track elements
C
      CALL TPBRTP(NTREL,IERR)
      IF (IERR.NE.0) GOTO 999
C
C  Loop over sectors
C
      DO 5 ISECT = 1, 36
C
C  Get the sector type
C
        ITYPE = ITPTYP(ISECT)
        MISECT = ISECT
        IF(ISECT.GT.18) MISECT = ISECT-18
C
C  Loop over track segments in event
C
        DO 4 ISEG = 1, NTREL
C
C  Find track parameters and break track element at sector boundaries
C
          CALL TPBRTK(ISECT,ITYPE,MISECT,ISEG,NBRTRE)
C
C  Loop over broken track elements (i.e., track elements which lie
C  entirely in sector isect)
C
          DO 3 IBRTRE = 1, NBRTRE
C
C  Load parameters for this broken track element into a common for the
C  dE/dX routine
C
            CALL TPGETR(IBRTRE)
C
C  Now generate the dE/dx for this track.  The dE/dx is discrete unless
C  the formation of primaries becomes too large, in which case it is
C  done continuously. For the continuous case, the clusters are formed
C  at fixed intervals.  The cluster size is set to account correctly for
C  the dE/dx.
C  The number and coordinates of super-broken segments are also
C  determined, to put the right charge on each wire.
C  If IRETD = 1 there is not enough space to extend signal bank.
C  Go to next sector. If IRETD = 2, there is not enough space to
C  extend cluster bank. Go to next broken segment.
C
            CALL T2DEDX(IRETD)
            IF (IRETD .EQ. 1) GOTO 99
            IF (IRETD .EQ. 2) THEN
              CALL WDROP(IW,IDCLUS)
              GOTO 4
            ENDIF
C
C  Loop over super-broken segments along this broken track segment
C
              DO 2 JSEGT =  1,NSEGT
C
C  Get pointer to this super-broken segment in segment bank.
C
              INDEX = 2 + IDCLUS + (JSEGT-1)*10
              IF(IW(INDEX+10).EQ.1.AND.JSEGT.NE.NSEGT) GOTO 2
C

C   Now transport the NCL clusters to the endplate,taking into
C   account the ExB effect and the diffusion.
C
              CALL T2TRAN(IWIR)
              IF(IWIR.EQ.0) GOTO 2
C
C  Loop over clusters in super-broken segment
C
              DO 1 JCL = 1 , NCL
C
C  Form the avalanche at the wire for each arriving cluster.
C  IRETA = 1 means that the avalanche is in an illegal time bin
C
                IF(NELE(JCL).EQ.0) GOTO 1
C
C  Determine the wire signal length (in time) for each arriving
C  cluster. Then form the avalanche at the wire and share the
C  released charge between the time bins. IRETA = 1 means that
C  the avalanche is in illegal time bins.
C
                CALL TPAVAL(JCL,IRETA)
                IF ( IRETA .EQ. 1) GOTO 1
C
C  Loop over affected time bins
C
                DO 11 IB = 1,NB
                  IBIN1 = IBIN(IB)
                  IF(IBIN1 .LE. 0 .OR. IBIN1 .GE. NTMXAN) GOTO 11
                  NAVELE = NAVBIN(IB)
C
C  Couple each avalanche to the long pads.
C
                  CALL TSPCPL(ISECT,ITYPE,WIRRAD(JCL),WIRPHI(JCL),
     &                      NLPHIT)
C
C  Add the signal to the ones that already exist
C  If there is not enough space to extend signal bank, skip processing
C  of analog signal for this sector.
C
                  CALL TPSGNL(IWIR,NLPHIT,NTPHIT,IRETS)
                  IF(IRETS.EQ.1) GOTO 99
C
C  End loop over time bins
C
11              CONTINUE
C
C  End loop over clusters in segment
C
 1            CONTINUE
C
C  End loop over super-broken segments
C
 2          CONTINUE
C
C  End loop over broken track elements
C
            CALL WDROP(IW,IDCLUS)
 3        CONTINUE
C
C  End loop over full track elements
C
 4      CONTINUE
C
C  Now we have analog signals for this sector. Compress out the
C  signal bank and save them if we want to
C
        CALL TPOPSG(NTSGHT,IERR)
        IF (IERR .EQ. 1) GOTO 99
C
C  Process the signals from this sector through the electronics.
C
        CALL TSRESP(ISECT,ITYPE)
C
C  End of sector; clean out sector-dependent banks and counters
C
 99     IF (IRETS .EQ. 1) WRITE(6,100) ISECT
        CALL TPENSC
          CALL BGARB(IW)
C
C  Next sector
C
 5    CONTINUE
C
C  Do wire data reduction and drop sector wire digitisings
C
      IF( ICTP(8) .EQ. 0) THEN
          CALL TWRRED(IER)
          IF (IER.EQ.1) THEN
            CALL ALTELL('TWRRED: No Run header found',0,'FATAL')
          ELSEIF (IER.EQ.2) THEN
            CALL ALTELL('TWRRED: Database Bank missing',0,'FATAL')
          ELSEIF (IER.EQ.5) THEN
            CALL ALTELL('TWRRED: BOS run out of space',1,'NEXT')
          ELSEIF (IER.EQ.6) THEN
            CALL ALTELL('TWRRED: NO pedestal Info found',0,'FATAL')
          ELSEIF (IER.EQ.7) THEN
            CALL ALTELL('TWRRED: missing TSWP Bank in DBASE',0,
     &                          'FATAL')
          ENDIF
          CALL BLIST(IW,'E+','TRIRTRDITSIRTSDITSLE')
          CALL BDROP(IW,'TWIRTWDI')
      ENDIF
C
C  End of event; write out and drop the event-dependent stuff
C
C
  999 CONTINUE
      CALL WDROP(IW,INDBRT)
      IF ( ICTP(4) .EQ. 1 ) THEN
         CALL BLIST(IW,'E+','TPTE')
         CALL BDROP(IW,'TPHE')
      ENDIF
C
      IF ( LTWDIG .AND. (ICTP(8).EQ.0) ) THEN
         CALL BLIST(IW,'E-','TWDI')
      ENDIF
C
C
C  Determine time used for digitizing TPC analog signal
C
      CALL TIMAX(T2)
      TDIGI = T2 - T1
      RTPSTA(44) = RTPSTA(44) + TDIGI
      IF (ICTP(5) .GT. 0) WRITE(IW(6),101) TDIGI

      RETURN
 100  FORMAT(//,' +++TGTEER+++ TPCSIM unable to create sufficient',
     &          ' space.',/,' Sector ',I2,' will not be fully',
     &          ' digitized.',//)
 101  FORMAT(/' +++TGTEER+++ Total time spent = ',F9.3,
     .        ' secs for digitizing TPC analog signal')
      END
