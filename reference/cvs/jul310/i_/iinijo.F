      SUBROUTINE IINIJO
C-----------------------------------------------------------------------
C! Initialise the ITC job
C!
C!   Author     :-  J. Sedgbeer  87/03/02
C!   Modified   :-  I. Tomalin   88/08/25
C!   Modified   :-  J. Sedgbeer  90/01/06 Set up ITC options as TINIJO
C!   Modified   :-  J. Sedgbeer  91/01/07 Correct write format.
C!
C!   Input:
C!       common       /BCS/
C!
C!   Output:
C!       common:     /ITTIME/ initialise timing information.
C!                   /IFLAGS/ ITC tracking options
C!
C!   called by : RINJOB
C!
C!   calls : IBNKIN      ITC Bos bank initialisation.
C!           IPDSTI      ITC Prepare Data Statistics Initialisation.
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
#include "iflags.h"
#include "ittime.h"
      CHARACTER*4 A,CHAINT
      EXTERNAL NAMIND,CHAINT
C-----------------------------------------------------------------------
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.3) WRITE(LDEBRL,1000)
 1000 FORMAT('0IINIJO: Initialise the ITC job')
C
C Initialise BOS banks for ITC
C
      CALL IBNKIN
C
C-----------------------------------------------------------------------
C Set ITC tracking options from the IOPT runcard.  Flags in /IFLAGS/
C have default values which can be changed with following options.
C
C  NOTP    turn off ITC-TPC tracking (i.e. no extending TPC tracks
C                                                       into ITC)
C  NOIT    turn off ITC alone tracking
C  NOEX    turn off extending of ITC alone tracks back into TPC (to pick
C          up extra TPC hits - especially for low theta tracks)
C  NOMA    turn off ITC-TPC track matching (subroutine IMATCH)
C  NOMC    turn off the track-MC association
C  NORE    turn off track refining/improving (sub. IREFIN)
C
      FTPCIF = .TRUE.
      FITCIF = .TRUE.
      FEXTIF = .TRUE.
      FMATIF = .TRUE.
      FMCAIF = .TRUE.
      FREFIF = .TRUE.
C
      KIOPT=IW(NAMIND('IOPT'))
      IF (KIOPT.NE.0) THEN
        DO 10 I=1,IW(KIOPT)
          A=CHAINT(IW(KIOPT+I))
C
C++       Ignore words which include a blank space
C
          DO 5 K=1,4
            IF (A(K:K).EQ.' ') GO TO 10
    5     CONTINUE
          IF (A.EQ.'NOTP' .OR. A.EQ.'notp') THEN
            FTPCIF = .FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOIT' .OR. A.EQ.'noit') THEN
            FITCIF = .FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOEX' .OR. A.EQ.'noex') THEN
            FEXTIF = .FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOMA' .OR. A.EQ.'noma') THEN
            FMATIF = .FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOMC' .OR. A.EQ.'nomc') THEN
            FMCAIF=.FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NORE' .OR. A.EQ.'nore') THEN
            FREFIF = .FALSE.
            WRITE(LOUTRL,105) A
          ELSE
            WRITE(LOUTRL,106) A
  106       FORMAT(' IINIJO:  unrecognized option ''',A4,
     +             ''' from the IOPT data card.')
          ENDIF
  105     FORMAT(' IINIJO: ''',A4,''' option set from IOPT data card.')
   10   CONTINUE
      ENDIF
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.1) THEN
        WRITE(LDEBRL,107) FTPCIF,FITCIF,FEXTIF,FMATIF,FREFIF,FMCAIF
  107   FORMAT(/' IINIJO:  Status of user flags set by',
     +                  ' IOPT data card:'/
     +    ' Do ITC-TPC tracking       =',L1/
     +    ' Do ITC alone tracking     =',L1/
     +    ' Add TPC hits to ITC alone tracks =',L1/
     +    ' Do ITC-TPC track matching =',L1/
     +    ' Do track refining/improving =',L1/
     +    ' Do track-MC association     =',L1/)
      ENDIF
C
C-----------------------------------------------------------------------
C Zero the time counters.
C
      TPREDA = 0.0
      TTRECI = 0.0
      TTCTPC = 0.0
      TTRECF = 0.0
      TTCTRK = 0.0
      TMATCH = 0.0
      TTRMCA = 0.0
      TNSECT = 0.0
      TSRCH  = 0.0
      TTLNK1 = 0.0
      TTREE1 = 0.0
      TFIND1 = 0.0
      TFITP1 = 0.0
      TTLNK2 = 0.0
      TTREE2 = 0.0
      TFIND2 = 0.0
      TFITP2 = 0.0
C
C-----------------------------------------------------------------------
C Initialise Prepare Data statistics
C
      CALL IPDSTI
C
      END
#endif
