      SUBROUTINE TINIJO
C
C-----------------------------------------------------------------
C! Initialize job for TPC
C!
C!    Author:   R. Johnson    15-04-87
C!    Modified: J.Knobloch    23-03-89
C!    Modified: R. Johnson    21-01-91   clean up unused flags
C!    Modified: D. Casper     04-11-95   add pad dE/dx monitoring
C!                            05-03-97   change default for TOPT 'TWPU';
C!                                       the bank TWPU is necessary for
C!                                       TPC wire coordinates
C!
C!    Description
C!    ===========
C!    This routine is called once at the JOB beginning to
C!    initialize the TPC part of the reconstruction program.
C!
C!----------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "rflags.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tmonit.h"
#include "bcs.h"
#include "rlunit.h"
#include "tflags.h"
#include "tpatel.h"
#include "tcocom.h"
#include "trfdct.h"
#include "tstate.h"
#include "tptime.h"
C
      CHARACTER A*4,TEXT*80,CHAINT*4
C
C++   Include data statements for lookup tables to initialize TCOCOM
C
#include "tplkup.h"
C
C++   Initialize array for measuring execution times
C
      DO 23 I=1,NTIMEC
        TPTSUM(I)=0.
   23 CONTINUE
C
C++   Initialize names and formats of TPC BOS banks
C
      CALL TBNKIN
C
C++   Initialize TLIPAK
C
      CALL TLINIT
C
C++   Initialize TPC coordinate algorithms
C
      CALL UCOPY(MP1,MOMPH1,100)
      CALL UCOPY(MP2,MOMPH2,100)
      CALL UCOPY(MP3,MOMPH3,100)
      CALL UCOPY(MP4,MOMPH4,100)
C
C++   Set the run options from the TOPT runcard.  Flags in TFLAGS
C++   will be set true if the appropriate keywords appear in the
C++   TOPT runcard.  Only the first 4 characters are matched.  The
C++   total number of charactes given per option should not exceed
C++   seven
C
C        MAGNet  results in trying to read B-field from Hall probe
C        NOWIres results in wire analysis being done
C        NOPAds  results in not doing analysis of pads and tracks
C        NOTRkf  results in not doing track finding and fitting
C        NOSTat  results in statistics not being accumulated
C        TWPU    enable filling of the bank TWPU by TWIRES
C        TCOR    results in drift field corrections for coordinates
C        NOMC    results in turning off the track-MC association
C        OLDW    use old raw wire banks TWIR and TWDI for input
C        MODE    use MODE=0 in call to TLACOR
C
      MAGNET=.FALSE.
      FNOWIR=.FALSE.
      NOPADS=.FALSE.
      NOTRKF=.FALSE.
      FTSTAT=.TRUE.
      FCLEAN=.TRUE.
      FWIRPU=.TRUE.
      FFCORR=.TRUE.
      FECORR=.TRUE.
      FMCTKA=.TRUE.
      FOLDWR=.FALSE.
      FSRTDX=.TRUE.
      FFMODE=.TRUE.
C
      KTOPT=IW(NAMIND('TOPT'))
      IF (KTOPT.NE.0) THEN
        DO 10 I=1,IW(KTOPT)
          A=CHAINT(IW(KTOPT+I))
C
C++       Ignore words which include a blank space
C
          DO 5 K=1,4
            IF (A(K:K).EQ.' ') GO TO 10
    5     CONTINUE
          IF (A.EQ.'MAGN' .OR. A.EQ.'magn') THEN
            MAGNET=.TRUE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOWI' .OR. A.EQ.'nowi') THEN
            FNOWIR=.TRUE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOPA' .OR. A.EQ.'nopa') THEN
            NOPADS=.TRUE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOTR' .OR. A.EQ.'notr') THEN
            NOTRKF=.TRUE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOST' .OR. A.EQ.'nost') THEN
            FTSTAT=.FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'JUNK' .OR. A.EQ.'junk') THEN
            FCLEAN=.FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'TWPU' .OR. A.EQ.'twpu') THEN
            FWIRPU=.FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOBC' .OR. A.EQ.'nobc') THEN
            FFCORR=.FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOEC' .OR. A.EQ.'noec') THEN
            FECORR=.FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'NOMC' .OR. A.EQ.'nomc') THEN
            FMCTKA=.FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'OLDW' .OR. A.EQ.'oldw') THEN
            FOLDWR=.TRUE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'OLDX' .OR. A.EQ.'oldx') THEN
            FSRTDX=.FALSE.
            WRITE(LOUTRL,105) A
          ELSEIF (A.EQ.'MODE' .OR. A.EQ.'mode') THEN
            FFMODE=.FALSE.
            WRITE(LOUTRL,105) A
          ELSE
            WRITE(LOUTRL,106) A
  106       FORMAT(' TINIJO:  unrecognized option ''',A4,
     &             ''' from the TOPT data card.')
          ENDIF
  105     FORMAT(' TINIJO: ''',A4,''' option set from TOPT data card.')
   10   CONTINUE
      ENDIF
      IF (FDEBRF) WRITE(LOUTRL,107) MAGNET,NOPADS,FNOWIR,NOTRKF,FTSTAT,
     &                              FCLEAN,FWIRPU,FFCORR,FECORR,FMCTKA,
     &                              FOLDWR,FSRTDX,FFMODE
  107 FORMAT(/' TINIJO:  Status of user flags set by',
     &                  ' TOPT data card:'/
     &        ' MAGNet-- take B-field from Hall probe=',L1/
     &        ' NOPAds-- don''t use pad data for clusters=',L1/
     &        ' NOWIre-- don''t use wire data=',L1/
     &        ' NOTRkf-- don''t do track finding=',L1/
     &        ' NOSTat-- print out statistics at EOJ=',L1/
     &        ' JUNK  -- keep some large banks=',L1/
     &        ' TWPU  -- fill TWPU expanded wire pulse bank=',L1/
     &        ' NOBCor-- correct coordinates for ',
     &                        'B-field distortions=',L1/,
     &        ' NOECor-- correct coordinates for ',
     &                        'transverse drift field=',L1/,
     &        ' NOMC  -- do track-MC association=',L1/,
     &        ' OLDW  -- force use of old wire raw data banks=',L1/,
     &        ' OLDX  -- sort dE/dx by sector for each track=',L1/,
     &        ' MODE  -- use B-field displacement table in',
     &                  ' TLACOR (MODE=1)=',L1)
C
C++   Print message about recent changes
C
C
C++   Initialize error and event and statistics counters in TSTATE
C
      DO 20 I=1,LNTCNT
        ICNTER(I)=0
   20 CONTINUE
      DO 40 I=1,LTSTAT
        ITSTAT(I)=0
   40 CONTINUE
C
C++   Initialize counters in common TMONIT
C
      NEVTTM=0
      DO 48 I=1,LTSECT
        NTKSTM(I)=0
        NUUCTM(I)=0
        DEDXTM(I)=0.
        DEX2TM(I)=0.
        NDEXTM(I)=0
        NUSCTM(I)=0
        NWTKTM(I)=0
        NPTKTM(I)=0
        NMISTM(I)=0
        NWSYTM(I)=0
        NPSYTM(I)=0
        NBADTM(I)=0
        NDRPTM(I)=0
        NHORTM(I)=0
        NROWTM(I)=0
        NBDHTM(I)=0
        NTPDTM(I)=0
        NCPDTM(I)=0
        DPDXTM(I)=0.
        DPX2TM(I)=0.
        NDPXTM(I)=0
   48 CONTINUE
      TDEXTM=0.
      TDX2TM=0.
      NTDXTM=0
      T0SHTM=0.
      T0S2TM=0.
      NT0STM=0
      TDPXTM=0.
      TPX2TM=0.
      NTPXTM=0
C
      RETURN
      END
#endif
