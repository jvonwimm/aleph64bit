      SUBROUTINE TPRSUM
C
C-------------------------------------------------------------------
C! Job summary from TPC reconstruction
C!
C!    Author:       R. Johnson     03-06-87
C!    Modified:     D. Casper      04-11-95  add pad dE/dx stats
C!
C!------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tstate.h"
#include "tptime.h"
#include "rlunit.h"
#include "tflags.h"
#include "rparac.h"
#include "rflags.h"
#include "tpgpar.h"
#include "tmonit.h"
C
      PARAMETER( NTXT =  3)
      CHARACTER*60 TRTXT(NTXT), DETXT(NTXT), DPTXT(NTXT)
      PARAMETER (MXLEN=130,LMXTX=400)
      CHARACTER TEXT*140,TBUF*(LMXTX),CHAINT*4,CALID*8

      DATA TRTXT /
     &       ' Number of tracks per event, number of used coordinates',
     &       ' per event, number of unused coordinates per event',
     &       ' and number of bad coordinates per event for each sector:'
     &       /
      DATA DETXT /
     &       '  ',
     &       ' Uncorrected mean dE/dx for mips',
     &       ' for each TPC sector:'
     &       /
      DATA DPTXT /
     &       '  ',
     &       ' Corrected mean dE/dx for mips',
     &       ' for each TPC sector:  '
     &       /
C
C--------------------------------------------------------------------
C
#include "bmacro.h"
C
C--------------------------------------------------------------------
C
      WRITE(LOUTRL,20)
   20 FORMAT(//1X,T20,'JOB Summary for TPC Reconstruction')
      WRITE(LOUTRL,21)
   21 FORMAT(1X,T20,'--- ------- --- --- --------------'/)
C
C++   Print summary of event counts
C
      WRITE(LOUTRL,50) ICNTER(LCPRED),
     &                 ICNTER(LCPADS),ICNTER(LCWIRE),ICNTER(LCMAGN),
     &                 ICNTER(LCTPCO),ICNTER(LCTRKF),ICNTER(LCTRFT),
     &                 ICNTER(LCTRWR),ICNTER(LCELOS),ICNTER(LCPDEX)
   50 FORMAT(' Number of events processed=',T55,I6/
     &       ' Number of events with pad data analyzed=',T55,I6/
     &       ' Number of events with wire data analyzed=',T55,I6/
     &       ' Number of events with HALL probe read=',T55,I6/
     &       ' Number of events with coordinates calculated=',T55,I6/
     &       ' Number of events with track finding=',T55,I6/
     &       ' Number of events with track fitting=',T55,I6/
     &       ' Number of events with track-wire association=',T55,I6/
     &       ' Number of events with wire energy loss',
     &       '  calculation=',T55,I6/
     &       ' Number of events with pad energy loss',
     &       ' calculation=',T55,I6)
C
C++   Print time summary
C
      IF (ICNTER(LCPRED).GT.0) THEN
        DO 54 I=1,NTIMEC
          TPTSUM(I)=TPTSUM(I)/FLOAT(ICNTER(LCPRED))
   54   CONTINUE
        WRITE(LOUTRL,57)
   57   FORMAT(/' Time in seconds per event spent in TPC',
     &          ' reconstruction modules:')
        WRITE(LOUTRL,58) (TPTSUM(I),I=1,NTIMEC)
   58   FORMAT('   TPREDA:',T25,F10.5/,
     &         '   TPCREC:',T25,F10.5/,
     &         '   TPADS:',T25,F10.5/,
     &         '   TWIRES:',T25,F10.5/,
     &         '   TCODRV:',T25,F10.5/,
     &         '   PATREC:',T25,F10.5/,
     &         '   TFITTK:',T25,F10.5/,
     &         '   TRKWRA:',T25,F10.5/,
     &         '   TRKELS + TTWODX:',T25,F10.5/,
     &         '   TFLNKS + TFCAND:',T25,F10.5/,
     &         '   TOVRLP:',T25,F10.5/,
     &         '   TCOORT:',T25,F10.5/,
     &         '   TFLNK2:',T25,F10.5/,
     &         '   TPADDX + TPDELS:',T25,F10.5)
      ENDIF
C
C++   Print job statistics
C
      REVT=FLOAT(ICNTER(LCPRED))
      IF (FTSTAT .AND. REVT.GT.0.) THEN
        RPUL=FLOAT(ITSTAT(LPULTS))/REVT
        RCLU=FLOAT(ITSTAT(LCLUTS))/REVT
        RTSP=FLOAT(ITSTAT(LTSPTS))/REVT
        RTSC=FLOAT(ITSTAT(LTSCTS))/REVT
        RTPC=FLOAT(ITSTAT(LTPCTS))/REVT
        RTWP=FLOAT(ITSTAT(LTWPTS))/REVT
        RTCH=FLOAT(ITSTAT(LTCHTS))/REVT
        RTGF=FLOAT(ITSTAT(LTGFTS))/REVT
        RTWT=FLOAT(ITSTAT(LTWTTS))/REVT
        RTEL=FLOAT(ITSTAT(LTELTS))/REVT
        RTOV=FLOAT(ITSTAT(LTPCOV))/REVT
        RTTW=FLOAT(ITSTAT(LTPCTW))/REVT
        RTTL=FLOAT(ITSTAT(LTPCTL))/REVT
        RTPX=FLOAT(ITSTAT(LTPLTS))/REVT
        WRITE(LOUTRL,60) RPUL,RCLU,RTSP,RTSC,RTPC,
     &                   RTWP,RTCH,RTOV,RTTW,RTGF,RTTL,RTWT,
     &                   RTEL,RTPX
   60   FORMAT(/' Statistics summary for TPC reconstruction:'/
     &          ' Number of pulses per event=',T50,F10.2/
     &          ' Number of clusters per event=',T50,F10.2/
     &          ' Number of subpulses per event=',T50,F10.2/
     &          ' Number of subclusters per event=',T50,F10.2/
     &          ' Number of coordinates per event=',T50,F10.2/
     &          ' Number of wire pulses per event=',T50,F10.2/
     &          ' Number of chains per event=',T50,F10.2/
     &          ' Number of coords. removed per event=',T50,F10.2/
     &          ' Number of twin coords used=',T50,F10.2/
     &          ' Number of tracks per event=',T50,F10.2/
     &          ' Number of tracks linked per event=',T50,F10.2/
     &          ' Number of wires on tracks per event=',T50,F10.2/
     &          ' Number of tracks with wire dE/dX per event=',
     &          T50,F10.2/
     &          ' Number of tracks with pad dE/dx per event=',
     &          T50,F10.2)
      ENDIF
C
C++   Print monitoring information
C
      IF (NEVTTM.LT.1) RETURN
      WRITE(LOUTRL,80) NEVTTM
      WRITE(LOUTRL,85) (TRTXT(I),DETXT(I),I=1,NTXT)
      WRITE(LOUTRL,86)
   80 FORMAT(/' TPC job monitoring summary for ',I6,' events:'//)
   85 FORMAT(A,5X,A)
   86 FORMAT(/,
     &       ' SLOT  ',4X,'TRACKS',3X,'USED COORD',3X,'UNUSED COORD',3X,
     &       'BAD COORD',T60,'ENTRIES',T76,'RPI(WIRE)',T95,'RMS',
     &        T109,'RPI(PAD)',T126,'RMS')
      DO 81 I=1,LTSECT
        RTKS=NTKSTM(I)/FLOAT(NEVTTM)
        RUSC=NUSCTM(I)/FLOAT(NEVTTM)
        RUUC=NUUCTM(I)/FLOAT(NEVTTM)
        RBAD=NBADTM(I)/FLOAT(NEVTTM)
        IF (NDEXTM(I).LT.1) THEN
          WRITE(LOUTRL,82) I,RTKS,RUSC,RUUC,RBAD,
     &      '  No information available'
   82     FORMAT(1X,I2,4(3X,F10.5),T60,A)
        ELSE
          ATRM=DEDXTM(I)/FLOAT(NDEXTM(I))
          STRM=DEX2TM(I)/FLOAT(NDEXTM(I))-ATRM**2
          STRM=SQRT(AMAX1(0.,STRM))
          ERR=STRM/SQRT(FLOAT(NDEXTM(I)))
          IF (NDPXTM(I).LT.1)THEN
            WRITE(LOUTRL,83) I,RTKS,RUSC,RUUC,RBAD,NDEXTM(I),
     &          ATRM,ERR,STRM,' No information available'
   83       FORMAT(
     &        1X,I2,4(3X,F10.5),T60,
     &        I6,3X,F9.5,' +- ',F7.5,3X,F7.5,T109,A)
          ELSE
            ATRMP=DPDXTM(I)/FLOAT(NDPXTM(I))
            STRMP=DPX2TM(I)/FLOAT(NDPXTM(I))-ATRMP**2
            STRMP=SQRT(AMAX1(0.,STRMP))
            ERRP=STRMP/SQRT(FLOAT(NDPXTM(I)))
            WRITE(LOUTRL,183) I,RTKS,RUSC,RUUC,RBAD,NDEXTM(I),
     &          ATRM,ERR,STRM,ATRMP,ERRP,STRMP
  183       FORMAT(
     &        1X,I2,4(3X,F10.5),T60,
     &        I6,2(3X,F9.5,' +- ',F7.5,3X,F7.5))
          ENDIF
        ENDIF
   81 CONTINUE
C
      IF (NT0STM.GE.1) THEN
        AT0=T0SHTM/FLOAT(NT0STM)
        ST0=T0S2TM/FLOAT(NT0STM)-AT0**2
        ST0=SQRT(AMAX1(0.,ST0))
        ERR=ST0/SQRT(FLOAT(NT0STM))
        WRITE(LOUTRL,84) NT0STM,AT0,ERR,ST0
   84   FORMAT(/' T0 difference, TPC end-A minus TPC end-B for ',
     &         I5,' events:'/
     &         ' delta-T=',F10.5,'+-',F7.5,' mus; rms=',F7.5,' mus')
      ENDIF
      IF (NTDXTM.GE.6) THEN
        ATRM=TDEXTM/FLOAT(NTDXTM)
        STRM=TDX2TM/FLOAT(NTDXTM)-ATRM**2
        STRM=SQRT(AMAX1(0.,STRM))
        ERR=STRM/SQRT(FLOAT(NTDXTM))
        WRITE(LOUTRL,88) NTDXTM,ATRM,ERR,STRM
   88   FORMAT(/' Average Ratio of wire dE/dx over the expected',
     &      ' for a pion,',
     &      ' measured from ',I6,' tracks=',F9.5,
     &      '+-',F7.5,';  rms=',F7.5)
      ENDIF
      IF (NTPXTM.GE.6) THEN
        ATRMP=TDPXTM/FLOAT(NTPXTM)
        STRMP=TPX2TM/FLOAT(NTPXTM)-ATRMP**2
        STRMP=SQRT(AMAX1(0.,STRMP))
        ERR=STRMP/SQRT(FLOAT(NTPXTM))
        WRITE(LOUTRL,188) NTPXTM,ATRMP,ERRP,STRMP
  188   FORMAT(/' Average Ratio of pad  dE/dx over the expected',
     &      ' for a pion,',
     &      ' measured from ',I6,' tracks=',F9.5,
     &      '+-',F7.5,';  rms=',F7.5)
      ENDIF
C
      IF ( JPRERF(JULTP).GE.2 ) THEN
        WRITE(LOUTRL,95)
   95   FORMAT(/' Summary of data format errors for the TPC readout:'/
     &    ' First, the number of times wire data are truncated,'/
     &    ' the number of times pad data are truncated, the ',/
     &    ' number of times data are missing on input, the number ',/
     &    ' of times the wire TPP is out of sequence in event counts',/
     &    ' and the number of times the pad TPP is out of sequence.',/
     &    /' SLOT',2X,'Truncated Pads',1X,'Truncated Wires',2X,
     &    'Missing Data',2X,'Wire Trig. Mis',2X,'Pad Trig. Mismatch')
        DO 97 I=1,LTSECT
          WRITE(LOUTRL,96) I,NWTKTM(I),NPTKTM(I),NMISTM(I),NWSYTM(I),
     &                     NPSYTM(I)
   96     FORMAT(1X,I2,5(5X,I10))
   97   CONTINUE
        WRITE(LOUTRL,99)
   99   FORMAT(/
     &    ' Second, the number of times each sector is dropped due'/
     &    ' to data format errors, the number of times the number of'/
     &    ' hits on a padrow is too large, the number of times the '/
     &    ' padrow number is out of range or out of sequence, the'/
     &    ' number of times a bad hit word is encountered, the number'/
     &    ' of times there is a TPAD, TPDI mismatch, and the number'/
     &    ' of times pad data are truncated due to data format errors.'/
     &    /' SLOT',2X,'Dropped Data',3X,'# Hits Too Big',1X,
     &    'Bad Padrow No.',1X,'Bad Hit Word',3X,'TPAD-TPDI Mis',2X,
     &    'Truncated Data')
        DO 101 I=1,LTSECT
          WRITE(LOUTRL,102) I,NDRPTM(I),NHORTM(I),NROWTM(I),NBDHTM(I),
     &                      NTPDTM(I),NCPDTM(I)
  102     FORMAT(1X,I2,6(5X,I10))
  101   CONTINUE
      ENDIF
C
      RETURN
      END
#endif
