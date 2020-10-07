      SUBROUTINE TPCREC
C
C----------------------------------------------------------------
C! Steering for TPC reconstruction
C!
C!    Author:    D. Schlatter
C!    Modified:  R. Johnson   19-01-88
C!    Modified:  R. Johnson   21-01-91 clean out unused code.
C!    Modified:  I. Tomalin    3-04-95 Calculate TPC bunch number.
C!    Modified:  D. Casper    05-11-94 calculate dE/dx from pad pulses.
C!    Modified:  D. Casper    23-10-95 refit after corrections, and remove
C!                                     filtered hits from coord list
C!
C!    Description
C!    ===========
C?      1.  find chains
C?      2.  find links
C?      3.  create track candidates
C?      4.  resolve coordinates in clusters with overlapping tracks
C?      5.  fit tracks
C?      6.  add wire pulses to tracks
C?      7.  calculate wire dE/dx for tracks
C?      8.  calculate pad dE/dx for tracks
C?      9.  recalculate coordinates, including wire corrections
C?      10. refit track and remove filtered coordinates
C?      11. associate tracks with MC truth information
C!---------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rlunit.h"
#include "tflags.h"
#include "bcs.h"
#include "tstate.h"
#include "tptime.h"
C Position of TPC bunch number in EVEH bank.
      PARAMETER(IBPOS=20,IBQUAL=23)
#include "evehjj.h"
C
      CHARACTER TEXT*100
      LOGICAL FIRST,RMOD
      LOGICAL IFTPTI
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        IFTPTI = IW( NAMIND('TPTI') ) .NE. 0
        FIRST=.FALSE.
        NTPCO=NAMIND('TPCO')
        NTCHA=NAMIND('TCHA')
        NTGFT=NAMIND('TGFT')
        NT1FT=NAMIND('T1FT')
        NTWRR=NAMIND('TWRR')
        NTWTB=NAMIND('TWTB')
        NTELS=NAMIND('TELS')
        NTARC=NAMIND('TARC')
        NTCAL=NAMIND('TCAL')
        NTPCH=NAMIND('TPCH')
        NT2XS=NAMIND('T2XS')
        NTNPX=NAMIND('TNPX')
        NTFIL=NAMIND('TFIL')
        NTPLS=NAMIND('TPLS')
      ENDIF
C
      IF( IFTPTI ) CALL ALTIME( TIME1 )
      IER=0
C
C++   Check that the data have been prepared
C
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0 .OR. NOTRKF) GO TO 999
C
C++   Find tracks using TPC coordinates (choice of two programs)
C
      ICNTER(LCTRKF)=ICNTER(LCTRKF)+1
C
      IF (FDEBRF) THEN
        KTPCO=IW(NTPCO)
        IF (KTPCO.NE.0) THEN
          NCO1=LROWS(KTPCO)
        ELSE
          NCO1=0
        ENDIF
      ENDIF
      IF( IFTPTI ) CALL ALTIME( TFCH1 )
      CALL TRKFND(IER)
      IF( IFTPTI ) CALL ALTIME( TFCH2 )
      TPTSUM(6) = TPTSUM(6) + (TFCH2 - TFCH1)
      IF (FDEBRF) THEN
        KTCHA=IW(NTCHA)
        IF (JDBDRF(JULTP).GE.1 .AND. KTCHA.GT.0) THEN
          WRITE(LDEBRL,1003) IRUNRC,IEVTRC,LROWS(KTCHA)
        ENDIF
 1003   FORMAT(' TRKFND for run ',I5,' event ',I6,' finds ',
     &         I3,' TPC chains')
        KTPCO=IW(NTPCO)
        IF (KTPCO.NE.0) THEN
          NCO2=LROWS(KTPCO)
        ELSE
          NCO2=0
        ENDIF
        IF (JDBDRF(JULTP).GE.1) THEN
          NCADD=NCO2-NCO1
          IF (NCADD.GT.0) WRITE(LDEBRL,1869) IRUNRC,IEVTRC,NCADD
 1869     FORMAT(' TRKFND for run ',I5,' event ',I6,' adds ',
     &           I2,' TPC coordinates at small angles.')
        ENDIF
        IF (JDBDRF(JULTP).GE.6) CALL TCHADP(LDEBRL)
      ENDIF
      IF (IER.NE.0) THEN
        IER=IER+2000
        GO TO 998
      ENDIF
      IF (FTSTAT) THEN
        KTCHA=IW(NTCHA)
        IF (KTCHA.GT.0) ITSTAT(LTCHTS)=ITSTAT(LTCHTS)+LROWS(KTCHA)
      ENDIF
C
C++   Find links between chains
C
      IF( IFTPTI ) CALL ALTIME( TMLK1 )
      CALL TFLNKS(IER)
      IF (IER.NE.0) THEN
        IER=IER+3000
        GO TO 998
      ENDIF
C
C++   Assemble track candidates
C
      CALL TFCAND(IER)
      IF (IER.NE.0) THEN
        IER=IER+4000
        GO TO 998
      ENDIF
      IF( IFTPTI ) CALL ALTIME( TMLK2 )
      IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
        KTCAL=IW(NTCAL)
        KTARC=IW(NTARC)
        WRITE(LDEBRL,1854) IRUNRC,IEVTRC,LROWS(KTCAL),LROWS(KTARC)
 1854   FORMAT(' TFCAND for run ',I5,' event ',I6,
     &         ' finds ',I3,' track candidates and ',I3,' arcs')
      ENDIF
      TPTSUM(10)=TPTSUM(10) + (TMLK2-TMLK1)
      IF (FCLEAN) CALL BDROP(IW,'TLNKTCRL')
      IF (JDBDRF(JULTP).GE.6) THEN
        CALL TARCDP(LDEBRL)
        CALL TCALDP(LDEBRL)
      ENDIF
C
C++   Correct coordinates at track overlaps
C
      IF( IFTPTI ) CALL ALTIME( TMOV1 )
      CALL TOVRLP(IER,NTOSS,NADD)
      IF( IFTPTI ) CALL ALTIME( TMOV2 )
      TPTSUM(11)=TPTSUM(11) + (TMOV2-TMOV1)
      IF (IER.NE.0) THEN
        IER=IER+5000
        GO TO 998
      ENDIF
      IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
        WRITE(LDEBRL,1654) IRUNRC,IEVTRC,NTOSS,NADD
 1654   FORMAT(' TOVRLP for run ',I5,' event ',I6,' removes',
     &         I3,' coordinates and adds ',I3,' twin coordinates',
     &         ' to track candidates')
      ENDIF
      IF (FTSTAT) THEN
        ITSTAT(LTPCOV)=ITSTAT(LTPCOV)+NTOSS
        ITSTAT(LTPCTW)=ITSTAT(LTPCTW)+NADD
      ENDIF
      IF (JDBDRF(JULTP).GE.6) THEN
        CALL TCALDP(LDEBRL)
      ENDIF
      IF (FCLEAN) CALL BDROP(IW,'TCHATCTC')
C
C++   Fit TPC tracks
C
      ICNTER(LCTRFT)=ICNTER(LCTRFT)+1
      IF( IFTPTI ) CALL ALTIME( TFIT1 )
      CALL TFITTK(IER)
      IF( IFTPTI ) CALL ALTIME( TFIT2 )
      TPTSUM(7)=TPTSUM(7) + (TFIT2-TFIT1)
      IF (IER.NE.0) THEN
        IER=IER+6000
        GO TO 998
      ENDIF
      IF (FDEBRF) THEN
        KT1FT=IW(NT1FT)
        IF (JDBDRF(JULTP).GE.1 .AND. KT1FT.GT.0) THEN
          WRITE(LDEBRL,1835) IRUNRC,IEVTRC,LROWS(KT1FT)
 1835     FORMAT(' TFITTK for run ',I5,' event ',I6,' fits ',
     &           I3,' TPC tracks')
        ENDIF
        IF (JDBDRF(JULTP).GE.3) CALL T1FTDP(LDEBRL)
      ENDIF
      IF (FTSTAT) THEN
        KT1FT=IW(NT1FT)
        IF (KT1FT.GT.0) ITSTAT(LTGFTS)=ITSTAT(LTGFTS)+LROWS(KT1FT)
      ENDIF
      IF (FCLEAN) CALL BDROP(IW,'TTCC')
C
C++   Try to link together broken tracks
C
      KT1FT=IW(NT1FT)
      IF (KT1FT.NE.0) THEN
        NTK1=LROWS(KT1FT)
      ELSE
        NTK1=0
      ENDIF
      IF( IFTPTI ) CALL ALTIME( TFLK1 )
      CALL TFLNK2(IER)
      IF( IFTPTI ) CALL ALTIME( TFLK2 )
      TPTSUM(13)=TPTSUM(13) + (TFLK2-TFLK1)
      KTGFT=IW(NTGFT)
      IF (KTGFT.NE.0) THEN
        NTK2=LROWS(KTGFT)
      ELSE
        NTK2=0
      ENDIF
      NJOIN=NTK1-NTK2
      IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
        WRITE(LDEBRL,1764) IRUNRC,IEVTRC,NJOIN
 1764   FORMAT(' TFLNK2 for run ',I5,' event ',I6,' joins ',
     &          I2,' TPC track pairs.')
      ENDIF
      IF (FTSTAT) THEN
        ITSTAT(LTPCTL)=ITSTAT(LTPCTL)+NJOIN
      ENDIF
      IF (FCLEAN) CALL BDROP(IW,'T1TLT1CL')
C
C++   Associate wires with tracks and calculate wire dE/dx
C
      KTWRR=IW(NTWRR)
      IF (.NOT.FNOWIR .AND. KTWRR.GT.0) THEN
        ICNTER(LCTRWR)=ICNTER(LCTRWR)+1
        CALL RNXMOD(MODTPW,RMOD)
      IF( IFTPTI ) CALL ALTIME( TMTW1 )
        IF (RMOD) THEN
          CALL TRKWRA(IER)
        ENDIF
      IF( IFTPTI ) CALL ALTIME( TMTW2 )
        TPTSUM(8)=TPTSUM(8) + (TMTW2-TMTW1)
        IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
          KTWTB=IW(NTWTB)
          IF (KTWTB.GT.0) THEN
            WRITE(LDEBRL,1402) IRUNRC,IEVTRC,LROWS(KTWTB)
          ENDIF
 1402     FORMAT(' TRKWRA for run ',I5,' event ',I6,' associates ',
     &           I5,' wire pulses with tracks')
        ENDIF
        IF (FTSTAT) THEN
          KTWTB=IW(NTWTB)
          IF (KTWTB.GT.0) ITSTAT(LTWTTS)=ITSTAT(LTWTTS)+LROWS(KTWTB)
        ENDIF
        IF (IER.NE.0) THEN
          IER=IER+7000
          GO TO 998
        ENDIF
C
        ICNTER(LCELOS)=ICNTER(LCELOS)+1
        CALL RNXMOD(MODDEX,RMOD)
      IF( IFTPTI ) CALL ALTIME( TMEL1 )
        IF (RMOD) THEN
C
C++       dE/dx for non-overlapping tracks
C
          CALL TRKELS(IER)
C
C++       dE/dx for pairs of overlapping tracks
C
          IF (IER.EQ.0) CALL TTWODX(IER)
        ENDIF
      IF( IFTPTI ) CALL ALTIME( TMEL2 )
        TPTSUM(9)=TPTSUM(9) + (TMEL2-TMEL1)
        IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
          KTELS=IW(NTELS)
          IF (KTELS.GT.0) THEN
            WRITE(LDEBRL,1302) IRUNRC,IEVTRC,LROWS(KTELS)
          ENDIF
 1302     FORMAT(' TRKELS for run ',I5,' event ',I6,
     &           ' calculates dE/dx for ',I3,' TPC tracks.')
          KT2XS=IW(NT2XS)
          IF (KT2XS.GT.0) THEN
            WRITE(LDEBRL,1303) IRUNRC,IEVTRC,LROWS(KT2XS)
 1303       FORMAT(' TTWODX for run ',I5,' event ',I6,
     &           ' calculates dE/dx for ',I3,' pairs.')
          ENDIF
        ENDIF
      ENDIF
      IF (FTSTAT) THEN
        KTELS=IW(NTELS)
        IF (KTELS.GT.0) ITSTAT(LTELTS)=ITSTAT(LTELTS)+LROWS(KTELS)
      ENDIF
C
C++   Update coordinate calculation now that the track parameters
C++   are known.
C
      IF( IFTPTI ) CALL ALTIME( TMCT1 )
      CALL TCOORT(IER)
      IF (FCLEAN) CALL BDROP(IW,'TSPU')
      IF( IFTPTI ) CALL ALTIME( TMCT2 )
      TPTSUM(12)=TPTSUM(12) + (TMCT2-TMCT1)
C
C++   Final TPC fit, using corrected coordinates
C++   Remove outliers from associated coordinate list
C
      IF (IW(NTFIL).NE.0) THEN
      IF( IFTPTI ) CALL ALTIME( TFIT1 )
        CALL TFITKF(IER)
      IF( IFTPTI ) CALL ALTIME( TFIT2 )
        TPTSUM(7)=TPTSUM(7) + (TFIT2-TFIT1)
        CALL AUBPRS('TGCL')
        IF (IER.NE.0) THEN
          IER=IER+9000
          GO TO 998
        ENDIF
        IF (FDEBRF) THEN
          KTGFT=IW(NTGFT)
          IF (JDBDRF(JULTP).GE.1 .AND. KTGFT.GT.0) THEN
            WRITE(LDEBRL,2835) IRUNRC,IEVTRC,LROWS(KTGFT)
 2835       FORMAT(' TFITKF for run ',I5,' event ',I6,' fits ',
     &           I3,' TPC tracks')
          ENDIF
          IF (JDBDRF(JULTP).GE.3) CALL TGFTDP(LDEBRL)
        ENDIF
      ENDIF
C
C++   Accumulate subcluster pulseheights for dE/dx using pads.
C
      IF (NTK2.GT.0) THEN
          IF(IW(NTNPX).EQ.0) THEN
            ICNTER(LCPDEX) = ICNTER(LCPDEX) + 1
      IF( IFTPTI ) CALL ALTIME( TPDX1 )
            CALL TPADDX
            CALL TPDELS
      IF( IFTPTI ) CALL ALTIME( TPDX2 )
            TPTSUM(14)=TPTSUM(14) + (TPDX2-TPDX1)
            IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
                KTPLS=IW(NTPLS)
                IF (KTPLS.GT.0) THEN
                    WRITE(LDEBRL,1304) IRUNRC,IEVTRC,LROWS(KTPLS)
                ENDIF
 1304           FORMAT(' TPDELS for run ',I5,' event ',I6,
     &           ' calculates pad dE/dx for ',I3,' TPC tracks.')
            ENDIF
            IF (FTSTAT) THEN
                KTPLS=IW(NTELS)
                IF (KTPLS.GT.0) ITSTAT(LTPLTS)=ITSTAT(LTPLTS)+
     &                  LROWS(KTPLS)
            ENDIF
          ENDIF
      END IF
C
C++   For MC data produced simply by smearing the MC hits, TPCH should
C++   be on the input tape.  Otherwise we construct it here.
C
      IF (FMCTKA) THEN
        KTPCH=IW(NTPCH)
        IF (KTPCH.NE.0) CALL BDROP(IW,'TPCH')
        CALL TREFCH(KTPCH,IGARB)
        IF (IGARB.EQ.2) THEN
          CALL RERROR('TPCREC',3,'No room for bank TPCH')
        ENDIF
      ENDIF
      IF (FCLEAN) CALL BDROP(IW,'TSRL')
C
C++   Associate tracks with the MC true tracks
C
      IF (FMCTKA) THEN
        CALL TRKMCA
        IF (FDEBRF .AND. JDBDRF(JULTP).GE.5) THEN
          CALL TGMADP(LDEBRL)
        ENDIF
      ENDIF
C
C++   Do FASGAL dE/dx analysis.  Does nothing to GALEPH output.
C
      CALL FAWIAN
C
C++   Calculate TPC bunch number
C
      IF (.NOT.FPASS0) THEN
        KEVEH = IW(NAMIND('EVEH'))
        IF (KEVEH.GT.0) THEN
          CALL TPCBUN(ITPBUN,IQUAL)
          IF (IQUAL.GT.0) THEN
            CALL MVBITS(ITPBUN,0,3,IW(KEVEH+JEVEM4),IBPOS)
            CALL MVBITS(IQUAL ,0,2,IW(KEVEH+JEVEM4),IBQUAL)
          END IF
        END IF
      END IF
C
C++   Accumulate monitoring statistics
C
      CALL TACCMN
C
      IF (FCLEAN) CALL BDROP(IW,'TWOLTWRRTWPUTWTBTWITTWAT')
      IF (IER.NE.0) THEN
        IER=IER+8000
        GO TO 998
      ENDIF
C
      GO TO 999
C
C++   Report errors which leave the event not fully reconstructed
C
  998 CONTINUE
      IF(IER.NE.4002)THEN
        WRITE(TEXT,101) IER
        CALL RERROR('TPCREC',1,TEXT(1:90))
      ENDIF
  101 FORMAT('ERROR RETURN=',I4,' FROM TPC RECONSTRUCTION.&',
     &       'Reconstruction is not complete for this event.')
  999 CONTINUE
C
C++   Debug summary:
C
      IF (FDEBRF) THEN
        IF (JDBDRF(JULTP).GE.1) WRITE(LDEBRL,1672) IRUNRC,IEVTRC
 1672   FORMAT(' TPC reconstruction finished for run ',I5,
     &         ' event ',I6)
        IF (JDBDRF(JULTP).EQ.2) CALL TGFTDP(LDEBRL)
        IF (JDBDRF(JULTP).EQ.3) CALL TRKDMP(LDEBRL,1)
        IF (JDBDRF(JULTP).GE.4) CALL TRKDMP(LDEBRL,3)
        IF (JDBDRF(JULTP).GE.6) THEN
          CALL TBPRNT
        ENDIF
      ENDIF
      IF( IFTPTI ) CALL ALTIME( TIME2 )
      TPTSUM(2)=TPTSUM(2) + (TIME2-TIME1)
      CALL RNXMOD(MODEND,RMOD)
C
      RETURN
      END
#endif
