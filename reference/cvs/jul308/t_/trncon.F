      SUBROUTINE TRNCON
C
C-----------------------------------------------------------------------
C! Load TPC run constants
C!
C!    Author:    R. Johnson   16-12-86
C!    Modified:  M.Mermikides 13-03-87
C!               R. Johnson   11-09-91
C!               E. Lancon    25-02-92 use alephlib routine TPDVEL to
C!                                     load drift velocity
C!               R. Johnson    2-3-93  add TC7X bank
C!                             5-9-93  add DZNLTW cut to TC7X
C!               E. Lancon    21.7.93  Load TDXC bank from daf
C!               F.Ranjard    23.3.94  use GTT0GL to get T0GL bank
C!                                     use T0GL bank even for MCdata
C!                                     when TPCSIM version > 217
C!                           10.5.94   fill sector gain map :
C!                                     /TMONIT/GAINTM(LTSECT) with
C!                                     TC2X info for run<20000 or
C!                                     TCSX info for run>20000
C!                                     if no bank is found then gain=1.
C!                                     book DEDX histograms or reset them
C!              F.Ranjard    29.8.94   wire pedestal is set to 0
C!                                     for runs > 28833 since pedestals
C!                                     are also substracted for
C!                                     unreduced wire hits in online
C!                                     DAQ program.
C!              I.Tomalin     3.4.95   Added TVOFS0 and TPCCLK to
C!                                     /TPCCON/ to cope with bunch trains.
C!              D.Casper     26.10.95  Read pad dE/dx constants, book histos
C!                           05.06.96  Read constants for pulse-shape correction
C!                                     from TSHP bank on database.
C!              M.Cattaneo   20.08.97  Read TNRN bank from database 
C!
C!    Called by TINIRU
C!
C!    Description
C!    ===========
C!    Parameters for TPC reconstruction which are constant for a run
C!    are initialized here.  The data comes from a direct access
C!    file.  Temporarily, at least, if the direct access data is not
C!    found then some default numbers are loaded in by hand.  The user
C!    may also put the banks onto input cards, in which case the
C!    direct access banks will be ignored.
C!----------------------------------------------------------------------
#ifndef DOC
      CHARACTER*3 CH3
#include "alcons.h"
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tmonit.h"
#include "rlunit.h"
#include "tswpjj.h"
#include "trscjj.h"
#include "t0gljj.h"
#include "t0rljj.h"
#include "tconjj.h"
#include "tccnjj.h"
#include "tcrcjj.h"
#include "tclbjj.h"
#include "twrcjj.h"
#include "twtcjj.h"
#include "tlctjj.h"
#include "tsimjj.h"
#include "tsorjj.h"
#include "tshpjj.h"
#include "bcs.h"
#include "rcurnt.h"
#include "talign.h"
#include "tpccon.h"
#include "tpccut.h"
#include "trfdct.h"
#include "tcluct.h"
#include "tzcorr.h"
#include "twirct.h"
#include "rflags.h"
#include "tflags.h"
#include "twircl.h"
#include "tpadcl.h"
#include "tshpco.h"
C
#include "tc1xjj.h"
#include "tp1xjj.h"
#include "tc2xjj.h"
#include "tc5xjj.h"
#include "tc6xjj.h"
#include "tc7xjj.h"
#include "tcsxjj.h"
#include "tdxcjj.h"
#include "jhdxjj.h"
#include "rconds.h"
#include "tpatel.h"
#include "tcercl.h"
C
      PARAMETER (LCMNT=100)
      CHARACTER IREP*50,CHAINT*4,ACMNT*(LCMNT),RNTYP*8
C Banks in Setup
      CHARACTER*2 DET
      CHARACTER*16 LIST
C 3 Components of TPC drift velocity for sides A and B
      DIMENSION DVSA (3), DVSB (3)
      INTEGER ALGTDB,LHEAD(10)
      INTEGER AGETDB
      INTEGER GTSTUP
      EXTERNAL GTT0GL, ALGTDB, AGETDB, GTSTUP
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA DET / 'TP' /
      DATA LIST / 'TC1XTC7XTWTCTP1X' /
      DATA IROLD / 0 /
C
C------------------ Statement Function Definitions ----------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------------
C
C++   Cuts and parameters for cluster finding and pulse analysis:
C     from TLCT,NR=0
      IRET=MDARD (IW,LRCONS,'TLCT',0)
      IF (IRET.EQ.0) THEN
        IREP= 'Constants bank TLCT is missing from D.A. file.'
        CALL REPORT('TRNCON',IREP(1:46),1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTLCT=IW(NAMIND('TLCT'))
        ITLCTA(1)=IW(KTLCT+LMHLEN+JTLCTA)
        ITLCTA(2)=IW(KTLCT+LMHLEN+JTLCTA+1)
        ITLCCA(1)=IW(KTLCT+LMHLEN+JTLCCA)
        ITLCCA(2)=IW(KTLCT+LMHLEN+JTLCCA+1)
        FTLCOO(1)=IW(KTLCT+LMHLEN+JTLCOO).EQ.1
        FTLCOO(2)=IW(KTLCT+LMHLEN+JTLCOO+1).EQ.1
        FTLCAF= IW(KTLCT+LMHLEN+JTLCAF).EQ.1
        ITLCPT= IW(KTLCT+LMHLEN+JTLCPT)
        RTLCPT= FLOAT(ITLCPT)
        ITLCPC= IW(KTLCT+LMHLEN+JTLCPC)
        RTLCDM(1)= RW(KTLCT+LMHLEN+JTLCDM)
        RTLCDM(2)= RW(KTLCT+LMHLEN+JTLCDM+1)
        RTLCDM(3)= RW(KTLCT+LMHLEN+JTLCDM+2)
        ITLCML= IW(KTLCT+LMHLEN+JTLCML)
        RTLCTL= RW(KTLCT+LMHLEN+JTLCTL)
        ITLCNP= IW(KTLCT+LMHLEN+JTLCNP)
        ITLCXP= IW(KTLCT+LMHLEN+JTLCXP)
        ITLCIB= IW(KTLCT+LMHLEN+JTLCIB)
        ITLCAB= IW(KTLCT+LMHLEN+JTLCAB)
        RTLCZW(1)= RW(KTLCT+LMHLEN+JTLCZW)
        RTLCZW(2)= RW(KTLCT+LMHLEN+JTLCZW+1)
        ITLCFA= IW(KTLCT+LMHLEN+JTLCFA)
        ITLCRM= IW(KTLCT+LMHLEN+JTLCRM)
        ITLCPM= IW(KTLCT+LMHLEN+JTLCPM)
        ITLCOC= IW(KTLCT+LMHLEN+JTLCOC)
        ITLCOP= IW(KTLCT+LMHLEN+JTLCOP)
        FTLCQK= IW(KTLCT+LMHLEN+JTLCQK).EQ.1
        RTLCFT= RW(KTLCT+LMHLEN+JTLCFT)
        ITLCMS= IW(KTLCT+LMHLEN+JTLCMS)
        RTLCZM= RW(KTLCT+LMHLEN+JTLCZM)
      ENDIF
C
      IF (FDEBRF .AND. JDBDRF(JULTP).GE.2) CALL TLCTDP(LDEBRL)
C
C  Read pulse-shape correction coefficients from bank TSHP
C
      IRET=ALGTDB(LRCONS,'TSHP',IRUNRC)
      IF (IRET.EQ.0) THEN
        IREP= 'Constants bank TSHP is missing from D.A. file.'
        CALL REPORT('TRNCON',IREP(1:46),1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTSHP = IW(NAMIND('TSHP'))
        CALL UCOPY(RW(KTSHP+LMHLEN+JTSHCO),TSHPCN(1),LTSHPA)
      ENDIF
C
C++   Cuts and parameters for coordinate finding into COMMON/TPCCUT/
C
      IRET=ALGTDB(LRCONS,'TCRC',IRUNRC)
      IF (IRET.EQ.0) THEN
        IREP= 'Constants bank TCRC is missing from D.A. file.'
        CALL REPORT('TRNCON',IREP(1:50),1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTCRC=IW(NAMIND('TCRC'))
        IALGTP=IW(KTCRC+LMHLEN+JTCRAN)
        ALGPTP=RW(KTCRC+LMHLEN+JTCRAP)
        NPHPTP=IW(KTCRC+LMHLEN+JTCREC)
        CRTLTP=RW(KTCRC+LMHLEN+JTCRPC)
      ENDIF
C
C++   Some parameters for TCOOR not yet on the data base
C
      CMNPTP=8.1
      RTMXTP=20.
      RLTHTP=0.15
      IMXPTP=6
C
C++   User may overide the default coordinate algorithm with data card
C
      KTALG=IW(NAMIND('TCOA'))
      IF (KTALG.NE.0) THEN
        IF(IW(KTALG+1).NE.0) IALGTP=IW(KTALG+1)
        IF(IW(KTALG).EQ.2)   ALGPTP=IW(KTALG+2)
      ENDIF
C
C++   dE/dx calibration constants
#include "gtdbbk.h"
      IRETS = IRET
      KTC1X = IW(NAMIND('TC1X'))
      IF (KTC1X.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TC1X is missing'//
     &              ' from the direct access file.',1)
      ENDIF
      IF (FIRST .OR. IRETS.LT.0) THEN
        DO 1832 I=1,LTSECT
          IDX= JTC1SN + (I-1)*2
          RNRMCL(I)= RTABL(KTC1X,1,JTC1NR)*RTABL(KTC1X,1,IDX)
 1832   CONTINUE
        SLOPCL= RTABL(KTC1X,1,JTC1SL)
        JTRNCL= ITABL(KTC1X,1,JTC1TP)
      ENDIF
C
      IRETS = IRET
C
C++  Turn off pad dE/dx with TNPX card
C
      KTNPX = IW(NAMIND('TNPX'))
      IF (KTNPX .EQ. 0) THEN
        KTP1X = IW(NAMIND('TP1X'))
        IF (KTP1X.EQ.0) THEN
          CALL REPORT('TRNCON','Constants bank TP1X is missing'//
     &              ' from the direct access file.',1)
        ENDIF
        IF (FIRST .OR. IRETS.LT.0) THEN
          PADNRM = RTABL(KTP1X,1,JTP1NR)
          IDX = JTP1SN
          DO 1833 I=1,LTSECT
            PNRMCL(I)= PADNRM*RTABL(KTP1X,1,IDX)*RNRMCL(I)
            IDX = IDX + 1
 1833     CONTINUE
          JPTRCL= ITABL(KTP1X,1,JTP1TP)
          MNSPTP = ITABL(KTP1X,1,JTP1MS)
        ENDIF
      ENDIF
C
      IRET=ALGTDB(LRCONS,'TC5X',IRUNRC)
      IF (IRET.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TC5X is missing'//
     &              ' from the direct access file.',1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTC5X=IW(NAMIND('TC5X'))
        ADSPCL= RTABL(KTC5X,1,JTC5NC)
        GRNMCL= RTABL(KTC5X,1,JTC5NC+1)
        RNLNCL= RTABL(KTC5X,1,JTC5NC+2)
        DO 3177 I=1,LTSTYP
          PWDPCL(0,I)=RTABL(KTC5X,1,JTC5P0+I-1)
          PWDPCL(1,I)=RTABL(KTC5X,1,JTC5P1+I-1)
          PWDPCL(2,I)=RTABL(KTC5X,1,JTC5P2+I-1)
          PWDPCL(3,I)=RTABL(KTC5X,1,JTC5P3+I-1)
 3177   CONTINUE
      ENDIF
C
      IRET=ALGTDB(LRCONS,'TC6X',IRUNRC)
      IF (IRET.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TC6X is missing'//
     &              ' from the direct access file.',1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTC6X=IW(NAMIND('TC6X'))
        DO 3178 I=1,LTSTYP
          EDGECL(1,I)=RTABL(KTC6X,I,JTC6AP)
          EDGECL(2,I)=RTABL(KTC6X,I,JTC6TU)
 3178   CONTINUE
      ENDIF
C
      KTC7X = IW(NAMIND('TC7X'))
      IF (KTC7X.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TC7X is missing'//
     &              ' from the direct access file.',1)
      ENDIF
      IF (FIRST .OR. IRETS.LT.0) THEN
        FZERTW= (ITABL(KTC7X,1,JTC7BT).EQ.1)
        KTRNTW= ITABL(KTC7X,1,JTC7LT)
        ADCMTW= RTABL(KTC7X,1,JTC7MA)
        ZALOTW= RTABL(KTC7X,1,JTC7ZA)
        MXTNTW= ITABL(KTC7X,1,JTC7MZ)
        MXRNTW= ITABL(KTC7X,1,JTC7MR)
        DZNLTW= 0.25*FLOAT(ITABL(KTC7X,1,JTC7R1))
        SPLITW= (ITABL(KTC7X,1,JTC7R2).EQ.1)
        MNRATW= ITABL(KTC7X,1,JTC7R3)
        THRSTW= FLOAT(ITABL(KTC7X,1,JTC7R4))/100.
      ENDIF
C
C?   Constants for dE/dx monitoring
C
      IF (IRUNRC.LT.20000) THEN
         IRET= AGETDB('TDXCTC2X',IRUNRC)
      ELSE
         IRET= AGETDB('TDXCTCSX',IRUNRC)
      ENDIF
      KTDXC = IW(NAMIND('TDXC'))
      IF (KTDXC.EQ.0) THEN
        CALL RERROR('TRNCON', -1, 'Cannot find the TDXC bank on DAF')
      ELSE
C++     Define Histograms for dE/dx per sector
C       book them at first entry, reset them at next entry
        IOFF = JULTP*1000 + 100
        IF (FIRST) THEN
          RPMI = RTABL(KTDXC,1,JTDXLI)
          RPMX = RTABL(KTDXC,1,JTDXUI)
          DO IS =  1, LTSECT
             WRITE (CH3,'(I3)') IS
             CALL HBOOK1 (IOFF+IS,' wire dE/dx for Sector :'//CH3,
     &            LJHDXA-JJHDBI+1,RPMI, RPMX, 0.)
             CALL HIDOPT (IOFF+IS, 'STAT')
             CALL HBOOK1 (IOFF+100+IS,' pad dE/dx for Sector :'//CH3,
     &            LJHDXA-JJHDBI+1,RPMI, RPMX, 0.)
             CALL HIDOPT (IOFF+100+IS, 'STAT')
          ENDDO
C
          CALL HBOOK1 (IOFF+LTSECT+1,
     &          ' Mean Corrected Wire dE/dx in Event',
     &         LJHDXA-JJHDBI+1,RPMI, RPMX, 0.)
          CALL HIDOPT (IOFF+LTSECT+1, 'STAT')
          CALL HBOOK1 (IOFF+100+LTSECT+1,
     &          ' Mean Corrected Pad dE/dx in Event',
     &         LJHDXA-JJHDBI+1,RPMI, RPMX, 0.)
          CALL HIDOPT (IOFF+100+LTSECT+1, 'STAT')
        ELSE
          DO IS =  1, LTSECT+1
            CALL HRESET (IOFF+IS, ' ')
            CALL HRESET (IOFF+100+IS, ' ')
          ENDDO
        ENDIF
      ENDIF
C++   Store sector gain map in /TMONIT/
      KTC2X = IW(NAMIND('TC2X'))
      KTCSX = IW(NAMIND('TCSX'))
      IF (KTC2X.GT.0) THEN
         DO I=1,LTSECT
            GAINTM(I) = RTABL(KTC2X,1,JTC2SN+I-1)
         ENDDO
      ELSEIF (KTCSX.GT.0) THEN
         DO I=1,LTSECT
            GAINTM(I) = RTABL(KTCSX,1,JTCSSN+I-1)
         ENDDO
      ELSE
        CALL RERROR('TRNCON',-2,'NO sector gain map on DAF: set to 1.')
        DO I=1,LTSECT
           GAINTM(I) = 1.
        ENDDO
      ENDIF
C
C++   Cuts and parameters for TRKFND track finding:
C
      IRET=ALGTDB(LRCONS,'TCCN',IRUNRC)
      IF (IRET.EQ.0) THEN
        IREP= 'Constants bank TCCN is missing from D.A. file.'
        CALL REPORT('TRNCON',IREP(1:46),1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTCCN=IW(NAMIND('TCCN'))
        KC1 = KTCCN + LMHLEN
        CALL UCOPY(RW(KC1 + JTCCZT),TDZTOL,8)
        MAXGAP  = IW(KC1 + JTCCMG)
        DZNEXT = RW(KC1 +JTCCZN)
        DPNEXT = RW(KC1 +JTCCPN)
        CURTOL = RW(KC1 +JTCCCU)
        DIPTOL = RW(KC1 +JTCCDT)
        MAXTRY  = IW(KC1 +JTCCMA)
        CALL UCOPY(RW(KC1 + JTCCSW),SIGMAW,7)
      ENDIF
C
C++   More tracking constants
C
      IRET=ALGTDB(LRCONS,'TTRC',IRUNRC)
      IF (IRET.EQ.0) THEN
        IREP= 'Constants bank TTRC is missing from D.A. file.'
        CALL REPORT('TRNCON',IREP(1:46),1)
      ENDIF
C
C++   Constants for TFLNK2 link between TPC tracks
C
      IRET=ALGTDB(LRCONS,'FCON',IRUNRC)
      IF (IRET.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank FCON is '//
     &              'missing from the D.A. file',1)
      ENDIF
C
C++   Constants for parameterization of coordinate errors and
C++   and pad response width as functions of track parameters.
C
      IRET=ALGTDB(LRCONS,'TERN',IRUNRC)
      IF (IRET.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TERN is '//
     &              ' missing from the D.A. file',1)
      ENDIF
C
      IRET=ALGTDB(LRCONS,'TNRN',IRUNRC)
      IF (IRET.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TNRN is '//
     &              ' missing from the D.A. file',1)
      ENDIF
C
C++   MC pad response function from TCON
C
      IRET=ALGTDB(LRCONS,'TCON',IRUNRC)
      IF (IRET.EQ.0) THEN
        IREP= 'Constants bank TCON is missing from D.A. file'
        CALL REPORT('TRNCON',IREP,1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTCON=IW(NAMIND('TCON'))
        TPRWDT=RW(KTCON+LMHLEN+JTCORW)
        TSIGMD=RW(KTCON+LMHLEN+JTCOSD)
        TOMTAU=RW(KTCON+LMHLEN+JTCOOT)
      ENDIF
C
C++   Parameterization of the TPC coordinate resolution
C
      IRET=ALGTDB(LRCONS,'TRSC',IRUNRC)
      IF (IRET.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TRSC is missing'//
     &                             ' from the D.A. file.',1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTRSC=IW(NAMIND('TRSC'))
        NPRPER=0
        NPZDER=0
        DO 9591 I=1,LROWS(KTRSC)
          IF (RTABL(KTRSC,I,JTRSXR).GE.0.) THEN
            NPRPER=NPRPER+1
            XBRPER(I)=RTABL(KTRSC,I,JTRSXR)
            DO 5931 J=1,3
              CFRPER(J,I)=RTABL(KTRSC,I,JTRSBR+J-1)
 5931       CONTINUE
          ENDIF
          IF (RTABL(KTRSC,I,JTRSXZ).GE.0.) THEN
            NPZDER=NPZDER+1
            XZRPER(I)=RTABL(KTRSC,I,JTRSXZ)
            DO 5932 J=1,3
              CFZDER(J,I)=RTABL(KTRSC,I,JTRSBZ+J-1)
 5932       CONTINUE
          ENDIF
 9591   CONTINUE
      ENDIF
C
C++   Sector alignment constants to correct for TPC90 construction error
C
      IF (FTPC90) THEN
        ISTYP=1
        TAPRPH(1,ISTYP)=-.0003/39.871
        TAPRPH(2,ISTYP)=+.0052/46.271
        TAPRPH(3,ISTYP)=-.0050/52.671
        TAPRPH(4,ISTYP)=-.0055/59.071
        TAPRPH(5,ISTYP)=-.0013/65.471
        TAPRPH(6,ISTYP)=+.0070/71.871
        TAPRPH(7,ISTYP)=+.0325/78.271
      ENDIF
C
C     We take the magnetic field from the global ALEPH common.
C     This may be set by the FIELD data card.
C
C     For TPC90 data, this value may be replaced by that in the TSOR
C     bank or by the user data card BFIELD.
C     Also, for TPC90 data only, it may be read by TMAGNT from the
C     Hall probe for each event.
C
      BFIELD=FIELRC
      KTSOR=IW(NAMIND('TSOR'))
      IF (KTSOR.EQ.0) THEN
C
C++     Sometimes the online folks forget the T in 'TSOR'
C
        KTSOR=IW(NAMIND(' SOR'))
      ENDIF
C
      TRGOF=0.
      SL=93.15
      KTCLK=IW(NAMIND('TCLK'))
      IF (KTCLK.NE.0) THEN
        SL=RW(KTCLK+1)
        WRITE(LOUTRL,466) SL
  466   FORMAT(' TRNCON: clock period set to ',F9.2,' ns ',
     &         ' from the user''s TCLK card.')
      ENDIF
C
C++     For real data, require presence of TSOR
C
      IF (IRUNRC.GT.2000.AND.KTSOR.EQ.0.AND.KTCLK.EQ.0)
     &  CALL RERROR('TRNCON',-3,' TSOR bank and TCLK card missing.')
C
      IF (KTSOR.NE.0) THEN
C
C++     Magnetic field from TSOR for TPC90
C
        IF (FTPC90) THEN
          BFIELD=IW(KTSOR+JTSOFI)
          BFIELD=BFIELD/1000.
          WRITE(LOUTRL,3) BFIELD
    3     FORMAT(/' TRNCON:  Magnetic field for TPC analysis set to',
     &            E12.5,' kG from the TPC start-ofrun bank (TSOR)'/)
        ENDIF
C
C++     Clock frequency from TSOR
C
        IFREQ=IW(KTSOR+JTSOCL)
        IF (IFREQ.GT.0) THEN
          IF (KTCLK.EQ.0) THEN
            SL=1.0E6/FLOAT(IFREQ)
            WRITE(LOUTRL,66) IFREQ
   66       FORMAT(/' TRNCON: clock frequency set to ',I7,
     &             ' kHz from the TPC start-of-run bank (TSOR)')
          ENDIF
          KRUNH=IW(NAMIND('RUNH'))
C
C++       Trigger offsets from TSOR
C
          IF (KRUNH.NE.0) THEN
            RNTYP(1:4)=CHAINT(IW(KRUNH+7))
            RNTYP(5:8)=CHAINT(IW(KRUNH+8))
            IF (RNTYP(1:7).EQ.'COSMICS') THEN
              IDEL= IW(KTSOR+JTSODE)
              TRGOF= -FLOAT(IDEL)/SL
              WRITE(LOUTRL,87) TRGOF
   87         FORMAT(' TRNCON: trigger offset set to ',
     &                 E12.5,' buckets from TSOR (cosmic trigger)')
            ELSEIF (RNTYP(1:8).EQ.'MOPA SYN') THEN
              IDEL= IW(KTSOR+JTSODE)-IW(KTSOR+JTSODL)
              TRGOF= -FLOAT(IDEL)/SL
              WRITE(LOUTRL,88) TRGOF
   88         FORMAT(' TRNCON: clock offset set to ',
     &           E12.5,' buckets from TSOR (mopa sync. trig.)')
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C?   Get TPC drift Velocity
C
      CALL RGETDV
C
      CALL TPDVEL ( ' ' , DVSA, DVSB, IER)
      IF ( IER.NE.0 ) THEN
        CALL RERROR ( 'TRNCON', -4, ' Unable to get TPC Drift Velocity')
      ELSE
        KTSIM=IW(NAMIND('TSIM'))
        IF (KTSIM.GT.0) SL=RTABL(KTSIM,1,JTSITD)
        TXDRFT(1)=DVSA(1)*SL/1000.
        TXDRFT(2)=DVSB(1)*SL/1000.
        TYDRFT(1)=DVSA(2)*SL/1000.
        TYDRFT(2)=DVSB(2)*SL/1000.
        TVDRFT(1)=DVSA(3)*SL/1000.
        TVDRFT(2)=DVSB(3)*SL/1000.
        WRITE(LOUTRL,7001) 'X',(1000.*TXDRFT(IJ)/SL, IJ=1,2)
        WRITE(LOUTRL,7001) 'Y',(1000.*TYDRFT(IJ)/SL, IJ=1,2)
        WRITE(LOUTRL,7001) 'Z',(1000.*TVDRFT(IJ)/SL, IJ=1,2)
 7001   FORMAT(' TRNCON:',A4,' drift velocities Side A and B ',2E12.5,
     &    ' cm/bucket')
      ENDIF
C
C++   Note TPC clock period in nanoseconds.
C
      TPCCLK = SL
C
C++   T0 calibration for real data
C
      T0GL = GTT0GL(IRUNRC)
      IRET=ALGTDB(LRCONS,'T0RL',IRUNRC)
      KT0GL=IW(NAMIND('T0GL'))
      KT0RL=IW(NAMIND('T0RL'))
      IF (KT0GL.EQ.0 .OR. KT0RL.EQ.0) THEN
        IREP= 'Constants banks T0GL and/or'//
     &                 ' T0RL are missing from the D.A. file.'
        CALL REPORT('TRNCON',IREP,1)
      ENDIF
C
C++     Algorithm corrections to z coordinates.
C++     TZOFFS is in units of buckets.
C
      SLM=SL/1000.
      TZOFFS(1)= RTABL(KT0GL,1,JT0GA1)/SLM
      TZOFFS(2)= RTABL(KT0GL,1,JT0GA2)/SLM
      TZOFFS(3)= RTABL(KT0GL,1,JT0GA3)/SLM
      TZOFFS(9)= RTABL(KT0GL,1,JT0GAW)/SLM
C
C++     Add special trigger offset (for lasers and cosmics)
C
      TVOFF = T0GL + TRGOF
      WRITE(LOUTRL,8001) TVOFF
 8001 FORMAT(' TRNCON: Global T0 Offset : ', E12.5, ' musecs ')
C
C++     Sector dependent offsets.  TVOFFS is in units of buckets.
C
      DO 756 I=1,LROWS(KT0RL)
        TVOFS0(I)= (TVOFF + RTABL(KT0RL,I,JT0RTP))/SLM
        TVOFFS(I)= TVOFS0(I)
  756 CONTINUE
C
C++   Set the T0 to zero for all sectors for MC (TPCSIM version < 218)
C
      IF (KTSIM.GT.0) THEN
        IF (ITABL(KTSIM,1,JTSITV) .LT. 218) THEN
          DO I=1,LTSECT
            TVOFS0(I)=0.
            TVOFFS(I)=0.
          ENDDO
        ENDIF
      ENDIF
C
C++   User can set the TPC B-field by a data card for TPC90 (use the
C++   JULIA data card FIELD for ALEPH data).
C
      IF (FTPC90) THEN
        KBFIE=IW(NAMIND('BFIE'))
        IF (KBFIE.NE.0) THEN
          BFIELD=RW(KBFIE+1)
          WRITE(LOUTRL,6) BFIELD
        ENDIF
    6   FORMAT(/' TRNCON:  Magnetic field for TPC analysis set to',
     &            E12.5,' kG from users BFIELD data card')
      ENDIF
C
C++   Convert the B-field to natural units
C
      BCFGEV = BFIELD*CLGHT*1.E-5
C
C++   Allow the user to overide the drift velocity
C
      KTDRF=IW(NAMIND('TDRF'))
      IF (KTDRF.NE.0) THEN
        TVDRFT(1)=RW(KTDRF+1)
        TVDRFT(2)=TVDRFT(1)
        WRITE(LOUTRL,8123) TVDRFT(1)
 8123   FORMAT(' TRNCON:  drift velocity set to ',E12.5,' cm/bucket',
     &         ' from the user''s TDRF data card')
      ENDIF
C
C++   The user can add in an additional overall time offset here
C
      KTOFF=IW(NAMIND('TOFF'))
      IF (KTOFF.NE.0) THEN
        TVOFF=RW(KTOFF+1)
        WRITE(LOUTRL,8124) TVOFF
 8124   FORMAT(' TRNCON:  add drift time offset of ',E12.5,' buckets',
     &         ' from the user''s TOFF data card')
        DO 8291 I=1,LTSECT
          TVOFS0(I)=TVOFS0(I) + TVOFF
          TVOFFS(I)=TVOFFS(I) + TVOFF
 8291   CONTINUE
      ENDIF
C
C++   Get pedestals from TSIM (MC data) or TCLB (RAW data for run no. < 28834).
C++
C++   JPEDST(I,1) is pad pedestal,(default=4) can be changed by TPPD data cards.
C++   JPEDST(I,2) is wire pedestal, (default=4 for run no. < 28834,
C++   default=0 for run no. >= 28834 because online wire pedestal substraction
C++   also applies to unreduced wire hits) can be  changed by TWPD data cards.
C
      IPDPD=4                        ! default pad pedestal
      IPDWR=0                        ! default wire pedestal
      IF (IRUNRC.LT.28834) IPDWR=4  !def wire pedestal for MC data and run<28834
C
      IF (IRUNRC.LE.2000) THEN
C     MC DATA
        KTSIM=IW(NAMIND('TSIM'))
        IF (KTSIM.GT.0) THEN
          IF (ITABL(KTSIM,1,JTSITV) .LT. 206) THEN
            IPDPD=IW(KTSIM+26)
          ELSE
            IPDPD = 255*(RTABL(KTSIM,1,JTSIPE)/2000.)
          ENDIF
          WRITE(LOUTRL,851) IPDPD
  851     FORMAT(/' TRNCON: TPD pedestal set to ',I3,
     &          ' from the Monte Carlo TSIM bank')
          IPDWR = IPDPD
        ELSE
          WRITE(LOUTRL,'(/A,I2,A,I2)')
     &     ' TRNCON:no TSIM bank,set pad pedestal to',IPDPD
     &    ,' ,wire pedestal to ',IPDWR
        ENDIF
        DO I=1,LTSECT
          JPEDST(I,1)=IPDPD
          JPEDST(I,2)=IPDWR
        ENDDO
C
      ELSE
C
C     For real data, we can have a different pedestal for each sector
C      when TCLB exists and run<28834
        KTCLB=IW(NAMIND('TCLB'))
        IF (KTCLB.GT.0) THEN
          DO ISLOT=1,LROWS(KTCLB)
            JPEDST(ISLOT,1)=ITABL(KTCLB,ISLOT,JTCLPD)
            IF (IRUNRC.LT.28834) THEN
               JPEDST(ISLOT,2)=ITABL(KTCLB,ISLOT,JTCLPD)
            ELSE
               JPEDST(ISLOT,2)=IPDWR
            ENDIF
          ENDDO
          WRITE(LOUTRL,'(/A,I2,A,I2)')' TRNCON: pad ped. set to',
     &    JPEDST(1,1),' from TCLB bank, wire ped. set to',JPEDST(1,2)

        ELSE
C       set default value if TCLB is not there
          DO I=1,LTSECT
            JPEDST(I,1)=IPDPD
            JPEDST(I,2)=IPDWR
          ENDDO
          WRITE(LOUTRL,'(/A,I2,A,I2)')
     &         ' TRNCON:no TCLB bank,pad pedestal set to',IPDPD,
     &         ' wire pedestal set to',IPDWR
        ENDIF
C
      ENDIF
C
C++   Allow the user to set pad or wire pedestals at will.
C
      KTPED=IW(NAMIND('TPED'))
      IF (KTPED.NE.0) THEN
        IPDPD=IW(KTPED+1)
        WRITE(LOUTRL,854) IPDPD
  854   FORMAT(/' TRNCON: TPC pad pedestal reset to ',I3,
     &          ' from users TPED data card')
        DO 55 I=1,LTSECT
          JPEDST(I,1)=IPDPD
   55   CONTINUE
      ENDIF
      KTWPD=IW(NAMIND('TWPD'))
      IF (KTWPD.NE.0) THEN
        IPDWR=IW(KTWPD+1)
        WRITE(LOUTRL,855) IPDWR
  855   FORMAT(/' TRNCON: TPC wire pedestal reset to ',I3,
     &          ' from users TWPD data card')
        DO 56 I=1,LTSECT
          JPEDST(I,2)=IPDWR
   56   CONTINUE
      ENDIF
C
C++   Print the TPC start of run bank (TSOR)
C
      CALL TSORDP(LOUTRL)
C
C++   Cuts and parameters for wire and dE/dx analysis.
C
      IRET=ALGTDB(LRCONS,'TWRC',IRUNRC)
      IF (IRET.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TWRC is missing'//
     &                ' from the direct access file.',1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTWRC=IW(NAMIND('TWRC'))
        TPACKW=RTABL(KTWRC,1,JTWRTP)
        MXSATW=ITABL(KTWRC,1,JTWRMS)
        IF (ABS(BFIELD).LT.13.) THEN
          AVPLTW=50.
        ELSE
          AVPLTW=RTABL(KTWRC,1,JTWRAL)
        ENDIF
        SLPLTW=RTABL(KTWRC,1,JTWRSD)
        SLPZTW=RTABL(KTWRC,1,JTWRSZ)
        JTOLTW=ITABL(KTWRC,1,JTWRMV)
        WRMXTW=RTABL(KTWRC,1,JTWRRM)
      ENDIF
C
C++   Find the online bank for wire reduction.  Get it from the database
C++   if it is not on the input tape or in data cards.
C
      KTSWP=IW(NAMIND('TSWP'))
      IF (KTSWP.EQ.0) THEN
        NR=NDANR(JUNIDB(0),'TSWP','LE',IRUNRC)
        IF (NR.EQ.0) THEN
          CALL REPORT('TRNCON','Constants bank TSWP is missing'//
     &                ' from the direct access file.',1)
        ELSE
          KTSWP=MDARD(IW,JUNIDB(0),'TSWP',NR)
        ENDIF
      ELSE
        IF (IW(KTSWP-2).NE.IRUNRC .AND. IW(KTSWP-2).GE.0) THEN
          NR=NDANR(JUNIDB(0),'TSWP','LE',IRUNRC)
          IF (NR.NE.0 .AND. NR.NE.IW(KTSWP-2)) THEN
            KTSWP=MDARD(IW,JUNIDB(0),'TSWP',NR)
          ENDIF
        ENDIF
      ENDIF
      IF (KTSWP.NE.0) THEN
        JCPAKW=ITABL(KTSWP,1,JTSWCN)
        JTHRTW=ITABL(KTSWP,1,JTSWTH)
        NPRETW=ITABL(KTSWP,1,JTSWPS)
        NPSTTW=ITABL(KTSWP,1,JTSWPO)
        IALGTW=ITABL(KTSWP,1,JTSWR1)
        RTHRTW=RTABL(KTSWP,1,JTSWR2)
      ENDIF
      IF (FDEBRF) WRITE(LOUTRL,1108) IRUNRC,JCPAKW,MXSATW,JTHRTW,
     &            NPRETW,NPSTTW,IALGTW,RTHRTW,TPACKW,AVPLTW,SLPLTW,
     &            SLPZTW,WRMXTW
 1108 FORMAT(/' TRNCON: wire reduction parameters taken from ',
     &        'banks TSWP and TWRC for run ',I6,':',/
     &        ' Charge normalization factor for packing=',I2,/
     &        ' Maximum number of saturated samples=',I2,/
     &        ' Threshold above pedestal=',I2,/
     &        ' Number of pre and post samples=',I2,2X,I2,/
     &        ' Algorithm choice=',I2,/
     &        ' Fractional threshold for time estimate=',F6.2,/
     &        ' Packing factor for the time=',F5.1,/
     &        ' Pulse length cuts=',3(1X,G7.2),/
     &        ' Maximum rms pulse width=',F8.2)
C
      KTWTC=IW(NAMIND('TWTC'))
      IF (KTWTC.EQ.0) THEN
        CALL REPORT('TRNCON','Constants bank TWTC is missing'//
     &                ' from the direct access file.',1)
      ENDIF
      IF (FIRST .OR. IRETS.LT.0) THEN
        WRTLTW=RTABL(KTWTC,1,JTWTFC)
        TRMXTW=RTABL(KTWTC,1,JTWTMT)
        ZWINTW=RTABL(KTWTC,1,JTWTZW)
        SPACTW=RTABL(KTWTC,1,JTWTSP)
        IDUMMY=60
        MNSPTW=ITABL(KTWTC,1,JTWTMS)
        CTMNTW=RTABL(KTWTC,1,JTWTCM)
        FSECTW=.TRUE.
        RNCRTW=RTABL(KTWTC,1,JTWTMD)
        MXMATW=ITABL(KTWTC,1,JTWTMK)
        RSMXTW=RTABL(KTWTC,1,JTWTRM)
        ZWSATW=RTABL(KTWTC,1,JTWTZS)
      ENDIF
C
C++   Find the comment record and convert to a character string
C
      KTCOM=NLINK('TCOM',IRUNRC)
      IF (KTCOM.NE.0) THEN
        LEN=IW(KTCOM)
        DO 563 IWD=1,LEN
          IC4=4*IWD
          IC1=IC4-3
          IF (IC4.GT.LCMNT) GO TO 564
          ACMNT(IC1:IC4)=CHAINT(IW(KTCOM+IWD))
  563   CONTINUE
  564   CONTINUE
        WRITE(LOUTRL,1104) ACMNT(1:92)
 1104   FORMAT(/' TRNCON: Comment record for this run:',/,1X,50A2/)
      ENDIF
C
      FIRST=.FALSE.
C
      RETURN
      END
#endif
