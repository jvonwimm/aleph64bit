      SUBROUTINE IMATCH
C-----------------------------------------------------------------------
C! Match up unextended TPC with ITC stand-alone tracks.
C!
C!    Authors     :- L. Garrido, I. Tomalin  89/05/01
C!    Modified    :- I. Tomalin  89/09/01
C!
C!   Input:
C!     need common:/BCS/
C!                 /ITRKCC/
C!                 /ITRKPA/ for tracking cuts
C!                 /UFITCB/
C!          params: ALCONS,IGTLJJ,TGTLJJ,ITFTJJ,TGFTJJ,ITCOJJ
C!
C!   Output:
C!      Modification of banks ITFT, IGTL and IGCL (no.'s 1 and 2)
C!
C!   called by : ITCREC
C!   calls     : UFITMS
C!
C!   Libraries required: BOS,CERNLIB
C!
C? Loop over all unextended TPC tracks
C?   Loop over all ITC stand-alone tracks
C?     Apply cuts:
C?     Phis at scatt. radius, RSCAIT, must differ by less than DELMIN.
C?     CHI**2 of comparison of helix params must be less than CHI2PM.
C?     Do a simple fit (IOPT=1) to the combined track.
C?     CHI**2 of fit must be less than CHI2T1.
C?     If track pair is O.K. then store it in IMATIW work bank.
C?   End Loop
C? End loop
C? If two or more matches have a track in common, then eliminate all but
C? the best match from the IMATIW bank.
C? Loop over the matches in the IMATIW bank.
C?   Do a proper fit (IOPT=4) to the combined track.
C?   CHI**2 of final fit must be less than CHI2T4.
C?   Store it, updating the ITFT, IGTL and IGTL banks.
C? End loop
C=======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O commons
#include "bcs.h"
#include "bosext.h"
#include "itrkcc.h"
#include "itrkpa.h"
#include "ufitcb.h"
#include "alcons.h"
#include "igtljj.h"
#include "tgtljj.h"
#include "itftjj.h"
#include "tgftjj.h"
#include "itcojj.h"
C-----------------------------------------------------------------------
C Dummy common block to save work bank indices.
      COMMON/IDUMYB/IMATIW
      LOGICAL SETZ
      DIMENSION VV1(12),VV2(12),VV0(6),EE0(21),COV(21)
      DIMENSION LIST1(21),LIST2(8)
      DATA SETZ/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      IF (SETZ) THEN
        IMATIW = 0
        SETZ = .FALSE.
      END IF
      KITF1 = NLINK('ITFT',1)
      KITF2 = NLINK('ITFT',2)
      IF(KITF1.EQ.0.OR.KITF2.EQ.0) GOTO 1000
      NTPC = LROWS(KITF1)
      NITC = LROWS(KITF2)
      IF(NTPC.EQ.0.OR.NITC.EQ.0) GOTO 1000
      KTGTL = NLINK('TGTL',1)
      KTGCL = NLINK('TGCL',1)
      IF (KTGTL.EQ.0.OR.KTGCL.EQ.0) GOTO 1000
      KIGT1 = NLINK('IGTL',1)
      KIGC1 = NLINK('IGCL',1)
      IF (KIGT1.EQ.0.OR.KIGC1.EQ.0) GOTO 1000
      KIGT2 = NLINK('IGTL',2)
      KIGC2 = NLINK('IGCL',2)
      IF (KIGT2.EQ.0.OR.KIGC2.EQ.0) GOTO 1000
      KITCO=NLINK('ITCO',0)
      IF(KITCO.LE.0) GOTO 1000
C
C Create a work bank in which to store the matched tracks.
C
      IF (IMATIW.GT.0) THEN
        CALL WDROP(IW,IMATIW)
        CALL RERROR('IMATCH',1,'IMATIW work bank still exists '//
     +  'from earlier.It should have been dropped.')
      END IF
      CALL WBANK(IW,IMATIW,LMHLEN+3*50,*998)
      IW(IMATIW+LMHCOL) = 3
      IW(IMATIW+LMHROW) = 0
C
C Loop over tracks found in the TPC and not extended into the ITC.
C
      DO 100 I1 = 1,NTPC
C Skip this track if it already has coordinates in the ITC.
        IF (ITABL(KIGT1,I1,JIGTN1).GT.0) GOTO 100
C
        IITF1 = KROW(KITF1,I1)
        CALL UCOPY(RW(IITF1+1),VV1,6)
        RR01  = VV1(1)
        DZDS1 = VV1(2)
        PH01  = VV1(3)
        DD01  = VV1(4)
        ZZ01  = VV1(5)
C Check that the track did actually intersect radius RSCAIT.
        IF (ABS(DD01).GT.RSCAIT.OR.ABS(2.0/RR01-DD01).LT.RSCAIT)
     +   GOTO 100
C
        VV1( 7) = SQRT(ABS(RW(IITF1+(JITFCM-1)+ 1)))
        VV1( 8) = SQRT(ABS(RW(IITF1+(JITFCM-1)+ 3)))
        VV1( 9) = SQRT(ABS(RW(IITF1+(JITFCM-1)+ 6)))
        VV1(10) = SQRT(ABS(RW(IITF1+(JITFCM-1)+10)))
        VV1(11) = SQRT(ABS(RW(IITF1+(JITFCM-1)+15)))
        VV1(12) = SQRT(ABS(RW(IITF1+(JITFCM-1)+21)))
        CHI21 = RW(IITF1+JITFCD)
C
C Calculate the phi coordinate of the track at a radius of RSCAIT.
        GG01 = 1.0 - RR01*DD01
        FF01 = DD01/RSCAIT - RR01*(RSCAIT**2-DD01**2)/(2.0*RSCAIT*GG01)
C Check that the track did actually intersect radius RSCAIT.
        IF (ABS(FF01).GT.1.0) GOTO 100
        PH1 = PH01 - ASIN(FF01)
        PH1 = MOD(PH1+TWOPI,TWOPI)
C Calculate the z coordinate of the track at a radius of RSCAIT.
C First check that the track did actually intersect radius RSCAIT.
        EEE1 = (RSCAIT**2-DD01**2)/GG01
        IF (EEE1.LT.0.0) GOTO 100
        FFF1 = 0.5*RR01*SQRT(EEE1)
        IF (ABS(FFF1).GT.1.0) GOTO 100
        ZZ1 = ZZ01 + 2.0*ASIN(FFF1)/RR01*DZDS1
C Note the TPC coordinates on track I1.
        NCTPC = ITABL(KTGTL,I1,JTGTN1)
        IOFF1 = ITABL(KTGTL,I1,JTGTIO)
        DO 10 J1 = 1,NCTPC
          LIST1(J1) = IW(KTGCL+LMHLEN+IOFF1+J1)
   10   CONTINUE
C
C Loop over the ITC stand-alone tracks.
C
        DO 90 I2 = 1,NITC
          IITF2 = KROW(KITF2,I2)
          CALL UCOPY(RW(IITF2+1),VV2,6)
          RR02  = VV2(1)
          DZDS2 = VV2(2)
          PH02  = VV2(3)
          DD02  = VV2(4)
          ZZ02  = VV2(5)
C Check that the track did actually intersect radius RSCAIT.
          IF (ABS(DD02).GT.RSCAIT.OR.ABS(2.0/RR02-DD02).LT.RSCAIT)
     +     GOTO 90
          VV2( 7) = SQRT(ABS(RW(IITF2+(JITFCM-1)+ 1)))
          VV2( 8) = SQRT(ABS(RW(IITF2+(JITFCM-1)+ 3)))
          VV2( 9) = SQRT(ABS(RW(IITF2+(JITFCM-1)+ 6)))
          VV2(10) = SQRT(ABS(RW(IITF2+(JITFCM-1)+10)))
          VV2(11) = SQRT(ABS(RW(IITF2+(JITFCM-1)+15)))
          VV2(12) = SQRT(ABS(RW(IITF2+(JITFCM-1)+21)))
          CHI22 = RW(IITF2+JITFCD)
C
C Calculate the phi coordinate of the track at a radius of RSCAIT.
          GG02 = 1.0 - RR02*DD02
          FF02 = DD02/RSCAIT-RR02*(RSCAIT**2-DD02**2)/(2.0*RSCAIT*GG02)
C Check that the track did actually intersect radius RSCAIT.
          IF (ABS(FF02).GT.1.0) GOTO 90
          PH2 = PH02 - ASIN(FF02)
          PH2 = MOD(PH2+TWOPI,TWOPI)
C Calculate the z coordinate of the track at a radius of RSCAIT.
C First check that the track did actually intersect radius RSCAIT.
          EEE2 = (RSCAIT**2-DD02**2)/GG02
          IF (EEE2.LT.0.0) GOTO 90
          FFF2 = 0.5*RR02*SQRT(EEE2)
          IF (ABS(FFF2).GT.1.0) GOTO 90
          ZZ2 = ZZ02 + 2.0*ASIN(FFF2)/RR02*DZDS2
C
C Note the ITC coordinates on the track.
          NCITC = ITABL(KIGT2,I2,JIGTN1)
          IOFF2 = ITABL(KIGT2,I2,JIGTIO)
          DO 20 J2 = 1,NCITC
            LIST2(J2) = IW(KIGC2+LMHLEN+IOFF2+J2)
   20     CONTINUE
C
C Find the difference in the phi's of the two tracks at radius RSCAIT.
          DELPH = PH2 - PH1
          IF (DELPH.GT.PI) DELPH = DELPH - TWOPI
          IF (DELPH.LT.-PI) DELPH = DELPH + TWOPI
C
C Find the CHI**2 of the comparison between the helix parameters.
          CHIP = 0.0
          DO 30 IK = 1,5
            CHIP=CHIP+(VV1(IK)-VV2(IK))**2/(VV1(IK+6)**2+VV2(IK+6)**2)
   30     CONTINUE
C If one of the tracks has <= 4 coordinates, do not look at CHIP, as
C a single incorrect coordinate can totally mess it up.
          IF (NCTPC.LE.4.OR.NCITC.LE.4) CHIP = CHI2PM - 1.0
C
          IF (FDEBRF.AND.JDBDRF(JULIT).GE.5)
     +                       WRITE(LDEBRL,40) I1,I2,(ABS(DELPH)),CHIP
   40     FORMAT(' IMATCH : testing tracks ',2I4,' DELPH=',F6.3,
     +    ' CHIP=',E10.3)
C
          IF (ABS(DELPH).GT.DELMIN.OR.CHIP.GT.CHI2PM) GOTO 90
C
C Now fit combine the ITC and TPC tracks and fit them.
          IOPT = 1
          NMULT = 0
          CALL UFITMS(NCTPC,NCITC,0,LIST1,LIST2,IDUM,IOPT,NMULT,
     +    RSCAIT,.027,VV0,EE0,COV,CHI2,IERR)
C
          IF(CHI2.LT.CHI2T1) THEN
C This pair of tracks is a possible candidate for linking. Store it
C in the work bank, positioned so that the candidates are in order of
C decreasing quality.
C Extend the work bank if necessary.
            IF (LFRROW(IMATIW).EQ.0) THEN
              LEN = IW(IMATIW)
              CALL WBANK(IW,IMATIW,LEN+3*50,*998)
            END IF
C
            KADDR = LROWS(IMATIW) + 1
            DO 50 K = LROWS(IMATIW),1,-1
              KMATIW = KROW(IMATIW,K)
C If the new match is worse than match "K", then stop looping.
              IF (CHI2.GT.RW(KMATIW+3)) GOTO 60
              KADDR = K
C Move the other rows up by one to make space for the new match.
              IW(KMATIW+3+1) = IW(KMATIW+1)
              IW(KMATIW+3+2) = IW(KMATIW+2)
              RW(KMATIW+3+3) = RW(KMATIW+3)
   50       CONTINUE
   60       CONTINUE
C Insert the new match.
            KMATIW = KROW(IMATIW,KADDR)
C   1st column contains the TPC track number.
            IW(KMATIW+1) = I1
C   2nd column contains the ITC track number.
            IW(KMATIW+2) = I2
C   3rd column contains the "quality" of the match.
            RW(KMATIW+3) = CHI2
C
            IW(IMATIW+LMHROW) = IW(IMATIW+LMHROW) + 1
C
            IF (FDEBRF.AND.JDBDRF(JULIT).GE.5) THEN
              WRITE(LDEBRL,*) '********** IMATCH : New Trail **********'
              WRITE(LDEBRL,*) 'Tracks=',I1,I2
              WRITE(LDEBRL,*) 'There are ',NCTPC,' TPC coords and ',
     +        NCITC,' ITC coords.'
              WRITE(LDEBRL,*) 'CHITRACK1=',CHI21,' CHITRACK2=',CHI22
              WRITE(LDEBRL,*) 'PHIS=',PH1,PH2,'ZS=',ZZ1,ZZ2
              WRITE(LDEBRL,*) 'DELPH=',DELPH,' CHI(PARAM.)=',CHIP
              WRITE(LDEBRL,*) 'CHI2(CIRCLE)=',CHI2
              WRITE(LDEBRL,*) 'VV1='
              WRITE(LDEBRL,80) VV1
  80          FORMAT(6(1X,F12.6))
              WRITE(LDEBRL,*) 'VV2='
              WRITE(LDEBRL,80) VV2
            END IF
          ELSE
            IF(FDEBRF.AND.JDBDRF(JULIT).GE.5)
     +              WRITE(LDEBRL,*) 'IMATCH : Link failed : CHI2=',CHI2
          ENDIF
  90    CONTINUE
 100  CONTINUE
C Give up if no matching pairs were found.
      IF (LROWS(IMATIW).EQ.0) GOTO 1000
C
C If any of the matches have a track in common, then take the better
C of the two.
      IF (LROWS(IMATIW).EQ.1) GOTO 107
      J = 1
 101  CONTINUE
        J1 = ITABL(IMATIW,J,1)
        J2 = ITABL(IMATIW,J,2)
        K = J + 1
 102    CONTINUE
          K1 = ITABL(IMATIW,K,1)
          K2 = ITABL(IMATIW,K,2)
          IF (J1.EQ.K1.OR.J2.EQ.K2) THEN
            DO 103 N = K+1,LROWS(IMATIW)
              KMATIW = KROW(IMATIW,N)
              IW(KMATIW-3+1) = IW(KMATIW+1)
              IW(KMATIW-3+2) = IW(KMATIW+2)
              RW(KMATIW-3+3) = RW(KMATIW+3)
 103        CONTINUE
            IW(IMATIW+LMHROW) = IW(IMATIW+LMHROW) - 1
          ELSE
            K = K + 1
          END IF
        IF (K.LE.LROWS(IMATIW)) GOTO 102
        J = J + 1
      IF (J.LE.LROWS(IMATIW)-1) GOTO 101
C
  107 CONTINUE
C Extend the IGCL bank (no. 1) by at least as much as is necessary.
      LEN = IW(KIGC1)
      CALL AUBOS('IGCL',1,LEN+LROWS(IMATIW)*8,KIGC1,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      IF (IGARB.EQ.1) THEN
        KITF1 = NLINK('ITFT',1)
        KITF2 = NLINK('ITFT',2)
        KTGTL = NLINK('TGTL',1)
        KTGCL = NLINK('TGCL',1)
        KIGT1 = NLINK('IGTL',1)
        KIGT2 = NLINK('IGTL',2)
        KIGC2 = NLINK('IGCL',2)
        KITCO = NLINK('ITCO',0)
      END IF
C
C Loop over the matched pairs of tracks.
      DO 500 N = 1,LROWS(IMATIW)
        I1 = ITABL(IMATIW,N,1)
        I2 = ITABL(IMATIW,N,2)
C First get the track coordinates.
        NCTPC = ITABL(KTGTL,I1,JTGTN1)
        IOFF1 = ITABL(KTGTL,I1,JTGTIO)
        DO 110 J1 = 1,NCTPC
          LIST1(J1) = IW(KTGCL+LMHLEN+IOFF1+J1)
  110   CONTINUE
        NCITC = ITABL(KIGT2,I2,JIGTN1)
        IOFF2 = ITABL(KIGT2,I2,JIGTIO)
        NCITZ = 0
        DO 120 J2 = 1,NCITC
          LIST2(J2) = IW(KIGC2+LMHLEN+IOFF2+J2)
C Note the number of ITC coords with z-info.
          ICO = ABS(LIST2(J2))
          IF (RTABL(KITCO,ICO,JITCSZ).LT.10000.) NCITZ = NCITZ + 1
  120   CONTINUE
        NTOT = NCTPC + NCITC
C Now do a proper fit to the combined tracks.
        IOPT = 4
        NMULT = 1
        CALL UFITMS(NCTPC,NCITC,0,LIST1,LIST2,IDUM,IOPT,NMULT,
     +  RSCAIT,.027,VV0,EE0,COV,CHI3,IERR)
C
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
          CMAX = 0.0
          DO 130 IKK = 1,NTOT
            IF(CHIACU(IKK).GT.CMAX) CMAX = CHIACU(IKK)
  130     CONTINUE
          WRITE(LDEBRL,*) 'IMATCH : Storing tracks ',I1,I2
          WRITE(LDEBRL,*) 'CMAX=',CMAX
          WRITE(LDEBRL,*) 'VVFIT='
          WRITE(LDEBRL,80) VV0
          WRITE(LDEBRL,*) 'CHI PER POINT='
          WRITE(LDEBRL,80) (CHIACU(IK),IK=1,NTOT)
          WRITE(LDEBRL,*) 'CHIMAX-POINT=',CMAX,'FROM CHI2=',CHI3
          WRITE(LDEBRL,*) '****************************************'
        END IF
C Apply the final CHI**2 cut.
        IF (CHI3.GT.CHI2T4) GOTO 500
C Update the ITFT bank (no. 1)
        IITF1 = KROW(KITF1,I1)
        DO 140 J = 1,6
          RW(IITF1+J) = VV0(J)
  140   CONTINUE
        DO 150 J = 1,21
          RW(IITF1+JITFCM-1+J) = COV(J)
  150   CONTINUE
        RW(IITF1+JITFCD) = CHI3
        IW(IITF1+JITFND) = 2*NCTPC + (NCITC+NCITZ) - 5
        IW(IITF1+JITFFT) = NMULT*100 + IOPT*10 + IERR
C Update the IGTL bank (no. 1)
        IW(KROW(KIGT1,I1)+JIGTN1) = NCITC
        DO 180 M = I1+1,LROWS(KIGT1)
          IIGT1 = KROW(KIGT1,M)
          IW(IIGT1+JIGTIO) = IW(IIGT1+JIGTIO) + NCITC
  180   CONTINUE
        IOFF = ITABL(KIGT1,I1,JIGTIO)
C Update the IGCL bank (no. 1)
        DO 190 K = LROWS(KIGC1),IOFF+1,-1
          IW(KIGC1+NCITC+LMHLEN+K) = IW(KIGC1+LMHLEN+K)
  190   CONTINUE
        JOFF = ITABL(KIGT2,I2,JIGTIO)
        DO 200 K = 1,NCITC
          IW(KIGC1+LMHLEN+IOFF+K) = IW(KIGC2+LMHLEN+JOFF+K)
  200   CONTINUE
        IW(KIGC1+LMHROW) = IW(KIGC1+LMHROW) + NCITC
C
C Update the ITFT bank (no. 2)
        DO 230 M = I2+1,LROWS(KITF2)
          IITF2 = KROW(KITF2,M)
          DO 210 J = 1,28
            RW(IITF2-LITFTA+J) = RW(IITF2+J)
  210     CONTINUE
          DO 220 J = 29,LITFTA
            IW(IITF2-LITFTA+J) = IW(IITF2+J)
  220     CONTINUE
  230   CONTINUE
        IW(KITF2+LMHROW) = IW(KITF2+LMHROW) - 1
C Update the IGTL bank (no. 2)
        DO 250 M = I2+1,LROWS(KIGT2)
          IIGT2 = KROW(KIGT2,M)
          IW(IIGT2-LIGTLA+JIGTIO) = IW(IIGT2+JIGTIO) - NCITC
          IW(IIGT2-LIGTLA+JIGTN1) = IW(IIGT2+JIGTN1)
          IW(IIGT2-LIGTLA+JIGTNR) = IW(IIGT2+JIGTNR)
  250   CONTINUE
        IW(KIGT2+LMHROW) = IW(KIGT2+LMHROW) - 1
C Update the IGCL bank (no. 2)
        DO 270 M = JOFF+NCITC+1,LROWS(KIGC2)
          IW(KIGC2-NCITC+LMHLEN+M) = IW(KIGC2+LMHLEN+M)
  270   CONTINUE
        IW(KIGC2+LMHROW) = IW(KIGC2+LMHROW) - NCITC
C
C Update the remaining rows of the IMATIW work bank.
        DO 300 M = N+1,LROWS(IMATIW)
          KMATIW = KROW(IMATIW,M)
          IF (IW(KMATIW+2).GT.I2) IW(KMATIW+2) = IW(KMATIW+2) - 1
  300   CONTINUE
  500 CONTINUE
C
C Compress track banks.
      KIGC1 = NLINK('IGCL',1)
      LIGC1 = LMHLEN + LROWS(KIGC1)*LCOLS(KIGC1)
      CALL AUBOS('IGCL',1,LIGC1,KIGC1,IGARB)
C
      KITF2 = NLINK('ITFT',2)
      LITF2 = LMHLEN + LROWS(KITF2)*LCOLS(KITF2)
      CALL AUBOS('ITFT',2,LITF2,KITF2,IGARB)
C
      KIGT2 = NLINK('IGTL',2)
      LIGT2 = LMHLEN + LROWS(KIGT2)*LCOLS(KIGT2)
      CALL AUBOS('IGTL',2,LIGT2,KIGT2,IGARB)
C
      KIGC2 = NLINK('IGCL',2)
      LIGC2 = LMHLEN + LROWS(KIGC2)*LCOLS(KIGC2)
      CALL AUBOS('IGCL',2,LIGC2,KIGC2,IGARB)
      GOTO 1000
  998 CONTINUE
C Error return
      CALL RERROR('IMATCH',2,' Insufficient BOS space')
 1000 CONTINUE
C Drop work bank.
      IF (IMATIW.GT.0) CALL WDROP(IW,IMATIW)
      END
#endif
