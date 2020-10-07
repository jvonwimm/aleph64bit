      SUBROUTINE ITRMCA
C-----------------------------------------------------------------------
C! Associate JULIA ITC tracks with true MC tracks.
C!
C!    Author     :- I. Tomalin    88/06/24
C!    Modified   :- J. Sedgbeer   89/08/28
C!    Modified   :- I. Tomalin    90/05/24 (To use IHIT instead of ITHT)
C!
C!    Input:
C!      commons:       /BCS/
C!                      => ITDR,ITCO,IDCR,IDHR,ITHT,IGTL,IGCL
C!                     /RCURNT/ run and event number
C!                     /ITNAMC/ pointers to ITC banks
C!                     /IWBNKT/ pointers to work banks for ITC tracking
C!      parameters:    ITCOJJ   parameters for ITCO bank
C!                     IGTLJJ   parameters for IGTL bank
C!                     IDHRJJ   parameters for IDHR bank
C!                     ITHTJJ   parameters for ITHT bank
C!                     IHITJJ   parameters for IHIT bank
C!                     IPJTJJ   parameters for IPJT bank
C!                     IRJTJJ   parameters for IRJT bank
C!
C!    Output:   IPJT and IRJT banks.
C!
C!    called by : ITCREC
C!    calls     : none
C!
C!    Libraries required: BOS
C!
C? Find the total # of true MC tracks which caused hits in the ITC
C? Create ISHRIW work bank
C? Create a work bank ICDRIW, containing coordinate to digit
C?   relationship (i.e. effectively invert the IDCR bank)
C?
C? Loop over the two different type of JULIA ITC tracks. (The first type
C? being those tracks which were found by extending the TPC ones. The
C? second type being those tracks which were found using only the ITC).
C?   Find the total # of JULIA ITC tracks of this type
C?   Create appropriate IPJT bank (# 1 or 2) and the IRJTIW work bank
C?   Loop over the ITC JULIA tracks
C?     Loop over all the coordinates on each track
C?       For each of these, find the offset in the ITCO bank and hence
C?        obtain the corresponding offset in the ITDI bank
C?       Find the true MC tracks which caused each digitisation
C?       Temporarily store this information in the ISHRIW work bank
C?     End Loop
C?     Fill the IPJT bank and the IRJTIW work bank
C?   End Loop
C?   Create the appropriate IRJT bank (# 1 or 2)
C?   Copy the IRJTIW work bank into the IRJT bank
C? End Loop
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
C-----------------------------------------------------------------------
C I/O commons and parameters
#include "bcs.h"
#include "bosext.h"
#include "rcurnt.h"
#include "itnamc.h"
#include "iwbnkt.h"
#include "itcojj.h"
#include "igtljj.h"
#include "idhrjj.h"
#include "ithtjj.h"
#include "ihitjj.h"
#include "ipjtjj.h"
#include "irjtjj.h"
C----------------------------------------------------------------------
C Dummy common block to save work bank indices.
      COMMON/IDUMYG/ISHRIW,ICDRIW,IRJTIW
      LOGICAL FIRST,NEWH
      DATA FIRST/.TRUE./
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C These matrices are only used with the debug lines commented out below.
C      DIMENSION ICOZ(2),ILAYZ(2),PHI1Z(2),PHI2Z(2),ZZZ(2),TPHIZ(2),
C      TZZ(2),IHITZ(2)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        ICDRIW = 0
        ISHRIW = 0
        IRJTIW = 0
      ENDIF
      KITDR = NLINK('ITDR',0)
      IF (KITDR.EQ.0) GOTO 2000
      KITDL = NLINK('ITDL',0)
      IF (KITDL.EQ.0) GOTO 2000
C
C  Find the total number of true MC tracks which caused hits in the ITC.
      NTRTR = LROWS(KITDR)
      IF(NTRTR.LE.0) GOTO 2000
C
C  Find out the range which the KINE track #s cover (NKS->NKF).
      NKS = 999999
      NKF = -999999
      DO 10 N=1,NTRTR
C  Note the KINE track number of track "N".
        ITR = ITABL(KITDR,N,1)
        IF (ITR.LT.NKS) NKS = ITR
        IF (ITR.GT.NKF) NKF = ITR
  10  CONTINUE
C
C  Create a work bank which will contain the number of hits on the "I"th
C  ITC JULIA track which are shared with each true KINE track.
      CALL WBANK(IW,ISHRIW,LMHLEN+NKF-NKS+1,*999)
      IW(ISHRIW+LMHCOL) = 1
      IW(ISHRIW+LMHROW) = NKF-NKS+1
C
      KITC0 = IW(NAITCO)
      IF (KITC0.LE.0) GOTO 2000
      KIDCR = IW(NAIDCR)
      IF (KIDCR.LE.0) GOTO 2000
C  Create a coordinate to digit relationship work bank from the IDCR one
C  First find the total number of coordinates.
      NCOOR = LROWS(KITC0)
      CALL WBANK(IW,ICDRIW,LMHLEN+NCOOR,*999)
      IW(ICDRIW+LMHCOL) = 1
      IW(ICDRIW+LMHROW) = NCOOR
      NDOFF = 0
      DO 20 N=1,NCOOR
C  Find the coordinate number (if any) corresponding to digit NDOFF.
  15    CONTINUE
        NDOFF = NDOFF + 1
        NUMC = IW(KIDCR+LMHLEN+NDOFF)
        IF (NUMC.EQ.0) GOTO 15
        IW(ICDRIW+LMHLEN+N) = NDOFF
  20  CONTINUE
C
      KIDHR = NLINK('IDHR',0)
      IF (KIDHR.LE.0) GOTO 2000
C Decide whether old ITHT or new IHIT bank is present.
      KIHIT = NLINK('IHIT',0)
      NEWH = .TRUE.
      IF (KIHIT.LE.0) THEN
        KITHT = NLINK('ITHT',0)
        NEWH = .FALSE.
        IF (KITHT.LE.0) GOTO 2000
      END IF
C
C  Start to find out which JULIA ITC tracks share hits with which
C  true Monte Carlo tracks.
C    Do this first for those JULIA ITC tracks which were extensions of
C  TPC ones and then for those which were only found in the ITC.
C
      DO 200 ITYPE=1,2
        KIGTL = NLINK('IGTL',ITYPE)
        IF (KIGTL.LE.0) GOTO 200
C  Note the total number of JULIA ITC tracks.
        NTRJU = LROWS(KIGTL)
C
C  Create the IPJT bank.
        CALL AUBOS('IPJT',ITYPE,LMHLEN+LIPJTA*NTRJU,KIPJT,IGARB)
        IF (IGARB.EQ.2) GOTO 999
        IF (IGARB.EQ.1) THEN
          KITDR = NLINK('ITDR',0)
          KITDL = NLINK('ITDL',0)
          KIDCR = IW(NAIDCR)
          KIDHR = NLINK('IDHR',0)
          IF (NEWH) KIHIT = NLINK('IHIT',0)
          IF (.NOT.NEWH) KITHT = NLINK('ITHT',0)
          KIGTL = NLINK('IGTL',ITYPE)
        END IF
        IW(KIPJT+LMHCOL) = LIPJTA
        IW(KIPJT+LMHROW) = NTRJU
C  Create a work bank which will eventually be copied into the IRJT bank
        CALL WBANK(IW,IRJTIW,LMHLEN+LIRJTA*NTRJU*NTRTR,*999)
        IW(IRJTIW+LMHCOL) = LIRJTA
        IW(IRJTIW+LMHROW) = NTRJU*NTRTR
C
        KIGCL = NLINK('IGCL',ITYPE)
        IF (KIGCL.LE.0) GOTO 200
C
        IOFF = 1
C  Loop over the ITC JULIA tracks.
        DO 150 I=1,NTRJU
          II = KROW(KIGTL,I)
C  Find the number of coordinates on track "I".
C         NC = IW(II+JIGTN1) + IW(II+JIGTNR)
C  Consider only those on the first arc of the track.
          NC = IW(II+JIGTN1)
C  Obtain offset in IGCL bank for track "I".
          MIGCL = IW(II+JIGTIO)
C  Zero the ISHRIW work bank.
          CALL VZERO(IW(ISHRIW+LMHLEN+1),NKF-NKS+1)
C  Loop over the coordinates on track "I".
          DO 40 J=1,NC
C  Obtain coord. number in ITCO bank for "Jth" coord. on the track.
            NITCO = ABS(IW(KIGCL+LMHLEN+MIGCL+J))
C  Obtain the corresponding digitisation number in the ITDI bank.
            NDIG = IW(ICDRIW+LMHLEN+NITCO)
C  Find the hit (if any) which caused this digitization.
            NHIT = ITABL(KIDHR,NDIG,JIDHHA)
            IF (NHIT.EQ.0) GOTO 40
C  Obtain the KINE track which really caused this hit.
            IF (NEWH) ITR = ITABL(KIHIT,NHIT,JIHITN)
            IF (.NOT.NEWH) ITR = ITABL(KITHT,NHIT,JITHTN)
C  Also note the layer number of the hit.
            IF (NEWH) ILAY = ITABL(KIHIT,NHIT,JIHILN)
            IF (.NOT.NEWH) ILAY = ITABL(KITHT,NHIT,JITHLN)
C  Reject STACK tracks.
            IF (ITR.LE.0) GOTO 40
C  Ignore this hit if it was not on the 1st arc of the associated KINE
C  track. (Look for track which was going outwards starting to move
C  inwards or visa-versa).
            KHIT = NHIT
            JLAY = ILAY
   25       CONTINUE
            KHIT = KHIT - 1
            IF (KHIT.GE.1) THEN
              IF (NEWH) KTR = ITABL(KIHIT,KHIT,JIHITN)
              IF (.NOT.NEWH) KTR = ITABL(KITHT,KHIT,JITHTN)
              IF (ITR.NE.KTR) GOTO 30
              IF (NEWH) KLAY = ITABL(KIHIT,KHIT,JIHILN)
              IF (.NOT.NEWH) KLAY = ITABL(KITHT,KHIT,JITHLN)
              IF (JLAY.EQ.ILAY.AND.KLAY.EQ.ILAY) GOTO 40
              IF (JLAY.NE.ILAY.AND.(KLAY-JLAY)*(JLAY-ILAY).LE.0) GOTO 40
              ILAY = JLAY
              JLAY = KLAY
              GOTO 25
            END IF
   30       CONTINUE
C  Note that this(these) tracks shared coords with JULIA ITC track "I".
            IW(ISHRIW+LMHLEN+ITR-NKS+1) =
     +            IW(ISHRIW+LMHLEN+ITR-NKS+1) + 1
C  Find wire number of hit.
C           IF (NEWH) NWIRE = ITABL(KIHIT,NHIT,JIHIWN)
C           IF (.NOT.NEWH) NWIRE = ITABL(KITHT,NHIT,JITHWN)
C           WRITE(LDEBRL,35) I,NDIG,NWIRE,ITR
  35        FORMAT(' ITRMCA : JULIA ITC track ',I3,' shares ',
     +      'digitisation ',I4,' (wire number) ',I4,' with true MC ',
     +      'track ',I3)
  40      CONTINUE
C  Find the number of true MC tracks sharing coords with track "I".
          NSHAR = 0
          DO 45 N=1,NTRTR
C  Get the KINE track number corresponding to track "N".
            NK = ITABL(KITDR,N,1)
            IF (IW(ISHRIW+LMHLEN+NK-NKS+1).GT.0) NSHAR = NSHAR + 1
  45      CONTINUE
C  Fill the IPJT bank.
C  1st column gives the number of coords assigned by JULIA to the 1st
C  half of the 1st orbit of the track.
          IW(KROW(KIPJT,I)+JIPJNH) = NC
C  2nd column gives the number of true MC tracks sharing coords with it.
          IW(KROW(KIPJT,I)+JIPJNS) = NSHAR
C  3nd column gives the row number of the 1st of these tracks in the IRJ
C  bank.
          IW(KROW(KIPJT,I)+JIPJRN) = IOFF
C
C  Fill the IRJTIW work bank.
          DO 120 M=1,NTRTR
C  Get the KINE track number corresponding to track "M".
            IK = ITABL(KITDR,M,1)
            IF (IW(ISHRIW+LMHLEN+IK-NKS+1).GT.0) THEN
C
C  1st column gives KINE track numbers of the tracks sharing coords with
C  the JULIA ITC one.
              IW(KROW(IRJTIW,IOFF)+JIRJGN) = IK
C
C  2nd column gives the total number of coords on the 1st half of the
C  1st orbit of this KINE track. So now calculate this;-
C  (Note that to have got this far, it must have at least 1 coord there)
C
              NCOOR = 0
              IADDR = ITABL(KITDR,M,3)
C
C  Get the first hit on the track.
              IDIG = ITABL(KITDL,IADDR,1)
              IHIT = ITABL(KIDHR,IDIG,JIDHHA)
C  If this is zero, then something very strange is going on !
              IF (IHIT.EQ.0) GOTO 110
C  As a check, reobtain the KINE track which really caused this hit.
              IF (NEWH) JK = ITABL(KIHIT,IHIT,JIHITN)
              IF (.NOT.NEWH) JK = ITABL(KITHT,IHIT,JITHTN)
              IF (IK.NE.JK) GOTO 110
C  Note the layer number of this hit.
              IF (NEWH) ILAY = ITABL(KIHIT,IHIT,JIHILN)
              IF (.NOT.NEWH) ILAY = ITABL(KITHT,IHIT,JITHLN)
C  Now find the final hit in the 1st half of the 1st orbit.
              KHIT = IHIT
              JLAY = ILAY
   80         CONTINUE
              KHIT = KHIT + 1
              IF (NEWH) THEN
                IF (KHIT.LE.LROWS(KIHIT)) THEN
C  Obtain the KINE track which really caused this hit.
                  KK = ITABL(KIHIT,KHIT,JIHITN)
                  IF (IK.NE.KK) GOTO 90
                  KLAY = ITABL(KIHIT,KHIT,JIHILN)
                  IF (JLAY.EQ.ILAY.AND.KLAY.EQ.ILAY) GOTO 90
                  IF (JLAY.NE.ILAY.AND.(KLAY-JLAY)*(JLAY-ILAY).LE.0)
     +              GOTO 90
                  ILAY = JLAY
                  JLAY = KLAY
                  GOTO 80
                END IF
              ELSE
                IF (KHIT.LE.LROWS(KITHT)) THEN
C  Obtain the KINE track which really caused this hit.
                  KK = ITABL(KITHT,KHIT,JITHTN)
                  IF (IK.NE.KK) GOTO 90
                  KLAY = ITABL(KITHT,KHIT,JITHLN)
                  IF (JLAY.EQ.ILAY.AND.KLAY.EQ.ILAY) GOTO 90
                  IF (JLAY.NE.ILAY.AND.(KLAY-JLAY)*(JLAY-ILAY).LE.0)
     +              GOTO 90
                  ILAY = JLAY
                  JLAY = KLAY
                  GOTO 80
                END IF
              END IF
   90         CONTINUE
C  Note number of last hit on the 1st half if the 1st orbit of the
C  FKIN track.
              ILHIT = KHIT - 1
C
C  Find the total number of coords on the 1st half of the 1st orbit of
C  the KINE track.
              DO 100 N=1,ITABL(KITDR,M,2)
                IDIG = ITABL(KITDL,IADDR-1+N,1)
                IHIT = ITABL(KIDHR,IDIG,JIDHHA)
                IF (IHIT.EQ.0) GOTO 100
                IF (IHIT.GT.ILHIT) GOTO 110
                ICO = IW(KIDCR+LMHLEN+IDIG)
                IF (ICO.EQ.0) GOTO 100
                NCOOR = NCOOR + 1
  100         CONTINUE
  110         CONTINUE
              IW(KROW(IRJTIW,IOFF)+JIRJNH) = NCOOR
C
C  3rd column gives the number of coords which the two tracks share in
C  the 1st halves of their 1st orbits.
              IW(KROW(IRJTIW,IOFF)+JIRJSH) = IW(ISHRIW+LMHLEN+IK-NKS+1)
C
C  Check that the IRJT bank is sensible.
              IF (ITABL(IRJTIW,IOFF,JIRJSH).GT.
     +                                 ITABL(IRJTIW,IOFF,JIRJNH))
     +        CALL RERROR('ITRMCA',1,'IRJT bank does not make sense.')
              IOFF = IOFF + 1
            END IF
 120      CONTINUE
 150    CONTINUE
C
C Create IRJT bank. Fill from work bank.
C
        CALL AUBOS('IRJT',ITYPE,LMHLEN+LIRJTA*(IOFF-1),KIRJT,IGARB)
        IF (IGARB.EQ.1) THEN
          KITDR = NLINK('ITDR',0)
          KITDL = NLINK('ITDL',0)
          KIDCR = IW(NAIDCR)
          KIDHR = NLINK('IDHR',0)
          IF (NEWH) KIHIT = NLINK('IHIT',0)
          IF (.NOT.NEWH) KITHT = NLINK('ITHT',0)
        END IF
        IF(IGARB.EQ.2) GOTO 999
        IW(KIRJT+LMHCOL) = LIRJTA
        IW(KIRJT+LMHROW) = IOFF - 1
        CALL UCOPY(IW(IRJTIW+LMHLEN+1),IW(KIRJT+LMHLEN+1),
     +  LIRJTA*(IOFF-1))
C Now drop the IRJTIW work bank.
        IF (IRJTIW.NE.0) CALL WDROP(IW,IRJTIW)
  200 CONTINUE
C
C Finished !
C      WRITE(LDEBRL,*) ' $$$$$$$$$$$$$$$$$$$ IPJT $$$$$$$$$$$$$$$$$$$$'
C      CALL PRTABL('IPJT',0)
C      WRITE(LDEBRL,*) ' $$$$$$$$$$$$$$$$$$$ IRJT $$$$$$$$$$$$$$$$$$$$'
C      CALL PRTABL('IRJT',0)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
CC Here (commented out) are some debug lines for showing which coords.
CC are really associated with each track (in the correct order).
CC They at present only work with the ITHT bank, not the IHIT bank.
C      KITC0 = IW(NAITCO)
C      DO 250 N=1,NTRTR
CC  Get the KINE track number corresponding to track "N".
C        NK = ITABL(KITDR,N,1)
C        NDIG = ITABL(KITDR,N,2)
C        IF (NDIG.GT.0) THEN
C          WRITE(LDEBRL,210) NK
C 210      FORMAT(' KINE track ',I4,' has ITC coordinates :')
C        ELSE
C          WRITE(LDEBRL,215) NK
C 215      FORMAT(' KINE track ',I4,' caused no hits in the ITC.')
C          GOTO 250
C        END IF
C        IADDR = ITABL(KITDR,N,3)
C        DO 225 I=1,NDIG
C         IDIG = IW(KITDL+LMHLEN+IADDR+I-1)
C          ICO = IW(KIDCR+LMHLEN+IDIG)
C          IF (ICO.EQ.0) GOTO 225
C          IHIT = ITABL(KIDHR,IDIG,1)
C Print out in batches of two.
C          IOFF = 2 - MOD(I,2)
C          IHITZ(IOFF) = IHIT
C          ILAYZ(IOFF) = ITABL(KITC0,ICO,JITCWN)/1000
C          ICOZ(IOFF) = ICO
C          PHI1Z(IOFF) = RTABL(KITC0,ICO,JITCP1)
C          PHI2Z(IOFF) = RTABL(KITC0,ICO,JITCP2)
C          TPHIZ(IOFF) = RTABL(KITHT,IHIT,JITHPI)
C          ZZZ(IOFF) = RTABL(KITC0,ICO,JITCZH)
C          TZZ(IOFF) = RTABL(KITHT,IHIT,JITHZI)
C          IF (MOD(I,2).EQ.0) THEN
C            WRITE(LDEBRL,235) (IHITZ(M),ILAYZ(M),ICOZ(M),
C     +      PHI2Z(M),PHI1Z(M),TPHIZ(M),ZZZ(M),TZZ(M),M=1,2)
C          ELSE IF (I.EQ.NDIG) THEN
C            WRITE(LDEBRL,235) IHITZ(1),ILAYZ(1),ICOZ(1),
C     +      PHI2Z(1),PHI1Z(1),TPHIZ(1),ZZZ(1),TZZ(1)
C          END IF
C 225    CONTINUE
C 235    FORMAT(2('  H=',I4,' L=',I1,' C=',I3,' P(2,1,T)=',F5.3,2F6.3,
C     +  ' Z(M,T)=',F6.1,F7.1,:))
C 250  CONTINUE
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      GOTO 2000
C
  999 CONTINUE
      CALL RERROR('ITRMCA',2,'Insufficient BOS space.')
 2000 CONTINUE
C
C Add IPJT and IRJT banks to list T
C
      CALL BLIST(IW,'T+','IPJTIRJT')
C Now drop the work banks.
      IF (ICDRIW.NE.0) CALL WDROP(IW,ICDRIW)
      IF (ISHRIW.NE.0) CALL WDROP(IW,ISHRIW)
      IF (IRJTIW.NE.0) CALL WDROP(IW,IRJTIW)
      END
#endif
