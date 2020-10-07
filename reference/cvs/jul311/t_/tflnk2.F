      SUBROUTINE TFLNK2(IER)
C-----------------------------------------------------
C! Finds links between pairs of tracks in the TPC.
C  If the chi**2 of the combined fit is too bad the point
C  with the largest contribution to the chi**2 is rejected
C  and the fit is repeated. A maximum of 2 points can be removed.
C
C  Input banks:  T1FT, T1TL, T1CL
C  Output banks: TGFT, TGTL, TGCL
C
C  Author : LL. Garrido 30/5/1989
C  Modified:  R. Johnson   29-05-91
C  Modified:  P. Colrain   01-08-95
C             Treatment of track candidates (2 linked tracks)
C             is now consistent with that in TFITTK, the main
C             TPC track fitting routine
C             D. Casper    09-07-97
c             Check for bogus fits
C-----------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "t1ftjj.h"
#include "t1tljj.h"
#include "bcs.h"
#include "tgcljj.h"
#include "tgftjj.h"
#include "tgtljj.h"
#include "tpcojj.h"
#include "rconds.h"
#include "ufitcb.h"
#include "fconjj.h"
C
      COMMON/TFLNWB/ NBPWRK
C
      PARAMETER (MXGD=25)
      DIMENSION VV1(10),VV2(10),VV0(6),EE0(21),COV(21)
      DIMENSION LIST1(21),LIST2(2),LIST3(2)
      DIMENSION LGOOD(MXGD,27),QGOOD(MXGD,21)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA RF/75./
C
C---> Steering constants follow:
C
      DATA MINPT/4/,PMIN/0.00030/,LOMAX/2/,CHIPM/300.0/
C          |        |            |         |
C          |        |            |         +- CHI2 DIFF OF LINKS
C          |        |            +- MAX. POINTS TO THROW AWAY
C          |        +- MIN. FIT PROBABILITY FOR TRACK
C          +- MINIMAL NUMBER OF POINTS ON A TRACK
C
#include "bmacro.h"
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        NBPWRK=0
        NTPCO=NAMIND('TPCO')
        NFCON=NAMIND('FCON')
        NATGFT=NAMIND('TGFT')
        NATGTL=NAMIND('TGTL')
        NATGCL=NAMIND('TGCL')
        NT1FT=NAMIND('T1FT')
        NT1TL=NAMIND('T1TL')
        NT1CL=NAMIND('T1CL')
      ENDIF
C
C++   Check if the input banks exist
C
      KT1FT=IW(NT1FT)
      IF(KT1FT.EQ.0) THEN
        IER=0
        RETURN
      ENDIF
      NTRI=LROWS(KT1FT)
      KT1TL=IW(NT1TL)
      KT1CL=IW(NT1CL)
      IF (KT1TL.EQ.0 .OR. KT1CL.EQ.0) THEN
        IER=5
        CALL RERROR('TFLNK2',IER,'T1TL and T1CL banks are missing.')
        RETURN
      ENDIF
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) THEN
        IER=6
        CALL RERROR('TFLNK2',IER,'TPCO coordinate banks is missing.')
        RETURN
      ENDIF
C
C++   Drop the output banks if they already exist
C
      KTGFT=NDROP('TGFT',1)
      KTGTL=NDROP('TGTL',1)
      KTGCL=NDROP('TGCL',1)
C
C++   Create the output banks
C
      LEN=NTRI*LTGFTA+LMHLEN
      CALL AUBOS('TGFT',1,LEN,KTGFT,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=8
        CALL RERROR('TFLNK2',IER,'No room in BOS for bank TGFT')
        RETURN
      ENDIF
      IW(KTGFT+LMHCOL)=LTGFTA
      LEN=NTRI*LTGTLA+LMHLEN
      CALL AUBOS('TGTL',1,LEN,KTGTL,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=9
        KTGFT=NDROP('TGFT',1)
        CALL RERROR('TFLNK2',IER,'No room in BOS for bank TGTL')
        RETURN
      ENDIF
      IF (IGARB.NE.0) THEN
        KTGFT=NLINK('TGFT',1)
      ENDIF
      IW(KTGTL+LMHCOL)=LTGTLA
      KT1CL=IW(NT1CL)
      LEN=IW(KT1CL)
      CALL AUBOS('TGCL',1,LEN,KTGCL,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=10
        KTGFT=NDROP('TGFT',1)
        KTGTL=NDROP('TGTL',1)
        CALL RERROR('TFLNK2',IER,'No room in BOS for bank TGCL')
        RETURN
      ENDIF
      IF (IGARB.NE.0) THEN
        KTGFT=NLINK('TGFT',1)
        KTGTL=NLINK('TGTL',1)
      ENDIF
      IW(KTGCL+LMHCOL)=LTGCLA
      CALL BLIST(IW,'T+','TGFTTGTLTGCL')
C
      KT1FT=IW(NT1FT)
      KT1TL=IW(NT1TL)
      KT1CL=IW(NT1CL)
C
C     Calculating bit pattern for each track in a workbank
C
      IW(1)=1
      CALL WBANK(IW,NBPWRK,NTRI,*848)
      DO 1 I=1,NTRI
        IW(NBPWRK+I)=0
        KP1=ITABL(KT1TL,I,JT1TIO)
        NPT=ITABL(KT1TL,I,JT1TN1)
        DO 11 J=1,NPT
          ICO=IW(KT1CL+LMHLEN+KP1+J)
          NPAD=ITABL(KTPCO,ICO,JTPCIN)/100000+10
          IF (NPAD.GT.10.AND.NPAD.LT.33) CALL SBIT1(IW(NBPWRK+I),NPAD)
   11   CONTINUE
    1 CONTINUE
C
C     Loop over pairs of tracks to find candidates to link
C
      NG=0
      DO 91 I1=1,NTRI-1
        NPT1=ITABL(KT1TL,I1,JT1TN1)
        IF (NPT1.EQ.0) GOTO 91
        IFF=0
        DO 89 I2=I1+1,NTRI
          NPT2=ITABL(KT1TL,I2,JT1TN1)
          IF (NPT2.EQ.0) GOTO 89
          IX=IAND(IW(NBPWRK+I1),IW(NBPWRK+I2))
          IF (IX.NE.0) GOTO 89
          IF (IFF.EQ.0) THEN
            IFF=1
            IT1F1=KROW(KT1FT,I1)
            DO 22 I=JT1FIR,JT1FZ0
              VV1(I)=RW(IT1F1+I)
   22       CONTINUE
            VV1( 6)=SQRT(ABS(RW(IT1F1+JT1FEM)))
            VV1( 7)=SQRT(ABS(RW(IT1F1+JT1FEM+ 2)))
            VV1( 8)=SQRT(ABS(RW(IT1F1+JT1FEM+ 5)))
            VV1( 9)=SQRT(ABS(RW(IT1F1+JT1FEM+ 9)))
            VV1(10)=SQRT(ABS(RW(IT1F1+JT1FEM+14)))
          ENDIF
          IT1F2=KROW(KT1FT,I2)
          DO 23 I=JT1FIR,JT1FZ0
            VV2(I)=RW(IT1F2+I)
  23      CONTINUE
          VV2( 6)=SQRT(ABS(RW(IT1F2+JT1FEM)))
          VV2( 7)=SQRT(ABS(RW(IT1F2+JT1FEM+ 2)))
          VV2( 8)=SQRT(ABS(RW(IT1F2+JT1FEM+ 5)))
          VV2( 9)=SQRT(ABS(RW(IT1F2+JT1FEM+ 9)))
          VV2(10)=SQRT(ABS(RW(IT1F2+JT1FEM+14)))
C
C         Check the consistency of the helix parameters of the 2 tracks
C
          CHIP=0.
          DPHI=VV1(3)-VV2(3)
          IF (ABS(DPHI) .GT. ACOS(-1.)) DPHI=2*ACOS(-1.)-ABS(DPHI)
          DO 67 IK=1,5
            IF (IK.EQ.3) THEN    ! PHI
              CHIP=CHIP+DPHI**2/(VV1(IK+5)**2+VV2(IK+5)**2)
            ELSE
              CHIP=CHIP+(VV1(IK)-VV2(IK))**2/(VV1(IK+5)**2+VV2(IK+5)**2)
            ENDIF
   67     CONTINUE
          IF (NPT1.EQ.3.OR.NPT2.EQ.3) CHIP=CHIPM-1.
          IF (CHIP.GT.CHIPM) GOTO 89
C
C         Assemble the TPC coordinate list in increasing pad row number
C
          DO J1=1,21
            LIST1(J1)=0
          ENDDO
          NPTPC1 =ITABL(KT1TL,I1,JT1TIO)
          DO J1=1,NPT1
            ICO=IW(KT1CL+LMHLEN+NPTPC1+J1)
            NPAD=ITABL(KTPCO,ICO,JTPCIN)/100000
            IF(NPAD.GT.0.AND.NPAD.LT.22) LIST1(NPAD)=ICO
          ENDDO
          NPTPC2 =ITABL(KT1TL,I2,JT1TIO)
          DO J2=1,NPT2
            ICO=IW(KT1CL+LMHLEN+NPTPC2+J2)
            NPAD=ITABL(KTPCO,ICO,JTPCIN)/100000
            IF(NPAD.GT.0.AND.NPAD.LT.22) LIST1(NPAD)=ICO
          ENDDO
          NTPC=0
          DO IJ=1,21
            IF(LIST1(IJ).NE.0.) THEN
              NTPC=NTPC+1
              LIST1(NTPC)=LIST1(IJ)
            ENDIF
          ENDDO
          DO IJ=NTPC+1,21
            LIST1(IJ)=0
          ENDDO
C
C         Fit the track candidate. Remove upto LOMAX points to get a
C         good fit, as in TFITTK.
C
          LOST=0
          IOPT=2
          NMULT=1
          NTPC=NPT1+NPT2
          NITC=0
          NVDET=0
  333     CONTINUE
          CALL UFITMS(NTPC,NITC,NVDET,LIST1,LIST2,LIST3,IOPT,NMULT
     +                              ,29.6,.027,VV0,EE0,COV,CHI2,IERR)
          PPP=PROB(CHI2,2*NTPC-5)
          IF (PPP.LT.PMIN) THEN
            IF (NTPC.GT.MINPT.AND.LOST.LT.LOMAX) THEN
              LOST=LOST+1
              CMAX=0.
              IMAX=0
              DO 234 IKK=1,NTPC
                IF(CHIACU(IKK).GT.CMAX) THEN
                  CMAX=CHIACU(IKK)
                  IMAX=IKK
                ENDIF
  234         CONTINUE
              J2=0
              DO J1=1,NTPC
                IF (J1.NE.IMAX) THEN
                  J2=J2+1
                  LIST1(J2)=LIST1(J1)
                ENDIF
              ENDDO
              LIST1(NTPC)=0
              NTPC=NTPC-1
              GOTO 333
            ELSE
              GOTO 89
            ENDIF
          ENDIF
C  Check for garbage fit
          CALL TFCHEK(VV0,.FALSE.,IERR)
          IF (IERR.EQ.0) THEN
C
C  Pair found, store relevant parameters
C
           IF(NG.LT.MXGD) THEN
             NG=NG+1
             LGOOD(NG,1)=I1
             LGOOD(NG,2)=I2
             LGOOD(NG,3)=LOST
             LGOOD(NG,4)=NINT(1000000.0*CHI2)
             LGOOD(NG,5)=0
             LGOOD(NG,6)=NTPC
             DO I=1,21
               LGOOD(NG,6+I)=LIST1(I)
             ENDDO
             DO I=1,5
               QGOOD(NG,I)=VV0(I)
             ENDDO
             DO I=1,15
               QGOOD(NG,5+I)=COV(I)
             ENDDO
             QGOOD(NG,21)=CHI2
           ELSE
            CALL RERROR('TFLNK2',2,'Overflow of buffer for track '//
     +                  ' pairs.  Some TPC track links lost.')
           ENDIF
          ENDIF
   89   CONTINUE
   91 CONTINUE
C
C     If 2 links have a track in common, flag the link with the
C     largest chi**2.
C
      IF (NG.GT.1) THEN
        DO 107 IT1=1,NG-1
          IF (LGOOD(IT1,5).EQ.1) GOTO 107
          N1A=LGOOD(IT1,1)
          N1B=LGOOD(IT1,2)
          ICH1=LGOOD(IT1,4)
          DO 7 IT2=IT1+1,NG
            IF (LGOOD(IT2,5).EQ.1) GOTO 7
            N2A=LGOOD(IT2,1)
            N2B=LGOOD(IT2,2)
            IF (N1A.NE.N2A .AND. N1A.NE.N2B .AND. N1B.NE.N2A
     +                                   .AND. N1B.NE.N2B) GOTO 7
            ICH2=LGOOD(IT2,4)
            IF (ICH1.LT.ICH2) THEN
              LGOOD(IT2,5)=1
            ELSE
              LGOOD(IT1,5)=1
              ICH1=ICH2
            ENDIF
    7     CONTINUE
  107   CONTINUE
      ENDIF
      NPREG=0
      NTASS=0
      DO 123 IT=1,NG
        IF (LGOOD(IT,5).EQ.0) THEN
          NTASS=NTASS+1
          NPREG=NPREG+LGOOD(IT,3)
        ENDIF
  123 CONTINUE
C
      NTGFT = NTRI - NTASS
      NTGCL = LROWS(KT1CL) - NPREG
      IW(KTGFT+LMHROW)=NTGFT
      IW(KTGTL+LMHROW)=NTGFT
      IW(KTGCL+LMHROW)=NTGCL
      NT=0
      NCL=0
C
C++   Loop over all tracks
C
      DO 567 I0=1,NTRI
C
C++     Look for a link
C
        DO 577 IT=1,NG
          I1=LGOOD(IT,1)
          I2=LGOOD(IT,2)
          IF (I1.NE.I0.AND.I2.NE.I0) GOTO 577
C
C++       Here is a link with another track
C
          NFL=LGOOD(IT,5)
C
C++       Skip this link if a better (smaller chi**2) link exists
C
          IF (NFL.EQ.1) GOTO 577
C
C++       Skip this track if it already was linked to a previous one
C
          IF (NFL.EQ.2) GOTO 567
C
C++       Copy fit results to the new bank
C
          LGOOD(IT,5)=2
C
          NTPC=LGOOD(IT,6)
          DO I=1,21
            LIST1(I)=LGOOD(IT,6+I)
          ENDDO
          DO I=1,5
            VV0(I)=QGOOD(IT,I)
          ENDDO
          DO I=1,15
            COV(I)=QGOOD(IT,5+I)
          ENDDO
          CHI2=QGOOD(IT,21)
C
          NT=NT+1
          IW(KROW(KT1FT,I1)+JT1FTG) = NT
          IW(KROW(KT1FT,I2)+JT1FTG) = NT
          ITGFT=KROW(KTGFT,NT)
          CALL UCOPY(VV0,RW(ITGFT+JTGFIR),5)
          CALL UCOPY(COV,RW(ITGFT+JTGFEM),15)
          RW(ITGFT+JTGFCD)=CHI2
          IW(ITGFT+JTGFDF)=2*NTPC-5
          IW(ITGFT+JTGFTC)=0  ! Pointer to fitted FRFT track
          ITGTL=KROW(KTGTL,NT)
          IW(ITGTL+JTGTIO)=NCL
          IW(ITGTL+JTGTN1)=NTPC
          NRT1=ITABL(KT1TL,I1,JTGTNR)
          NRT2=ITABL(KT1TL,I2,JTGTNR)
          NRT=NRT1+NRT2
          IW(ITGTL+JTGTNR)=NRT
          CALL UCOPY(LIST1,IW(KTGCL+LMHLEN+NCL+1),NTPC)
          IF (NRT1.NE.0) THEN
            KP1=ITABL(KT1TL,I1,JTGTIO)
            NPT1=ITABL(KT1TL,I1,JTGTN1)
            CALL UCOPY(IW(KT1CL+LMHLEN+KP1+NPT1+1),
     +                       IW(KTGCL+LMHLEN+NCL+NTPC+1),NRT1)
          ENDIF
          IF (NRT2.NE.0) THEN
            KP2=ITABL(KT1TL,I2,JTGTIO)
            NPT2=ITABL(KT1TL,I2,JTGTN1)
            CALL UCOPY(IW(KT1CL+LMHLEN+KP2+NPT2+1),
     +                       IW(KTGCL+LMHLEN+NCL+NTPC+NRT1+1),NRT2)
          ENDIF
          NCL=NCL+NTPC+NRT
          GOTO 567
C
  577   CONTINUE
C
C++     Copy the unchanged tracks
C
        NT=NT+1
        IT1FT=KROW(KT1FT,I0)
        IW(IT1FT+JT1FTG) = NT
        ITGFT=KROW(KTGFT,NT)
        CALL UCOPY(RW(IT1FT+JTGFIR),RW(ITGFT+JTGFIR),5)
        CALL UCOPY(RW(IT1FT+JTGFEM),RW(ITGFT+JTGFEM),15)
        RW(ITGFT+JTGFCD)=RW(IT1FT+JTGFCD)
        IW(ITGFT+JTGFDF)=IW(IT1FT+JTGFDF)
        IW(ITGFT+JTGFTC)=IW(IT1FT+JT1FTC)
        IT1TL=KROW(KT1TL,I0)
        ITGTL=KROW(KTGTL,NT)
        IW(ITGTL+JTGTIO)=NCL
        IW(ITGTL+JTGTN1)=IW(IT1TL+JTGTN1)
        IW(ITGTL+JTGTNR)=IW(IT1TL+JTGTNR)
        KP=ITABL(KT1TL,I0,JTGTIO)
        NTOT=IW(ITGTL+JTGTN1)+IW(ITGTL+JTGTNR)
        CALL UCOPY(IW(KT1CL+LMHLEN+KP+1),IW(KTGCL+LMHLEN+NCL+1),NTOT)
        NCL=NCL+NTOT
C
  567 CONTINUE
C
C Update the pointers in TPCO to the TGFT bank
C
      DO 705 IC=1,LROWS(KTPCO)
        IW(KROW(KTPCO,IC)+JTPCTN)=0
  705 CONTINUE
      DO 756 ITK=1,LROWS(KTGTL)
        IOFF=ITABL(KTGTL,ITK,JTGTIO)+LMHLEN
        N1=ITABL(KTGTL,ITK,JTGTN1)
        NTOT=N1+ITABL(KTGTL,ITK,JTGTNR)
        DO 732 II=1,N1
          IC=IW(KTGCL+IOFF+II)
          IW(KROW(KTPCO,IC)+JTPCTN)=ITK
  732   CONTINUE
        DO 742 II=N1+1,NTOT
          IC=IW(KTGCL+IOFF+II)
          IW(KROW(KTPCO,IC)+JTPCTN)=-ITK
  742   CONTINUE
  756 CONTINUE
C
C Compress the new banks
C
      CALL AUBPRS('TGFTTGTLTGCL')
C
      IER=0
      CALL WDROP(IW,NBPWRK)
      RETURN
  848 CONTINUE
      IER=11
      CALL RERROR('TFLNK2',IER,'No room to create a workbank.')
      KTGFT=NDROP('TGFT',1)
      KTGTL=NDROP('TGTL',1)
      KTGCL=NDROP('TGCL',1)
      END
#endif
