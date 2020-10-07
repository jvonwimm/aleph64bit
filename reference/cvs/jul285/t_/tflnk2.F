      SUBROUTINE TFLNK2(IER)
C-----------------------------------------------------
C! Finds links between pairs of tracks in the TPC.
C  If the chi**2 of the combined fit is too bad the point
C  with the bigest contribution to the chi**2 is rejected.
C
C  Input banks:  T1FT, T1TL, T1CL
C  Output banks: TGFT, TGTL, TGCL
C
C  Author : LL. Garrido 30/5/1989
C  Modified: R. Johnson 29-05-91
C  Modified: I. Tomalin 18-10-95 (protected against straight tracks).
C-----------------------------------------------------
#ifndef DOC
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
      DIMENSION LGOOD(MXGD,6)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA RF/75./
      PARAMETER(EPSIL=1.0E-15)
C
#include "bmacro.h"
C
      IF(FIRST) THEN
        PI=2.*ASIN(1.)
        PIT2=2.*PI
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
C     Get cut parameters from the database
C
      KFCON=IW(NFCON)
      IF(KFCON.EQ.0) THEN
        CALL RERROR('TFLNK2',4,'FCON bank not found.  Using defaults'//
     +              ' for track linking parameters.')
        DELMIN=.3
        CHIPM=300.
        CHIC0=500.
        CHIC1=125.
        CHIC2=255.
      ELSE
        IFCON=KROW(KFCON,1)
        DELMIN=RW(IFCON+JFCODM)
        CHIPM =RW(IFCON+JFCOCP)
        CHIC0 =RW(IFCON+JFCOC0)
        CHIC1 =RW(IFCON+JFCOC1)
        CHIC2 =RW(IFCON+JFCOC2)
      ENDIF
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
        NRT1=ITABL(KT1TL,I1,JT1TNR)
        IFF=0
        DO 89 I2=I1+1,NTRI
          NPT2=ITABL(KT1TL,I2,JT1TN1)
          IF (NPT2.EQ.0) GOTO 89
          NRT2=ITABL(KT1TL,I2,JT1TNR)
          IF (NRT1.NE.0.AND.NRT2.NE.0) GOTO 89
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
            RR01  = VV1(1)
            IF (ABS(RR01).LT.EPSIL) RR01 = SIGN(EPSIL,RR01)
            DZDS1 = VV1(2)
            PH01  = VV1(3)
            DD01  = VV1(4)
            ZZ01  = VV1(5)
            SST1=1.
            IF (RR01.LT.0.) SST1=-1.
            GG01=RR01*DD01-1.
            HH01=1./GG01
            FF01=SST1*(RR01*(RF*RF-DD01**2)/(2.0*RF*GG01)+DD01/RF)
            IF (FF01.GT.1.0)  FF01 = 1.0
            IF (FF01.LT.-1.0) FF01 = -1.0
            PH1=PH01-SST1*ASIN(FF01)
            IF (PH1.LT.0.) PH1=PIT2+PH1
            IF (PH1.GT.PIT2) PH1=PH1-PIT2
            EEE1=-0.5*RR01
     +         *SQRT(ABS((RF*RF-DD01**2)/(1.0-RR01*DD01)))
            IF (EEE1.GT. 0.99990)  EEE1=  0.99990
            IF (EEE1.LT.-0.99990)  EEE1= -0.99990
            SXY1=-2.0*ASIN(EEE1)/RR01
            ZZ1= ZZ01+DZDS1*SXY1
            NPTPC1 =ITABL(KT1TL,I1,JT1TIO)
            DO 31 J1=1,NPT1
              LIST1(J1)=IW(KT1CL+LMHLEN+NPTPC1+J1)
  31        CONTINUE
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
          RR02  = VV2(1)
          IF (ABS(RR02).LT.EPSIL) RR02 = SIGN(EPSIL,RR02)
          DZDS2 = VV2(2)
          PH02  = VV2(3)
          DD02  = VV2(4)
          ZZ02  = VV2(5)
          SST2=1.
          IF (RR02.LT.0.) SST2=-1.
          GG02=RR02*DD02-1.
          HH02=1./GG02
          FF02=SST2*(RR02*(RF*RF-DD02**2)/(2.0*RF*GG02)+DD02/RF)
          IF (FF02.GT.1.0)  FF02 = 1.0
          IF (FF02.LT.-1.0) FF02 = -1.0
          PH2=PH02-SST2*ASIN(FF02)
          IF (PH2.LT.0.) PH2=PIT2+PH2
          IF (PH2.GT.PIT2) PH2=PH2-PIT2
          EEE2=-0.5*RR02
     +     *SQRT(ABS((RF*RF-DD02**2)/(1.0-RR02*DD02)))
          IF (EEE2.GT. 0.99990)  EEE2=  0.99990
          IF (EEE2.LT.-0.99990)  EEE2= -0.99990
          SXY2=-2.0*ASIN(EEE2)/RR02
          ZZ2= ZZ02+DZDS2*SXY2
          DELPH=PH2-PH1
          IF (DELPH.GT. PI) DELPH=DELPH-PIT2
          IF (DELPH.LT.-PI) DELPH=DELPH+PIT2
          CHIP=0.
          DO 67 IK=1,5
            CHIP=CHIP+(VV1(IK)-VV2(IK))**2/(VV1(IK+5)**2+VV2(IK+5)**2)
   67     CONTINUE
          IF (NPT1.EQ.3.OR.NPT1.EQ.4.OR.NPT2.EQ.3.OR.NPT2.EQ.4)
     +        CHIP=CHIPM-1.
          IF (ABS(DELPH).GT.DELMIN.OR.CHIP.GT.CHIPM) GOTO 89
          NPTPC2 =ITABL(KT1TL,I2,JT1TIO)
          DO 931 J1=1,NPT2
            LIST1(J1+NPT1)=IW(KT1CL+LMHLEN+NPTPC2+J1)
  931     CONTINUE
          IOPT=1
          NMULT=0
          NTPC=NPT1+NPT2
          NITC=0
          NVDET=0
          CALL UFITMS(NTPC,NITC,NVDET,LIST1,LIST2,LIST3,IOPT,NMULT
     +                              ,29.6,.027,VV0,EE0,COV,CHI2,IERR)
          IF (CHI2.GT.CHIC0) GOTO 89
          CMAX=0.
          IMAX=1
          DO 234 IKK=1,NTPC
            IF(CHIACU(IKK).GT.CMAX) THEN
              CMAX=CHIACU(IKK)
              IMAX=IKK
            ENDIF
  234     CONTINUE
          IF(CHI2-CMAX.GT.CHIC2) GOTO 89
C
C  Pair found, store relevant parameters
C
          IF(CHI2.LT.CHIC1) IMAX=0
          IF(NG.LT.MXGD) THEN
            NG=NG+1
            LGOOD(NG,1)=I1
            LGOOD(NG,2)=I2
            LGOOD(NG,3)=IMAX
            LGOOD(NG,4)=CHI2-CMAX
            LGOOD(NG,5)=0
            IF (IMAX.EQ.0) THEN
              LGOOD(NG,6)=0
            ELSE
              LGOOD(NG,6)=LIST1(IMAX)
            ENDIF
          ELSE
            CALL RERROR('TFLNK2',2,'Overflow of buffer for track '//
     +                  ' pairs.  Some TPC track links lost.')
          ENDIF
   89   CONTINUE
   91 CONTINUE
C
C Fit match candidates and store them
C
      IF (NG.GT.1) THEN
        DO 107 IT1=1,NG-1
          IF (LGOOD(IT1,5).EQ.1) GOTO 107
          N1A=LGOOD(IT1,1)
          N1B=LGOOD(IT1,2)
          IDEL=IT1
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
              IDEL=IT2
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
          IF (LGOOD(IT,3).NE.0) NPREG=NPREG+1
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
          IF (NFL.EQ.1) GOTO 577
C
C++       Skip this track if it already was linked to a previous one
C
          IF (NFL.EQ.2) GOTO 567
C
C++       Fit the combined track and copy it to the new bank
C
          LGOOD(IT,5)=2
          DO 344 IJ=1,21
            LIST1(IJ)=0
  344     CONTINUE
          KP1=ITABL(KT1TL,I1,JTGTIO)
          NPT1=ITABL(KT1TL,I1,JTGTN1)
          NRT1=ITABL(KT1TL,I1,JTGTNR)
          IMCOU=0
          IMAX=LGOOD(IT,3)
          DO 345 JI1=1,NPT1
            IMCOU=IMCOU+1
            IF(IMCOU.EQ.IMAX) GOTO 345
            ICO=IW(KT1CL+LMHLEN+KP1+JI1)
            NPAD=ITABL(KTPCO,ICO,JTPCIN)/100000
            IF(NPAD.GT.0.AND.NPAD.LT.22) LIST1(NPAD)=ICO
  345     CONTINUE
          KP2=ITABL(KT1TL,I2,JTGTIO)
          NPT2=ITABL(KT1TL,I2,JTGTN1)
          NRT2=ITABL(KT1TL,I2,JTGTNR)
          NRRR=0
          IF(NRT1.NE.0) NRRR=I1
          IF(NRT2.NE.0) NRRR=I2
          DO 346 JI2=1,NPT2
            IMCOU=IMCOU+1
            IF(IMCOU.EQ.IMAX) GOTO 346
            ICO=IW(KT1CL+LMHLEN+KP2+JI2)
            NPAD=ITABL(KTPCO,ICO,JTPCIN)/100000
            IF(NPAD.GT.0.AND.NPAD.LT.22) LIST1(NPAD)=ICO
  346     CONTINUE
          NTPC=0
          DO 347 IJ=1,21
            IF(LIST1(IJ).NE.0.) THEN
            NTPC=NTPC+1
            LIST1(NTPC)=LIST1(IJ)
          ENDIF
  347     CONTINUE
          IOPT=2
          CALL UFITMS(NTPC,NITC,NVDET,LIST1,LIST2,LIST3,IOPT,NMULT
     +                               ,29.6,.027,VV0,EE0,COV,CHI3,IERR)
          NT=NT+1
          IW(KROW(KT1FT,I1)+JT1FTG) = NT
          IW(KROW(KT1FT,I2)+JT1FTG) = NT
          ITGFT=KROW(KTGFT,NT)
          CALL UCOPY(VV0,RW(ITGFT+JTGFIR),5)
          CALL UCOPY(COV,RW(ITGFT+JTGFEM),15)
          RW(ITGFT+JTGFCD)=CHI3
          IW(ITGFT+JTGFDF)=2*NTPC-5
          IF(NRRR.EQ.0) THEN
            IW(ITGFT+JTGFTC)=0
          ELSE
            IW(ITGFT+JTGFTC)=ITABL(KT1FT,NRRR,JT1FTC)
          ENDIF
          ITGTL=KROW(KTGTL,NT)
          IW(ITGTL+JTGTIO)=NCL
          IW(ITGTL+JTGTN1)=NTPC
          NRT=NRT1+NRT2
          IW(ITGTL+JTGTNR)=NRT
          CALL UCOPY(LIST1,IW(KTGCL+LMHLEN+NCL+1),NTPC)
          IF(NRRR.NE.0) THEN
            KP=ITABL(KT1TL,NRRR,JTGTIO)
            N11=ITABL(KT1TL,NRRR,JTGTN1)
            NR1=ITABL(KT1TL,NRRR,JTGTNR)
            CALL UCOPY(IW(KT1CL+LMHLEN+KP+N11+1),
     +                       IW(KTGCL+LMHLEN+NCL+NTPC+1),NR1)
          ENDIF
          NCL=NCL+NTPC+NRT
          GOTO 567
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
