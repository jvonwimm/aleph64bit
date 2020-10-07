      SUBROUTINE FPIDEN
C------------------------------------------------------------------
C! Fills bank FRID containing bit patterns for tracks and calorimeters
C! and particle ID.
C!             Author: Lluis Garrido  (1988)
C!             Modified: R. Johnson  19-6-90
C!   Modified :- E. Lancon             26-MAY-1993
C!        Replace explicit link to FRFT 0 by NAMIND
C!              - F.Ranjard              1-DEC-1995
C!         call TPDHYP('WIRE', instead of TIDHYP
C!              - M.Cattaneo            31-MAY-1996
C!         Bugfix: TPC bits in NDZ start at 10, not 8
C!         Remove duplicate call to NAMIND('FRFT')
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "fridjj.h"
C-----------------------------------------------------------------------
#include "tpgpar.h"
#include "itcinf.h"
#include "rconds.h"
C-----------------------------------------------------------------------
C
      DIMENSION IOUT(32)
      DIMENSION XMASS(5),Q(5),SIGMA(5),P(5),RIEXP(5)
      DIMENSION IV(LTPDRO),IFLG(LTPDRO)
      LOGICAL FIRST
      INTEGER  UFITQL
      EXTERNAL UFITQL
      DATA XMASS/.00051,.1057,.1395,.4937,.9383/
      DATA Q/1.,1.,1.,1.,1./
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      KFRFT=IW( NAMIND('FRFT') )
      IF(KFRFT.EQ.0) RETURN
      NTRI=LROWS(KFRFT)
      IF(NTRI.EQ.0) RETURN
C
C     CREATING BANK FRID
C
C
      IF (IW(NAMIND('FRID')).NE.0) CALL BDROP(IW,'FRID')
      CALL AUBOS('FRID',0,LMHLEN+LFRIDA*NTRI,KFRID,I1)
      IF(I1.EQ.2) RETURN
      IW(KFRID+LMHCOL)=LFRIDA
      IW(KFRID+LMHROW)=NTRI
      KFRTL=NLINK('FRTL',0)
      KITFT=NLINK('ITFT',2)
      KFTCL=NLINK('FTCL',0)
      KFICL=NLINK('FICL',0)
      KFVCL=NLINK('FVCL',0)
      KTPCO=NLINK('TPCO',0)
      KITCO=NLINK('ITCO',0)
      KVDCO=NLINK('VDCO',0)
      KRUNH=NLINK('RUNH',0)
      KHMAD=NLINK('HMAD',0)
      IRUN=IW(KRUNH+2)
      NDOS=LROWS(KITFT)
      NUNO=NTRI-NDOS
C
C     LOOP OVER TRACKS
C
      DO 1 I=1,NTRI
        NBP=0
        NDZ=0
        NBC=0
        NDC=0
        NPV=ITABL(KFRTL,I,2)
        NPI=ITABL(KFRTL,I,4)
        NPT=ITABL(KFRTL,I,7)
        KP3=ITABL(KFRTL,I,1)
        KP2=ITABL(KFRTL,I,3)
        KP1=ITABL(KFRTL,I,6)
C
C     CALCULATING BIT PATTER FOR TRACKS IN VDET, ITC AND TPC
C
        DO 11 J=1,NPT
          NTPCO=IW(KFTCL+LMHLEN+KP1+J)
          NPAD=ITABL(KTPCO,NTPCO,1)/100000+10
          IF(NPAD.GT.10.AND.NPAD.LT.33) CALL SBIT1(NBP,NPAD)
   11   CONTINUE
        DO 12 J=1,NPI
          NITCO=ABS(IW(KFICL+LMHLEN+KP2+J))
          NPAD=ITABL(KITCO,NITCO,1)/1000+2
          IF(NPAD.GT.2.AND.NPAD.LT.11) CALL SBIT1(NBP,NPAD)
   12   CONTINUE
        DO 13 J=1,NPV
          NVDCO=IW(KFVCL+LMHLEN+KP3+J)
          R=RTABL(KVDCO,NVDCO,2)
          IF(R.LT.10.) THEN
            NPAD=1
          ELSE
            NPAD=2
          ENDIF
          IF(NPAD.GT.0.AND.NPAD.LT.3) CALL SBIT1(NBP,NPAD)
   13   CONTINUE
        IF(I.GT.NUNO) THEN
          ITRK=I-NUNO
          ITCO=1
        ELSE
          ITRK=I
          ITCO=0
        ENDIF

C  Micro Vertex Inefficent Hit Section
        CALL  VCKTRK(ITRK,ITCO,IHIT)
        IF(IHIT.GT.0) THEN
          CALL SBIT1(NDZ,1)
          IF(IHIT.GT.1) CALL SBIT1(NDZ,2)
        ENDIF

C  ITC Inefficent Hit Section
        IF(NTKITC.GT.0) THEN
          ITKC = 0
          DO 451 I1 = 1, NTKITC
            IF(ITKITC(I1).GT.I) GOTO 452
            IF(ITKITC(I1).EQ.I) THEN
              ITKC = I1
            ENDIF
  451     CONTINUE
  452     IF(ITKC.GT.0) THEN
            DO 455 JJ = 1,8
              IF(FLGITC(JJ,ITKC).EQ.3) CALL SBIT1(NDZ,JJ+2)
  455       CONTINUE
          ENDIF
        ENDIF

C  TPC Inefficent Hit Section
        CALL THTROW(RW(KROW(KFRFT,I)+1),IFLG)
        CALL THTFLG(I,IV)
        DO 456 JJ=1,LTPDRO
          IF(IV(JJ).EQ.1 .AND. IFLG(JJ).NE.1) CALL SBIT1(NDZ,JJ+10)
  456   CONTINUE
C
C     Calculating bit pattern for tracks in ecal and hcal
C
        IE=0
        IT=0
        IF(KHMAD.NE.0) THEN
          NH  =LROWS(KHMAD)
          DO 67 JJ=1,NH
            ITRA= ITABL(KHMAD,JJ,12)
            IF(ITRA.EQ.I) THEN
              IE  = ITABL(KHMAD,JJ,9)
              IT  = ITABL(KHMAD,JJ,10)
              GOTO 68
            ENDIF
   67     CONTINUE
        ENDIF
   68   NBC=IT*8
        NDC=IE*8
        CALL EPATRN(I,I1,I2,I3,I4,I5,I6)
        IF(I1.EQ.1) CALL SBIT1(NBC,1)
        IF(I2.EQ.1) CALL SBIT1(NBC,2)
        IF(I3.EQ.1) CALL SBIT1(NBC,3)
        IF(I4.EQ.1) CALL SBIT1(NDC,1)
        IF(I5.EQ.1) CALL SBIT1(NDC,2)
        IF(I6.EQ.1) CALL SBIT1(NDC,3)
        IFRID=KROW(KFRID,I)
        IW(IFRID+JFRIBP)=NBP
        IW(IFRID+JFRIDZ)=NDZ
        IW(IFRID+JFRIBC)=NBC
        IW(IFRID+JFRIDC)=NDC
C
C     CALCUL OF PROB.
C
        CALL EPROBA(I,IFLAG)
        IF(IFLAG.EQ.1) THEN
          P(1)=1.
          DO 175 J=2,5
            P(J)=0.
  175     CONTINUE
          GOTO 180
        ENDIF
        DO 176 J=1,5
          P(J)=1./5.
  176   CONTINUE
        CALL TPDHYP('WIRE',I,FIELRC,5,XMASS,Q,RI,XNS,TL,RIEXP,
     &              SIGMA,IER)
        IF(IER.GT.0) GOTO 178
        DO 177 J=1,5
          CHI2=(RI-RIEXP(J))**2/SIGMA(J)**2
          IF(CHI2.LT.200..AND.SIGMA(J).GT.0.) THEN
            P(J)=EXP(-CHI2/2.)/SIGMA(J)
          ELSE
            P(J)=0.
          ENDIF
  177   CONTINUE
  178   CALL FMUID(I,PRPI,PRK,PRMU,IFLAG)
        IF(IFLAG.EQ.0) THEN
          P(1)=0.
          P(2)=P(2)*PRMU
          P(3)=P(3)*PRPI
          P(4)=P(4)*PRK
          P(5)=0.
        ENDIF
  180   CONTINUE
        PSUM=0.
        DO 331 II=1,5
          PSUM=PSUM+P(II)
  331   CONTINUE
        IF(PSUM.EQ.0.) PSUM=1.
        DO 179 J=1,5
          RW(IFRID+JFRIPE-1+J)=P(J)/PSUM
  179   CONTINUE
C
C     Search for kinks in tracks with p > 1.5 GeV
C
        RHO = 1./ABS(RTABL(KFRFT,I,1))
        PFIT = RHO*SQRT(RTABL(KFRFT,I,2)**2+1)*0.000299979*FIELRC
C       WRITE(6,*) 'PFIT =',PFIT
        RW(IFRID+JFRINK)=1.
        IF (PFIT.GT.1.5) THEN
          CALL FKINK(I,PRONK,IFAIL)
          IF (IFAIL.EQ.0) RW(IFRID+JFRINK)=PRONK
        ENDIF
C
C Fill the track quality flag
C
        IQAL = UFITQL(I)
        IW(IFRID+JFRIQF) = IQAL
    1 CONTINUE
      RETURN
      END

#endif
