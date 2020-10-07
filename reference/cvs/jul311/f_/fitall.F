      SUBROUTINE FITALL(NROUT,LKAL)
C-----------------------------------------------------------------------
C!  Global fit of VDET, ITC and TPC
C
C  Input:   NROUT     /I       NR of the output FRFT bank
C                              0= ITC + TPC fit
C                              1= ITC + TPC + VDET
C           LKAL      /L       .TRUE. to use KALMAN filter
C
C   Author         L. Garrido    10-may-1988
C   Modified       R. Johnson    12-dec-1989 to use KALMAN filter
C   Modified       R. Johnson    21-Jan-1991 to add NROUT and LKAL
C                                arguments
C                  D. Casper     04-Dec-1995 to remove filtered coords
C                                from coordinate lists.
C                  F.Ranjard     19-Feb-1997 to call FDETCO,FRMHIT
C?
C!======================================================================
#ifndef DOC
#include "igtljj.h"
#include "vgtljj.h"
#include "tgtljj.h"
#include "tgftjj.h"
#include "tpcojj.h"
#include "itcojj.h"
#include "vdcojj.h"
#include "ficljj.h"
#include "frftjj.h"
#include "frtljj.h"
#include "ftcljj.h"
#include "fvcljj.h"
#include "trikjj.h"
C
#include "bcs.h"
#include "rconds.h"
C
      INTEGER IRET
      PARAMETER (MPT = 40)
      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      INTEGER SORTARR(MPT)
      INTEGER LISTRM(MPT)
C
      DIMENSION VV0(6),EE0(21),COV(21)
      LOGICAL LKAL
      DIMENSION X_ECAL(8),E_ECAL(2)
      INTEGER UFECAL,UFTTRA,UFDALI
      EXTERNAL UFECAL,UFTTRA,UFDALI
      DATA NENKF/0/
C
#include "bmacro.h"
C
      IF (NENKF.EQ.0) NENKF = NAMIND('ENKF')
      KENKF = IW(NENKF)
C
C     Dropping banks to be created
C
      IND = NDROP('FRFT',NROUT)
      IND = NDROP('FRTL',0)
      IND = NDROP('FTCL',0)
      IND = NDROP('FICL',0)
      IND = NDROP('FVCL',0)
C
C     checking if ITC reconstructed
C
      KITF1 = NLINK('ITFT',1)
      KITF2 = NLINK('ITFT',2)
      IF (KITF1.NE.0) THEN
        NR1 = LROWS(KITF1)
      ELSE
        NR1 = 0
      ENDIF
      IF (KITF2.NE.0) THEN
        NR2 = LROWS(KITF2)
      ELSE
        NR2 = 0
      ENDIF
      NTOT = NR1 + NR2
      IF(NTOT.EQ.0) THEN
C************************************************************
C     ITC NOT RECONSTRUCTED. BANKS FILLED USING TPC AND VDET
C************************************************************
C
C     creating banks FRFT FRTL FTCL FVCL
C
        KTGFT = NLINK('TGFT',1)
        IF (KTGFT.NE.0) THEN
          NTGFT = LROWS(KTGFT)
        ELSE
          NTGFT= 0
        ENDIF
        CALL AUBOS('FRFT',NROUT,LMHLEN+LFRFTA*NTGFT,KFRFT,IGAR1)
        IF (IGAR1.EQ.2) GOTO 999
        CALL AUBOS('FRTL',0,LMHLEN+LFRTLA*NTGFT,KFRTL,IGAR2)
        IF (IGAR2.EQ.2) GOTO 999
        KTGCL = NLINK('TGCL',1)
        IF (KTGCL.NE.0) THEN
          NTGCL = LROWS(KTGCL)
        ELSE
          NTGCL = 0
        ENDIF
        CALL AUBOS('FTCL',0,LMHLEN+LFTCLA*NTGCL,KFTCL,IGARB)
        IF (IGARB.EQ.2) GOTO 999
        NIGCL=0
        CALL AUBOS('FICL',0,LMHLEN+LFICLA*NIGCL,KFICL,IGARB)
        IF (IGARB.EQ.2) GOTO 999
        KVGCL = NLINK('VGCL',1)
        IF (KVGCL.NE.0) THEN
          NVGCL = LROWS(KVGCL)
        ELSE
          NVGCL = 0
        ENDIF
        CALL AUBOS('FVCL',0,LMHLEN+LFVCLA*NVGCL,KFVCL,IGARB)
        IF (IGARB.EQ.2) GOTO 999
C
C     POINTERS
C
        KFRFT  = NLINK('FRFT',NROUT)
        KFRTL  = NLINK('FRTL',0)
        KFTCL  = NLINK('FTCL',0)
        KFICL  = NLINK('FICL',0)
        KFVCL  = NLINK('FVCL',0)
        KTGFT  = NLINK('TGFT',1)
        KTGCL  = NLINK('TGCL',1)
        KTGTL  = NLINK('TGTL',1)
        KVGTL  = NLINK('VGTL',1)
        KVGCL  = NLINK('VGCL',1)
        KTPCO  = NLINK('TPCO',0)
        KVDCO  = NLINK('VDCO',0)
        KITCO  = NLINK('ITCO',0)
C
C     FILLING BANKS FTCL VTCL
C
        IW(KFTCL+LMHCOL) = LFTCLA
        IW(KFTCL+LMHROW) = NTGCL
        IF(NTGCL.NE.0) THEN
          IFTCL  = KROW(KFTCL,1)
          ITGCL  = KROW(KTGCL,1)
          CALL UCOPY(IW(ITGCL+1),IW(IFTCL+1),LCOLS(KTGCL)*LROWS(KTGCL))
        ENDIF
        IW(KFICL+LMHCOL) = LFICLA
        IW(KFICL+LMHROW) = 0
        IW(KFVCL+LMHCOL) = LFVCLA
        IW(KFVCL+LMHROW) = NVGCL
        IF(NVGCL.NE.0) THEN
          IFVCL  = KROW(KFVCL,1)
          IVGCL  = KROW(KVGCL,1)
          CALL UCOPY(IW(IVGCL+1),IW(IFVCL+1),LCOLS(KVGCL)*LROWS(KVGCL))
        ENDIF
C
C     FILLING BANK FRTL
C
        IW(KFRTL+LMHCOL) = LFRTLA
        IW(KFRTL+LMHROW) = NTGFT
        DO 12 J=1,NTGFT
          IFRTL=KROW(KFRTL,J)
          IF(KVGTL.NE.0) THEN
            IW(IFRTL+JFRTIV)=ITABL(KVGTL,J,JVGTIO)
            IW(IFRTL+JFRTNV)=ITABL(KVGTL,J,JVGTNA)
          ELSE
            IW(IFRTL+JFRTIV)=0
            IW(IFRTL+JFRTNV)=0
          ENDIF
          IW(IFRTL+JFRTII)=0
          IW(IFRTL+JFRTNI)=0
          IW(IFRTL+JFRTNE)=0
          IF(KTGTL.NE.0) THEN
            IW(IFRTL+JFRTIT)=ITABL(KTGTL,J,JTGTIO)
            IW(IFRTL+JFRTNT)=ITABL(KTGTL,J,JTGTN1)
            IW(IFRTL+JFRTNR)=ITABL(KTGTL,J,JTGTNR)
          ELSE
            IW(IFRTL+JFRTIT)=0
            IW(IFRTL+JFRTNT)=0
            IW(IFRTL+JFRTNR)=0
          ENDIF
12      CONTINUE
C
C       Refit using KALMAN filter.  Use TGFT track for starting values
C
        IW(KFRFT+LMHCOL) = LFRFTA
        IW(KFRFT+LMHROW) = NTGFT
        DO 13 J=1,NTGFT
          NVDET=ITABL(KFRTL,J,JFRTNV)
          NTPC =ITABL(KFRTL,J,JFRTNT)
          NPVDT=ITABL(KFRTL,J,JFRTIV)
          NPTPC=ITABL(KFRTL,J,JFRTIT)
          IF (LKAL) THEN
            ICODE = UFTTRA(J,FIELRC,RW(KROW(KTGFT,J)+JTGFIR),
     +                  RTABL(KTGFT,J,JTGFCD),NTPC,0,NVDET,
     +                  IW(KFTCL+LMHLEN+NPTPC+1),IDUM,
     +                  IW(KFVCL+LMHLEN+NPVDT+1),
     +                  VV0,COV,CHI2,NDEG)
C
C           If the fit fails, then just keep the old TPC fit
C
            IF (CHI2.GT.1.E10 .OR. ICODE.NE.0) THEN
              IERR=4
              CALL UCOPY(RW(KROW(KTGFT,J)+JTGFEM),COV,15)
              CALL VZERO(COV(16),6)
              CHI2=RTABL(KTGFT,J,JTGFCD)
              NDEG=ITABL(KTGFT,J,JTGFDF)
              CALL RERROR('UFTKAL',ICODE,'Kalman Filter error')
            ELSE
              IF (KENKF.EQ.0) THEN
                IERRU = UFECAL(X_ECAL,E_ECAL)
                IF (IERRU.EQ.0)THEN
                    CALL AUBOS('TRIK',J,LMHLEN+LTRIKA,KTRIK,IGARB)
                    IF (IGARB.NE.0) THEN
                     CALL RERROR('FITALL',-1,'Insufficient BOS space')
                     CALL BDROP(IW,'TRIK')
                     RETURN
                    ENDIF
                    IW(KTRIK+LMHCOL) = LTRIKA
                    IW(KTRIK+LMHROW) = 1
                    CALL UCOPY(X_ECAL,RW(KTRIK+LMHLEN+JTRIX3),8)
                    RW(KTRIK+LMHLEN+JTRISR) = E_ECAL(1)
                    RW(KTRIK+LMHLEN+JTRISZ) = E_ECAL(2)
                ENDIF
              ENDIF
              IRET = UFDALI(NLOW,NHIGH,XSTS,XME,RF,IUSED,VMEAS)
C             CALL UFSORT(SORTARR)
              IERR=0
              NRM = 0
              DO I = 1, NTPC
                IF(IUSED(I).EQ.0 .OR. IUSED(I).EQ.-1)THEN
                  CALL FDETCO('TP',SNGL(RF(I)),J,ITPCO)
                  IF (ITPCO .NE. 0)THEN
                    NRM = NRM+1
                    LISTRM(NRM) = ITPCO
                  ELSE
                    CALL RERROR('FITALL',2,'TPC hit not found on list')
                  ENDIF
                ENDIF
              ENDDO
              IF (NRM.GT.0) THEN
               CALL FRMHIT('TP',J,NRM,LISTRM)
              ENDIF
              NRM = 0
              DO I = NTPC+1, NTPC+NVDET
                IF(IUSED(I).EQ.0 .OR. IUSED(I).EQ.-1)THEN
                  CALL FDETCO('VD',SNGL(RF(I)),J,IVDCO)
                  IF (IVDCO .NE. 0)THEN
                    NRM = NRM+1
                    LISTRM(NRM) = IVDCO
                  ELSE
                    CALL RERROR('FITALL',3,'VDET hit not found on list')
                  ENDIF
                ENDIF
              ENDDO
              IF (NRM.GT.0) THEN
                CALL FRMHIT('VD',J,NRM,LISTRM)
              ENDIF
            ENDIF
            IOPT=9
            NMULT=0
          ELSE
C
C           Old circle fit chosen
C
            IF (NVDET.GT.0) THEN
              IOPT=4
            ELSE
              IOPT=2
            ENDIF
            NMULT=0
            CALL UFITMS(NTPC,0,NVDET,IW(KFTCL+LMHLEN+NPTPC+1),
     +             IDUM,IW(KFVCL+LMHLEN+NPVDT+1),
     +             IOPT,NMULT,29.6,.027,VV0,EE0,COV,CHI2,IERR)
            NVIZ=0
            DO 333 J2=1,NVDET
              IC=IW(KFVCL+LMHLEN+NPVDT+J2)
              IF (RTABL(KVDCO,IC,JVDCSZ).GT.4.) NVIZ=NVIZ+1
              IF (RTABL(KVDCO,IC,JVDCSR).GT.4.) NVIZ=NVIZ+1
  333       CONTINUE
            NDEG= 2*(NTPC+NVDET)-5-NVIZ
          ENDIF
          IFRFT=KROW(KFRFT,J)
          DO 41 K=JFRFIR,JFRFAL
            RW(IFRFT+K)=VV0(K)
   41     CONTINUE
          DO 42 K=1,21
            RW(IFRFT+JFRFEM-1+K)=COV(K)
   42     CONTINUE
          RW(IFRFT+JFRFC2)=CHI2
          IW(IFRFT+JFRFDF)=NDEG
          IW(IFRFT+JFRFNO)=NMULT*100+IOPT*10+IERR
          IW(KROW(KTGFT,J)+JTGFTC) = J
   13   CONTINUE
        CALL BLIST(IW,'E+','TRIK')
        CALL AUBPRS('FTCLFICLFVCL')
        RETURN
      ENDIF
C************************************************************
C     ITC RECONSTRUCTED. BANKS FILLED USING TPC ITC AND VDET
C************************************************************
C
C     CREATING BANKS FRFT AND FRTL
C
      CALL AUBOS('FRFT',NROUT,LMHLEN+LFRFTA*NTOT,KFRFT,IGAR1)
      CALL AUBOS('FRTL',0,LMHLEN+LFRTLA*NTOT,KFRTL,IGAR2)
      IF (IGAR1.EQ.2.OR.IGAR2.EQ.2) GOTO 999
C
C     CREATING BANK FTCL
C
      KTGC1 = NLINK('TGCL',1)
      KTGC2 = NLINK('TGCL',2)
      IF (KTGC1.NE.0) THEN
        NR1 = LROWS(KTGC1)
      ELSE
        NR1 = 0
      ENDIF
      IF (KTGC2.NE.0) THEN
        NR2 = LROWS(KTGC2)
      ELSE
        NR2 = 0
      ENDIF
      NTOT = NR1 + NR2
      CALL AUBOS('FTCL',0,LMHLEN+LFTCLA*NTOT,KFTCL,IGARB)
      IF (IGARB.EQ.2) GOTO 999
C
C     CREATING BANK FICL
C
      KIGC1 = NLINK('IGCL',1)
      KIGC2 = NLINK('IGCL',2)
      IF (KIGC1.NE.0) THEN
        NR1 = LROWS(KIGC1)
      ELSE
        NR1 = 0
      ENDIF
      IF (KIGC2.NE.0) THEN
        NR2 = LROWS(KIGC2)
      ELSE
        NR2 = 0
      ENDIF
      NTOT = NR1 + NR2
      CALL AUBOS('FICL',0,LMHLEN+LFICLA*NTOT,KFICL,IGARB)
      IF (IGARB.EQ.2) GOTO 999
C
C     CREATING BANK FVCL
C
      KVGC1 = NLINK('VGCL',1)
      KVGC2 = NLINK('VGCL',2)
      IF (KVGC1.NE.0) THEN
        NR1 = LROWS(KVGC1)
      ELSE
        NR1 = 0
      ENDIF
      IF (KVGC2.NE.0) THEN
        NR2 = LROWS(KVGC2)
      ELSE
        NR2 = 0
      ENDIF
      NTOT = NR1 + NR2
      CALL AUBOS('FVCL',0,LMHLEN+LFVCLA*NTOT,KFVCL,IGARB)
      IF (IGARB.EQ.2) GOTO 999
C
C     POINTERS
C
      KFRFT = NLINK('FRFT',NROUT)
      KITF1 = NLINK('ITFT',1)
      KITF2 = NLINK('ITFT',2)
      KFRTL = NLINK('FRTL',0)
      KTGT1 = NLINK('TGTL',1)
      KTGT2 = NLINK('TGTL',2)
      KIGT1 = NLINK('IGTL',1)
      KIGT2 = NLINK('IGTL',2)
      KVGT1 = NLINK('VGTL',1)
      KVGT2 = NLINK('VGTL',2)
      KFTCL = NLINK('FTCL',0)
      KTGC1 = NLINK('TGCL',1)
      KTGC2 = NLINK('TGCL',2)
      KFICL = NLINK('FICL',0)
      KIGC1 = NLINK('IGCL',1)
      KIGC2 = NLINK('IGCL',2)
      KFVCL = NLINK('FVCL',0)
      KVGC1 = NLINK('VGCL',1)
      KVGC2 = NLINK('VGCL',2)
      KITCO = NLINK('ITCO',0)
      KVDCO = NLINK('VDCO',0)
      KTGFT = NLINK('TGFT',1)
C
C     FILLING FTCL
C
      IF (KTGC1.NE.0) THEN
        NR1 = LROWS(KTGC1)
      ELSE
        NR1 = 0
      ENDIF
      IF (KTGC2.NE.0) THEN
        NR2 = LROWS(KTGC2)
      ELSE
        NR2 = 0
      ENDIF
      NTOT = NR1 + NR2
      IW(KFTCL+LMHCOL) = LFTCLA
      IW(KFTCL+LMHROW) = NTOT
      IF(NR1.GT.0) THEN
        IFTCL  = KROW(KFTCL,1)
        ITGC1 = KROW(KTGC1,1)
        CALL UCOPY(IW(ITGC1+1),IW(IFTCL+1),LCOLS(KTGC1)*LROWS(KTGC1))
      ENDIF
      IF(NR2.GT.0) THEN
         IFTCL = KROW(KFTCL,NR1+1)
         ITGC2 = KROW(KTGC2,1)
         CALL UCOPY(IW(ITGC2+1),IW(IFTCL+1),LCOLS(KTGC2)*LROWS(KTGC2))
      ENDIF
C
C     FILLING FICL
C
      IF (KIGC1.NE.0) THEN
        NR1 = LROWS(KIGC1)
      ELSE
        NR1 = 0
      ENDIF
      IF (KIGC2.NE.0) THEN
        NR2 = LROWS(KIGC2)
      ELSE
        NR2 = 0
      ENDIF
      NTOT = NR1 + NR2
      IW(KFICL+LMHCOL) = LFICLA
      IW(KFICL+LMHROW) = NTOT
      IF(NR1.GT.0) THEN
        IFICL  = KROW(KFICL,1)
        IIGC1 = KROW(KIGC1,1)
        CALL UCOPY(IW(IIGC1+1),IW(IFICL+1),LCOLS(KIGC1)*LROWS(KIGC1))
      ENDIF
      IF(NR2.GT.0) THEN
         IFICL = KROW(KFICL,NR1+1)
         IIGC2 = KROW(KIGC2,1)
         CALL UCOPY(IW(IIGC2+1),IW(IFICL+1),LCOLS(KIGC2)*LROWS(KIGC2))
      ENDIF
C
C     FILLING FVCL
C
      IF (KVGC1.NE.0) THEN
        NR1 = LROWS(KVGC1)
      ELSE
        NR1 = 0
      ENDIF
      IF (KVGC2.NE.0) THEN
        NR2 = LROWS(KVGC2)
      ELSE
        NR2 = 0
      ENDIF
      NTOT = NR1 + NR2
      IW(KFVCL+LMHCOL) = LFVCLA
      IW(KFVCL+LMHROW) = NTOT
      IF(NR1.GT.0) THEN
        IFVCL  = KROW(KFVCL,1)
        IVGC1 = KROW(KVGC1,1)
        CALL UCOPY(IW(IVGC1+1),IW(IFVCL+1),LCOLS(KVGC1)*LROWS(KVGC1))
      ENDIF
      IF(NR2.GT.0) THEN
         IFVCL = KROW(KFVCL,NR1+1)
         IVGC2 = KROW(KVGC2,1)
         CALL UCOPY(IW(IVGC2+1),IW(IFVCL+1),LCOLS(KVGC2)*LROWS(KVGC2))
      ENDIF
C
C     FILLING FRFT
C
      IF (KITF1.NE.0) THEN
        NR1 = LROWS(KITF1)
      ELSE
        NR1 = 0
      ENDIF
      IF (KITF2.NE.0) THEN
        NR2 = LROWS(KITF2)
      ELSE
        NR2 = 0
      ENDIF
      NTOT = NR1 + NR2
      IW(KFRFT+LMHCOL) = LFRFTA
      IW(KFRFT+LMHROW) = NTOT
      IF(NR1.GT.0) THEN
        IFRFT = KROW(KFRFT,1)
        IITF1 = KROW(KITF1,1)
        CALL UCOPY(IW(IITF1+1),IW(IFRFT+1),LCOLS(KITF1)*LROWS(KITF1))
      ENDIF
      IF(NR2.GT.0) THEN
         IFRFT = KROW(KFRFT,NR1+1)
         IITF2 = KROW(KITF2,1)
         CALL UCOPY(IW(IITF2+1),IW(IFRFT+1),LCOLS(KITF2)*LROWS(KITF2))
      ENDIF
C
C     FILLING FRTL
C
      IW(KFRTL+LMHCOL) = LFRTLA
      IW(KFRTL+LMHROW) = NTOT
      DO 1 J=1,NR1
        IFRTL=KROW(KFRTL,J)
        IF(KVGT1.NE.0) THEN
          IW(IFRTL+JFRTIV)=ITABL(KVGT1,J,JVGTIO)
          IW(IFRTL+JFRTNV)=ITABL(KVGT1,J,JVGTNA)
        ELSE
          IW(IFRTL+JFRTIV)=0
          IW(IFRTL+JFRTNV)=0
        ENDIF
        IF(KIGT1.NE.0) THEN
          IW(IFRTL+JFRTII)=ITABL(KIGT1,J,JIGTIO)
          IW(IFRTL+JFRTNI)=ITABL(KIGT1,J,JIGTN1)
          IW(IFRTL+JFRTNE)=ITABL(KIGT1,J,JIGTNR)
        ELSE
          IW(IFRTL+JFRTII)=0
          IW(IFRTL+JFRTNI)=0
          IW(IFRTL+JFRTNE)=0
        ENDIF
        IF(KTGT1.NE.0) THEN
          IW(IFRTL+JFRTIT)=ITABL(KTGT1,J,JTGTIO)
          IW(IFRTL+JFRTNT)=ITABL(KTGT1,J,JTGTN1)
          IW(IFRTL+JFRTNR)=ITABL(KTGT1,J,JTGTNR)
        ELSE
          IW(IFRTL+JFRTIT)=0
          IW(IFRTL+JFRTNT)=0
          IW(IFRTL+JFRTNR)=0
        ENDIF
1     CONTINUE
      NADDV=LROWS(KVGC1)
      NADDI=LROWS(KIGC1)
      NADDT=LROWS(KTGC1)
      DO 2 J=1,NR2
        IFRTL=KROW(KFRTL,J+NR1)
        IF(KVGT2.NE.0) THEN
          IW(IFRTL+JFRTIV)=ITABL(KVGT2,J,JVGTIO)+NADDV
          IW(IFRTL+JFRTNV)=ITABL(KVGT2,J,JVGTNA)
        ELSE
          IW(IFRTL+JFRTIV)=0
          IW(IFRTL+JFRTNV)=0
        ENDIF
        IF(KIGT2.NE.0) THEN
          IW(IFRTL+JFRTII)=ITABL(KIGT2,J,JIGTIO)+NADDI
          IW(IFRTL+JFRTNI)=ITABL(KIGT2,J,JIGTN1)
          IW(IFRTL+JFRTNE)=ITABL(KIGT2,J,JIGTNR)
        ELSE
          IW(IFRTL+JFRTII)=0
          IW(IFRTL+JFRTNI)=0
          IW(IFRTL+JFRTNE)=0
        ENDIF
        IF(KTGT2.NE.0) THEN
          IW(IFRTL+JFRTIT)=ITABL(KTGT2,J,JTGTIO)+NADDT
          IW(IFRTL+JFRTNT)=ITABL(KTGT2,J,JTGTN1)
          IW(IFRTL+JFRTNR)=ITABL(KTGT2,J,JTGTNR)
        ELSE
          IW(IFRTL+JFRTIT)=0
          IW(IFRTL+JFRTNT)=0
          IW(IFRTL+JFRTNR)=0
        ENDIF
    2 CONTINUE
C
C    If there are points in the TPC, do full fit using the
C    Kalman filter.  If there are ITC plus VDET points, then
C    just update the old ITC fit with the VDET.
C
      DO 3 J=1,NTOT
        NTPC=ITABL(KFRTL,J,JFRTNT)
        NITC=ITABL(KFRTL,J,JFRTNI)
        NVDET=ITABL(KFRTL,J,JFRTNV)
        IFRFT=KROW(KFRFT,J)
C
        IF (NVDET+NTPC.EQ.0) GOTO 3
        NPVDT=ITABL(KFRTL,J,JFRTIV)
        NPITC=ITABL(KFRTL,J,JFRTII)
        NPTPC=ITABL(KFRTL,J,JFRTIT)
        IF (NTPC.GT.0 .AND. LKAL) THEN
          ICODE = UFTTRA(J,FIELRC,RW(KROW(KFRFT,J)+JFRFIR),
     +           RTABL(KFRFT,J,JFRFC2),
     +           NTPC,NITC,NVDET,IW(KFTCL+LMHLEN+NPTPC+1),
     +           IW(KFICL+LMHLEN+NPITC+1),
     +           IW(KFVCL+LMHLEN+NPVDT+1),VV0,COV,CHI2,NDEG)
          IF (CHI2.GT.1.E10 .OR. ICODE.NE.0) THEN
            IERR=4
            CALL RERROR('UFTKAL',ICODE,'Kalman Filter error')
          ELSE
            IF (KENKF.EQ.0)THEN
                IERRU = UFECAL(X_ECAL,E_ECAL)
                IF (IERRU .EQ. 0) THEN
                    CALL AUBOS('TRIK',J,LMHLEN+LTRIKA,KTRIK,IGARB)
                    IF (IGARB.NE.0) THEN
                     CALL RERROR('FITALL',-1,'Insufficient BOS space')
                     CALL BDROP(IW,'TRIK')
                     RETURN
                    ENDIF
                    IW(KTRIK+LMHCOL) = LTRIKA
                    IW(KTRIK+LMHROW) = 1
                    CALL UCOPY(X_ECAL,RW(KTRIK+LMHROW+JTRIX3),8)
                    RW(KTRIK+LMHLEN+JTRISR) = E_ECAL(1)
                    RW(KTRIK+LMHLEN+JTRISZ) = E_ECAL(2)
                ENDIF
            ENDIF
            IRET = UFDALI(NLOW,NHIGH,XSTS,XME,RF,IUSED,VMEAS)
C           CALL UFSORT(SORTARR)
            IERR=0
            IW(IFRFT+JFRFDF)=NDEG
            NRM = 0
            DO I = 1, NTPC
              IF(IUSED(I).EQ.0 .OR. IUSED(I).EQ.-1)THEN
                CALL FDETCO('TP',SNGL(RF(I)),J,ITPCO)
                IF (ITPCO .NE. 0) THEN
                    NRM = NRM+1
                    LISTRM(NRM) = ITPCO
                ELSE
                    CALL RERROR('FITALL',2,'TPC hit not found on list')
                ENDIF
              ENDIF
            ENDDO
            IF (NRM.GT.0) THEN
              CALL FRMHIT('TP',J,NRM,LISTRM)
            ENDIF
            NRM = 0
            DO I = NTPC+1, NTPC+NITC
              IF(IUSED(I).EQ.0 .OR. IUSED(I).EQ.-1)THEN
                CALL FDETCO('IT',SNGL(RF(I)),J,IITCO)
                IF (IITCO.NE.0)THEN
                    NRM = NRM+1
                    LISTRM(NRM) = IITCO
                ELSE
                    CALL RERROR('FITALL',4,'ITC hit not found on list')
                ENDIF
              ENDIF
            ENDDO
            IF (NRM.GT.0) THEN
              CALL FRMHIT('IT',J,NRM,LISTRM)
            ENDIF
            NRM = 0
            DO I = NTPC+NITC+1, NTPC+NITC+NVDET
              IF(IUSED(I).EQ.0 .OR. IUSED(I).EQ.-1)THEN
                CALL FDETCO('VD',SNGL(RF(I)),J,IVDCO)
                IF (IVDCO .NE. 0)THEN
                    NRM = NRM+1
                    LISTRM(NRM) = IVDCO
                ELSE
                    CALL RERROR('FITALL',3,'VDET hit not found on list')
                ENDIF
              ENDIF
            ENDDO
            IF (NRM.GT.0) THEN
              CALL FRMHIT('VD',J,NRM,LISTRM)
            ENDIF
          ENDIF
          IW(IFRFT+JFRFNO)=90+IERR
        ELSE
          IF (NTPC.GT.0.AND.(NITC+NVDET.GT.0)) THEN
            IOPT=4
          ELSE
            IOPT=2
          ENDIF
          NMULT=0
          CALL UFITMS(NTPC,NITC,NVDET,IW(KFTCL+LMHLEN+NPTPC+1),
     +       IW(KFICL+LMHLEN+NPITC+1),IW(KFVCL+LMHLEN+NPVDT+1),
     +       IOPT,NMULT,29.6,.027,VV0,EE0,COV,CHI2,IERR)
          NCIZ=0
          DO 32 J2=1,NITC
            IC=IABS(IW(KFICL+LMHLEN+NPITC+J2))
            IF (RTABL(KITCO,IC,JITCSZ).GT.900.) NCIZ=NCIZ+1
   32     CONTINUE
          NVIZ=0
          DO 33 J2=1,NVDET
            IC=IW(KFVCL+LMHLEN+NPVDT+J2)
            IF (RTABL(KVDCO,IC,JVDCSZ).GT.4.) NVIZ=NVIZ+1
            IF (RTABL(KVDCO,IC,JVDCSR).GT.4.) NVIZ=NVIZ+1
   33     CONTINUE
          IF (NVDET.LT.2) THEN
            NSOFF=3
          ELSE
            NSOFF=5
          ENDIF
          IW(IFRFT+JFRFDF)=2*(NITC+NVDET)-NSOFF - NCIZ - NVIZ
          IW(IFRFT+JFRFNO)=NMULT*100+IOPT*10+IERR
        ENDIF
        IF (IERR.LT.4) THEN
          DO 741 K=1,6
            RW(IFRFT+K)=VV0(K)
  741     CONTINUE
          DO 742 K=1,21
            RW(IFRFT+6+K)=COV(K)
  742     CONTINUE
          RW(IFRFT+JFRFC2)=CHI2
        ENDIF
        IF (J.LE.LROWS(KTGFT))IW(KROW(KTGFT,J)+JTGFTC)=J
    3 CONTINUE
      CALL AUBPRS('FTCLFICLFVCL')
      CALL BLIST(IW,'E+','TRIK')
      RETURN
  999 CALL RERROR('FITALL',1,'No room to create named banks.')
      END
#endif
