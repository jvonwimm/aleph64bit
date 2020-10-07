      SUBROUTINE SILUMI
C----------------------------------------------------------------------
C!  - Apply Lumi cuts and compute Luminosity in SICAL
C!
C!
C!    Author:     John Rander ,B. Bloch-Devaux   10-Feb-1993
C!              Modified for Bunch train mode October 1994
C      Allow reprocessing POT to POT    October 1994   B. Bloch
C               modified to select only intime clusters in 95 BB Oct 95
C               modified to skip identified electronic noise  BB June 98
C               and any identified background                 BB April 99
C!
C!    Input:      - banks 'SPDA' ,'SCLS'             from Sical
C!                  from data base: 'SBHA'  'SIBX' to setup Lumi cuts
C!    Output:
C!                - banks 'SILU' , 'SLUM'
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "spdajj.h"
#include "sclsjj.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rparac.h"
#include "sinamc.h"
#include "sbhajj.h"
#include "sibxjj.h"
#include "silhjj.h"
#include "silujj.h"
#include "siidjj.h"
C   BC is mag field ( 1.5 T ) X speed of light (3 E08 m/sec)
C   BC2 IS BC/200.
      PARAMETER ( BC2 = 0.45/200. )
      INTEGER NEMAX(2),NEMX2(2),NPAD(2)
      REAL ECLU(2),ECLU2(2),EFRAC(12),DP(3),EVOI(45)
      DIMENSION SILCUT(10),ISIPTCU(8),ISIPLCU(8),SIBCUT(2)
      LOGICAL SOK,SICAOK,S95LEP1
      INTEGER SIRIT1,SIROT1,SIRIL1,SIROL1,SIRIT2,SIROT2,SIRIL2,SIROL2
      INTEGER SIGTWA,GTSTUP
      EXTERNAL SIGTWA,GTSTUP
      COMMON/SILUMC/NSILEV(25,4)
C==================================================================
C    LUMI CUTS
      DATA SIRIT1,SIROT1,SIRIL1,SIROL1/3,12,2,15/
      DATA SIRIT2,SIROT2,SIRIL2,SIROL2/4,12,2,15/
      DATA SILCUT/20.,20.,55.,0.0,0.0,0.0,0.0,150.,210.,0./
      DATA SIBCUT/ 0.4, 37. /
C ADD PHICUTS FOR METHODS 3,4,5,6  ... JOHN 4/02/93 ,BBL,BVA
      DATA ISIPTCU/ 5,12,21,28, 4,29,13,20/
      DATA ISIPLCU/ 1,16,17,32, 8,25, 9,24/
C==================================================================
      DATA KFIDSI/1/
      DATA IRUNO/0/
#include "bmacro.h"
C----------------------------------------------------------------------
      IF ( IRUNRC.NE.IRUNO) THEN
         IRUNO= IRUNRC
         S95LEP1 = ( GTSTUP('SI',IRUNRC).EQ.4 )
         KSBHA = IW(NASBHA)
         NASIID = NAMIND('SIID')
         IF ( KSBHA.GT.0) THEN
            SILCUT(1) = 0.5 * RENLEP * RTABL(KSBHA,2,JSBHES)
            SILCUT(2) = 0.5 * RENLEP * RTABL(KSBHA,2,JSBHES)
            SILCUT(3) = RENLEP * RTABL(KSBHA,2,JSBHET)
            SILCUT(4) = RTABL(KSBHA,1,JSBHAC)
            SILCUT(5) = RTABL(KSBHA,1,JSBHAC)
            SILCUT(6) = RTABL(KSBHA,1,JSBHAC)
            SILCUT(7) = RTABL(KSBHA,1,JSBHAC)
            SILCUT(8) = RTABL(KSBHA,2,JSBHDN)
            SILCUT(9) = RTABL(KSBHA,2,JSBHDX)
            SILCUT(10)= RTABL(KSBHA,2,JSBHAC)
            SIRIT1    = ITABL(KSBHA,1,JSBHTI)
            SIROT1    = ITABL(KSBHA,1,JSBHTO)
            SIRIL1    = ITABL(KSBHA,1,JSBHLI)
            SIROL1    = ITABL(KSBHA,1,JSBHLO)
            SIRIT2    = ITABL(KSBHA,2,JSBHTI)
            SIROT2    = ITABL(KSBHA,2,JSBHTO)
            SIRIL2    = ITABL(KSBHA,2,JSBHLI)
            SIROL2    = ITABL(KSBHA,2,JSBHLO)
            ISIPTCU(1) = ITABL(KSBHA,2,JSBHVN)
            ISIPTCU(2) = ITABL(KSBHA,2,JSBHVX)
            ISIPTCU(3) = ISIPTCU(1) + 16
            ISIPTCU(4) = ISIPTCU(2) + 16
            ISIPTCU(5) = ITABL(KSBHA,2,JSBHHN)
            ISIPTCU(6) = ITABL(KSBHA,2,JSBHHX)
            ISIPTCU(7) = MOD (ISIPTCU(6) + 16 ,32 )
            ISIPTCU(8) = ISIPTCU(5) + 16
C protection against wrong Energies  at LEP I
            IF ((ABS( SILCUT(1)-20.).GT.5.)
     $            .and.(irunrc.lt.40000)) THEN
               SILCUT(1) = 20.
               SILCUT(2) = 20.
               SILCUT(3) = 55.
               if ( irunrc.ge.25000) SILCUT(3) = 65.
               if ( irunrc.ge.35000) SILCUT(3) = 70.
            ENDIF
C define 'BOX' cut for 95 LEP1 data
            NASIBX = NAMIND('SIBX')
            KSIBX = IW(NASIBX)
            IF ( KSIBX.GT.0) THEN
               SIBCUT(1) = RTABL(KSIBX,1,JSIBDR)
               SIBCUT(2) = RTABL(KSIBX,1,JSIBEC)* RENLEP*0.5
            ENDIF
         ENDIF
         IF(JDBDRF(JULSI).GE.1) THEN
            WRITE (IW(6),1000) IRUNRC,RENLEP,
     $ (SILCUT(LI),LI=1,3),(SILCUT(LI),LI=8,9),
     $ SIRIT1-1,SIRIT1,SIROT1,SIROT1+1,SIRIL1-1,SIRIL1,SIROL1,SIROL1+1,
     $ SIRIT2-1,SIRIT2,SIROT2,SIROT2+1,SIRIL2-1,SIRIL2,SIROL2,SIROL2+1,
     $(SILCUT(LI),LI=4,7),(ISIPTCU(LI),LI=1,4),(ISIPLCU(LI),LI=1,4),
     $ ISIPTCU(6),ISIPTCU(5),(ISIPTCU(LI),LI=7,8),
     $ ISIPLCU(6),ISIPLCU(5),(ISIPLCU(LI),LI=7,8)
            IF ( S95LEP1 ) WRITE (IW(6),1001) SIBCUT
         ENDIF
      ENDIF
      IND = NDROP('SILU',0)
      NAFVER = NAMIND('FVER')
C Drop SPDA  if no cluster found
      JSCLS = IW(NASCLS)
      IF (JSCLS .LE. 0) THEN
        IND = NDROP('SPDA',0)
      ENDIF
      IMETH = 0
C do we have a  SCAL trigger?
      JSIX2 = IW(NASIX2)
      IWSIX2 = 0
      IF (JSIX2.GT.0 )   IWSIX2 = ITABL(JSIX2,1,1)
C   If reprocessing from POT to POT , SILH contains the trigger word
      IF ( FREPRO) THEN
         JSILH = IW(NASILH)
         IF ( JSILH.GT.0) IWSIX2 = ITABL(JSILH,1,14)
      ENDIF
      IF (IWSIX2.EQ.0) GO TO 999
C on both sides, find most energetic, and second most energetic cluster
      CALL VZERO(NEMAX,2)
      CALL VZERO(ECLU,2)
      CALL VZERO(NEMX2,2)
      CALL VZERO(ECLU2,2)
      JSCLS = IW(NASCLS)
      IF (JSCLS .LE. 0) GO TO 999
      NCLU = LROWS(JSCLS)
      IF ( NCLU.LE.0) GO TO 999
      DO 320 ICL = 1, NCLU
C  Skip identified electronic noise
        CALL SICLEAN(ICL,IFLAG)
        IF (IFLAG.GT.0) GO TO 320
        ISIDE = 1
        Z = RTABL(JSCLS,ICL,JSCLZC)
        EN= RTABL(JSCLS,ICL,JSCLEC)
        DR = RTABL(JSCLS,ICL,JSCLWR)
C   do not consider 'out of time' clusters  for 1995 LEP1 Data
C   remove clusters in 'out of time' Box in the Dr-E plane
       if (S95LEP1.AND.(EN.LT.SIBCUT(2)).and.(DR.GT.SIBCUT(1))) goto 320
        IF (Z.LT.0. ) ISIDE = 2
        IF (EN .GT. ECLU(ISIDE)) THEN
          NEMX2(ISIDE) = NEMAX(ISIDE)
          ECLU2(ISIDE) = ECLU(ISIDE)
          NEMAX(ISIDE) = ICL
          ECLU(ISIDE) = EN
        ELSE IF (EN .GT. ECLU2(ISIDE)) THEN
          NEMX2(ISIDE) = ICL
          ECLU2(ISIDE) = EN
        ENDIF
  320 CONTINUE
      CALL AUBOS('SILU',0,LMHLEN+2*LSILUA,JSILU,IGARB)
      IF (JSILU .LE. 0 .OR. IGARB .EQ. 2)        GOTO 902
      IW(JSILU+LMHCOL) = LSILUA
      IW(JSILU+LMHROW) = 0
      JSPDA = IW(NASPDA)
      JSCLS = IW(NASCLS)
      DO 30 ICL = 1,2
        JCL = NEMAX(ICL)
        IF ( JCL.LE.0) GO TO 30
        KSILU = KNEXT(JSILU)
        IW(JSILU+LMHROW) = IW(JSILU+LMHROW) +1
        IW(KSILU+JSILIS) = 0
        RW(KSILU+JSILEC) = ECLU(ICL)
        RW(KSILU+JSILER) = RTABL(JSCLS,JCL,JSCLER)
        CALL SIFSUM(JCL,EFRAC,NPAD)
        IW(KSILU+JSILNP) = NPAD(1)
        IW(KSILU+JSILND) = NPAD(2)
        DO 40 IZZ = 1,12
          RW(KSILU+JSILEF+IZZ-1) = EFRAC(IZZ)/ECLU(ICL)
   40   CONTINUE
        RW(KSILU+JSILRC) = RTABL(JSCLS,JCL,JSCLRC)
        RCLU = RTABL(JSCLS,JCL,JSCLCR)
        RW(KSILU+JSILCR) = RCLU
        RW(KSILU+JSILPC) = RTABL(JSCLS,JCL,JSCLPC)
        PCLU = RTABL(JSCLS,JCL,JSCLCP)
        RW(KSILU+JSILCP) = PCLU
        RW(KSILU+JSILTC) = RTABL(JSCLS,JCL,JSCLTC)
        RW(KSILU+JSILCT) = RTABL(JSCLS,JCL,JSCLCT)
        ZCLU = RTABL(JSCLS,JCL,JSCLZC)
        RW(KSILU+JSILZC) = ZCLU
        RW(KSILU+JSILDR) = RTABL(JSCLS,JCL,JSCLWR)
        RW(KSILU+JSILDP) = RTABL(JSCLS,JCL,JSCLWP)
        CALL SIXTOI ( RCLU,PCLU,ZCLU,IRAD,IPHI,ISTA,DR,DP,IOK)
        IW(KSILU+JSILRI) = IRAD
        IW(KSILU+JSILPI) = IPHI
C   Extrapolate position to z=0.
        DPH = BC2*ZCLU/ECLU(ICL)
        X = RCLU*COS(PCLU-DPH)
        Y = RCLU*SIN(PCLU-DPH)
        RW(KSILU+JSILXI) = X
        RW(KSILU+JSILYI) = Y
C    FRONT FACE TO BE FILLED WITH TRUTH IF POSSIBLE (MC), EXTRAPOLATED
C    TO FRONT FACE OTHERWISE
        KFVER = IW(NAFVER)
        IF ( KFVER.GT.0) THEN
           DO 34 I = 2,3
              IF ( ZCLU*RTABL(KFVER,I,3).GT.0.) THEN
                 RW(KSILU+JSILXF) = RTABL(KFVER,I,1)
                 RW(KSILU+JSILYF) = RTABL(KFVER,I,2)
              ENDIF
  34       CONTINUE
        ELSE
           CALL SIZFRI(ZFR,1,ICL)
           ZFRONT = SIGN(1.,ZFR)*(ABS(ZFR)-.808)
           DPHP = BC2*(ZCLU-ZFRONT)/ECLU(ICL)
           XF = RCLU*COS(PCLU-DPHP)
           YF = RCLU*SIN(PCLU-DPHP)
           RW(KSILU+JSILXF) = XF
           RW(KSILU+JSILYF) = YF
        ENDIF
        CALL SIPVOI(IRAD,IPHI,ICL,EVOI)
        DO 35 IEE = 1,45
          RW(KSILU+JSILEP+IEE-1) = EVOI(IEE)
   35   CONTINUE
C    Fill index to SILU in SIID
        KSIID = IW(NASIID) 
        KID = KROW(KSIID,JCL)
        IW(KID+JSIISI) = LROWS(JSILU)
C    Fill possible second cluster quantity
        JCL = NEMX2(ICL)
        IF ( JCL.LE.0) GO TO 30
        RW(KSILU+JSILES) = ECLU2(ICL)
        RW(KSILU+JSILRS) = RTABL(JSCLS,JCL,JSCLCR)
        RW(KSILU+JSILPS) = RTABL(JSCLS,JCL,JSCLCP)
        RW(KSILU+JSILZS) = RTABL(JSCLS,JCL,JSCLZC)
        CALL SIFSUM(JCL,EFRAC,NPAD)
        IW(KSILU+JSILNS) = NPAD(1)
   30 CONTINUE
C
C all is filled apart from Fiducial cut side in SILU and accept Method
C  in SILH
C   get wagon number
      NWA  = SIGTWA(IDUM)
      NSILEV(1,NWA) = NSILEV(1,NWA) + 1
C   perform Lumi cuts now
      CALL AUBPRS('SILU')
      JSILU = IW (NASILU)
      JSCLS = IW (NASCLS)
      IF ( JSILU.LE.0) GO TO 999
      NCLU = LROWS(JSILU)
C  1. at least 1 cluster on each side
      IF ( NCLU.NE.2) GO TO 999
      NSILEV(2,NWA) = NSILEV(2,NWA) + 1
C  2.  Energy CUTS : 0.44 * EBEAM = 20 GEV EACH SIDE,
C            AND 0.60 * ECM = 55 GEV FOR THE SUM
C             OR 0.713* ECM = 65 Gev for the sum at peak
      IF(ECLU(1).LT.SILCUT(1)) GOTO 999
      NSILEV(3,NWA) = NSILEV(3,NWA) + 1
      IF(ECLU(2).LT.SILCUT(2)) GOTO 999
      NSILEV(4,NWA) = NSILEV(4,NWA) + 1
      SOK = SICAOK(IRUNRC)
      IF ( SOK ) THEN
         XOK = SILUAD(1,1)
         XOK = SILUAD(2,1)
      ENDIF
      IF(ECLU(1)+ECLU(2).LT.SILCUT(3)) GOTO 999
      NSILEV(5,NWA) = NSILEV(5,NWA) + 1
      IF ( SOK ) THEN
         XOK = SILUAD(1,2)
         XOK = SILUAD(2,2)
      ENDIF
C  3.  Choose fiducial side , flip it for each trigger
      KFIDSI = 3 - KFIDSI
      KLOOSI = 3 - KFIDSI
C  FILL fidu side word in SILU
      DO 38 ICL = 1,2
        KSILU = KROW(JSILU,ICL)
        IF ( KFIDSI.EQ.ICL) IW(KSILU+JSILIS) =1
        IF ( KLOOSI.EQ.ICL) IW(KSILU+JSILIS) =2
   38 CONTINUE
C-----------------------------------------------
C NEW PHI CUTS FOR METH 3,4,5,6 ... JOHN 4/02/93
C MODIFIED TO PHI INDEX CUTS .....BBL 13/02/93
C-----------------------------------------------
      IVER = 0
      IHOR = 0
      ICLU = NEMAX(KFIDSI)
      CALL SIPSUM(ICLU,IPHM)
      IPCTI  = IPHM
C METH 3 AND 5 TIGHT CHECK---> VERTICAL ACCEPTANCE
C METH 3 AND 5 LOOSE CHECK----> NO MORE CUT
      IF( (IPCTI.GT.ISIPTCU(1).AND.IPCTI.LT.ISIPTCU(2) ).OR.
     &  ( IPCTI.GT.ISIPTCU(3).AND.IPCTI.LT.ISIPTCU(4) ))     IVER=1
C METH 4 AND 6 TIGHT CHECK----> HORIZONTAL ACCEPTANCE
C METH 4 AND 6 LOOSE CHECK----> NO MORE CUT
      IF((IPCTI.LT.ISIPTCU(5).OR.IPCTI.GT.ISIPTCU(6)).OR.
     &   (IPCTI.GT.ISIPTCU(7).AND.IPCTI.LT.ISIPTCU(8)))     IHOR=1
C
C==> CHECK DELTAPHI (USE CLUSTER INFO)
C
      PHCLUM1= RTABL(JSILU,KFIDSI,JSILCP)
      PHCLUM2= RTABL(JSILU,KLOOSI,JSILCP)
      DPHILS=(PHCLUM1-PHCLUM2)*RADEG
      IF(DPHILS.LT.0.0) DPHILS=DPHILS+360.
      IF(DPHILS.LT.SILCUT(8)) GOTO 999
      NSILEV(10,NWA) = NSILEV(10,NWA) + 1
      IF(DPHILS.GT.SILCUT(9)) GOTO 999
      NSILEV(11,NWA) = NSILEV(11,NWA) + 1
      IF ( SOK ) THEN
         XOK = SILUAD(1,3)
         XOK = SILUAD(2,3)
      ENDIF
C--------------------------------------------------------------------
C METHOD ONE : TIGHT SIDE CUTS INNER=R_BINS 2/3, OUTER=R_BINS 12/13
C--------------------------------------------------------------------
      IRCLUM = ITABL(JSILU,KFIDSI,JSILRI)
      IF(IRCLUM.LE.0) GOTO 999
C...R_INNER ==> USE LAYERS 3 AND 4
      LAYSIL=4
      IR=SIRIT1-1
      IF(IRCLUM.LT.IR) GOTO 999
      IF(IRCLUM.LT.IR+2) THEN
        KIN=3-IRCLUM+IR
        KOUT=KIN-1
        EIN=RTABL(JSCLS,NEMAX(KFIDSI),JSCLE1+5*(KIN-1)+LAYSIL-1)
        EOUT=RTABL(JSCLS,NEMAX(KFIDSI),JSCLE1+5*(KOUT-1)+LAYSIL-1)
        IF((EIN+EOUT).LE.0.0) THEN
          NSILEV(17,NWA) = NSILEV(17,NWA) + 1
          GOTO 999
        ENDIF
        ASYM=(EIN-EOUT)/(EIN+EOUT)
        IF(ASYM.LT.SILCUT(4)) GOTO 999
      ENDIF
      NSILEV(6,NWA) = NSILEV(6,NWA) + 1
      IF ( SOK )    XOK = SILUAD(1,4)
C
C...R_OUTER ==> USE LAYERS 3 AND 4
      LAYSIL=4
      IR=SIROT1
      IF(IRCLUM.GT.IR+1) GOTO 999
      IF(IRCLUM.GT.IR-1) THEN
        KOUT=3-IRCLUM+IR
        KIN=KOUT-1
        EIN=RTABL(JSCLS,NEMAX(KFIDSI),JSCLE1+5*(KIN-1)+LAYSIL-1)
        EOUT=RTABL(JSCLS,NEMAX(KFIDSI),JSCLE1+5*(KOUT-1)+LAYSIL-1)
        IF((EIN+EOUT).LE.0.0) THEN
          NSILEV(18,NWA) = NSILEV(18,NWA) + 1
          GOTO 999
        ENDIF
        ASYM=(EIN-EOUT)/(EIN+EOUT)
        IF(ASYM.LT.SILCUT(5)) GOTO 999
      ENDIF
      NSILEV(7,NWA) = NSILEV(7,NWA) + 1
      IF ( SOK ) THEN
         XOK = SILUAD(1,5)
         XOK = SILUAD(2,5)
      ENDIF
C
C==> CHECK LOOSE-SIDE
C
      IRCLUM = ITABL(JSILU,KLOOSI,JSILRI)
      IF(IRCLUM.LE.0) GOTO 999
C...R_INNER ==> LAYERS 3 AND 5
      LAYSIL=5
C...R_INNER ==> LAYERS 3 AND 4
      LAYSIL=4
      IR=SIRIL1-1
      IF(IRCLUM.LT.IR) GOTO 999
      IF(IRCLUM.LT.IR+2) THEN
        KIN=3-IRCLUM+IR
        KOUT=KIN-1
        EIN=RTABL(JSCLS,NEMAX(KLOOSI),JSCLE1+5*(KIN-1)+LAYSIL-1)
        EOUT=RTABL(JSCLS,NEMAX(KLOOSI),JSCLE1+5*(KOUT-1)+LAYSIL-1)
        IF((EIN+EOUT).LE.0.0) THEN
          NSILEV(19,NWA) = NSILEV(19,NWA) + 1
          GOTO 999
        ENDIF
        ASYM=(EIN-EOUT)/(EIN+EOUT)
        IF(ASYM.LT.SILCUT(6)) GOTO 999
      ENDIF
      NSILEV(8,NWA) = NSILEV(8,NWA) + 1
      IF ( SOK ) THEN
         XOK = SILUAD(1,6)
         XOK = SILUAD(2,6)
      ENDIF
C
C...R_OUTER ==> LAYERS 3 AND 5
      LAYSIL=5
C...R_OUTER ==> LAYERS 3 AND 4
      LAYSIL=4
      IR=SIROL1
      IF(IRCLUM.GT.IR+1) GOTO 999
      IF(IRCLUM.GT.IR-1) THEN
        KOUT=3-IRCLUM+IR
        KIN=KOUT-1
        EIN=RTABL(JSCLS,NEMAX(KLOOSI),JSCLE1+5*(KIN-1)+LAYSIL-1)
        EOUT=RTABL(JSCLS,NEMAX(KLOOSI),JSCLE1+5*(KOUT-1)+LAYSIL-1)
        IF((EIN+EOUT).LE.0.0) THEN
          NSILEV(20,NWA) = NSILEV(20,NWA) + 1
          GOTO 999
        ENDIF
        ASYM=(EIN-EOUT)/(EIN+EOUT)
        IF(ASYM.LT.SILCUT(7)) GOTO 999
      ENDIF
      NSILEV(9,NWA) = NSILEV(9,NWA) + 1
      IF ( SOK ) THEN
         XOK = SILUAD(1,7)
         XOK = SILUAD(2,7)
         XOK = SILUAD(1,10)
      ENDIF
      IMETH = IBSET(IMETH,0)
C--------------------------------------------------------------------
C METHOD 3   : METHOD 1 AND VERTICAL SELECTION IVER = 1
C--------------------------------------------------------------------
      IF ( IVER.EQ.1) THEN
      NSILEV(12,NWA) = NSILEV(12,NWA) + 1
        IMETH = IBSET(IMETH,2)
        IF ( SOK ) THEN
          XOK = SILUAD(1,8)
          XOK = SILUAD(1,11)
        ENDIF
      ENDIF
C--------------------------------------------------------------------
C METHOD 4   : METHOD 1 AND HORIZONTAL SELECTION IHOR = 1
C--------------------------------------------------------------------
      IF ( IHOR.EQ.1) THEN
      NSILEV(13,NWA) = NSILEV(13,NWA) + 1
        IMETH = IBSET(IMETH,3)
        IF ( SOK ) THEN
          XOK = SILUAD(1,9)
          XOK = SILUAD(1,12)
        ENDIF
      ENDIF
C
C--------------------------------------------------------------------
C METHOD TWO : TIGHT SIDE CUTS INNER=R_BINS 3/4, OUTER=R_BINS 12/13
C ONLY INNER R IS TIGHTER , WE CAN USE METHOD 1 EVENTS TO START
C--------------------------------------------------------------------
      IRCLUM = ITABL(JSILU,KFIDSI,JSILRI)
      IF(IRCLUM.LE.0) GOTO 999
C...R_INNER ==> USE LAYERS 3 AND 4
      LAYSIL=4
      IR=SIRIT2-1
      IF(IRCLUM.LT.IR) GOTO 999
      IF(IRCLUM.LT.IR+2) THEN
        KIN=3-IRCLUM+IR
        KOUT=KIN-1
        EIN=RTABL(JSCLS,NEMAX(KFIDSI),JSCLE1+5*(KIN-1)+LAYSIL-1)
        EOUT=RTABL(JSCLS,NEMAX(KFIDSI),JSCLE1+5*(KOUT-1)+LAYSIL-1)
        IF((EIN+EOUT).LE.0.0) THEN
          NSILEV(21,NWA) = NSILEV(21,NWA) + 1
          GOTO 999
        ENDIF
        ASYM=(EIN-EOUT)/(EIN+EOUT)
        IF(ASYM.LT.SILCUT(10)) GOTO 999
      ENDIF
      NSILEV(14,NWA) = NSILEV(14,NWA) + 1
      IMETH = IBSET(IMETH,1)
      IF ( SOK ) THEN
         XOK = SILUAD(2,4)
         XOK = SILUAD(2,10)
      ENDIF
C--------------------------------------------------------------------
C METHOD 5   : METHOD 2 AND VERTICAL SELECTION IVER = 1
C--------------------------------------------------------------------
      IF ( IVER.EQ.1) THEN
      NSILEV(15,NWA) = NSILEV(15,NWA) + 1
        IMETH = IBSET(IMETH,4)
        IF ( SOK ) THEN
           XOK = SILUAD(2,8)
           XOK = SILUAD(2,11)
        ENDIF
      ENDIF
C--------------------------------------------------------------------
C METHOD 6   : METHOD 2 AND HORIZONTAL SELECTION IHOR = 1
C--------------------------------------------------------------------
      IF ( IHOR.EQ.1) THEN
      NSILEV(16,NWA) = NSILEV(16,NWA) + 1
        IMETH = IBSET(IMETH,5)
        IF ( SOK ) THEN
           XOK = SILUAD(2,9)
           XOK = SILUAD(2,12)
        ENDIF
      ENDIF
      GO TO 999
C----------------------------------------------------------------------
  902 WRITE (IW(6),'(A)') ' SILUMI --> NO SPACE FOR NEW BANK SILU'
      IER = 1
C     ENDIF
  999 CONTINUE
      JSILH = IW(NASILH)
      IF ( IW(JSILH).GT.0) THEN
      IW(JSILH+LMHLEN+JSILAM) = IMETH
      ENDIF
      RETURN
 1001 FORMAT (
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +      SICAL INTIME CUTS FOR SCAN 95 -Box definition   +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +  Minimum Sigma2 in R direction  : ',F6.2,'             +',/,
     $' +  Maximum Energy (GeV)           : ',F6.2,'             +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
 1000 FORMAT (
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +      SICAL LUMINOSITY CUTS                           +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +    RUN ',I5,'  Center of mass energy :  ',F6.2,'  Gev   +',/,
     $' +    Single Side   Energy threshold  :',2F6.2,' Gev +',/,
     $' +    Total         Energy threshold  :   ',F6.2,'  Gev   +',/,
     $' +    Minimum Delta PHI               :  ',F6.2,'  deg    +',/,
     $' +    Maximum Delta PHI               :  ',F6.2,'  deg    +',/,
     $' +    Radial Fiducial cuts        Tight         Loose   +',/,
     $' +                            inner  outer  inner  outer+',/,
     $' +    Method 1 pad boundary',1X,I3,I4,1X,3I3,I4,1X,2I3,'+',/,
     $' +    Method 2 pad boundary',1X,I3,I4,1X,3I3,I4,1X,2I3,'+',/,
     $' +    Asymmetry cut  at :  ',4F7.2,' +',/,
     $' +    Method 3,5 Vertical phi cuts : tight fidu side    +',/,
     $' +            from ',I2,' to ',I2,'   and from  ',I2,'  to ',I2,
     $'       +',/,
     $' +                                   loose fidu side    +'
     $,/,' +            from ',I2,' to ',I2,'   and from  ',I2,'  to ',
     $I2,'       +',/,
     $' +    Method 4,6 Horizontal phi cuts : tight fidu side  +',/,
     $' +            from ',I2,' to ',I2,'   and from  ',I2,'  to ',I2,
     $'       +',/,
     $' +                                   loose fidu side    +'
     $,/,' +            from ',I2,' to ',I2,'   and from  ',I2,'  to ',
     $I2,'       +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      END
#endif
