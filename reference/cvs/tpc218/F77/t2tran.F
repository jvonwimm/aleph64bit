*DK t2tran
      SUBROUTINE T2TRAN(IWIR)
C
C! Routine to transport the super-broken segment charge to wire grid.
C  The routine accounts for longitudinal and transverse diffusion as
C  well as ExB effect.
C
C  Called from:   T2TEER
C  Calls:         RANNOR,TPEXBS,TPEXB2,TPEXB3,TPEXMI
C
C  Inputs:   FASTER    :  --JSEGT,  num of current segment
C                         --NSEGT,  total number of segments
C                         --ITYPE,  sector type
C            TPCOND.INC:  --DRFVEL, drift velocity, cm/ns
C                         --SIGTR,  rms transverse diffusion, cm**.5
C                         --SIGMA,  rms longitudinal diffusion, cm**.5
C                         --ITRCON, field configuration
C            TPGEOM.INC:  --endplate position, wire geometry
C            TPGEOW.INC:
C
C  Outputs:  FASTER    :  --IWIR,   wire number hits; = 0 if none hit
C                         --WIRRAD, radius at point of wire hits
C                         --WIRPHI, phi at point of wire hits
C                         --AVTIM,  avalanche time for each cluster
C                         --NELE,   number of electron in each cluster
C                         --SIGT,   time width for each cluster
C                         --NCL,    number of clusters (1 or 2)
C
C  P. Janot  11/15/87
C
C  Modifications:
C
C                1. D. Casper - 12Oct92 - a) remove features related
C                to double counting of electrons, b) add fluctuations
C                in adsorption, c) modify treatment of transverse
C                diffusion slightly.
C
#include "tpcond.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeow.h"
#include "alcons.h"
#include "hiscom.h"
#include "faster.h"
      LOGICAL LIN,LDBT1,LDBT2,LDBT3
C
C  Debug and simulation levels
C
      ICALLS = ICALLS + 1
      LDBT1 = ( NTPCDT .GE. 1 .AND. ICALLS .LE. NCALDT )
      LDBT2 = ( NTPCDT .GE. 2 .AND. ICALLS .LE. NCALDT )
      LDBT3 = ( NTPCDT .GE. 3 .AND. ICALLS .LE. NCALDT )
C
C  Determine segment middle and extremities coords
C
      CALL TPEXMI(ZTPCMX,XEX)
      IF(LDBT3) WRITE(6,1000) JSEGT,((XEX(I,J),J=1,4),I=1,3),XEX(4,1)
     *                               ,XEX(4,2)
1000  FORMAT(1x,'Segment      : ',I6/
     *       1x,'beg. coords  : ',4F10.4/
     *       1x,'mid. coords  : ',4F10.4/
     *       1x,'end  coords  : ',4F10.4/
     *       1x,'total charge : ',2F5.0)
C
C  Determine which wire is hit and where; quit if none is hit
C  First get the position of the first wire.
C
      YWIR1 = TWIRE1(ITYPE)
      WSTEP = TWSTEP(ITYPE)
C
C  Get the wire number and height of the wire.  The field wires are
C  +-1/2 wire spacings from the sense wires.
C
      YWIR =  XEX(2,2) +WSTEP/2.
      IWIR = INT ( (YWIR-YWIR1) / WSTEP ) + 1
      IF(YWIR.LT.YWIR1) IWIR = IWIR - 1
      YWIR = YWIR1 + (FLOAT(IWIR)-1.)*WSTEP
      WMIN = TWIRMN(IWIR,ITYPE)
      WMAX = TWIRMX(IWIR,ITYPE)
      IF(LDBT3) WRITE(6,654) YWIR
  654 FORMAT(1X,' WIRE COORD : ',F10.5)
C
C   See if we have overshot the sector. If so, set IWIR=0 as a flag
C   and quit.
C
      IF(IWIR.LE.0.OR.IWIR.GT.NTWIRE(ITYPE)) THEN
         IWIR=0
         RETURN
      ENDIF
C
C  First find the bare drift distance and time
C
      XX2      = XEX(2,1)
      ZZ      = XEX(2,3)
      ZDRIFT  = ZTPCMX - ABS(ZZ)
      IF (ZDRIFT .LT. 0.0) ZDRIFT = 0.0
      TDRIFT  = ZDRIFT/DRFVEL
      IF(LDBT3) WRITE(6,1007) XX2,ZZ
 1007 FORMAT(1X,'XX2/ZZ : ',2F10.4)
C
C  Consider the effects of the diffusion .
C
      SQRZD = SQRT(ZDRIFT)
C
C  R-phi diffusion
C
      RPHSIG = SIGTR * SQRZD
C
C  Z-diffusion
C
      ZEDSIG   = SIGMA * SQRZD/DRFVEL
C
C  Adsorption (2.5% / Meter)
C
      XABSOR   = 0.025 * ZDRIFT/100.
C
C Treat separately the extremities of the track element
C
      IF(ABS(XEX(1,2)-XEX(2,2)).LT.0.001  .AND.
     *   ABS(XEX(2,2)-XEX(3,2)).LT.0.001) THEN
        NCL = 1
        CALL TPEXBS(ITRCON,YWIR-(XEX(1,2)+XEX(3,2))/2.,XSHP2,IRET)
        IF ( IRET .NE. 0 ) XSHP2 = 0.
        XX2 =  (XEX(1,1)+XEX(3,1))/2.+XSHP2
        ZZ =  (XEX(1,3)+XEX(3,3))/2.
        TT =  (XEX(1,4)+XEX(3,4))/2.
C
C  Since adsorption would be corrected for in the mean, we only care
C  about the fluctuations from the average.  Get expected number
C  of electrons attenuated.
C
        XATTEN = XABSOR * (XEX(4,1)+XEX(4,2))
C
C  See how many are actually attenuated using poisson distribution
C
        CALL TPELSG(XATTEN,1.,NATTEN)
C
C  Alter the number of electrons by the difference between observed
C  and expected (note this preserves the mean number of electrons)
C
        NELE(1) = XEX(4,1)+XEX(4,2)+(XATTEN - NATTEN + 0.5)
        IF(NELE(1).LE.0)THEN
              NELE(1) = 0
              IWIR = 0
              RETURN
        ENDIF
C
C  Check if we're still on the wire after the drift and ExB shift
C
        LIN = ( (XX2 .GE. WMIN .AND. XX2 .LE. WMAX) .OR.
     &          (XX2 .GE.-WMAX .AND. XX2 .LE.-WMIN))
        IF( .NOT. LIN .OR. IRET .NE. 0 ) NELE(1) = 0.
        WIRRAD(1) = SQRT(XX2**2+YWIR**2)
        WIRPHI(1) = ATAN2(XX2,YWIR)
        AVTIM(1) = XEX(2,4)+(ZTPCMX-ABS(ZZ))/DRFVEL
        SIGT(1)  = ZEDSIG
        SIGT(3)  = 0.
        IF(LDBT3) GOTO 999
        RETURN
      ENDIF
C
C  Most cases.
C
      IF(ABS(XEX(1,2)-XEX(2,2)).GT.0.001  .AND.
     *     ABS(XEX(2,2)-XEX(3,2)).GT.0.001) THEN
         CALL TPEXB3(XEX,WSTEP,XX1,XX3,XSHT1,XSHT3)
      ELSE
         CALL TPEXB2(XEX,YWIR,WSTEP,XX1,XX3,XSHT1,XSHT3)
      ENDIF
      IF(ABS(XEX(1,2)-XEX(2,2)) .LT. .001 .AND.
     *    ABS(XEX(1,1)-XEX(2,1)) .GT. .001) THEN
         XX1 = XEX(2,1) + (XEX(1,1)-XEX(2,1))/SQ3
         XSHT1 = 0.
      ENDIF
      IF(ABS(XEX(3,2)-XEX(2,2)) .LT. .001 .AND.
     *    ABS(XEX(3,1)-XEX(2,1)) .GT. .001) THEN
         XX3 = XEX(2,1) + (XEX(3,1)-XEX(2,1))/SQ3
         XSHT3 = 0.
      ENDIF
C
C  Find the avalanche time of the points.
C
      DFT1 =  (ZTPCMX-ABS(XEX(1,3)))/DRFVEL
      DFT3 =  (ZTPCMX-ABS(XEX(3,3)))/DRFVEL
      AVTT =  TDRIFT + XEX(2,4)
      AVT1 =  DFT1   + XEX(1,4)
      AVT3 =  DFT3   + XEX(3,4)
      DET1 = ABS(AVT1 - AVTT)
      DET3 = ABS(AVT3 - AVTT)
      AVT1 = (AVT1 + AVTT) * 0.5
      AVT3 = (AVT3 + AVTT) * 0.5
C
C  Determine the fluctuations.
C
      SIGT1  = SQRT(  ZEDSIG**2 + (XSHT1/DRFVEL/SQ2)**2 )
      SIGT3  = SQRT(  ZEDSIG**2 + (XSHT3/DRFVEL/SQ2)**2 )
      SIGP1  = SQRT(  RPHSIG**2
     *              + ((XX1 - XX2)/SQ3)**2)
      SIGP3  = SQRT(  RPHSIG**2
     *              + ((XX3 - XX2)/SQ3)**2)
C
C  ..and now, fill the arrays AVTIM, WIRPHI, WIRRAD, SIGT and NELE
C
C     CALL RANNOR(A1,A2)
      CALL RANNOR(A3,A4)
      NCL   = 2
C
C  Since adsorption would be corrected for in the mean, we only care
C  about the fluctuations from the average.  Get expected number
C  of electrons attenuated.
C
      XATTEN1 = XABSOR * (XEX(4,1))
      XATTEN2 = XABSOR * (XEX(4,2))
C
C  See how many are actually attenuated using poisson distribution
C
      CALL TPELSG(XATTEN1,1.,NATTEN1)
      CALL TPELSG(XATTEN2,1.,NATTEN2)
C
C  Alter the number of electrons by the difference between observed
C  and expected (note this preserves the mean number of electrons)
C
      NELE(1) = XEX(4,1)+(XATTEN1 - NATTEN1 + 0.5)
      NELE(2) = XEX(4,2)+(XATTEN2 - NATTEN2 + 0.5)
      IF(NELE(1).LT.0)NELE(1)=0
      IF(NELE(2).LT.0)NELE(2)=0
      SQNE = SQRT(XEX(4,1)+XEX(4,2))
      IF(SQNE.EQ.0.) RETURN
      AVTIM(1) = AVT1
      AVTIM(2) = AVT3
      IF(AVTIM(1).LT.0.) AVTIM(1)=0.
      IF(AVTIM(2).LT.0.) AVTIM(2)=0.
C  Check if we're still on the wire after the drift and ExB shift
      XX1 = XX1 + A3*SIGP1/SQNE
      XX3 = XX3 + A4*SIGP3/SQNE
      LIN = ( (XX1 .GE. WMIN .AND. XX1 .LE. WMAX) .OR.
     &        (XX1 .GE.-WMAX .AND. XX1 .LE.-WMIN))
      IF( .NOT. LIN) NELE(1) = 0.
      LIN = ( (XX3 .GE. WMIN .AND. XX3 .LE. WMAX) .OR.
     &        (XX3 .GE.-WMAX .AND. XX3 .LE.-WMIN))
      IF( .NOT. LIN) NELE(2) = 0.
      WIRRAD(1) = SQRT(XX1**2+YWIR**2)
      WIRPHI(1) = ATAN2(XX1,YWIR)
      SIGT(1)   = SIGT1
      IF(SIGT1 .GT. 0.1)THEN
         SIGT(3)   = DET1 / SIGT1
      ELSE
         SIGT(3)   = 0.
      ENDIF
      WIRRAD(2) = SQRT(XX3**2+YWIR**2)
      WIRPHI(2) = ATAN2(XX3,YWIR)
      SIGT(2)   = SIGT3
      IF(SIGT3 .GT. 0.1)THEN
         SIGT(4)   = DET3 / SIGT3
      ELSE
         SIGT(4) = 0.
      ENDIF
      IF(LDBT3) GOTO 999
      RETURN
 999  CONTINUE
      WRITE(6,1004) (I,WIRRAD(I),WIRPHI(I),NELE(I),AVTIM(I),I=1,NCL)
 1004 FORMAT(1X,'Cluster ',I1,' :'/
     &       1X,'Radius/Phi/Nel/Avalanche Time : '/1X,2F10.4,I6,F13.4)
      RETURN
      END
