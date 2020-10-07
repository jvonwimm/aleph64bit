C
C***********************************************************************
C
      SUBROUTINE OCBPRE(IEVT,PAR,ERR)
C     ===============================
C
C     Olivier Schneider, CERN-PPE/ALE, Aug 19, 1996
C
C!    Modify transverse parameters of a track in event IEVT
C!    to refer to a new origin defined as the BOM+QS0 beam spot position.
C!
C!    Input:   IEVT = event number 
C!    ------   PAR  = vector of track parameters in transverse plane
C!                          PAR(1) = PAR(JIR) = 1 / curvature
C!                          PAR(2) = PAR(JP0) = phi_0
C!                          PAR(3) = PAR(JD0) = d_0
C!             ERR  = error matrix on PAR;
C!                    triangular matrix stored in the following order:
C!                                              1
C!                                              2 3
C!                                              4 5 6
C!                    i.e.  ERR(1) = cov(JIR,JIR)
C!                          ERR(2) = cov(JIR,JP0)
C!                          ERR(3) = cov(JP0,JP0)
C!                          ERR(4) = cov(JIR,JD0)
C!                          ERR(5) = cov(JP0,JD0)
C!                          ERR(6) = cov(JD0,JD0)
C!
C!    Output:  IEVT     = 0 if an error occurred (<0 otherwise)
C!    -------  PAR(JP0) = value    of phi_0  with respect to new origin
C!             PAR(JD0) = value    of d_0    with respect to new origin
C!             ERR(6)   = variance of d_0    with respect to new origin
C!
#ifndef DOC
      IMPLICIT NONE
C
C     Arguments
C
      INTEGER IEVT
      REAL PAR(3),ERR(6)
C
C     Externals
C
      INTEGER NLINK,OGTBLQ
C
C     Parameters
C
      INTEGER MASK
      PARAMETER(MASK='0F001F00'X) ! bits 8-12 and 24-27
      INTEGER JIR,JP0,JD0
      PARAMETER(JIR=1,JP0=2,JD0=3)
#include "blqpjj.h"
C
C     Local variables
C
      INTEGER KBLQP,ISTAT
      REAL BLQPOS(2),BLQERR(2)
      DOUBLE PRECISION R,XC,YC,XN,YN
      DOUBLE PRECISION CN,PHIOLD,DOLD
      DOUBLE PRECISION DCNXC,DCNYC,DXCR,DXCDOLD,DXCPHIOLD,
     &                 DYCR,DYCDOLD,DYCPHIOLD,DDR,DDDOLD,DDPHIOLD
      DOUBLE PRECISION D,PHI,VARD
C
C     BOS and JULIA common blocks
C
#include "bcs.h"
#include "rcurnt.h"
C
C     Declarations for function definition statements
C
      DOUBLE PRECISION VAR
      INTEGER I,J
#include "bmacrod.h"
C
C     Function definition statements
C
      VAR(I,J)=DBLE(ERR((MAX0(I,J)*(MAX0(I,J)-1))/2+MIN0(I,J)))
#include "bmacro.h"
C
C     Return if transformation already applied
C
      IF(IEVT.LE.0) RETURN
C
C     Get BOM+QS0 beam spot position
C
      ISTAT=OGTBLQ(IRUNRC,IEVT,BLQPOS,BLQERR)
      IF(ISTAT.EQ.0) THEN 
        CALL RERROR('OCBPRE',-1,'BLQP data already aligned')
      ELSE IF(ISTAT.EQ.1) THEN
C       This is what we want ...
      ELSE IF(ISTAT.EQ.2) THEN
        CALL RERROR('OCBPRE',-2,'No QS0 correction available')
      ELSE IF(ISTAT.EQ.-1) THEN
        CALL RERROR('OCBPRE',-3,'Bad arguments to OGTBLQ')
      ELSE IF(ISTAT.EQ.-2) THEN
        CALL RERROR('OCBPRE',-4,'Missing BLQP bank')
      ELSE IF(ISTAT.EQ.-3) THEN
C        No valid data in BLQP for this event ...
      ELSE IF(ISTAT.NE.1) THEN 
        CALL RERROR('OCBPRE',-5,'Unknown error from OGTBLQ')
      ENDIF
      IF(ISTAT.NE.1) THEN 
        IEVT=0
        RETURN
      ENDIF
C
      XN=DBLE(BLQPOS(1))
      YN=DBLE(BLQPOS(2))
C
      DOLD=DBLE(PAR(JD0))
      PHIOLD=DBLE(PAR(JP0))
      R=1./DBLE(PAR(JIR))
C
C     compute the position (XC,YC) of the center C of the circle obtained by projecting 
C     the trajectory on the x,y plane and the distance between C and the new origin N
C
      XC = (DOLD-R)*DSIN(PHIOLD)
      YC = -(DOLD-R)*DCOS(PHIOLD)
C
C     Compute new impact parameter
C
      CN = DSQRT((XN-XC)**2+(YN-YC)**2)
      IF (R.GT.0.D0) THEN
         D = R-CN
      ELSE 
         D = R+CN
      ENDIF 
C
C     Compute the new angle phi
C
      PHI = PHIOLD + DATAN2((-XC*(YN-YC)+YC*(XN-XC)),
     &                      (-XC*(XN-XC)-YC*(YN-YC)))
C
C     Compute the new error on new impact parameter D
C
      DCNXC     = (XC-XN)/CN
      DCNYC     = (YC-YN)/CN
      DXCR      =(R**2)*DSIN(PHIOLD)
      DXCDOLD   = DSIN(PHIOLD)
      DXCPHIOLD = (DOLD-R)*DCOS(PHIOLD)
      DYCR      = -(R**2)*DCOS(PHIOLD)
      DYCDOLD   = -DCOS(PHIOLD)
      DYCPHIOLD = (DOLD-R)*DSIN(PHIOLD) 
C
      ISTAT = 0
      IF (R.GT.10.D0) THEN
        DDR      = -R**2-DCNXC*DXCR   - DCNYC*DYCR
        DDDOLD   =  -DCNXC*DXCDOLD   - DCNYC*DYCDOLD
        DDPHIOLD =  -DCNXC*DXCPHIOLD - DCNYC*DYCPHIOLD
      ELSE IF (R.LT.-10.D0) THEN
        DDR      = -R**2+DCNXC*DXCR   + DCNYC*DYCR
        DDDOLD   =  +DCNXC*DXCDOLD   + DCNYC*DYCDOLD
        DDPHIOLD =  +DCNXC*DXCPHIOLD + DCNYC*DYCPHIOLD
      ELSE 
        ISTAT =1
      ENDIF
C
      IF(ISTAT.EQ.0) THEN
      VARD = (DDR**2)*VAR(JIR,JIR) + (DDPHIOLD**2)*VAR(JP0,JP0) +
     &  (DDDOLD**2)*VAR(JD0,JD0) + 2*DDR*DDPHIOLD*VAR(JP0,JIR)  +
     &  2*DDR*DDDOLD*VAR(JD0,JIR)+ 2*DDPHIOLD*DDDOLD*VAR(JP0,JD0)
      ELSE
        VARD=VAR(JD0,JD0)
        CALL RERROR('OCBPRE',6,'Track radius < 10 cm')
      ENDIF
C
C     Output arguments
C
      IEVT=ISIGN(IEVT,-1)
      PAR(JP0) = SNGL(PHI)
      PAR(JD0) = SNGL(D)
      ERR(6)   = SNGL(VARD)
      END
#endif
