      SUBROUTINE VDMSEC(ITRAC)
C!-------------------------------------------------------------------
C!
C! Add rows to the VDMS 10000 bank to account for multiple scattering
C! in the VDET support rings
CKEY VDET TRACK
C!
C!    AUTHOR: G. Taylor   23.9.1992
C!
C!    INPUT: Track number of FRFT 0 track
C!    OUTPUT: VDMS 10000 bank with corresponding rows
C!
C!-------------------------------------------------------------------
#ifndef DOC
      SAVE UKZ,UKRI,UKRO,UKS
#include "vrldcom.h"
#include "bcs.h"
#include "vdmsjj.h"
#include "frftjj.h"
      PARAMETER (NRING=2)
      REAL UKZ(NRING), UKRI(NRING), UKRO(NRING), UKS(NRING)
      REAL ZSIGN(2)
      PARAMETER(RADCF=8.0)
      REAL SC, RC, XC, YC
      REAL   OMEGA, TANL, PHI0, D0, Z0, RI, RO,TINY
      REAL S(2),PHI(2),Z(2)
      LOGICAL FIRST
      DATA NAFRFT /0/
      DATA TINY / 1.E-3 /
      DATA FIRST /.TRUE./
#include "bmacro.h"
C-
C get the multiple scattering  material description from the database
C RETURN if VRLD bank is missing - job should STOP
C-
      IF(NAFRFT.EQ.0) NAFRFT = NAMIND ('FRFT')
      CALL VRLDGT( IER)
      IF(IER.LT.0) RETURN
C
      UKZ(1)   = UKZICA
      UKZ(2)   = UKZOCA
      UKRI(1)  = UKRIICA
      UKRI(2)  = UKRIOCA
      UKRO(1)  = UKROICA
      UKRO(2)  = UKROOCA
      UKS(1)   = UKSICA
      UKS(2)   = UKSOCA
C
      KFRFT=IW(NAFRFT)
      IF(KFRFT.LE.0) RETURN
      KVDMS=NLINK('VDMS',10000)
      IF(KVDMS.LE.0) RETURN
      OMEGA  = RTABL(KFRFT,ITRAC,JFRFIR)
      IF ( OMEGA .EQ. 0. ) OMEGA = .0000001
      TANL = RTABL(KFRFT,ITRAC,JFRFTL)
      PHI0  = RTABL(KFRFT,ITRAC,JFRFP0)
      D0  = RTABL(KFRFT,ITRAC,JFRFD0)
      Z0  = RTABL(KFRFT,ITRAC,JFRFZ0)
      ZSIGN(1)=1.
      ZSIGN(2)=-1.
C
C does this track pass through the VDET support rings
C there are 4 rings ( two with +z and two with -z)
C
      DO 10 IZ=1,2
       DO 10 ISR=1,2
        IF ( ABS(TANL) .GE. TINY ) THEN
         SC = ( ZSIGN(IZ)*UKZ(ISR) - Z0 ) / TANL
        ELSE
         SC = ( ZSIGN(IZ)*UKZ(ISR) - Z0 ) / TINY
        ENDIF
        IF(SC.GT.0.002) THEN
         SC = SC - 0.001
         XC =  SIN(OMEGA*SC+PHI0)/OMEGA + (D0-1./OMEGA)*SIN(PHI0)
         YC = -COS(OMEGA*SC+PHI0)/OMEGA - (D0-1./OMEGA)*COS(PHI0)
         RC = SQRT( XC*XC + YC*YC )
         IF ( RC .GT. UKRI(ISR) .AND. RC .LT. UKRO(ISR) ) THEN
          IROW=IW(KVDMS+LMHROW)+1
C flag that this is not a wafer
          IW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMWI)=-1
          IW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMFL)=3
          RW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMRA)=RC
          RW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMUC)=0.
          RW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMWC)=0.
          RW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMPV)
     &                           = SQRT(TANL**2/(1+TANL**2))
          RW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMPU)=1.
          RW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMPW)=1.
          RW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMCU)=1.
          RW(KVDMS+LMHLEN+(IROW-1)*IW(KVDMS+1)+JVDMSG)=UKS(ISR)
          IW(KVDMS+LMHROW)=IROW
         ENDIF
        ENDIF
 10   CONTINUE
      RETURN
      END
#endif
