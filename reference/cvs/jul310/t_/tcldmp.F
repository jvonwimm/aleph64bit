      SUBROUTINE TCLDMP(IC,ISLOT,IUNIT)
C
C----------------------------------------------------------------------
C! Dump a single TPC cluster
C!
C!    Author:   R. Johnson    6-05-87
C!
C!    Input:
C!        - IC         /I    Cluster index in TCLU
C!        - ISLOT      /I    Number of sector containing the cluster
C!        - IUNIT      /I    Logical unit number to dump to
C!
C!    Called by TCDUMP
C!
C!    Description
C!    ===========
C!    This routine dumps all information available about a single
C!    TPC cluster---raw data, subpulses, subclusters, and coordinates.
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tclujj.h"
#include "tpuljj.h"
#include "tpcojj.h"
#include "tcrljj.h"
#include "tlrljj.h"
#include "tscljj.h"
#include "tspujj.h"
#include "bcs.h"
C
      PARAMETER (MAXPR=25,MWPHP=16)
      CHARACTER*40 FMT
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
#include "tsfunc.h"
C
C----------------------------------------------------------------------
C
      KTCLU=NLINK('TCLU',ISLOT)
      IF (KTCLU.LE.0) RETURN
C
      KTPCO=IW(NAMIND('TPCO'))
      KTSCL=NLINK('TSCL',ISLOT)
      KTSPU=NLINK('TSPU',ISLOT)
      KTPUL=NLINK('TPUL',ISLOT)
      KTPAD=NLINK('TPAD',ISLOT)
      KTPDI=NLINK('TPDI',ISLOT)
C
      IF (NPULS(IC).EQ.0) RETURN
C
      IROW=IW(KROW(KTCLU,IC)+JTCLPN)
C
      WRITE(IUNIT,1001) IC,IROW
C
      MINBN=IW(KROW(KTCLU,IC)+JTCLFB)
      MAXBN=IW(KROW(KTCLU,IC)+JTCLLB)
      NN=MIN0(MAXBN,MINBN+MAXPR-1)
      WRITE(IUNIT,1002) (LL,LL=MINBN,NN)
C
C++   Loop over all pulses in the cluster and print the samples
C
      DO 80 IP=1,IW(KROW(KTCLU,IC)+JTCLNP)
        NN=5*(ITIME0(IC,IP)-MINBN)
        NN=MIN0(NN,5*MAXPR)
        IF (NN.LE.0) NN=0
        NN1=NN+1
        WRITE(FMT,'(''(1X,I5,'',I3,''X,I4,24I5)'')' ) NN1
        NN=MIN0(MAXPR-NN/5,NSAMP(IC,IP) )
        WRITE(IUNIT,FMT) IPADNR(IC,IP),
     1                     ( ISAMPH(IC,IP,IS),IS=1,NN )
C
   80 CONTINUE
C
C++   Now find all the subclusters for this cluster and print
C++   subclusters, subpulses, and coordinates
C
      IF (KTSCL.GT.0) THEN
        DO 86 ISCL=1,IW(KTSCL+LMHROW)
          IF (IW(KROW(KTSCL,ISCL)+JTSCCL).EQ.IC) THEN
            IF (IW(KROW(KTSCL,ISCL)+JTSCNC).GT.0) THEN
              IERR=0
            ELSE
              IERR=-IW(KROW(KTSCL,ISCL)+JTSCNC)
            ENDIF
            WRITE(IUNIT,4001) ISCL,IW(KROW(KTSCL,ISCL)+JTSCAW),
     &                               IW(KROW(KTSCL,ISCL)+JTSCQU),
     &                               IW(KROW(KTSCL,ISCL)+JTSCTC),IERR,
     &                               RW(KROW(KTSCL,ISCL)+JTSCSP),
     &                               RW(KROW(KTSCL,ISCL)+JTSCSB),
     &                               RW(KROW(KTSCL,ISCL)+JTSCCR)
            WRITE(IUNIT,4003)
            IPAD1=IABS(IW(KROW(KTSCL,ISCL)+JTSCFP))
            ISGN=ISIGN(1,IW(KROW(KTSCL,ISCL)+JTSCFP))
            DO 87 K=1,IW(KROW(KTSCL,ISCL)+JTSCNS)
              ISPL=K+IW(KROW(KTSCL,ISCL)+JTSCPO)
              IPADN=IPAD1+ISGN*(K-1)
              WRITE(IUNIT,4002) IPADN,ISPL,
     &                           IW(KROW(KTSPU,ISPL)+1),
     &                           (RW(KROW(KTSPU,ISPL)+J),J=2,6),
     &                           IW(KROW(KTSPU,ISPL)+7)
   87       CONTINUE
            IF (IW(KROW(KTSCL,ISCL)+JTSCNC).GT.0
     &                                 .AND. KTPCO.GT.0) THEN
              WRITE(IUNIT,4005)
              DO 88 K=1,IW(KROW(KTSCL,ISCL)+JTSCNC)
                ICO=K+IW(KROW(KTSCL,ISCL)+JTSCCO)
                RPHI=RW(KROW(KTPCO,ICO)+JTPCRV)*
     &                         RW(KROW(KTPCO,ICO)+JTPCPH)
                WRITE(IUNIT,4004)ICO,
     &                           RW(KROW(KTPCO,ICO)+JTPCRV),RPHI,
     &                           (RW(KROW(KTPCO,ICO)+L),L=4,6),
     &                           (IW(KROW(KTPCO,ICO)+L),L=7,8)
   88         CONTINUE
            ENDIF
          ENDIF
   86   CONTINUE
      ENDIF
C
C++   Print all unassociated subpulses
C
      IF (KTSPU.NE.0) THEN
        IFND=0
        DO 280 I=1,IW(KROW(KTCLU,IC)+JTCLNP)
          IP=I+IW(KROW(KTCLU,IC)+JTCLOP)
          DO 260 ISPL=1,IW(KTSPU+LMHROW)
            IF (IW(KROW(KTSPU,ISPL)+JTSPPN).EQ.IP) THEN
              IF (IW(KROW(KTSPU,ISPL)+JTSPSC).EQ.0) THEN
                IFND=IFND+1
                IF (IFND.EQ.1) WRITE(IUNIT,5003)
                IOFTP=IW(KROW(KTPUL,IP)+JTPUOP)
                IHTWD=IW(KTPAD+IOFTP+1)
                IPADN=IBITS(IHTWD,24,8)
                WRITE(IUNIT,4002) IPADN,ISPL,
     &                            IW(KROW(KTSPU,ISPL)+1),
     &                            (RW(KROW(KTSPU,ISPL)+J),J=2,6),
     &                            IW(KROW(KTSPU,ISPL)+7)
              ENDIF
            ENDIF
  260     CONTINUE
  280   CONTINUE
      ENDIF
      RETURN
C
 1001 FORMAT(/' *** Cluster ',I4,' in row ',I2,':')
 1002 FORMAT(6X,25(I5))
 4001 FORMAT(4X,'Subcluster ',I4,'  Algorithm=',I5,'  Quality=',I6,
     &       ' Charge=',I6/,5X,'IER=',I4,' Sigma-Pad=',E11.4,
     &       ' Sigma-Bucket=',E11.4,' Correlation=',E11.4)
 4003 FORMAT(7X,'Subpulses:'/
     &        11X,'Pad',4X,'I',5X,'TC ',7X,'PC',10X,'PT',10X,'IT',10X,
     &        'T1',10X,'T2',7X,'QF')
 5003 FORMAT(7X,'Unassociated subpulses:'/
     &        11X,'Pad',4X,'I',5X,'TC ',7X,'PC',10X,'PT',10X,'IT',10X,
     &        'T1',10X,'T2',7X,'QF')
 4002 FORMAT(10X,I3,2X,I4,3X,I5,5(1X,E11.4),1X,I6)
 4004 FORMAT(8X,I4,5(1X,E11.4),5X,I2,5X,3X,I3)
 4005 FORMAT(7X,'Coordinates:'/,11X,'I',7X,'R',10X,'R*Phi',7X,'Z',7X,
     &       'S**2 RPhi',5X,'S**2 Z',4X,'Origin',5X,'Track')
      END
#endif
