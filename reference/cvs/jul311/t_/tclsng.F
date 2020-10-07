      SUBROUTINE TCLSNG
C
C-----------------------------------------------------------------------
C! Quick cluster analysis
C!
C!    Author:    R. Johnson   04-02-88
C!    Modified:  R. Johnson   31-05-88
C!
C!    Called by TCLCOR
C!
C!    Description
C!    ===========
C!    This routine sets up the subcluster and subpulse workbanks of
C!    common TCLCOW under the assumption that the cluster consists
C!    of only a single subcluster (only one track).
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "bcs.h"
#include "tclcow.h"
#include "tcluct.h"
C
      CHARACTER TEXT*130
C
C-------------------------- Statement Function Definitions ------------
C
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
C++   Make a single subcluster
C
      IW(ISCLUS+LMHROW)=1
      IW(KROW(ISCLUS,1)+JICLNS)=LROWS(ITPADW)
      IW(KROW(ISCLUS,1)+JICLOF)=0
      IW(KROW(ISCLUS,1)+JICLPN)=ITABL(ITPADW,1,JTPDPN)
      IW(KROW(ISCLUS,1)+JICLCA)=ITLCCA(JTANLM)
      IW(KROW(ISCLUS,1)+JICLTA)=ITLCTA(JTANLM)
      IF ((LROWS(ITPADW)*LENSPW+LMHLEN).GT.IW(ITSPUW)) THEN
        WRITE(TEXT,1101) LROWS(ITSPUW)
 1101   FORMAT('Extending workbank ITSPUW.',
     &         ' CURRENT LENGTH=',I3,' SUBPULSES.&',
     &         ' Increase MXSBCL in TCLCOW if this occurs',
     &         ' too often.')
        CALL RERROR('TCLSNG',1,TEXT)
        MXSP=LROWS(ITPADW)+4
        LEN= MXSP*LENSPW+LMHLEN
        IW(1)=1
        CALL WBANK(IW,ITSPUW,LEN,*998)
        IW(1)=1
        CALL WBANK(IW,ITPRTW,MXSP,*998)
      ENDIF
      IW(ITSPUW+LMHROW)=LROWS(ITPADW)
C
C++   Make a single subpulse from each pulse
C
      JCHT=0
      JSXB=0
      JSYB=0
      JSXYB=0
      JSX2B=0
      JSY2B=0
      ISAT=0
      DO 100 IP=1,LROWS(ITPADW)
        IW(ITPRTW+IP)=IP
        NOVER=0
        IPHMX=0
        IPHSM=0
        IPHZ=0
        IPHZ2=0
        I1=1
        ITOFF=ITABL(ITPADW,IP,JTPDT0)-JFSTBK-1
C
C++     For each pulse, find the initial and final threshold crossings,
C++     the peak position, and the pulse height sums.
C
        NS=ITABL(ITPADW,IP,JTPDNS)
        DO 50 IS=1,NS
          IPH=IW(ITPDIW+ITABL(ITPADW,IP,JTPDOF)+IS)
          IF (IPH.GT.IPHMX) THEN
            IPHMX=IPH
            ISMX=IS
          ENDIF
          IF (NOVER.EQ.0) THEN
            IF (IPH.GT.ITLCPT) THEN
              NOVER=NOVER+1
              I1=IS
              IPHSM=IPHSM + IPH
              JT=IS+ITOFF
              IPHZ=IPHZ + JT*IPH
              IPHZ2=IPHZ2 + JT*JT*IPH
            ENDIF
          ELSE
            IF (IPH.LE.ITLCPT .AND. NOVER.GE.ITLCML) THEN
              I2=IS-1
              GO TO 51
            ELSE
              NOVER=NOVER+1
              IPHSM=IPHSM + IPH
              JT=IS+ITOFF
              IPHZ=IPHZ + JT*IPH
              IPHZ2=IPHZ2 + JT*JT*IPH
            ENDIF
          ENDIF
   50   CONTINUE
        I2=NS
   51   CONTINUE
C
C++     Fill subpulse information into the workbank
C
        IW(KROW(ITSPUW,IP)+JSPWT0)=ITABL(ITPADW,IP,JTPDT0)+I1-1
        IW(KROW(ITSPUW,IP)+JSPWNS)=I2-I1+1
        IW(KROW(ITSPUW,IP)+JSPWOF)=ITABL(ITPADW,IP,JTPDOF)+I1-1
        IW(KROW(ITSPUW,IP)+JSPWPK)=ISMX-I1+1
        IW(KROW(ITSPUW,IP)+JSPWTC)=IPHSM
        IW(KROW(ITSPUW,IP)+JSPWSZ)=IPHZ
        IW(KROW(ITSPUW,IP)+JSPWS2)=IPHZ2
        IF (IPHMX.GE.LVLSAT) THEN
          ISAT=100000
          IW(KROW(ITSPUW,IP)+JSPWQF)=100000
        ELSE
          IW(KROW(ITSPUW,IP)+JSPWQF)=0
        ENDIF
        IW(KROW(ITSPUW,IP)+JSPWPF)=0
        IW(KROW(ITSPUW,IP)+JSPWPU)=IP
C
C++     Sums for the subcluster widths
C
        JCHT=JCHT + IPHSM
        JSXB=JSXB + IPHZ
        JSX2B=JSX2B + IPHZ2
        JSXYB=JSXYB + IP*IPHZ
        JSYB=JSYB + IP*IPHSM
        JSY2B=JSY2B + IP*IP*IPHSM
  100 CONTINUE
C
C++   Complete the calculation of the subcluster widths
C
      RCHT=FLOAT(JCHT)
      RSY2B=FLOAT(JSY2B)/RCHT
      RSYB=FLOAT(JSYB)/RCHT
      RSX2B=FLOAT(JSX2B)/RCHT
      RSXB=FLOAT(JSXB)/RCHT
      RSXYB=FLOAT(JSXYB)/RCHT
      RW(KROW(ISCLUS,1)+JICLRW)= RSY2B-RSYB**2
      RW(KROW(ISCLUS,1)+JICLZW)= RSX2B-RSXB**2
      RW(KROW(ISCLUS,1)+JICLCV)= RSXYB-RSYB*RSXB
      IW(KROW(ISCLUS,1)+JICLQF)= ISAT
      GO TO 999
  998 CONTINUE
      CALL RERROR('TCLSNG',2,'No room to extend workbank ITSPUW')
  999 CONTINUE
      RETURN
      END
#endif
