      SUBROUTINE TCLANA
C
C-----------------------------------------------------------------------
C! Analyze a cluster to attempt separation into subclusters
C!
C!    Author:    R. Johnson   22-12-86
C!    Modified:  R. Johnson   19-01-88
C!
C!    Called from TCLCOR
C!
C!    Description
C!    -----------
C!    This routine is called once for each cluster.  For each pulse
C!    in the cluster it calls TPLANA to separate the pulse into
C!    subpulses and to analyze each subpulse.  It then calls
C!    TCLPAT to associate the subpulses into subclusters.  The
C!    parameters of each subcluster then are determined from
C!    the associated subpulses and filled into the ISCLUS workbank.
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tparac.h"
#include "tcluct.h"
#include "tclcow.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpccon.h"
C
      LOGICAL LFRPU
C
C------------------------ Statements function definitions --------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      NPULS=IW(ITPADW+LMHROW)
      IW(ITSPUW+LMHROW)=0
C
C++   Loop over and analyze each of the pulses in the cluster.
C
      IFPN=ITABL(ITPADW,1,JTPDPN)
      NPADS=0
      LSTPD=IFPN
      LSTSP=0
      DO 80 IP=1,NPULS
        IPADN=ITABL(ITPADW,IP,JTPDPN)
        IF (IPADN.NE.LSTPD) THEN
          LSTPD=IPADN
          NPADS=NPADS+1
          IPIDXW(JIPDOU,NPADS)=LSTSP
          LSTSP=0
        ENDIF
        CALL TPLANA(IP,LSTSP)
   80 CONTINUE
      NPADS=NPADS+1
      IPIDXW(JIPDOU,NPADS)=LSTSP
C
C++   Call routine to associate subpulses into subclusters
C
      CALL TCLPAT(NPADS,IFPN)
C
C++   Store in BOS banks all subpulses and subclusters for only those
C++   subclusters with at least two subpulses.
C
      NTSPU=0
      DO 155 ISC=1,LROWS(ISCLUS)
        NSP=ITABL(ISCLUS,ISC,JICLNS)
        IF (NSP.LT.2) GO TO 155
C
        IQMXR=0
        IQMXZ=0
        MXPK=0
        ICSAT=0
C
C++     Initialize variables for subcluster pulse height sums
C
        JCHT=0
        JSXB=0
        JSYB=0
        JSXYB=0
        JSX2B=0
        JSY2B=0
        ICHOP=0
C
C++     Loop over all subpulses in this subcluster
C
        NTSPU=NTSPU+NSP
        DO 154 J=1,NSP
          ISP= IW(ITPRTW+J+ITABL(ISCLUS,ISC,JICLOF))
C
C++       Check for subpulses hanging off the TPC end
C
          I1=ITABL(ITSPUW,ISP,JSPWOF)
          IT1=ITABL(ITSPUW,ISP,JSPWT0)+ITABL(ITSPUW,ISP,JSPWOF)
          Z1=(FLOAT(IT1)-TVOFFS(ISLTCL))*TVDRFT(IENDTP(ISLTCL))
          IF (Z1.LT.RTLCZM) ICHOP=1000000
C
C++       Accumulate sums for subcluster width and length
C
          JCHT=JCHT + ITABL(ITSPUW,ISP,JSPWTC)
          JSXB=JSXB + ITABL(ITSPUW,ISP,JSPWSZ)
          JSX2B=JSX2B + ITABL(ITSPUW,ISP,JSPWS2)
          JSXYB=JSXYB + J*ITABL(ITSPUW,ISP,JSPWSZ)
          JSYB=JSYB + J*ITABL(ITSPUW,ISP,JSPWTC)
          JSY2B=JSY2B + J*J*ITABL(ITSPUW,ISP,JSPWTC)
C
C++       Unpack pulse quality word and use to set subcluster quality
C++       and subcluster algorithm limitations.
C
          IQW=ITABL(ITSPUW,ISP,JSPWQF)
C
C++       If only one subpulse has a saturated sample, then the
C++       subcluster is considered saturated.
C
          ISAT=(IQW/100000)*100000
          IF (ISAT.GT.0) ICSAT=100000
C
C++       Check all subpulses for overlap with ph. on adjacent pads
C
          IQW=IQW-ISAT
          IQS=IQW/10000
          IF (IQS.GT.IQMXR) IQMXR=IQS
C
C++       Check all subpulses for overlap in z
C
          IQW=IQW-IQS*10000
          IQZ=IQW/1000
          IF (IQZ.GT.IQMXZ) IQMXZ=IQZ
C
C++       The subcluster is multiple peaked in z if any subpulse is so.
C
          NPEAK=IQW-1000*IQZ
          IF (NPEAK.GT.MXPK) MXPK=NPEAK
  154   CONTINUE
C
C++     Conclude calculations of cluster widths.  Note that we are NOT
C++     accounting for the smaller size of half pads.
C
        RCHT=FLOAT(JCHT)
        RSY2B=FLOAT(JSY2B)/RCHT
        RSYB=FLOAT(JSYB)/RCHT
        RSX2B=FLOAT(JSX2B)/RCHT
        RSXB=FLOAT(JSXB)/RCHT
        RSXYB=FLOAT(JSXYB)/RCHT
        RW(KROW(ISCLUS,ISC)+JICLRW)= RSY2B-RSYB**2
        RW(KROW(ISCLUS,ISC)+JICLZW)= RSX2B-RSXB**2
        RW(KROW(ISCLUS,ISC)+JICLCV)= RSXYB-RSYB*RSXB
C
C++     Check whether usable charge and time algorithms exist.
C++     If any of the subpulses are overlapping with other
C++     pulses outside the subcluster, then the program will
C++     try to switch to center algorithms, if requested by the
C++     user.  Otherwise it will either say there is no suitable
C++     estimator, or it will go ahead with the default estimates,
C++     according to the flag FTLCAF.
C
        JTALG=ITLCTA(JTANLM)
        JCALG=ITLCCA(JTANLM)
        IF (IQMXR.GT.0 .OR. IQMXZ.GT.0) THEN
          IF (.NOT.FTLCAF) THEN
            IF (JCALG.NE.1) THEN
              IF (FTLCOO(JTANLM)) THEN
                JCALG=1
              ELSE
                JCALG=0
              ENDIF
            ENDIF
            IF (JTALG.NE.1) THEN
              IF (FTLCOO(JTANLM)) THEN
                JTALG=1
              ELSE
                JTALG=0
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
C++     Store subcluster algorithm word and update the quality word
C
        IW(KROW(ISCLUS,ISC)+JICLCA)=JCALG
        IW(KROW(ISCLUS,ISC)+JICLTA)=JTALG
        LMP=MXPK-1
        IF (LMP.GT.9) LMP=9
        IW(KROW(ISCLUS,ISC)+JICLQF)=ITABL(ISCLUS,ISC,JICLQF)
     &                     + ICSAT + IQMXZ + 100*LMP + ICHOP
  155 CONTINUE
C
      RETURN
      END
#endif
