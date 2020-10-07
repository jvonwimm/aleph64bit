      SUBROUTINE TSCFIL(ICLUS,KTSCL,KTSPU)
C
C-----------------------------------------------------------------------
C! Fill subpulse and subcluster banks
C!
C!    Author:    R. Johnson   11-01-88
C!
C!    Modified:  D. Casper    05-06-96    Replace algorithm 1 with new code:
C!                                        same as #2, except first moment of
C!                                        pulse wrt the midpoint of the edges
C!                                        is used to correct the z value.
C!                                        Load the value of the first moment
C!                                        in an unused location of TSPU.
C!
C! Input:
C!     - ICLUS        /I     Cluster number in the bank KTCLU
C!     - KTSCL        /I     BOS index of the subcluster bank
C!     - KTSPU        /I     BOS index of the subpulse bank
C!
C! Called by TCLCOR
C!
C! Description
C! ===========
C! In this routine the information from the temporary workbanks ISCLUS
C! and ITSPUW is used to fill the subclusters for the current cluster
C! into the TSCL bank.  The TSPU bank is filled, and in doing so
C! the charge and time estimates for each subpulse are found using
C! the algorithms specified in ISCLUS.
C!
C!---------------------------------------------------------------------
#ifndef DOC 
#include "tspujj.h"
#include "tscljj.h"
C
#include "bcs.h"
#include "tparac.h"
#include "tclcow.h"
#include "tcluct.h"

      LOGICAL INIT/.TRUE./
C
#include "bmacro.h"
C
C---------------------------------------------------------------------
C
C++   Loop over all subclusters found for this cluster
C
      DO 500 ISC=1,LROWS(ISCLUS)
C
C++     Skip subclusters with only one subpulse
C
        IF (ITABL(ISCLUS,ISC,JICLNS).LT.2) GO TO 500
        IW(KTSCL+LMHROW)=IW(KTSCL+LMHROW)+1
        NSCL=IW(KTSCL+LMHROW)
        IW(KROW(KTSCL,NSCL)+JTSCNS)= ITABL(ISCLUS,ISC,JICLNS)
        IW(KROW(KTSCL,NSCL)+JTSCPO)= IW(KTSPU+LMHROW)
        IW(KROW(KTSCL,NSCL)+JTSCFP)= ITABL(ISCLUS,ISC,JICLPN)
        IW(KROW(KTSCL,NSCL)+JTSCNC)= 0
        IW(KROW(KTSCL,NSCL)+JTSCCO)= 0
        IQALG=ITABL(ISCLUS,ISC,JICLCA)
        ITALG=ITABL(ISCLUS,ISC,JICLTA)
        IW(KROW(KTSCL,NSCL)+JTSCAW)= 1000*IQALG + ITALG
        IW(KROW(KTSCL,NSCL)+JTSCQU)= ITABL(ISCLUS,ISC,JICLQF)
        RW(KROW(KTSCL,NSCL)+JTSCSP)= SQRT(RTABL(ISCLUS,ISC,JICLRW))
        RW(KROW(KTSCL,NSCL)+JTSCSB)= SQRT(RTABL(ISCLUS,ISC,JICLZW))
        RW(KROW(KTSCL,NSCL)+JTSCCR)= RTABL(ISCLUS,ISC,JICLCV)
     &         / RTABL(KTSCL,NSCL,JTSCSP) / RTABL(KTSCL,NSCL,JTSCSB)
        IW(KROW(KTSCL,NSCL)+JTSCCL)= ICLUS
C
C++     Loop over all subpulses in this subcluster
C
        ICHT=0
        DO 400 J=1,ITABL(ISCLUS,ISC,JICLNS)
          ISP= IW(ITPRTW+J+ITABL(ISCLUS,ISC,JICLOF))
          ICHT=ICHT + ITABL(ITSPUW,ISP,JSPWTC)
C
          IW(KTSPU+LMHROW)=IW(KTSPU+LMHROW)+1
          NSP=IW(KTSPU+LMHROW)
          IW(KROW(KTSPU,NSP)+JTSPTC)= ITABL(ITSPUW,ISP,JSPWTC)
          IW(KROW(KTSPU,NSP)+JTSPQF)= ITABL(ITSPUW,ISP,JSPWQF)
          IP=ITABL(ITSPUW,ISP,JSPWPU)
          IW(KROW(KTSPU,NSP)+JTSPPN)= ITABL(ITPADW,IP,JTPDTP)
          IW(KROW(KTSPU,NSP)+JTSPSC)= NSCL
C
C++       For bad subclusters, store some simple values for the
C++       time and charge estimates.  These values will not be
C++       useful for coordinate calculation.
C
          ITOFF= ITABL(ITSPUW,ISP,JSPWT0)-1
          IPOFF= ITPDIW+ITABL(ITSPUW,ISP,JSPWOF)
          IF (ITABL(KTSCL,NSCL,JTSCQU).GE.100) THEN
            PHMAX=FLOAT(IW(IPOFF+ITABL(ITSPUW,ISP,JSPWPK)))
            T1= FLOAT(ITABL(ITSPUW,ISP,JSPWT0))
            T2= FLOAT(ITOFF+ITABL(ITSPUW,ISP,JSPWNS))
            TLEAD = T1
            TMAX= FLOAT(ITOFF+ITABL(ITSPUW,ISP,JSPWPK))
          ELSE
C
C++         Parabola algorithm for peak height and/or peak time
C
            IF (IQALG.EQ.1) THEN
              TOFF= FLOAT(ITOFF+ITABL(ITSPUW,ISP,JSPWPK))
              CALL TPARAB(TOFF,IW(IPOFF+ITABL(ITSPUW,ISP,JSPWPK)-1),
     &                                                TMAX,PHMAX)
            ELSEIF (IQALG.NE.1) THEN
              PHMAX=FLOAT(IW(IPOFF+ITABL(ITSPUW,ISP,JSPWPK)))
            ENDIF
C
C++         Interpolation of leading and trailing edges.  Time
C++         estimate is the average of the two interpolations.
C
            IF (ITALG.EQ.1 .OR. ITALG.EQ.2) THEN
C
C++           Threshold is a constant fraction of the average ph.
C
              THRSH=RTLCFT*FLOAT(ITABL(ITSPUW,ISP,JSPWTC))
     &                          / FLOAT(ITABL(ITSPUW,ISP,JSPWNS))
              IF (THRSH.LT.RTLCPT) THEN
                IS1=1
                IS2=ITABL(ITSPUW,ISP,JSPWNS)
                THRSH=RTLCPT
              ELSE
                IHRSH=INT(THRSH)
C
C++             Find the initial threshold crossing
C
                DO 76 IS1=1,ITABL(ITSPUW,ISP,JSPWNS)
                  IPHL2= IW(ITPDIW+ITABL(ITSPUW,ISP,JSPWOF)+IS1)
                  IF (IPHL2.GT.IHRSH) GO TO 77
   76           CONTINUE
   77           CONTINUE
C
C++             Find the final threshold crossing
C
                DO 86 IS2=ITABL(ITSPUW,ISP,JSPWNS),1,-1
                  IPHT2= IW(ITPDIW+ITABL(ITSPUW,ISP,JSPWOF)+IS2)
                  IF (IPHT2.GT.IHRSH) GO TO 87
   86           CONTINUE
   87           CONTINUE
              ENDIF
C
C++           Linearly interpolate to find the threshold crossings
C
              IF (IS1.EQ.1) THEN
                IF (ITABL(ITSPUW,ISP,JSPWOF)
     &             .GT. ITABL(ITPADW,IP,JTPDOF)) THEN
                  IPH1= IW(ITPDIW+ITABL(ITSPUW,ISP,JSPWOF))
                ELSE
                  IPH1= 0
                ENDIF
              ELSE
                IPH1=IW(ITPDIW+ITABL(ITSPUW,ISP,JSPWOF)+IS1-1)
              ENDIF
              IF (IPHL2.EQ.IPH1) THEN
                T1=FLOAT(ITOFF+IS1)
              ELSE
                T1= FLOAT(ITOFF+IS1-1)
     &              + (THRSH-FLOAT(IPH1))/FLOAT(IPHL2-IPH1)
              ENDIF
              IF (IS2.EQ.ITABL(ITSPUW,ISP,JSPWNS)) THEN
                IPLST= ITABL(ITPADW,IP,JTPDOF)+ITABL(ITPADW,IP,JTPDNS)
                IS= ITABL(ITSPUW,ISP,JSPWOF)+ITABL(ITSPUW,ISP,JSPWNS)
                IF (IS.NE.IPLST) THEN
                  IPH1= IW(ITPDIW+IS+1)
                ELSE
                  IPH1= 0
                ENDIF
              ELSE
                IPH1=IW(ITPDIW+ITABL(ITSPUW,ISP,JSPWOF)+IS2+1)
              ENDIF
              IF (IPHT2.EQ.IPH1) THEN
                T2= FLOAT(ITOFF+IS2+1)
              ELSE
                T2= FLOAT(ITOFF+IS2+1)
     &              - (THRSH-FLOAT(IPH1))/FLOAT(IPHT2-IPH1)
              ENDIF
              TMAX= 0.5*(T1+T2)
              IF (ITALG.EQ.1) THEN
C
C++  Calculate first moment of pulse w.r.t. the midpoint of the edges
C
                NSAMP = ITABL(ITSPUW,ISP,JSPWNS)
                SUMT0 = 0.
                SUMT1 = 0.
                DO IS=1,NSAMP
                  IQSAMP = IW(ITPDIW+ITABL(ITSPUW,ISP,JSPWOF)+IS)
                  SUMT0 = SUMT0 + IQSAMP
                  SUMT1 = SUMT1 + IQSAMP*(ITOFF+IS-TMAX)
                ENDDO
                TLEAD = SUMT1/SUMT0
              ELSE
                TLEAD = T1
              ENDIF
            ELSE
C
C++           Weighted mean for the time estimate
C
              IF (ITALG.EQ.3) THEN
                TMAX= FLOAT(JFSTBK) + FLOAT(ITABL(ITSPUW,ISP,JSPWSZ))
     &                             / FLOAT(ITABL(ITSPUW,ISP,JSPWTC))
              ENDIF
              T1= FLOAT(ITABL(ITSPUW,ISP,JSPWT0))
              T2= FLOAT(ITOFF+ITABL(ITSPUW,ISP,JSPWNS))
              TLEAD = T1
            ENDIF
          ENDIF
          RW(KROW(KTSPU,NSP)+JTSPT1)= T1
          RW(KROW(KTSPU,NSP)+JTSPT2)= T2
          RW(KROW(KTSPU,NSP)+JTSPIT)= TLEAD
          RW(KROW(KTSPU,NSP)+JTSPPT)= TMAX
          RW(KROW(KTSPU,NSP)+JTSPPC)= PHMAX
  400   CONTINUE
C
        IW(KROW(KTSCL,NSCL)+JTSCTC)=ICHT
  500 CONTINUE
C
      RETURN
      END
#endif
