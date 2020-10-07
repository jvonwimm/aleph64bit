      SUBROUTINE TCLPAT(NPADS,IFPN)
C
C--------------------------------------------------------------------
C! Associate subpulses into subclusters
C!
C!    Author:   R. Johnson    14-04-87
C!    Modified: R. Johnson    19-01-88
C!
C!    Input:
C!        - NPADS    /I     Number of pads in the cluster
C!        - IFPN     /I     Pad number of first pad in cluster
C!
C!    Called by TCLANA
C!
C!    Description
C!    -----------
C!    This routine is called once for each cluster once the subpulses
C!    have been found and analyzed, the subpulses being stored in the
C!    workbank ITSPUW (see TCLCOW).  Starting at one end in r*phi of
C!    the cluster, for each subpulse it tries to find a match on
C!    the next pad.  A match is determined by the relative z positions
C!    of the subpulse peaks.  Once no more subpulses can be added,
C!    then the program goes back over those which have been picked up,
C!    and after first checking that the boundary subpulses do not
C!    overlap too much with pulses on the adjacent pads, the program
C!    analyzes the pulse height profile in r*phi.  All maximum
C!    and minimum points are marked, and then the group of subpulses
C!    is divided at each mimimum if it is low enough with respect to
C!    the adjacent peaks, resulting in the final subclusters.  The
C!    subclusters are marked as bad if they contain minima at which
C!    they could not be further divided.
C!
C--------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "bcs.h"
#include "tcluct.h"
#include "tclcow.h"
C
C------------------------ local variables -----------------------------
C
      LOGICAL LOVLP(2),NXBAD
      DIMENSION IOVLP(2),INFL(LMXPCL)
      CHARACTER TEXT*140
C
C------------------------ Statements function definitions --------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      ISP=0
      IW(ISCLUS+LMHROW)=0
C
C++   Loop over pads from which to start the subcluster.
C
      JFPAD=1
   84 IF (JFPAD.GE.NPADS) GO TO 91
C
C++     Loop over unused subpulses on this initial pad.
C++     JPST markes the pad on which the subcluster on which we are
C++     working begins, while JFPAD is kept updated to point to the
C++     first pad with unused subpulses.  JPST can differ from JFPAD
C++     in the case where a set of matching subpulses is split into
C++     more than one subcluster.   ISP marks the point in the
C++     ITSPU array just before where the next subpulse will go.
C
        JPST=JFPAD
        ISTRT=IPIDXW(JIPDOU,JPST)
  186   IF (ISTRT.EQ.0) GO TO 86
C
C++       Save offset (ISPI) to pointer to the initial subpulse
C
          ISPI=ISP
          ISP=ISP+1
          IW(ITPRTW+ISP)=ISTRT
C
C++       Define the reference time for a pulse to be the
C++       average of the leading and trailing edge times
C
          IZREF=ITABL(ITSPUW,ISTRT,JSPWNS)/2
     &                       + ITABL(ITSPUW,ISTRT,JSPWT0)
          MXPH=ITABL(ITSPUW,ISTRT,JSPWTC)
C
C++       Loop over following pads until one is found which has no
C++       matching subpulse
C
          ISPL2=ISTRT
          IPAD2=JPST
   85     IF (IPAD2.GE.NPADS) GO TO 93
            ISPL1=ISPL2
            IPAD1=IPAD2
            IPAD2=IPAD1+1
C
C++         Loop over unused subpulses on this pad and find the
C++         one closest in z to the subpulse on previous pad.
C
            MINDZ=9999
            ISTRY=IPIDXW(JIPDOU,IPAD2)
  187       IF (ISTRY.EQ.0) GO TO 87
              IZNEW= ITABL(ITSPUW,ISTRY,JSPWNS)/2
     &                        + ITABL(ITSPUW,ISTRY,JSPWT0)
              IZDIF= IABS(IZNEW - IZREF)
              IF (IZDIF.LT.MINDZ) THEN
                MINDZ=IZDIF
                ISPL2=ISTRY
                IZSAV=IZNEW
              ENDIF
              ISTRY=ITABL(ITSPUW,ISTRY,JSPWPF)
              GO TO 187
   87       CONTINUE
C
C++         Check for a match using the z-window cut
C
            IF (MINDZ.LE.JWNDOW) THEN
              IZREF=IZSAV
              MXPH=MAX(MXPH,ITABL(ITSPUW,ISPL2,JSPWTC))
C
C++           Remove subpulse from the list of unused ones
C
              IFWD=ITABL(ITSPUW,ISPL2,JSPWPF)
              IBCK=ITABL(ITSPUW,ISPL2,JSPWPB)
              IF (IBCK.NE.0) THEN
                IW(KROW(ITSPUW,IBCK)+JSPWPF)=IFWD
              ELSE
                IPIDXW(JIPDOU,IPAD2)=IFWD
              ENDIF
              IF (IFWD.NE.0) THEN
                IW(KROW(ITSPUW,IFWD)+JSPWPB)=IBCK
              ENDIF
C
C++           Save pointer to the subpulse
C
              ISP=ISP+1
              IW(ITPRTW+ISP)=ISPL2
C
C++           Move on to the next pad
C
              GO TO 85
            ENDIF
C
C++         We failed to find a match on this pad, so the subcluster
C++         is complete and we exit the loop over pads.
C
   93     CONTINUE
C
C++       Find number of subpulses picked up.  Subclusters of only
C++       a single subpulse are not treated further.
C
          NSP=ISP-ISPI
          IF (NSP.EQ.1) THEN
            ISP=ISP-1
          ELSE
C
C++         Check the first and last subpulses for excessive overlap
C++         with pulses on adjacent pads outside this subcluster
C
            LOVLP(1)=.FALSE.
            LOVLP(2)=.FALSE.
            IF (ITLCOC.GT.0) THEN
              DO 149 ISIDE=1,2
                IF (ISIDE.EQ.1) THEN
                  IPDN=JPST-1
                  IF (IPDN.LT.1) GO TO 149
                  ISP1=IW(ITPRTW+ISPI+1)
                ELSE
                  IPDN=JPST+NSP
                  IF (IPDN.GT.NPADS) GO TO 149
                  ISP1=IW(ITPRTW+ISP)
                ENDIF
C
C++             Check overlap with all pulses on adjacent pad
C
                DO 147 II=1,IPIDXW(JIPDNP,IPDN)
                  IP2=II + IPIDXW(JIPDOP,IPDN)
                  IT0=ITABL(ITPADW,IP2,JTPDT0)
                  NSMP=ITABL(ITPADW,IP2,JTPDNS)
                  IT1=IT0
                  IT2=IT0+NSMP-1
                  JT1=ITABL(ITSPUW,ISP1,JSPWT0)
                  JT2=JT1 + ITABL(ITSPUW,ISP1,JSPWNS) - 1
                  IOVR2=MIN(IT2,JT2)
                  IOVR1=MAX(IT1,JT1)
                  JVRLP=MIN(IT2,JT2)-MAX(IT1,JT1)
                  IF (JVRLP.GT.ITLCOP) THEN
C
C++                 In case of significant overlap, check the ph
C++                 of the overlap region.
C
                    IQF=MOD(ITABL(ITSPUW,ISP1,JSPWQF)/1000,10)
                    IF (IQF.GT.1) THEN
                      JCALG=1
                    ELSE
                      JCALG=ITLCCA(JTANLM)
                    ENDIF
                    IF (JCALG.EQ.2) THEN
C
C++                   If the area algorithm is usable (so far) for
C++                   the subpulse, then we add up and check the
C++                   charge overlapping with the full subpulse.
C
                      I1=MAX(1,IOVR1-IT0+1)
                      I2=MIN(NSMP,IOVR2-IT0+1)
                      IPHSM=0
                      DO 151 KK=I1,I2
                        IS=KK+ITABL(ITPADW,IP2,JTPDOF)
                        IPHSM=IPHSM+IW(ITPDIW+IS)
  151                 CONTINUE
C
C++                   If the overlap with full pulse is too great, then
C++                   if the user will allow the algorithm to be
C++                   switched to a center estimate, we must check for
C++                   overlap of pulse height with just the 3 center
C++                   samples.  Otherwise mark the subpulse no good.
C
                      IF (IPHSM
     &                      .GT. ITABL(ITSPUW,ISP1,JSPWTC)/ITLCOC) THEN
                        IF (FTLCOO(JTANLM)) THEN
                          JCALG=1
                          IW(KROW(ITSPUW,ISP1)+JSPWQF)
     &                           = ITABL(ITSPUW,ISP1,JSPWQF)+10000
                        ELSE
                          LOVLP(ISIDE)=.TRUE.
                          IOVLP(ISIDE)=ISP1
                          GO TO 149
                        ENDIF
                      ENDIF
                    ENDIF
C
C++                 For cases where only the peak algorithm is useable
C++                 for the charge, check just 3 samples about maximum.
C
                    IF (JCALG.EQ.1) THEN
                      IPEAK=ITABL(ITSPUW,ISP1,JSPWPK)
     &                             + ITABL(ITSPUW,ISP1,JSPWT0)
                      IPKCH=IW(ITPDIW+ITABL(ITSPUW,ISP1,JSPWOF)
     &                                    + ITABL(ITSPUW,ISP1,JSPWPK))
                      DO 153 KK=1,3
                        IS=(IPEAK-IT0-1) + KK
                        IF (IS.LT.1) GO TO 153
                        IF (IS.GT.NSMP) GO TO 154
                        IS=IS+ITABL(ITPADW,IP2,JTPDOF)
C
C++                     If the overlap with the peak is too large, then
C++                     flag this subcluster as bad.
C
                        IF (IW(ITPDIW+IS).GT.IPKCH/ITLCOC) THEN
                          LOVLP(ISIDE)=.TRUE.
                          IOVLP(ISIDE)=ISP1
                          GO TO 149
                        ENDIF
  153                 CONTINUE
  154                 CONTINUE
C
C++                   The subpulse peak is OK, but note that it is
C++                   overlapping at its ends.
C
                      IW(KROW(ITSPUW,ISP1)+JSPWQF)
     &                           = ITABL(ITSPUW,ISP1,JSPWQF)+1000
                    ENDIF
                  ENDIF
  147           CONTINUE
  149         CONTINUE
            ENDIF
C
C++         Analyse the r*phi profile of the subcluster.
C++         First, find the maxima and minima.  INFL marks the position
C++         of each inflection point.  MOFF=2 means the first is a
C++         maximum and MOFF=3 means it is a minimum.
C++         We don't bother with this for very long or short subclusters
C++         We don't bother with this for very long or short subclusters
C
            IF (NSP.GT.3 .AND. NSP.LE.ITLCFA) THEN
              NINFL=1
              INFL(1)=ISPI+1
              IPH1=ITABL(ITSPUW,IW(ITPRTW+INFL(1)),JSPWTC)
C
              ICASE=1
              DO 96 K=2,NSP
                KSP=ISPI+K
                IPH=ITABL(ITSPUW,IW(ITPRTW+KSP),JSPWTC)
                GO TO (951,952,953), ICASE
C
  951           CONTINUE
                  IF (IPH.GT.IPH1) THEN
                    ICASE=2
                    MOFF=3
                    IPHMX=IPH
                  ELSEIF (IPH.LT.IPH1) THEN
                    ICASE=3
                    MOFF=2
                    IPHMN=IPH
                  ENDIF
                  GO TO 96
  952           CONTINUE
                  IF (IPH.LT.IPHMX) THEN
                    ICASE=3
                    IPHMN=IPH
                    NINFL=NINFL+1
                    INFL(NINFL)=KSP-1
                  ELSEIF (IPH.GT.IPHMX) THEN
                    IPHMX=IPH
                  ENDIF
                  GO TO 96
  953           CONTINUE
                  IF (IPH.GT.IPHMN) THEN
                    ICASE=2
                    IPHMX=IPH
                    NINFL=NINFL+1
                    INFL(NINFL)=KSP-1
                  ELSEIF (IPH.LT.IPHMN) THEN
                    IPHMN=IPH
                  ENDIF
   96         CONTINUE
              NINFL=NINFL+1
              INFL(NINFL)=ISPI+NSP
            ELSE
              NINFL=3
              MOFF=3
            ENDIF
C
C++         Loop over the minima and try to divide up the subcluster.
C++         LBRK is used to flag whether the subcluster has been divided
C++         NMIN counts the number of substantial minima.
C
            LBRK=0
            NMIN=0
            NXBAD=.FALSE.
            DO 99 K=MOFF,NINFL-1,2
C
              IC2=ITABL(ITSPUW,IW(ITPRTW+INFL(K)),JSPWTC)
              ITHRU= (IC2*ITLCRM)/10
              IC1=ITABL(ITSPUW,IW(ITPRTW+INFL(K-1)),JSPWTC)
              IC3=ITABL(ITSPUW,IW(ITPRTW+INFL(K+1)),JSPWTC)
              MINC=NMIN
              IF (IC1.GE.ITHRU .AND. IC3.GE.ITHRU) THEN
                NMIN=NMIN+1
              ENDIF
C
              IPTST=IC2*ITLCPM
              IF (IC1.GT.IPTST .OR. IC3.GT.IPTST) THEN
C
C++             Here we have a small enough dip to divide subcluster.
C++             Don't allow the minimum pulse to be part of either
C++             subcluster.  Don't store any subclusters with only
C++             one subpulse.
C
                NINCL=INFL(K)-ISPI-1
                IF (NINCL.GE.2) THEN
                  IW(ISCLUS+LMHROW)=LROWS(ISCLUS)+1
                  NSCLU=LROWS(ISCLUS)
                  IF ((NSCLU*LENICL)+LMHLEN.GT.IW(ISCLUS)) THEN
                    WRITE(TEXT,1101) LROWS(ISCLUS)
 1101               FORMAT('Extending workbank ISCLUS.',
     &                     ' CURRENT LENGTH=',I3,' SUBCLUSTERS.&',
     &                     ' Increase MXSBCL in TCLCOW if this occurs',
     &                     ' too often.')
                    CALL RERROR('TCLPAT',1,TEXT)
                    LEN= (NSCLU+10)*LENICL+LMHLEN
                    IW(1)=1
                    CALL WBANK(IW,ISCLUS,LEN,*998)
                  ENDIF
C
C++               Handle the case where the first subpulse overlaps
C++               too much with ph. on the adjacent pad.
C
                  IF (LBRK.EQ.0 .AND. LOVLP(1)) THEN
                    ICHG=ITABL(ITSPUW,IOVLP(1),JSPWTC)
                    IF (NINCL.GT.2 .AND. ICHG.LE.IC1/ITLCPM) THEN
C
C++                   Here we will have a valid subcluster
C++                   if only we drop the edge subpulse
C
                      NINCL=NINCL-1
                      ISPI=ISPI+1
                      JPST=JPST+1
                      IOVQU=0
                    ELSE
                      IOVQU=10000
                    ENDIF
                  ELSE
                    IOVQU=0
                  ENDIF
C
C++               Is this subcluster large enough relative to the
C++               minima between it and the preceding subcluster?
C
                  IF (NXBAD .OR. IC1.LE.IPTST) IOVQU=10000
C
                  IW(KROW(ISCLUS,NSCLU)+JICLNS)=NINCL
                  IW(KROW(ISCLUS,NSCLU)+JICLOF)=ISPI
                  IW(KROW(ISCLUS,NSCLU)+JICLPN)=IFPN + (JPST-1)
                  MINC=MIN(MINC,9)
                  IW(KROW(ISCLUS,NSCLU)+JICLQF)=IOVQU+10+1000*MINC
                ENDIF
                LBRK=10
C
C++             Update pointers to beginning of the next subcluster.
C
                NMIN=0
                NXBAD= IC3.LE.IPTST
                JPST=JPST+INFL(K)-ISPI
                ISPI=INFL(K)
              ENDIF
   99       CONTINUE
C
C++         Fill in the pointers for the last (or only) subcluster
C
            NINCL=ISP-ISPI
            IF (NINCL.GE.2) THEN
              IW(ISCLUS+LMHROW)=LROWS(ISCLUS)+1
              NSCLU=LROWS(ISCLUS)
              IF ((NSCLU*LENICL)+LMHLEN.GT.IW(ISCLUS)) THEN
                WRITE(TEXT,1101) LROWS(ISCLUS)
                CALL RERROR('TCLPAT',1,TEXT)
                LEN= (NSCLU+10)*LENICL+LMHLEN
                IW(1)=1
                CALL WBANK(IW,ISCLUS,LEN,*998)
              ENDIF
C
C++           Handle the case where the edge subpulses overlap
C++           too much with ph. on the adjacent pad.
C
              IOVQU=0
              IF (LOVLP(2)) THEN
                ICHG=ITABL(ITSPUW,IOVLP(2),JSPWTC)
                IF (LBRK.GT.0) MXPH=IC3
                IF (NINCL.GT.2 .AND. ICHG.LE.MXPH/ITLCPM) THEN
C
C++               Here we will have a valid subcluster
C++               if only we drop the edge subpulse
C
                  NINCL=NINCL-1
                ELSE
                  IOVQU=10000
                ENDIF
              ENDIF
              IF (LOVLP(1) .AND. LBRK.EQ.0) THEN
                ICHG=ITABL(ITSPUW,IOVLP(1),JSPWTC)
                IF (NINCL.GT.2 .AND. ICHG.LE.MXPH/ITLCPM) THEN
                  NINCL=NINCL-1
                  ISPI=ISPI+1
                  JPST=JPST+1
                ELSE
                  IOVQU=10000
                ENDIF
              ENDIF
C
C++           Is this subcluster large enough relative to the
C++           minima between it and the preceding subcluster?
C
              IF (NXBAD) IOVQU=10000
C
              IW(KROW(ISCLUS,NSCLU)+JICLNS)=NINCL
              IW(KROW(ISCLUS,NSCLU)+JICLOF)=ISPI
              IW(KROW(ISCLUS,NSCLU)+JICLPN)=IFPN + (JPST-1)
              NMIN=MIN(NMIN,9)
              IW(KROW(ISCLUS,NSCLU)+JICLQF)=IOVQU+LBRK+1000*NMIN
            ENDIF
          ENDIF
          ISTRT=ITABL(ITSPUW,ISTRT,JSPWPF)
C
C++       We are finished now with this set of matching subpulses.
C++       Now try to find another matching set, starting with the
C++       next free subpulse on the JFPAD.
C
          GO TO 186
   86   CONTINUE
C
C++     There are no more free subpulses on JFPAD, so we start looking
C++     for matches starting from the next pad.
C
        JFPAD=JFPAD+1
        GO TO 84
   91 CONTINUE
      GO TO 999
C
  998 CONTINUE
      IW(ISCLUS+LMHROW)=LROWS(ISCLUS)-1
      CALL RERROR('TCLPAT',2,'No room to extend workbank ISCLUS')
  999 CONTINUE
      RETURN
      END
#endif
