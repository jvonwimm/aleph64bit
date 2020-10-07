      SUBROUTINE TPLANA(IP,LSTSP)
C
C-----------------------------------------------------------------------
C! Analyze a TPC pulse
C!
C!    Author:   R. Johnson   23-12-86
C!    Modified: R. Johnson   19-01-88
C!
C!    Input:
C!       - IP      /I      Pulse number in workbank ITPAD
C!    Output and Input:
C!       - LSTSP   /I      Pointer to the last subpulse found
C!                         on this pad
C!
C!    Called from TCLANA
C!
C!    Description:
C!    ------------
C!    This program is called for each TPC pulse in a cluster, where a
C!    pulse is defined by TPD zero suppression hardware and a cluster
C!    is a topologically connected group of pulses.  It considers
C!    separately groups of consecutive samples above a fixed software
C!    threshold.  The beginnings and endings of such groups are marked,
C!    and the points of all significant local maxima and minima are
C!    also marked, except for pulses at large dip angle (assuming
C!    a track coming from the origin), for which local maxima and
C!    minima are ignored. Each maximum along with its adjacent minima
C!    are then considered to form a separate subpulse if the maximum is
C!    sufficiently greater than the adjacent minima and not too much
C!    less than any adjacent maxima.  Precise definitions of the cuts
C!    are found in common TCLUCT.  The results are output into the
C!    workbank ITSPUW, which is described in comdeck TCLCOW.
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "bcs.h"
#include "tcluct.h"
#include "tclcow.h"
C
C------------------ local variables -----------------------------------
C
C     MXINF is a limit on the number of inflection points which can be
C     found on a single pulse.  ISM will contain for each inflection
C     point (in order MIN,MAX,MIN. . .MIN) the corresponding sample in
C     ITPDI.  IPK will contain pointers into ISM for a list of those
C     peaks which are significant.  ISPPK points to samples in ITPDI
C     corresponding to significant peaks which are sufficiently
C     isolated to become subpulses.  ISPV1 and ISPV2 point to the
C     subpulse beginning and ends in ITPDI.  ISPNP gives the number
C     of significant peaks contained within the subpulse, and ISPIS
C     is 1 if the subpulse has any neighboring significant peaks
C     in z and 0 if not.
C
      PARAMETER (MXINF=LMXSCL/2,MXPK=MXINF/2-1)
      DIMENSION ISM(MXINF),IPK(MXPK),ISPPK(MXPK),ISPV1(MXPK)
      DIMENSION ISPV2(MXPK),ISPNP(MXPK),ISPIS(MXPK)
      CHARACTER TEXT*130
C
C------------------------ Statements function definitions --------------
C
#include "bmacro.h"
C
      ITPDI(IA)= IW(IOFFS + IA)
C
C-----------------------------------------------------------------------
C
      IT0= ITABL(ITPADW,IP,JTPDT0)
      ITOFF=IT0-1
      NSAMP= ITABL(ITPADW,IP,JTPDNS)
      IOFF1= ITABL(ITPADW,IP,JTPDOF)
      IOFFS= ITPDIW+IOFF1
C
C++   Look out for pulses starting out above threshold with the
C++   first sample on a maximum.  Ensure here that in the following
C++   search for maxima and minima we start at a minimum.
C
      IPH1=ITPDI(1)
      IF (IPH1.GT.ITLCPT) THEN
C
C++     Loop over samples until a rise or drop is detected
C
        DO 55 IS2=2,NSAMP
          IPH=ITPDI(IS2)
          IF (IPH.GT.IPH1) THEN
C
C++         Everything is OK if leading edge is a minimum
C
            IS=IS2-1
            GO TO 56
          ELSEIF (IPH.LT.IPH1) THEN
C
C++         If the pulse starts at a maximum, look for the first
C++         minimum and mark it.
C
            IPHMN=IPH
            INXT=IS2+1
            GO TO 52
          ENDIF
  55    CONTINUE
C
C++     Skip this pulse if all samples are equal
C
        GO TO 501
C
  52    CONTINUE
C
C++       Loop over samples until the first minimum is found
C
          DO 53 IS2=INXT,NSAMP
            IPH=ITPDI(IS2)
            IF (IPH.GT.IPHMN) THEN
              IS=IS2-1
              GO TO 56
            ELSE
              IPHMN=IPH
            ENDIF
  53      CONTINUE
C
C++       Skip this pulse if no minimum ever is found
C
          GO TO 501
C
  56    CONTINUE
      ELSE
        IS=2
      ENDIF
C
C++   Case 1:  Look for a rise above the fixed threshold.
C
      ILST=NSAMP-ITLCML
  100 CONTINUE
C
C++     Exit if not enough samples are left for a complete subpulse
C
        IF (IS.GT.ILST) GO TO 501
C
C++     Initialize variables for case 2 when threshold xing is found
C
        IPH=ITPDI(IS)
        IF (IPH.GT.ITLCPT) THEN
          NINF=1
          ISM(NINF)=IS
          IPHMX=IPH
          IS=IS+1
          GO TO 200
        ENDIF
        IS=IS+1
        GO TO 100
  200 CONTINUE
C
C++     Case 2:  Look for a local maximum
C
        IPH=ITPDI(IS)
        IF (IPH.LE.ITLCPT) THEN
          NOVER=IS-ISM(1)
          IF (NOVER.LT.ITLCML) THEN
            IS=IS+1
            GO TO 100
          ENDIF
          IF (NINF.EQ.1) THEN
            NINF=NINF+1
            ISM(NINF)=IS-1
            NINF=NINF+1
            ISM(NINF)=IS-1
          ENDIF
          GO TO 400
        ENDIF
        IF (IS.EQ.NSAMP) THEN
          IF (NINF.LT.3) THEN
            IF (IPH.LT.IPHMX) THEN
              NINF=NINF+1
              ISM(NINF)=IS
              NINF=NINF+1
              ISM(NINF)=IS
            ELSE
              GO TO 501
            ENDIF
          ENDIF
          NOVER=IS-ISM(1)+1
          IF (NOVER.LT.ITLCML) GO TO 501
          GO TO 400
        ENDIF
C
C++     Check here for a local maximum.
C
        IF (IPH.LT.IPHMX) THEN
C
C++       Mark the maximum and initialize variables for case 3.
C
          NINF=NINF+1
          ISM(NINF)=IS-1
          IPHMN=IPH
          IS=IS+1
          GO TO 300
        ENDIF
C
C++     Keep track of maximum pulse height
C
        IPHMX=IPH
        IS=IS+1
        GO TO 200
  300 CONTINUE
        IPH=ITPDI(IS)
C
C++     Case 3:  Look for a local minimum or a fall below
C++     threshold or the end of the pulse
C
        IF (IPH.LE.ITLCPT) THEN
          NOVER=IS-ISM(1)
          IF (NOVER.LT.ITLCML) THEN
            IS=IS+1
            GO TO 100
          ENDIF
          NINF=NINF+1
          ISM(NINF)=IS-1
          GO TO 400
        ENDIF
        IF (IS.EQ.NSAMP) THEN
          NOVER=IS-ISM(1)+1
          IF (NOVER.LT.ITLCML) GO TO 501
          NINF=NINF+1
          ISM(NINF)=IS
          GO TO 400
        ENDIF
C
C++     Check here for a local minimum.
C
        IF (IPH.GT.IPHMN) THEN
C
C++       Mark position of the minimum and initialize variables
C++       for another search for a maximum.
C
          NINF=NINF+1
          ISM(NINF)=IS-1
          IPHMX=IPH
          IS=IS+1
          GO TO 200
        ENDIF
C
C++     Keep track of mimimum pulse height.
C
        IPHMN=IPH
        IS=IS+1
        GO TO 300
  400 CONTINUE
C
C++     At this point we are at the end of the subpulse.  Now
C++     go over the peaks and keep only those which are
C++     significantly higher than the valleys to each side.
C
        IF (NINF.GT.3) THEN
          MNLST=-1
          NPK=0
          MXTST=-1
          DO 450 IMX=2,NINF,2
            IPTST=ITPDI(ISM(IMX))*10/JPLMXD
            IPVAL=ITPDI(ISM(IMX-1))
            IF (IPVAL.LE.MNLST) THEN
              MNLST=IPVAL
              MXTST=IPTST
              MXLOC=IMX
            ELSEIF (IPTST.GE.MXTST) THEN
              MXTST=IPTST
              MXLOC=IMX
            ENDIF
            IF (MXTST.GT.MNLST) THEN
              IVAL=IMX+1
              IPVAL=ITPDI(ISM(IVAL))
              IF (MXTST.GT.IPVAL .OR. IVAL.EQ.NINF) THEN
                MNLST=IPVAL
                MXTST=-1
                NPK=NPK+1
                IPK(NPK)=MXLOC
              ENDIF
            ENDIF
  450     CONTINUE
C
C++       For each significant peak, see if we can form a separate
C++       relatively isolated subpulse.
C
          ISVL1=ISM(1)
          NSP=0
          IPTST=ITPDI(ISM(IPK(1)))*10/ITLCPC
          DO 480 I=2,NPK
            MIN1=IPK(I-1)+1
            MIN2=IPK(I)-1
            IPVAL=255
            DO 475 IMN=MIN1,MIN2,2
              IPH=ITPDI(ISM(IMN))
              IF (IPH.LT.IPVAL) THEN
                IPVAL=IPH
                ISVL2=ISM(IMN)
              ENDIF
  475       CONTINUE
            IF (ISVL1.GT.0) THEN
              IF (IPTST.GT.IPVAL) THEN
                NSP=NSP+1
                ISPPK(NSP)=ISM(IPK(I-1))
                ISPV1(NSP)=ISVL1
                ISPV2(NSP)=ISVL2
                ISPNP(NSP)=1
                ISPIS(NSP)=1
              ENDIF
            ENDIF
            IPTST=ITPDI(ISM(IPK(I)))*10/ITLCPC
            IF (IPTST.GT.IPVAL) THEN
              ISVL1=ISVL2
            ELSE
              ISVL1=0
            ENDIF
  480     CONTINUE
          IF (ISVL1.GT.0) THEN
            NSP=NSP+1
            ISPPK(NSP)=ISM(IPK(NPK))
            ISPV1(NSP)=ISVL1
            ISPV2(NSP)=ISM(NINF)
            ISPNP(NSP)=1
            IF (NPK.GT.1) THEN
              ISPIS(NSP)=1
            ELSE
              ISPIS(NSP)=0
            ENDIF
          ELSEIF (NSP.EQ.0) THEN
            NSP=1
            ISPPK(NSP)=ISM(IPK(1))
            ISPV1(NSP)=ISM(1)
            ISPV2(NSP)=ISM(NINF)
            ISPNP(NSP)=MIN(NPK,9)
            ISPIS(NSP)=0
          ENDIF
        ELSE
          NPK=1
          NSP=1
          ISPPK(NSP)=ISM(2)
          ISPV1(NSP)=ISM(1)
          ISPV2(NSP)=ISM(3)
          ISPNP(NSP)=1
          ISPIS(NSP)=0
        ENDIF
C
C++     Fill the subpulses into the ITSPUW workbank
C
        DO 490 ISP=1,NSP
          IW(ITSPUW+LMHROW)=LROWS(ITSPUW)+1
          IF ((LROWS(ITSPUW)*LENSPW+LMHLEN).GE.IW(ITSPUW)) THEN
            WRITE(TEXT,1101) LROWS(ITSPUW)
 1101       FORMAT('Extending workbank ITSPUW.',
     &             ' CURRENT LENGTH=',I3,' SUBPULSES.&',
     &             ' Increase MXSBCL in TCLCOW if this occurs',
     &             ' too often.')
            CALL RERROR('TPLANA',1,TEXT)
            LEN= (LROWS(ITSPUW)+20)*LENSPW+LMHLEN
            IW(1)=1
            CALL WBANK(IW,ITSPUW,LEN,*998)
            LEN= LROWS(ITSPUW)+20
            IW(1)=1
            CALL WBANK(IW,ITPRTW,LEN,*998)
          ENDIF
C
C++       Add up the subpulse total charge, and accumulate sums
C++       for the rms length of the subpulse.  JFSTBK comes from
C++       comdeck TCLCOW and is necessary to prevent int. overflow.
C
          NSAT=0
          IPHSM=0
          IPHZ=0
          IPHZ2=0
          JTOFF= ITOFF-JFSTBK
          DO 495 L=ISPV1(ISP),ISPV2(ISP)
            JT=L+JTOFF
            IPH=ITPDI(L)
            NSAT=NSAT + IPH/LVLSAT
            IPHSM=IPHSM + IPH
            IPHZ=IPHZ + IPH*JT
            IPHZ2=IPHZ2 + IPH*JT*JT
  495     CONTINUE
          NTSPU=LROWS(ITSPUW)
          IW(KROW(ITSPUW,NTSPU)+JSPWT0)= ITOFF + ISPV1(ISP)
          IW(KROW(ITSPUW,NTSPU)+JSPWNS)= ISPV2(ISP)-ISPV1(ISP)+1
          IW(KROW(ITSPUW,NTSPU)+JSPWOF)= IOFF1 + ISPV1(ISP)-1
          IW(KROW(ITSPUW,NTSPU)+JSPWPK)= ISPPK(ISP)-ISPV1(ISP)+1
          IW(KROW(ITSPUW,NTSPU)+JSPWTC)= IPHSM
          IW(KROW(ITSPUW,NTSPU)+JSPWSZ)= IPHZ
          IW(KROW(ITSPUW,NTSPU)+JSPWS2)= IPHZ2
          IF (NSAT.GT.ITLCMS) THEN
            IW(KROW(ITSPUW,NTSPU)+JSPWQF)= ISPNP(ISP) + ISPIS(ISP)*1000
     &                                       + 100000
          ELSE
            IW(KROW(ITSPUW,NTSPU)+JSPWQF)= ISPNP(ISP) + ISPIS(ISP)*1000
          ENDIF
          IW(KROW(ITSPUW,NTSPU)+JSPWPF)= LSTSP
          IW(KROW(ITSPUW,NTSPU)+JSPWPB)= 0
          IF (LSTSP.GT.0) THEN
            IW(KROW(ITSPUW,LSTSP)+JSPWPB)= NTSPU
          ENDIF
          LSTSP=NTSPU
          IW(KROW(ITSPUW,NTSPU)+JSPWPU)= IP
  490   CONTINUE
C
C++     Go on and look for the next group of consecutive samples above
C++     the fixed threshold.
C
        IS=IS+1
        GO TO 100
  501 CONTINUE
      GO TO 999
  998 CONTINUE
      IW(ITSPUW+LMHROW)=LROWS(ITSPUW)-1
      CALL RERROR('TPLANA',2,'No room to extend workbank ITSPUW')
  999 CONTINUE
      RETURN
      END
#endif
