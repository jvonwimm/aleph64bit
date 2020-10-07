      SUBROUTINE TREFCH(KTPCH,JGARB)
C
C--------------------------------------------------------------------
C! Reference between clusters, coordinates and MC hits
C!
C!    Author:   R. Johnson    22-10-87
C!    Modified: R. Johnson    19-01-87
C!
C!    Output:
C!       - KTPCH    /I       BOS pointer to cross reference bank
C!                           for coordinates
C!       - JGARB    /I       Set to one if a garbage collection
C!                           has occured.  Set to two if no space
C!                           was available in BOS
C!
C!    Called by TPHIST
C!
C!    Note that tracks traveling almost parallel to the padrow can
C!    leave ionization in one sector padrow without ever passing over
C!    the center of the padrow.  These cases can produce coordinates
C!    with no MC reference.
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
#include "tpgpar.h"
#include "bcs.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tcluct.h"
#include "tpccon.h"
#include "tzcorr.h"
#include "tphtjj.h"
#include "tpcojj.h"
#include "tclujj.h"
#include "trcljj.h"
#include "tmcljj.h"
#include "tscljj.h"
#include "tpchjj.h"
#include "tflags.h"
C
      COMMON/TREFWK/ ITSCLW
      DIMENSION IRLST(2,LTPDRO)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C------------------ statement functions -----------------------------
C
#include "bmacro.h"
C
C--------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPCO=NAMIND('TPCO')
        NTCLU=NAMIND('TCLU')
        NTSCL=NAMIND('TSCL')
        NTPHT=NAMIND('TPHT')
        NTPCH=NAMIND('TPCH')
        ITSCLW=0
      ENDIF
      JGARB=0
C
      KTPCH=0
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) GO TO 999
      KTCLU=IW(NTCLU)
      IF (KTCLU.EQ.0) GO TO 999
      KTSCL=IW(NTSCL)
      IF (KTSCL.EQ.0) GO TO 999
C
C++   Make a row list for the TPHT bank
C
      KTPHT=IW(NTPHT)
      IF (KTPHT.EQ.0) GO TO 999
      ILAST=-1
      CALL VZERO(IRLST,2*LTPDRO)
      DO 50 IHT=1,IW(KTPHT+LMHROW)
        IROW= ITABL(KTPHT,IHT,JTPHCI)/100000
        IF (IROW.NE.ILAST) THEN
          ILAST=IROW
          IRLST(2,IROW)=IHT-1
        ENDIF
        IRLST(1,IROW)=IRLST(1,IROW)+1
   50 CONTINUE
C
C++   Create the bank for hit association
C
      NDATA = LTPCHA*IW(KTPCO+LMHROW) + LMHLEN
      IW(1)=1
      CALL AUBOS('TPCH',0,NDATA,KTPCH,IGARB)
      IF (IGARB.NE.0) JGARB=1
      IF (IGARB.EQ.2) GO TO 998
      IW(KTPCH+LMHCOL) = LTPCHA
      IW(KTPCH+LMHROW) = IW(KTPCO+LMHROW)
      CALL BLIST(IW,'T+','TMCLTPCHTRCL')
C
      KTCLU=IW(NTCLU)
      KTPCO=IW(NTPCO)
      KTPHT=IW(NTPHT)
      CALL BWIND(IW,'TSCL',50,ITSCLW)
C
C++   Loop over all clusters in all sectors
C
   85 IF (KTCLU.EQ.0) GO TO 800
        ISLOT=IW(KTCLU-2)
        ISTYP=ITPTYP(ISLOT)
C
C++     Create cluster cross reference banks
C
        NDATA= LTRCLA*LROWS(KTCLU)+LMHLEN
        IW(1)=1
        CALL AUBOS('TRCL',ISLOT,NDATA,KTRCL,IGARB)
        IF (IGARB.NE.0) JGARB=1
        IF (IGARB.EQ.2) GO TO 998
        IF (IGARB.EQ.1) THEN
          KTCLU=NLINK('TCLU',ISLOT)
          CALL BWIND(IW,'TSCL',50,ITSCLW)
          KTPCO=IW(NTPCO)
          KTPHT=IW(NTPHT)
          KTPCH=IW(NTPCH)
        ENDIF
        IW(KTRCL+LMHCOL)=LTRCLA
        IW(KTRCL+LMHROW)=LROWS(KTCLU)
C
        NDATA= LTMCLA*LROWS(KTPHT)+LMHLEN
        IW(1)=1
        CALL AUBOS('TMCL',ISLOT,NDATA,KTMCL,IGARB)
        IF (IGARB.NE.0) JGARB=1
        IF (IGARB.EQ.2) GO TO 998
        IF (IGARB.EQ.1) THEN
          KTRCL=NLINK('TRCL',ISLOT)
          KTCLU=NLINK('TCLU',ISLOT)
          CALL BWIND(IW,'TSCL',50,ITSCLW)
          KTPCO=IW(NTPCO)
          KTPHT=IW(NTPHT)
          KTPCH=IW(NTPCH)
        ENDIF
        IW(KTMCL+LMHCOL)=LTMCLA
        IW(KTMCL+LMHROW)=0
C
        DO 700 ICL=1,LROWS(KTCLU)
          IW(KROW(KTRCL,ICL)+JTRCNH)=0
          IW(KROW(KTRCL,ICL)+JTRCOH)=LROWS(KTMCL)
C
C++       Get minimum and maximum extents of cluster in both directions
C
          IBMN=ITABL(KTCLU,ICL,JTCLFB)
          IBMX=ITABL(KTCLU,ICL,JTCLLB)
          IPMN=ITABL(KTCLU,ICL,JTCLFP)
          IPMX=ITABL(KTCLU,ICL,JTCLLP)
          IROW=ITABL(KTCLU,ICL,JTCLPN)
          IF (ISTYP.EQ.1) THEN
            IROWG=IROW
          ELSE
            IROWG=IROW+NTPDRW(1)
          ENDIF
C
C++       Apply Z offset correction according to tan(lambda)
C
          Z0= ZTPCMX-(FLOAT(IBMN)-TVOFFS(ISLOT))
     &                         * TVDRFT(IENDTP(ISLOT))
          TANL= Z0/TPPROW(IROWG)
          IF (TANL.LT.RTLCTL) THEN
            JTANL=1
          ELSE
            JTANL=2
          ENDIF
          ZBMN= FLOAT(IBMN) + TZOFFS(ITLCTA(JTANL))
          ZBMX= FLOAT(IBMX) + TZOFFS(ITLCTA(JTANL))
          RPMN= FLOAT(IPMN) - 0.5
          RPMX= FLOAT(IPMX) + 0.5
C
C++       Change pad and bucket numbers to centimeters
C
          CALL TSCOOR(ISLOT,IROW,RPMN,ZBMN,RS,RPSMN,ZSMN)
          CALL TSCOOR(ISLOT,IROW,RPMX,ZBMX,RS,RPSMX,ZSMX)
C
C++       Transform these coordinates to the global frame
C
          IF (FTPC90) THEN
            R=RS
            RPHG1=RPHMN
            ZG1=ZSMN
            RPHG2=RPSMX
            ZG2=ZSMX
          ELSE
            CALL TCTGLB(ISLOT,RS,RPSMN,ZSMN,R,RPHG1,ZG1)
            CALL TCTGLB(ISLOT,RS,RPSMX,ZSMX,R,RPHG2,ZG2)
          ENDIF
          RPHMN=AMIN1(RPHG1,RPHG2)
          RPHMX=AMAX1(RPHG1,RPHG2)
          ZMN=AMIN1(ZG1,ZG2)
          ZMX=AMAX1(ZG1,ZG2)
C
C++       Search all MC hits in this row for those within the cluster.
C++       Note that it is possible for a group of pads to fire without
C++       the track ever even crossing the CENTER of the padrow, so
C++       we expect some clusters (from low pt tracks) with no MC hits.
C
          DO 600 II=1,IRLST(1,IROWG)
            IHT= II + IRLST(2,IROWG)
            ID= ITABL(KTPHT,IHT,JTPHCI)
            IF ((MOD(ID,100000)/1000).NE.ISLOT) GO TO 600
            RPHMC= RTABL(KTPHT,IHT,JTPHPH) * R
            IF (RPHMC.LT.RPHMN) GO TO 600
            IF (RPHMC.GT.RPHMX) GO TO 600
            ZMC= RTABL(KTPHT,IHT,JTPHZV)
            IF (ZMC.LT.ZMN) GO TO 600
            IF (ZMC.GT.ZMX) GO TO 600
            IW(KROW(KTRCL,ICL)+JTRCNH)=ITABL(KTRCL,ICL,JTRCNH)+1
            IW(KTMCL+LMHROW)=LROWS(KTMCL)+1
            IW(KROW(KTMCL,LROWS(KTMCL))+JTMCPT)=IHT
  600     CONTINUE
  601     CONTINUE
C
C++       Loop over all subclusters in this cluster
C
          IF (ISLOT.LE.IW(ITSCLW)) THEN
            KTSCL=IW(ITSCLW+ISLOT)
          ELSE
            GO TO 700
          ENDIF
          IF (KTSCL.EQ.0) GO TO 700
          DO 650 ISC=1,IW(KTSCL+LMHROW)
            IF (ITABL(KTSCL,ISC,JTSCCL).NE.ICL) GO TO 650
C
C++         For each coordinate, find the closest MC hit.
C
            DO 635 JJ=1,ITABL(KTSCL,ISC,JTSCNC)
              IC= JJ + ITABL(KTSCL,ISC,JTSCCO)
              RPHIC= RTABL(KTPCO,IC,JTPCRV)*RTABL(KTPCO,IC,JTPCPH)
              ERPHI= RTABL(KTPCO,IC,JTPCSR)
              ZC= RTABL(KTPCO,IC,JTPCZV)
              EZ= RTABL(KTPCO,IC,JTPCSZ)
              DO 620 IMC=1,ITABL(KTRCL,ICL,JTRCNH)
                IHT= IW(KTMCL+LMHLEN+ITABL(KTRCL,ICL,JTRCOH)+IMC)
                RPMC= RTABL(KTPCO,IC,JTPCRV)*RTABL(KTPHT,IHT,JTPHPH)
                ZMC= RTABL(KTPHT,IHT,JTPHZV)
                CHI2= (RPMC-RPHIC)**2/ERPHI + (ZMC-ZC)**2/EZ
                IF (IMC.EQ.1 .OR. CHI2.LT.CHIMN) THEN
                  CHIMN=CHI2
                  IHTMC=IHT
                ENDIF
  620         CONTINUE
C
              IF (ITABL(KTRCL,ICL,JTRCNH).EQ.0) IHTMC=0
              IW(KROW(KTPCH,IC)+JTPCIH)= IHTMC
              IW(KROW(KTPCH,IC)+JTPCNH)= ITABL(KTRCL,ICL,JTRCNH)
C
  635       CONTINUE
  650     CONTINUE
  700   CONTINUE
C
C++     Set the final length of the TMCL bank
C
        NDATA=LROWS(KTMCL)*LCOLS(KTMCL)+LMHLEN
        CALL AUBOS('TMCL',ISLOT,NDATA,KTMCL,IGARB)
C
        KTCLU=IW(KTCLU-1)
        GO TO 85
  800 CONTINUE
      GO TO 999
C
  998 CONTINUE
      JGARB=2
  999 CONTINUE
      RETURN
      END
#endif
