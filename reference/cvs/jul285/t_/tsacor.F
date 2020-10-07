      SUBROUTINE TSACOR
C----------------------------------------------------------------------
C! Update and sort coordinate banks after small angle tracking
C
C  Author:  M. Mermikides          22-DEC-1989
C
C     Input:    Banks TPBO,TBCO,JTNWCO,JTTCHA,JTTCTC
C
C     Output:   Updated TPCO(NR=2),TBCO(NR=0),TCHA and banks
C               referencing TPCO (TCTC,TCRL,TPCH,TSCL)
C
C     Called by TSATRK
C
C----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tpcojj.h"
#include "tbcojj.h"
#include "tchajj.h"
#include "tctcjj.h"
#include "tpchjj.h"
#include "tcrljj.h"
#include "tscljj.h"
#include "tsawrk.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpccon.h"
C
C The following 4 comdecks are for debug printout only:
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
#include "rcurnt.h"
C
      LOGICAL LFRST
      SAVE NTPCO,NTBCO,NTCTC,NTCHA,NTPCH,NTCRL,NTSCL
      DATA LFRST/.TRUE./
      DATA NDCHA/10/, NDTCT/100/
      DATA NXTPC/10/
      DATA JJTSCL/0/
C
#include "bmacro.h"
C
      IF (LFRST) THEN
         NTPCO = NAMIND('TPCO')
         NTBCO = NAMIND('TBCO')
         NTCTC = NAMIND('TCTC')
         NTCHA = NAMIND('TCHA')
         NTPCH = NAMIND('TPCH')
         NTCRL = NAMIND('TCRL')
         NTSCL = NAMIND('TSCL')
         LFRST = .FALSE.
      ENDIF
C
      KTPCO = IW(NTPCO)
      KTBCO = IW(NTBCO)
      NTRAN = 0
C  Get indices into TSCL for each sector
      CALL BWIND(IW,'TSCL',36,JJTSCL)
C----------------------------------------------------------------
C  Loop through the new coordinates
C----------------------------------------------------------------
      DO 10 IH=1,LROWS(JTNWCO)
C  Skip deleted hits
         ICHA = ITABL(JTNWCO,IH,JTNWZO)
         IF (ICHA.LT.0) GO TO 10
         IC = ITABL(JTNWCO,IH,JTNWIC)
C
C++  Reference to TPCO, update z and error
C
            IF (IC.GT.0) THEN
               RW(KROW(KTPCO,IC)+JTPCZV)=RTABL(JTNWCO,IH,JTNWZZ)
               IRS= ITABL(KTPCO,IC,JTPCIN)
               ISEC= MOD(IRS/1000,100)
               SZ2= RTABL(JTNWCO,IH,JTNWG2)*TVDRFT(IENDTP(ISEC))**2
               RW(KROW(KTPCO,IC)+JTPCSZ) = SZ2
            ELSE
C
C++  Reference to TBCO, append to TPCO and mark TBCO row for deletion
C
               PHI = RTABL(JTNWCO,IH,JTNWPH)
               IF (LFRROW(KTPCO).LT.1) THEN
                  NDAT = IW(KTPCO) + NXTPC*LCOLS(KTPCO)
                  CALL AUBOS('TPCO',IW(KTPCO-2),NDAT,KTPCO,IGARB)
                  IF (IGARB.EQ.2) GO TO 800
                  IF (IGARB.EQ.1) THEN
                     KTBCO = IW(NTBCO)
                     CALL BWIND(IW,'TSCL',36,JJTSCL)
                  ENDIF
               ENDIF
               IRS = IABS(ITABL(KTBCO,-IC,JTBCIN))
               IW(KNEXT(KTPCO)+JTPCIN) = IRS
               RW(KNEXT(KTPCO)+JTPCRV) = RTABL(JTNWCO,IH,JTNWRR)
               RW(KNEXT(KTPCO)+JTPCPH) = PHI
               RW(KNEXT(KTPCO)+JTPCZV) = RTABL(JTNWCO,IH,JTNWZZ)
               RW(KNEXT(KTPCO)+JTPCSR) = RTABL(JTNWCO,IH,JTNWS2)
               RW(KNEXT(KTPCO)+JTPCRR) = RTABL(JTNWCO,IH,JTNWPS)
               RW(KNEXT(KTPCO)+JTPCRZ) = RTABL(JTNWCO,IH,JTNWZS)
               ISEC=MOD(IRS/1000,100)
               SZ2=RTABL(JTNWCO,IH,JTNWG2)*TVDRFT(IENDTP(ISEC))**2
               RW(KNEXT(KTPCO)+JTPCSZ) = SZ2
               IW(KNEXT(KTPCO)+JTPCOF) = 3
               IW(KNEXT(KTPCO)+JTPCTN) = 0
               ISCL = ITABL(KTBCO,-IC,JTBCSC)
               IF (ISCL.LT.0) ISCL = 0
               IW(KNEXT(KTPCO)+JTPCCN) = ISCL
               IW(KNEXT(KTPCO)+JTPCIT) = 0
C Change TPCO offset of associated subcluster to index new coordinate
               IF(ISCL.GT.0) THEN
                  KTSCL = IW(JJTSCL+ISEC)
                  IF(KTSCL.GT.0) THEN
                     IW(KROW(KTSCL,ISCL)+JTSCCO) = IW(KTPCO+LMHROW)
                     IW(KROW(KTSCL,ISCL)+JTSCNC) = 1
                  ENDIF
               ENDIF
               NTRAN = NTRAN + 1
               IW(KROW(KTBCO,-IC)+JTBCIN) = -IABS(IRS)
               IW(KTPCO+LMHROW) = IW(KTPCO+LMHROW) + 1
C Change index in JTNWCO to point to new TPCO coordinate
               IW(KROW(JTNWCO,IH)+JTNWIC) = IW(KTPCO+LMHROW)
            ENDIF
C
   10 CONTINUE
C
C++ Now create TPCO bank NR=2 with correct sequence and drop NR=3
C   -------------------------------------------------------------
C
      IF (NTRAN.GT.0) THEN
C
         NDAT = LMHLEN+ LROWS(KTPCO)
         CALL WBANK(IW,JTWRK1,NDAT,*800)
         IW (JTWRK1+LMHCOL) = 1
         IW (JTWRK1+LMHROW) = LROWS(KTPCO)
         CALL WBANK(IW,JTWRK2,NDAT,*800)
         IW (JTWRK2+LMHCOL) = 1
         IW (JTWRK2+LMHROW) = LROWS(KTPCO)
         CALL WBANK(IW,JTWRK3,NDAT,*800)
         IW (JTWRK3+LMHCOL) = 1
         IW (JTWRK3+LMHROW) = LROWS(KTPCO)
         DO 20 I=1,LROWS(KTPCO)
            IW(JTWRK1+LMHLEN+I) = ITABL(KTPCO,I,JTPCIN)
   20    CONTINUE
C
C  Store in JTWRK2 the hit sequence according to increasing index
         CALL SORTZV(IW(JTWRK1+LMHLEN+1),IW(JTWRK2+LMHLEN+1),
     &               IW(JTWRK1+LMHROW),-1,0, 0,0,0)
C  Store in JTWRK3 relationship between old and new TPCO order
         CALL SORTZV(IW(JTWRK2+LMHLEN+1),IW(JTWRK3+LMHLEN+1),
     &               IW(JTWRK2+LMHROW),-1,0, 0,0,0)
      ENDIF
C
      NDAT = LMHLEN + LCOLS(KTPCO)*LROWS(KTPCO)
      CALL AUBOS('TPCO',2,NDAT,KTPC2,IGARB)
      IF (IGARB.EQ.2) GO TO 800
      IF (IGARB.EQ.1) KTPCO = NLINK(KTPCO,3)
      IF (NTRAN.GT.0) THEN
C If new coordinates were added we have to resequence TPCO bank
         IW(KTPC2+LMHCOL) = LCOLS(KTPCO)
         IW(KTPC2+LMHROW) = LROWS(KTPCO)
         DO 30 I=1,LROWS(KTPCO)
            IND = IW(JTWRK2+LMHLEN+I)
            CALL UCOPY(IW(KROW(KTPCO,IND)+1),IW(KROW(KTPC2,I)+1),
     &                     LCOLS(KTPCO))
   30    CONTINUE
C
C++  If this is a Monte Carlo run (ie TPCH exists), increase size
C++  of TPCH bank to maintain one-to-one relation with TPCO bank
C++  and resequence according to the new TPCO order.
C++  NB the new entries will not have associations
C     ------------------------------------------------------------
         KTPCH = IW(NTPCH)
         IF (KTPCH.GT.0) THEN
            NDATA = LTPCHA*LROWS(KTPCO) + LMHLEN
            CALL AUBOS('TPCH',0,NDATA,KTPCH,IGARB)
            IF (IGARB.EQ.2) GO TO 800
            IF (IGARB.EQ.1) KTPC2 = NLINK('TPCO',2)
            IW(KTPCH+LMHCOL) = LTPCHA
            IW(KTPCH+LMHROW) = LROWS(KTPCO)
            DO 40 I=1,LROWS(KTPCH)
               IND = IW(JTWRK2+LMHLEN+I)
               IH = ITABL(KTPCH,IND,JTPCIH)
               NH = ITABL(KTPCH,IND,JTPCNH)
C  Reuse JTWRK1
               IW(JTWRK1+LMHLEN+I) = IH*10000 + NH
   40       CONTINUE
            DO 45 I=1,LROWS(KTPCH)
               IPK = IW(JTWRK1+LMHLEN+I)
               IW(KROW(KTPCH,I)+JTPCIH) = IPK/10000
               IW(KROW(KTPCH,I)+JTPCNH) = MOD(IPK,10000)
   45       CONTINUE
         ENDIF
C
C Redefine TCRL row list if it exists
C
         KTCRL=IW(NTCRL)
         IF (KTCRL.GT.0) THEN
            CALL VZERO(IW(KTCRL+LMHLEN+1),LROWS(KTCRL)*LCOLS(KTCRL))
            IRL=-1
            DO 46 IC=1,LROWS(KTPC2)
               ID=ITABL(KTPC2,IC,JTPCIN)
               IROW=ID/100000
               IF (IROW.NE.IRL) THEN
                  IRL=IROW
                  IW(KROW(KTCRL,IROW)+JTCROC)=IC-1
                  IW(KROW(KTCRL,IROW)+JTCRNC)=0
                  IW(KROW(KTCRL,IROW)+JTCRN1)=0
               ENDIF
               IW(KROW(KTCRL,IROW)+JTCRNC)=ITABL(KTCRL,IROW,JTCRNC)+1
               ISLOT=(ID-IROW*100000)/1000
               IF (IENDTP(ISLOT).EQ.1) THEN
                  IW(KROW(KTCRL,IROW)+JTCRN1)=ITABL(KTCRL,IROW,JTCRN1)+1
               ENDIF
   46       CONTINUE
         ENDIF
C
      ELSE
C
        CALL UCOPY(IW(KTPCO+1),IW(KTPC2+1),NDAT)
C
      ENDIF
C
      IDRP = NDROP('TPCO',3)
C
C
C++ Now create new TBCO bank, removing hits which have been
C++ transferred to the TPCO bank, and delete NR=1
C   -------------------------------------------------------
C
      IF (KTBCO.EQ.0) GO TO 55
      IF(NTRAN.LT.LROWS(KTBCO)) THEN
         NDAT = LMHLEN + LCOLS(KTBCO)*(LROWS(KTBCO)-NTRAN)
         CALL AUBOS('TBCO',0,NDAT,KTBC2,IGARB)
         IF (IGARB.EQ.1) KTBCO = NLINK('TBCO',1)
         IF (IGARB.EQ.2) GO TO 800
         IW(KTBC2+LMHCOL) = LCOLS(KTBCO)
         IW(KTBC2+LMHROW) = LROWS(KTBCO)-NTRAN
         NOUT = 0
         DO 50 I = 1,LROWS(KTBCO)
            IF(ITABL(KTBCO,I,JTBCIN).LT.0) GO TO 50
            NOUT = NOUT + 1
            CALL UCOPY(IW(KROW(KTBCO, I)+1),
     &                 IW(KROW(KTBC2,NOUT)+1),IW(KTBCO+LMHCOL))
   50    CONTINUE
      ENDIF
C
      IDRP = NDROP('TBCO',1)
C
C  Now redefine TPCO pointers in existing TCTC banks if we have added
C  any new coordinates
C
   55 KTCTC = IW(NTCTC)
      LSTOF = 0
      IF (KTCTC.GT.0) LSTOF=LROWS(KTCTC)
      IF(NTRAN.GT.0.AND.KTCTC.GT.0) THEN
         DO 60 I=1,LROWS(KTCTC)
            IC = IW(KTCTC+LMHLEN+I)
            IW(KTCTC+LMHLEN+I) = IW(JTWRK3+LMHLEN+IC)
   60    CONTINUE
      ENDIF
C
C++ Enlarge TCHA,TCTC banks to acommodate new chains
C   ------------------------------------------------
C
      IF (LROWS(JTTCHA).GT.0) THEN
         KTCHA = IW(NTCHA)
         IW(1) = 0
         IF (KTCHA.EQ.0) THEN
            NDAT = LMHLEN+LROWS(JTTCHA)*LTCHAA
            CALL AUBOS('TCHA',0,NDAT,KTCHA,IGARB)
            IW(KTCHA + LMHCOL) = LTCHAA
            IW(KTCHA + LMHROW) = 0
            CALL BLIST(IW,'T+','TCHA')
         ELSE
            IF (LFRROW(KTCHA).LT.LROWS(JTTCHA)) THEN
               NDAT = IW(KTCHA) + LROWS(JTTCHA)*LTCHAA
               CALL AUBOS('TCHA',0,NDAT,KTCHA,IGARB)
            ENDIF
         ENDIF
         IF (IGARB.EQ.1) KTCTC = IW(NTCTC)
         IF (IGARB.EQ.2) GO TO 800
         DO 70 I=1,LROWS(JTTCHA)
            CALL UCOPY(IW(KROW(JTTCHA,I)+1),
     &                 IW(KNEXT(KTCHA)+1),LTCHAA)
C   Take care of offset in TCTC
            IW(KNEXT(KTCHA)+JTCHOF) = LSTOF + IW(KROW(JTTCHA,I)+JTCHOF)
            IW(KTCHA+LMHROW) = IW(KTCHA+LMHROW) + 1
   70    CONTINUE
      ENDIF
C
      IF (LROWS(JTTCTC).GT.0) THEN
         IW(1) = 1
         KTCTC = IW(NTCTC)
         IF (KTCTC.EQ.0) THEN
            NDAT = LMHLEN+LROWS(JTTCTC)*LTCTCA
            CALL AUBOS('TCTC',0,NDAT,KTCTC,IGARB)
            IW(KTCTC + LMHCOL) = LTCTCA
            IW(KTCTC + LMHROW) = 0
            CALL BLIST(IW,'T+','TCTC')
         ELSE
            IF (LFRROW(KTCTC).LT.LROWS(JTTCTC)) THEN
               NDAT = IW(KTCTC) + LROWS(JTTCTC)*LTCTCA
               CALL AUBOS('TCTC',0,NDAT,KTCTC,IGARB)
            ENDIF
         ENDIF
         IF (IGARB.EQ.2) GO TO 800
C
C++ Add hits of new chains to TCTC
C
         IW(KTCTC+LMHROW) = LROWS(KTCTC) + LROWS(JTTCTC)
         DO 80 I=1,LROWS(JTTCTC)
            IH = IW(JTTCTC+LMHLEN+I)
            IC = ITABL(JTNWCO,IH,JTNWIC)
            IF (IC.LT.0) GO TO 80
            IF (NTRAN.EQ.0) THEN
               IW(KTCTC+LMHLEN+LSTOF+I) = IC
            ELSE
               IW(KTCTC+LMHLEN+LSTOF+I) = IW(JTWRK3+LMHLEN+IC)
            ENDIF
   80    CONTINUE
C
      ENDIF
C
C Now update TPCO offsets in TSCL
C
      KTSCL = IW(NTSCL)
      IF (NTRAN.GT.0) THEN
   90    IF(KTSCL.EQ.0) GO TO 100
         DO 95 I=1,LROWS(KTSCL)
            IF (ITABL(KTSCL,I,JTSCNC).LE.0) GO TO 95
            IC = ITABL(KTSCL,I,JTSCCO) + 1
            IW(KROW(KTSCL,I)+JTSCCO) = IW(JTWRK3+LMHLEN+IC) - 1
   95    CONTINUE
         KTSCL = IW(KTSCL-1)
         GO TO 90
      ENDIF
C
 100  IF (JJTSCL.GT.0) CALL WDROP(IW,JJTSCL)
      IF (JTWRK1.GT.0) CALL WDROP(IW,JTWRK1)
      IF (JTWRK2.GT.0) CALL WDROP(IW,JTWRK2)
      IF (JTWRK3.GT.0) CALL WDROP(IW,JTWRK3)
C
C Debug printout
C
      IF (FDEBRF .AND. JDBDRF(JULTP).GE.1) THEN
          IF(NTRAN.GT.0.OR.LROWS(JTTCHA).GT.0) THEN
             WRITE(LDEBRL,767) IRUNRC,IEVTRC,NTRAN,LROWS(JTTCHA)
          ENDIF
  767     FORMAT(' TSACOR for run ',I5,' event ',I6,' adds ',
     &           I3,' TPC coords and ',I3,' chains')
      ENDIF
      GO TO 999
  800 CALL RERROR('TSACOR',4,'Error extending TCHA/TCTC/TPCO bank')
      GO TO 100
C
  999 RETURN
      END
#endif
