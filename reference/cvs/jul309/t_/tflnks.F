      SUBROUTINE TFLNKS(IER)
C-----*****************---------------------------------------------
C! FIND TPC LINKS BETWEEN CHAINS
C!
C!    AUTHOR        :- MARTIN POPPE 87/5/25
C!    Modified      :  P. Colrain 95/10/26
C!                     D. Casper  98/2/04  Correct typo in "long-range" code
C!                                         Select best link, not first...
C!
C!    OUTPUT:  IER = ERROR CODE:  =0 IF OK, =1 IF INCOMPLETE
C!
C!    INPUT  BANKS  : 'TCHA' , 'TCTC'
C!    OUTPUT BANK   : 'TLNK'
C!
C!
C!    DESCRIPTION
C!    ===========
C!
C!    Creates a table of links between chains defining
C!    which chains belong to the same particle. This table is
C!    stored in the bank 'TLNK' which has as many rows as there
C!    are chains. Four types of links  are possible: "up", "down"
C!    "top" , and "bottom". These links are stored in columns 1 to 4.
C!    For example, the number 16 in row 3 of column 10 means that
C!    there is a "top" link between chains 10 an 16, i.e. this is
C!    a spiraling particle.
C-------------------------------------------------------------------C
#ifndef DOC
C
#include "ttrcjj.h"
#include "alcons.h"
#include "tlnkjj.h"
#include "tchajj.h"
#include "tpcojj.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "bcs.h"
C
      PARAMETER(MCAND=300,EPS=0.0000001)
      INTEGER LILLY(LTPDRO)
      REAL VAL(5),ERR(15)
      INTEGER IBUF1(MCAND),IBUF2(MCAND),IPATL(MCAND)
      INTEGER I1PAT(MCAND),I2PAT(MCAND)
      LOGICAL LB1(MCAND),LB2(MCAND)
      LOGICAL FIRST,LFUND
      DATA FIRST/.TRUE./
C
C-------------------- Statement Function Definitions ----------------
C
#include "bmacro.h"
C
      LIST(I)   = KTCTC + LMHLEN + ITABL(KTCHA,I,JTCHOF)
C-----> offset for chain point list
      RPT(IND)  = RW(IND+2)
C-----> r coordinate from offset in 'TPCO'
      ZPT(IND)  = RW(IND+4)
C-----> z coordinate from offset in 'TPCO'
C-----> track parameters from 'TCHA'  follow now:
      OME(JJ)  = RTABL(KTCHA,JJ,JTCHIR)
C-----> inv. rad of track from offset in 'TCHA'
      ALP(JJ)  = RTABL(KTCHA,JJ,JTCHTL)
C-----> TAN(LAM) of track from offset in 'TCHA'
      PH0(JJ)  = RTABL(KTCHA,JJ,JTCHP0)
C-----> PHI(0)   of track from offset in 'TCHA'
      DD0(JJ)  = RTABL(KTCHA,JJ,JTCHD0)
C-----> IMPACT   of track from offset in 'TCHA'
      ZZ0(JJ)  = RTABL(KTCHA,JJ,JTCHZ0)
C-----> Z(0)     of track from offset in 'TCHA'
      CHF(JJ)  = RTABL(KTCHA,JJ,JTCHC1)
C-----> chi**2/freedom of track from offset in 'TCHA'
C-----> derived track quantities  follow
      RADTK(JJ)= 1.0/ABS(RTABL(KTCHA,JJ,JTCHIR)+0.01*EPS)
      RTOP(JJ) = 2.0*RADTK(JJ)-RTABL(KTCHA,JJ,JTCHD0)
C-----> inverse errors squared on track quantities follow now:
      EOME(JJ)= RTABL(KTCHA,JJ,JTCHER)
C                           ... FOR 1/R
      EALP(JJ)= RTABL(KTCHA,JJ,JTCHER+2)
C                           ... FOR TAN(LAM)
      EPH0(JJ)= RTABL(KTCHA,JJ,JTCHER+5)
C                           ... FOR PHI(0)
      EDD0(JJ)= RTABL(KTCHA,JJ,JTCHER+9)
C                           ... FOR D(0)
      EZZ0(JJ)= RTABL(KTCHA,JJ,JTCHER+14)
C                           ... FOR Z(0)
C-----> error weights scaled with mean chi**2/freedom
      EOMES(JJ)= RTABL(KTCHA,JJ,JTCHER)/(RTABL(KTCHA,JJ,JTCHC2)+1.5)
C                           ... FOR 1/R
      EALPS(JJ)= RTABL(KTCHA,JJ,JTCHER+2)/(RTABL(KTCHA,JJ,JTCHC2)+1.5)
C                           ... FOR TAN(LAM)
      EPH0S(JJ)= RTABL(KTCHA,JJ,JTCHER+5)/(RTABL(KTCHA,JJ,JTCHC2)+1.5)
C                           ... FOR PHI(0)
      EDD0S(JJ)= RTABL(KTCHA,JJ,JTCHER+9)/(RTABL(KTCHA,JJ,JTCHC2)+1.5)
C                           ... FOR D(0)
      EZZ0S(JJ)= RTABL(KTCHA,JJ,JTCHER+14)/(RTABL(KTCHA,JJ,JTCHC2)+1.5)
C                           ... FOR Z(0)
C
C------------------------------------------------------------------
C
      IER=0
      IF (FIRST) THEN
        CHMAX = 100.0
        CHTMX = 500.0
        CHPMX = 1000.0
        IOUT1 = 2**LTPDRO
        IOUT2 = 2**(LTPDRO-1)
        IOUT3 = 2**(LTPDRO-2)
        IOUT4 = 2**(LTPDRO-3)
        IF15  = 2**15 - 1
        IL06  = (2**7-1) * (2**(LTPDRO-6))
        NTCHA = NAMIND('TCHA')
        NTCTC = NAMIND('TCTC')
        NTPCO = NAMIND('TPCO')
        NTTRC = NAMIND('TTRC')
        NTLNK = NAMIND('TLNK')
        FIRST = .FALSE.
      END IF
      OCUT= 1.0/(5.0*RTPCMX)
      DROW= TPDRST(1)
C
C-->  Get steering constants from the TTRC bank
C
      KTTRC = IW(NTTRC)
      IF (KTTRC.GT.0) THEN
        CHFRE = RW(KTTRC+LMHLEN+JTTRCC)
        CHMAX = RW(KTTRC+LMHLEN+JTTRSU)**2  * 10.0
        CHTMX = RW(KTTRC+LMHLEN+JTTRST)**2  * 10.0
        CHPMX = RW(KTTRC+LMHLEN+JTTRSI)**2  * 2.0
      ELSE
        CALL RERROR('TFLNKS',2,'Cannot find steering constants.')
        RETURN
      END IF
C
C-->  Link to the necessary input banks
C
      KTCHA = IW(NTCHA)
      IF (KTCHA.LE.0)                               GO TO 998
      KTCTC = IW(NTCTC)
      IF (KTCTC.LE.0)                               GO TO 998
      KTPCO = IW(NTPCO)
      IF (KTPCO.LE.0)                               GO TO 998
C
C-->  Create the output TLNK bank.  Delete old one if necessary
C
      IF (IW(NTLNK).NE.0) CALL BDROP(IW,'TLNK')
      LTLNK = LROWS(KTCHA)*LTLNKA+LMHLEN
      CALL AUBOS('TLNK',0,LTLNK,KTLNK,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IF (IGARB.EQ.1) THEN
        KTCHA = IW(NTCHA)
        KTCTC = IW(NTCTC)
        KTPCO = IW(NTPCO)
      END IF
      CALL BLIST(IW,'T+','TLNK')
      IW(KTLNK+LMHCOL)= LTLNKA
      IW(KTLNK+LMHROW)= LROWS(KTCHA)
C
C-------------------------------------------------------------------C
C              SELECT CANDIDATES FOR UP/DOWN LINKS                  C
C              ***********************************                  C
C-------------------------------------------------------------------C
C---> Find candidates for upper chains in up-down links
C     criterium: at most 2 hits in the inner 5 pad rows
C
      NCNDU = 0
      DO 10 I=1,LROWS(KTCHA)
        IF (NCNDU.EQ.MCAND) GO TO 11
        ITEST = ITABL(KTCHA,I,JTCHPI)
        NHITS = IAND(ITEST,32)/32 + IAND(ITEST,16)/16 + IAND(ITEST,8)/8
     1         +IAND(ITEST,4)/4   +IAND(ITEST,2)/2
        IF (NHITS.GT.2) GO TO 10
        NCNDU = NCNDU+1
        IBUF1(NCNDU) = I
        I1PAT(NCNDU) = ITEST
   10 CONTINUE
   11 CONTINUE
C
C---> IBUF1 now contains the chains which might be an upper
C     part of a set of linked chains
C
      IF (NCNDU.EQ.0) GO TO 100
C
C---> Now find candidates for lower chains in up-down links
C     Criteria: 1) at most 2 hits in the outer 4 pad rows
C               2) no extrapolation outside the chamber in z
C               3) not being closer than 3 rows to the max radius
C
      NCNDL  = 0
      DO 20 I=1,LROWS(KTCHA)
        IF (NCNDL.EQ.MCAND) GO TO 21
        ITEST = ITABL(KTCHA,I,JTCHPI)
        NHITS = IAND(ITEST,IOUT1)/IOUT1 +IAND(ITEST,IOUT2)/IOUT2
     1         +IAND(ITEST,IOUT3)/IOUT3 +IAND(ITEST,IOUT4)/IOUT4
        IF (NHITS.GT.2)                               GO TO 20
        LSTPT = ITABL(KTCHA,I,JTCHOF) + ITABL(KTCHA,I,JTCHNC)
        LB1PT = LSTPT - 1
        LSTCO = IW(KTCTC+2+LSTPT)
        LB1CO = IW(KTCTC+2+LB1PT)
        R1= RTABL(KTPCO,LB1CO,JTPCRV)
        R2= RTABL(KTPCO,LSTCO,JTPCRV)
        Z1= RTABL(KTPCO,LB1CO,JTPCZV)
        Z2= RTABL(KTPCO,LSTCO,JTPCZV)
        ZEXT   = ABS(Z2 + 2.0*DROW*(Z2-Z1)/(R2-R1))
        IF (ZEXT.GT.ZTPCMX)                           GO TO 20
        RMATK  = RTOP(I)
        DR1 = 0.25*(RMATK-R1)
        DR2 = 0.25*(RMATK-R2)
        IF (DR1.LT.DROW.AND.DR2.LT.DROW)          GO TO 20
        NCNDL = NCNDL+1
        IBUF2(NCNDL) = I
        I2PAT(NCNDL) = ITEST
        LB2(NCNDL)   = .FALSE.
   20 CONTINUE
   21 CONTINUE
C
      IF (NCNDL.LE.0)                                GO TO 100
C
C-------------------------------------------------------------------C
C              DEFINE UP/DOWN LINKS BETWEEN CANDIDATES              C
C              ***************************************              C
C-------------------------------------------------------------------C
C
      DO 40 ICNDU = 1,NCNDU
        JCH1=IBUF1(ICNDU)
        ITSTU = I1PAT(ICNDU)
        FUDGE  = 1.E30
        LFUND = .FALSE.
        DO 30 ICNDL = 1,NCNDL
          IF (LB2(ICNDL))                      GO TO 30
          JCH2=IBUF2(ICNDL)
          ITSTL = I2PAT(ICNDL)
C
C-->      Check consistency of hit pattern
C
          IF (ITSTL.GT.ITSTU)                 GO TO 30
C
          ITEST = 0
          ITEST = IAND(ITSTL,ITSTU)
          IF (ITEST.NE.0)                       GO TO 30
C
C-->      Check consistency of track parameters
C
          DPHI=RTABL(KTCHA,JCH1,JTCHP0)-RTABL(KTCHA,JCH2,JTCHP0)
          IF (ABS(DPHI) .GT. PI) DPHI=TWOPI-ABS(DPHI)
          CHI2 = (RTABL(KTCHA,JCH1,JTCHIR)
     1                        -RTABL(KTCHA,JCH2,JTCHIR))**2
     2          *EOMES(JCH1)*EOMES(JCH2)/(EOMES(JCH1)+EOMES(JCH2))
     3          + (RTABL(KTCHA,JCH1,JTCHTL)
     4                        -RTABL(KTCHA,JCH2,JTCHTL))**2
     5          *EALPS(JCH1)*EALPS(JCH2)/(EALPS(JCH1)+EALPS(JCH2))
     6          + DPHI**2
     7          *EPH0S(JCH1)*EPH0S(JCH2)/(EPH0S(JCH1)+EPH0S(JCH2))
     8          + (RTABL(KTCHA,JCH1,JTCHD0)
     9                        -RTABL(KTCHA,JCH2,JTCHD0))**2
     1          *EDD0S(JCH1)*EDD0S(JCH2)/(EDD0S(JCH1)+EDD0S(JCH2))
     2          + (RTABL(KTCHA,JCH1,JTCHZ0)
     3                        -RTABL(KTCHA,JCH2,JTCHZ0))**2
     4          *EZZ0S(JCH1)*EZZ0S(JCH2)/(EZZ0S(JCH1)+EZZ0S(JCH2))
C
          CHI2 = 0.2*CHI2
          IF (CHI2.LT.CHMAX) THEN
            FUDG1 = CHI2*ALOG(0.5*FLOAT(ITSTU)/FLOAT(I2PAT(ICNDL)))
C
C---->      Minimise SQRT( DELTA(R/MAX) ) * CHI**2
C
            IF (FUDG1.LT.FUDGE) THEN
              LFUND = .TRUE.
              FUDGE  = FUDG1
              ITOP   = IBUF1(ICNDU)
              IBOT   = IBUF2(ICNDL)
              LOOPL  = ICNDL
            END IF
          END IF
   30   CONTINUE
C
C-----> Test via fit over all coordinates
C
        IF (LFUND) THEN
          NQL = ITABL(KTCHA,IBOT,JTCHNC)
          NQU = ITABL(KTCHA,ITOP,JTCHNC)
          DO 36 IPT = 1,NQL
            LILLY(IPT) = IW(LIST(IBOT)+IPT)
   36     CONTINUE
          NLILL  = NQU+NQL
          DO 37 IPT = 1,NQU
            LILLY(NQL+IPT) = IW(LIST(ITOP)+IPT)
   37     CONTINUE
          CALL TFTHEL('TPCO',NLILL,LILLY,1,VAL,ERR,CHI2)
          CHI2 = CHI2/FLOAT(2*NLILL-5)
          LFUND =.FALSE.
          IF (CHI2.LT.CHFRE) LFUND=.TRUE.
        END IF
C
C-----> Store and disable
C
        IF (LFUND)THEN
          LB2(LOOPL)  = .TRUE.
          IDUM1 = ITSTU
          IDUM2 = I2PAT(LOOPL)
          IDUM3 = IOR(IDUM1,IDUM2)
          I1PAT(ICNDU) = IDUM3
          I2PAT(LOOPL ) = IDUM3
          IW(KROW(KTLNK,IBOT)+JTLNUP) =ITOP
          IW(KROW(KTLNK,ITOP)+JTLNDN) =IBOT
        END IF
   40 CONTINUE
C
  100 CONTINUE
C
C-------------------------------------------------------------------C
C              SELECT CANDIDATES FOR TOP LINKS                      C
C              *******************************                      C
C-------------------------------------------------------------------C
C-----> Find seperately candidates for left and right parts
C       Criteria:  curvature positive
C                  max radius less than max chamber radius
C                  no upward link found
C
      NCNDL = 0
      DO 110 I=1,LROWS(KTCHA)
        IF (NCNDL.EQ.MCAND)                        GO TO 110
        IF (RTABL(KTCHA,I,JTCHIR).LE.EPS)          GO TO 110
        RMTRK = 2.0/RTABL(KTCHA,I,JTCHIR) - RTABL(KTCHA,I,JTCHD0)
        IF (RMTRK.GT.RTPCMX)                       GO TO 110
        IF (ITABL(KTLNK,I,JTLNUP).NE.0)            GO TO 110
        NCNDL = NCNDL+1
        IBUF1(NCNDL) = I
  110 CONTINUE
      IF (NCNDL.LE.0)                              GO TO 200
C
C---> IBUF1 now contains the chains which might be the left
C     part of a spiral
c
      NCNDR = 0
      DO 120 I=1,LROWS(KTCHA)
        IF (NCNDR.EQ.MCAND)                        GO TO 120
        IF (RTABL(KTCHA,I,JTCHIR).GE.-EPS)         GO TO 120
        RMTRK=-2.0/RTABL(KTCHA,I,JTCHIR) - RTABL(KTCHA,I,JTCHD0)
        IF (RMTRK.GT.RTPCMX)                       GO TO 120
        IF (ITABL(KTLNK,I,JTLNUP).NE.0)            GO TO 120
        NCNDR= NCNDR+1
        IBUF2(NCNDR)= I
        LB2(NCNDR)= .FALSE.
  120 CONTINUE
      IF (NCNDR.LE.0)                              GO TO 200
C
C-------------------------------------------------------------------C
C              DEFINE TOP LINKS BETWEEN CANDIDATES                  C
C              ***********************************                  C
C-------------------------------------------------------------------C
C
      DO 140 ICNDL = 1,NCNDL
        K1     = IBUF1(ICNDL)
        CHX    = CHTMX
        DO 130 ICNDR = 1,NCNDR
          IF (LB2(ICNDR))                               GO TO 130
          K2     = IBUF2(ICNDR)
          CHI2 = (ALP(K1)+ALP(K2))**2
     1         /( (1.0+CHF(K1))/EALP(K1) +(1.0+CHF(K2))/EALP(K2) )
C
          IF (CHI2.GT.(5.0*CHTMX))                       GO TO 130
C
          DPHI = AMOD(PH0(K1)-PH0(K2)-PI,TWOPI)
          IF (DPHI.LT.-5.0)DPHI=DPHI+TWOPI
          CHI2 = CHI2
     1      +   ( 2.0*(1.0/OME(K1)+1.0/OME(K2))-DD0(K1)+DD0(K2) )**2
     2        /( 4.0*(  1.0/(EOME(K1)*OME(K1)*OME(K1))
     3               +1.0/(EOME(K1)*OME(K2)*OME(K2)) )
     4        +1.0/EDD0(K1) + 1.0/EDD0(K2)               )
     5      +    DPHI**2
     6        /((2.0+CHF(K1))/EPH0(K1) + (2.0+CHF(K2))/EPH0(K2) )
     7    +   (ZZ0(K1)-ZZ0(K2)+PI*(ALP(K1)/OME(K1)+ALP(K2)/OME(K2)))**2
     8        /(   2.0/EZZ0(K1) + 2.0/EZZ0(K2)   )
          CHI2 = 0.25*CHI2
          IF (CHI2.LT.CHX) THEN
            LOOPR = ICNDR
            CHX = CHI2
          END IF
  130   CONTINUE
        IF (CHX.LT.CHTMX) THEN
          LB2(LOOPR) = .TRUE.
          IW(KROW(KTLNK,IBUF1(ICNDL))+JTLNTP)=IBUF2(LOOPR)
          IW(KROW(KTLNK,IBUF2(LOOPR))+JTLNTP)=IBUF1(ICNDL)
        END IF
  140 CONTINUE
C
  200 CONTINUE
C
C-------------------------------------------------------------------C
C              SELECT CANDIDATES FOR BOTTOM LINKS                   C
C                      WITHIN THE T.P.C.                            C
C              **********************************                   C
C-------------------------------------------------------------------C
C-----> Find seperately candidates for left and right parts
C       Criteria:  curvature positive
C                  minimum distance to beam line = R(ITC)
C                  no downward link found
C
      NCNDL = 0
      DO 210 I=1,LROWS(KTCHA)
        IF (NCNDL.EQ.MCAND)                        GO TO 210
        IF (RTABL(KTCHA,I,JTCHIR).LE.EPS)          GO TO 210
        RMINI = ABS(RTABL(KTCHA,I,JTCHD0))
        IF (RMINI.LT.TPPROW(1))                    GO TO 210
        IF (ITABL(KTLNK,I,JTLNDN).NE.0)            GO TO 210
        NCNDL = NCNDL+1
        IBUF1(NCNDL) = I
  210 CONTINUE
      IF (NCNDL.LE.0)                              GO TO 300
C
      NCNDR = 0
      DO 220 I=1,LROWS(KTCHA)
        IF (NCNDR.EQ.MCAND)                         GO TO 220
        IF (RTABL(KTCHA,I,JTCHIR).GE.-EPS)          GO TO 220
        RMINI = ABS(RTABL(KTCHA,I,JTCHD0))
        IF (RMINI.LT.TPPROW(1))                     GO TO 220
        IF (ITABL(KTLNK,I,JTLNDN).NE.0)             GO TO 220
        NCNDR = NCNDR+1
        IBUF2(NCNDR) = I
        LB2(NCNDR)   = .FALSE.
  220 CONTINUE
      IF (NCNDR.LE.0)                               GO TO 300
C
C-------------------------------------------------------------------C
C              DEFINE BOTTOM LINKS BETWEEN CANDIDATES               C
C              **************************************               C
C-------------------------------------------------------------------C
C
      DO 240 ICNDL = 1,NCNDL
        K1     = IBUF1(ICNDL)
        CHX    = CHTMX
        DO 230 ICNDR = 1,NCNDR
          IF (LB2(ICNDR))                               GO TO 230
          K2     = IBUF2(ICNDR)
          CHI2 = (DD0(K1)-DD0(K2))**2
     1        /((1.0+CHF(K1))/EDD0(K1) + (1.0+CHF(K2))/EDD0(K2))
C
          IF (CHI2.GT.(5.0*CHTMX))                       GO TO 230
C
          DPHI = AMOD(PH0(K1)-PH0(K2)-PI,TWOPI)
          IF (DPHI.LT.-5.0)DPHI=DPHI+TWOPI
          CHI2 = CHI2
     1       +   (ALP(K1)+ALP(K2))**2
     2       /( (1.0+CHF(K1))/EALP(K1) +(1.0+CHF(K2))/EALP(K2) )
     3       +     0.2*DPHI**2
     4       /((2.0+CHF(K1))/EPH0(K1) + (2.0+CHF(K2))/EPH0(K2) )
     5       +     (ZZ0(K1)-ZZ0(K2))**2
     6       /((1.0+CHF(K1))/EZZ0(K1) + (1.0+CHF(K2))/EZZ0(K2) )
          CHI2 = 0.25*CHI2
          IF (CHI2.LT.CHX) THEN
            LOOPR = ICNDR
            CHX = CHI2
          END IF
  230   CONTINUE
        IF (CHX.LT.CHTMX) THEN
          LB2(LOOPR) = .TRUE.
          IW(KROW(KTLNK,IBUF1(ICNDL))+JTLNBM)=IBUF2(LOOPR)
          IW(KROW(KTLNK,IBUF2(LOOPR))+JTLNBM)=IBUF1(ICNDL)
        END IF
  240 CONTINUE
C
  300 CONTINUE
C
C-------------------------------------------------------------------C
C              SELECT CANDIDATES FOR BOTTOM LINKS                   C
C                EXTRAPOLATING INTO THE I.T.C.                      C
C              **********************************                   C
C-------------------------------------------------------------------C
c-----> Find seperately candidates for left and right parts
C       Criteria:  curvature positive
C                  2* radius < TPC radius
C                  minimum distance to beam line < R(ITC)
C                  no downward link found
C
      NCNDL = 0
      DO 310 I=1,LROWS(KTCHA)
        IF (NCNDL.EQ.MCAND)                         GO TO 310
        IF (RTABL(KTCHA,I,JTCHIR).LE.EPS)           GO TO 310
        RLRGE = RTOP(I)
        IF (RLRGE.GT.RTPCMX)                        GO TO 310
        RMINI = ABS(RTABL(KTCHA,I,JTCHD0))
        IF (RMINI.GE.TPPROW(1))                     GO TO 310
        IF (ITABL(KTLNK,I,JTLNDN).NE.0)             GO TO 310
        NCNDL = NCNDL+1
        IBUF1(NCNDL) = I
  310 CONTINUE
      IF (NCNDL.LE.0)                               GO TO 400
C
      NCNDR = 0
      DO 320 I=1,LROWS(KTCHA)
        IF (NCNDR.EQ.MCAND)                        GO TO 320
        IF (RTABL(KTCHA,I,JTCHIR).GE.-EPS)         GO TO 320
        RLRGE = RTOP(I)
        IF (RLRGE.GT.RTPCMX)                       GO TO 320
        RMINI = ABS(RTABL(KTCHA,I,JTCHD0))
        IF (RMINI.GE.TPPROW(1))                    GO TO 320
        IF (ITABL(KTLNK,I,JTLNDN).NE.0)            GO TO 320
        NCNDR = NCNDR+1
        IBUF2(NCNDR)= I
        LB2(NCNDR)= .FALSE.
  320 CONTINUE
      IF (NCNDR.LE.0)                              GO TO 400
C
C-------------------------------------------------------------------C
C              DEFINE BOTTOM LINKS BETWEEN CANDIDATES               C
C              **************************************               C
C-------------------------------------------------------------------C
C
      DO 340 ICNDL = 1,NCNDL
        K1 = IBUF1(ICNDL)
        CHX = CHPMX
        DO 330 ICNDR = 1,NCNDR
          IF (LB2(ICNDR))                               GO TO 330
          K2 = IBUF2(ICNDR)
          CHI2 = 0.2 * (DD0(K1)-DD0(K2))**2
     1         /((1.0+CHF(K1))/EDD0(K1) + (1.0+CHF(K2))/EDD0(K2))
C
          IF (CHI2.GT.(5.0*CHTMX))                       GO TO 330
C
          DPHI = AMOD(PH0(K1)-PH0(K2)-PI,TWOPI)
          IF (DPHI.LT.-5.0)DPHI=DPHI+TWOPI
          CHI2 = CHI2
     1         +   (ALP(K1)+ALP(K2))**2
     2         /( (1.0+CHF(K1))/EALP(K1) +(1.0+CHF(K2))/EALP(K2) )
     3         +     0.2*DPHI**2
     4         /((2.0+CHF(K1))/EPH0(K1) + (2.0+CHF(K2))/EPH0(K2) )
     5         +    (ZZ0(K1)-ZZ0(K2))**2
     6         /((1.0+CHF(K1))/EZZ0(K1) + (1.0+CHF(K2))/EZZ0(K2) )
          CHI2 = 0.25*CHI2
          IF (CHI2.LT.CHX) THEN
            LOOPR = ICNDR
            CHX = CHI2
          END IF
  330   CONTINUE
        IF (CHX.LT.CHPMX) THEN
          LB2(LOOPR) = .TRUE.
          IW(KROW(KTLNK,IBUF1(ICNDL))+JTLNBM)=IBUF2(LOOPR)
          IW(KROW(KTLNK,IBUF2(LOOPR))+JTLNBM)=IBUF1(ICNDL)
        END IF
  340 CONTINUE
C
  400 CONTINUE
C
C-------------------------------------------------------------------C
C       Select Candidates for "long range + high pt" Links          C
C       ( for example muons passing sector boundaries )             C
C       ***************************************************         C
C-------------------------------------------------------------------C
C-----> Find separately candidates for top and bottom parts.
C       Criteria:  Curvature  > 5*R_tpc
C                  Only hits in the outer 6 pad rows
C                  no link found yet
C
      NCNDU = 0
      DO 410 I=1,LROWS(KTCHA)
        IF (NCNDU.EQ.MCAND)                          GO TO 410
        IF (ABS(RTABL(KTCHA,I,JTCHIR)).GT.OCUT)      GO TO 410
        ITEST = ITABL(KTCHA,I,JTCHPI)
        ISEL  = 0
        ISEL  = IAND(ITEST,IF15)
        IF (ISEL.NE.0)                               GO TO 410
        IF (ITABL(KTLNK,I,JTLNDN).NE.0)              GO TO 410
        NCNDU = NCNDU + 1
        IBUF1(NCNDU) = I
        I1PAT(NCNDU) = ITEST
  410 CONTINUE
      IF (NCNDU.LE.0)                     GO TO 500
C
      NCNDL  = 0
      DO 420 I=1,LROWS(KTCHA)
        IF (NCNDL.EQ.MCAND)                           GO TO 420
        IF (ABS(RTABL(KTCHA,I,JTCHIR)).GT.OCUT)       GO TO 420
        ITEST = ITABL(KTCHA,I,JTCHPI)
        ISEL  = 0
        ISEL  = IAND(ITEST,IL06)
        IF (ISEL.NE.0)                                GO TO 420
        IF (ITABL(KTLNK,I,JTLNUP).NE.0)               GO TO 420
        LSTPT = ITABL(KTCHA,I,JTCHOF) + ITABL(KTCHA,I,JTCHNC)
        LB1PT = LSTPT - 1
        LSTCO = IW(KTCTC+2+LSTPT)
        LB1CO = IW(KTCTC+2+LB1PT)
        R1= RTABL(KTPCO,LB1CO,JTPCRV)
        R2= RTABL(KTPCO,LSTCO,JTPCRV)
        Z1= RTABL(KTPCO,LB1CO,JTPCZV)
        Z2= RTABL(KTPCO,LSTCO,JTPCZV)
        ZEXT= ABS(Z2 + 2.0*DROW*(Z2-Z1)/(R2-R1))
        IF (ZEXT.GT.ZTPCMX)                             GO TO 420
        NCNDL = NCNDL + 1
        IBUF2(NCNDL) = I
        LB2(NCNDL)   = .FALSE.
  420 CONTINUE
      IF (NCNDL.LE.0)                      GO TO 500
C
C-------------------------------------------------------------------C
C              DEFINE UP/DOWN LINKS BETWEEN CANDIDATES              C
C              ***************************************              C
C-------------------------------------------------------------------C
C
      DO 440 ICNDU = 1,NCNDU
        ITOP  = IBUF1(ICNDU)
        NQU   = ITABL(KTCHA,ITOP,JTCHNC)
        DO 425 IPT = 1,NQU
          LILLY(IPT) = IW(LIST(ITOP)+IPT)
  425   CONTINUE
        chi2_best = 1.e30
        link_best = 0
        DO 430 ICNDL = 1,NCNDL
          IBOT   = IBUF2(ICNDL)
          IF (LB2(ICNDL))           GO TO 430
          NQL   = ITABL(KTCHA,IBOT,JTCHNC)
          DO 426 IPT = 1,NQL
            LILLY(NQU+IPT) = IW(LIST(IBOT)+IPT)
  426     CONTINUE
          NLILL  = NQU+NQL
          CALL TFTHEL('TPCO',NLILL,LILLY,1,VAL,ERR,CHI2)
          CHI2 = CHI2/FLOAT(2*NLILL-5)
          IF (CHI2.LT.CHFRE .and. chi2.lt.chi2_best) THEN
            chi2_best = chi2
            link_best = icndl
          endif
  430   CONTINUE
C+ Select best link rather than the first
        if (link_best.gt.0) then
            LB2(link_best)   = .TRUE.
            ibot = ibuf2(link_best)
            IW(KROW(KTLNK,IBOT)+JTLNUP) =ITOP
            IW(KROW(KTLNK,ITOP)+JTLNDN) =IBOT
        endif
  440 CONTINUE
C
  500 CONTINUE
      RETURN
  998 CONTINUE
      RETURN
  999 CONTINUE
      CALL RERROR('TFLNKS',1,'No room in BOS for named banks.')
      IER=1
      RETURN
      END
#endif
