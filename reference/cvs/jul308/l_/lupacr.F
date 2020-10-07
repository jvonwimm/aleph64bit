      SUBROUTINE LUPACR(IER)
C----------------------------------------------------------------------
C! Create and fill the LUPA bank
C!
C!    Author:     H. Meinhard       16-Mar-1990
C!    Modified:   P. Hansen         04-Apr-1990  (1)
C!    Modified:   H. Meinhard       10-Apr-1990  (2)
C!    Modified:   H. Meinhard       27-Aug-1990  (3)
C!    Modified :- E. Lancon             10-APR-1991
C!                      Use ALTRIG to access Trigger bits
C!    Modified:   H. Meinhard       24-Apr-1991  (5)
C!    Modified:   H. Meinhard       30-Apr-1991  (6)
C!    Modified:   H. Meinhard       09-Aug-1991  (7)
C!                      Create LUPA for MC regardless of trigger pattern
C!    Modified:   P. Hansen         01-Aug-1991  (7)
C!                   Remove the NDROP('LMAP',0)
C!    Modified:   P. Hansen         01-MAY-1996  (8)
C!                   Replace refs to obsolete banks XTCN and XTOP
C!                   by refs to X1RG and X1TT
C!    Modified:   M. Cattaneo       17-JAN-1997  (9)
C!                   Reinstate filling of trigger enable mask in LUPA
C!    Modified:   M. Cattaneo       06-NOV-1997  (10)
C!                   Store X1RG, XTPB, XTCN, SFTR name indices locally
C!
C!    Input:      - banks 'LCLU','LIDT','LIFL','LOLE','LUMI','LOCL'
C!                        'PLPD','PLSD','PSCO','SFTR','XTBP','XTCN',
C!                        'XTOP'
C!                  from data base: 'LCRE'
C!    Output:     - IER       /I    error flag:
C!                                  0 = no error
C!                                  1 = some input bank missing
C!                                  2 = no space for new bank
C!                                  -1= LUPA not created
C!                - bank 'LUPA'
C----------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "lcnamc.h"
#include "lclujj.h"
#include "lcrejj.h"
#include "lidtjj.h"
#include "lifljj.h"
#include "locljj.h"
#include "lolejj.h"
#include "lpotun.h"
#include "lsdajj.h"
#include "lumijj.h"
#include "lupacm.h"
#include "lupajj.h"
#include "plpdjj.h"
#include "plsdjj.h"
#include "pscojj.h"
#include "psnamc.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "sftrjj.h"
#include "xtcnjj.h"
#include "x1ttjj.h"
#include "x1rgjj.h"
      PARAMETER(JXTBTT=1,JXTBCL=3,JXTBT1=4,JXTBT2=5,JXTBGC=6,JXTBLL=7,
     +          JXTBHT=8,JXTBHW=17,JXTBET=26,JXTBEW=35,JXTBLT=44,
     +          JXTBTE=48,JXTBIT=50,JXTBTP=53,JXTBTR=56,LXTBPA=58)
      INTEGER NEMAX(2),ISTRK(2),IPOI(2),ICOOR(2),MADDR(2),NEMX2(2),
     +  MADD2(2)
      LOGICAL TLCAL,TRAND,TPHYS,TSATR,FAIL
      REAL ECLU(2),THCLU(2),PHCLU(2),THTRK(2),PHTRK(2),X0TRK(2),
     +  Y0TRK(2),CH2TR(2),EWIR(2),XCLU(2),YCLU(2),XWIDT(2),YWIDT(2),
     +  XYCOR(2),E1CLU(2),E2CLU(2),XLOC(3,2),ECLU2(2),XLOC2(3,2)
#include "bosext.h"
      SAVE NAXTBP, NAXTCN, NAX1RG, NASFTR, NLOLE
      INTEGER ALTRIG
      EXTERNAL ALTRIG
      DATA ICALL /0/
#include "bmacro.h"
#include "lcmacr.h"
C----------------------------------------------------------------------
C initialization
      IF (ICALL .EQ. 0) THEN
        KX1TT = IW(NAMIND('X1TT'))
        IF(KX1TT.GT.0) THEN
          JOFF = KX1TT+LMHLEN+JX1TDS
          IDOWN = IW(JOFF)
        ENDIF
C Store name indices
        NLOLE = NAMIND('LOLE')
        NASFTR = NAMIND('SFTR')
        NAXTBP = NAMIND('XTBP')
        NAXTCN = NAMIND('XTCN')
        NAX1RG = NAMIND('X1RG')
        ICALL = 1
      ENDIF
C reset the error flag
      IER = 0
C do we have an LCAL trigger?
      CALL RTRIGF(IRUNRC,TLCAL,TRAND,TPHYS,TSATR,FAIL)
      IF (.NOT.FAIL .AND. .NOT.FMCRUN) THEN
        IF (.NOT.TLCAL)                                     GOTO 998
      ENDIF
C create LUPA bank
      CALL AUBOS('LUPA',0,LMHLEN+LLUPAA,JLUPA,IGARB)
      IF (JLUPA .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IW(JLUPA+LMHCOL) = LLUPAA
      IW(JLUPA+LMHROW) = 1
C link to banks needed
      JLCLU = IW(NALCLU)
      JLCRE = IW(NALCRE)
      JLIDT = IW(NALIDT)
      JLIFL = IW(NALIFL)
      JLOCL = IW(NALOCL)
      JLOLE = IW(NLOLE)
      JLSDA = IW(NALSDA)
      JLUMI = IW(NALUMI)
      JPLPD = IW(NAPLPD)
      JPLSD = IW(NAPLSD)
      JPSCO = IW(NAPSCO)
      JSFTR = IW(NASFTR)
      JXTBP = IW(NAXTBP)
      JXTCN = IW(NAXTCN)
      JX1RG = IW(NAX1RG)
C pointer to first LUPA row
      KLUPA = KROW(JLUPA,1)
C get side of fiducial cut
      IF (MOD(IEVTRC,2) .EQ. 0) THEN
        IPOI(1) = 1
        IPOI(2) = 2
      ELSE
        IPOI(1) = 2
        IPOI(2) = 1
      ENDIF
C GBX counter
      CALL LGNGBX(NGBX)
      IW(KLUPA+JLUPGB) = NGBX
C HV bit pattern
      IF(JX1RG.NE.0) THEN
        DO 2 IROW=1,LROWS(JX1RG)
          IF(CHAINT(ITABL(JX1RG,IROW,1)).EQ.'TSR ') THEN
            IW(KLUPA+JLUPHV)=ITABL(JX1RG,IROW,4)
            GOTO 3
          ENDIF
    2   CONTINUE
    3   CONTINUE
      ELSEIF(JXTCN.NE.0) THEN
        IW(KLUPA+JLUPHV)=ITABL(JXTCN,1,JXTCTR)
      ENDIF
C trigger level 1 bit pattern
      IALTR = ALTRIG(IT1,IT2,IL2)
      JTRL1 = 0
      IF ( IALTR.GT.0 ) THEN
        JTRL1 = IT1
      ELSE IF (JXTBP .NE. 0) THEN
        JTRL1 = ITABL(JXTBP,1,JXTBT1)
      ENDIF
      IW(KLUPA+JLUPT1) = JTRL1
C trigger level 2 bit pattern
      IF ( IALTR.GT.0 ) IW(KLUPA+JLUPL2) = IL2
C trigger enable bit pattern
      JTREN = 0
      IF (JXTCN .NE. 0) JTREN = ITABL(JXTCN,1,JXTCTR+1)
      IW(KLUPA+JLUPTE) = JTREN
C downscale factor
      IW(KLUPA+JLUPMD) = 1
      IF (BTEST(IL2,5)) THEN
        IW(KLUPA+JLUPMD) = IDOWN
      ENDIF
C LOLE information word
      IF (JLOLE .NE. 0) THEN
        IF (ITABL(JLOLE,1,JLOLER) .NE. 0) THEN
          DO 200 IL=1,11
            IF(ITABL(JLOLE,1,IL).NE.0) THEN
              IW(KLUPA+JLUPLO) = 1000*IL
              IF(IL.LE.5.OR.IL.GE.11) THEN
                IW(KLUPA+JLUPLO) = IW(KLUPA+JLUPLO)+ITABL(JLOLE,1,IL)
              ENDIF
            ENDIF
  200     CONTINUE
        ENDIF
      ELSE
        IW(KLUPA+JLUPLO) = -1
      ENDIF
C total fiducial LCAL energy
      EFIDU = 0.
      IF (JPLSD .NE. 0) THEN
        DO 310 ITOW = 1, LROWS(JPLSD)
          IADDR = ITABL(JPLSD,ITOW,JPLSAD)
          IPMOD = LCMOD(IADDR)
          IPROW = LCROW(IADDR)
          IPCOL = LCCOL(IADDR)
          IF (ISTOLU(IPMOD,IPROW,IPCOL) .GT. 0) THEN
            SCALE = REAL(ISCALU(IPMOD,IPROW,IPCOL)) / 100.
            EFIDU = EFIDU + SCALE*UNITES*
     +        REAL(ITABL(JPLSD,ITOW,JPLSE1) + ITABL(JPLSD,ITOW,JPLSE2) +
     +             ITABL(JPLSD,ITOW,JPLSE3))
          ENDIF
  310   CONTINUE
        RW(KLUPA+JLUPEF) = EFIDU
      ENDIF
C on both sides, find most energetic, and second most energetic,
C LCAL cluster
      CALL VZERO(NEMAX,2)
      CALL VZERO(ECLU,2)
      CALL VZERO(NEMX2,2)
      CALL VZERO(ECLU2,2)
      IF (JLIDT .NE. 0) THEN
        DO 320 ICL = 1, LROWS(JLIDT)
          THETA = RTABL(JLIDT,ICL,JLIDTC)
          ENERG = RTABL(JLIDT,ICL,JLIDEC)
          IF (THETA .LT. 0.) THETA = THETA + PI
          ISIDE = INT(THETA/PIBY2) + 1
          IF (ENERG .GT. ECLU(ISIDE)) THEN
            NEMX2(ISIDE) = NEMAX(ISIDE)
            ECLU2(ISIDE) = ECLU(ISIDE)
            NEMAX(ISIDE) = ICL
            ECLU(ISIDE) = ENERG
          ELSE IF (ENERG .GT. ECLU2(ISIDE)) THEN
            NEMX2(ISIDE) = ICL
            ECLU2(ISIDE) = ENERG
          ENDIF
  320   CONTINUE
      ENDIF
C require some LCAL energy
      IF (EFIDU .LE. 0. .AND. ECLU(1) .LE. 0. .AND.
     +    ECLU(2) .LE. 0 .AND. .NOT. TSATR)                 GOTO 999
C acceptance method bit pattern
C bit pattern is now:
C bits  0 ...  9 : Overall acceptance bit for method i-1
C bits 10 ... 19 : Fiducial cut survived on tight acceptance side
C bits 20 ... 29 : Fiducial cut survived on loose acceptance side
C bits 30 ... 31 : = 1 (Version number of acceptance code)
      IACC = 0
      IACC1 = 0
      IACC2 = 0
      IF (JLUMI .NE. 0) THEN
        DO 330 IMETH = 1, LROWS(JLUMI)
          IF (RTABL(JLUMI,IMETH,JLUMBH) .NE. ACPTLU(IMETH))
     +      IACC = IBSET(IACC,IMETH-1)
          ACPTLU(IMETH) = RTABL(JLUMI,IMETH,JLUMBH)
  330   CONTINUE
      ENDIF
      ICL = NEMAX(IPOI(1))
      IF (ICL .GT. 0) IACC1 = ITABL(JLIFL,ICL,JLIFK1)
      ICL = NEMAX(IPOI(2))
      IF (ICL .GT. 0) IACC2 = ITABL(JLIFL,ICL,JLIFK2)
      CALL MVBITS(IACC1,0,10,IACC,10)
      CALL MVBITS(IACC2,0,10,IACC,20)
      CALL MVBITS(1,0,2,IACC,30)
      IW(KLUPA+JLUPAM) = IACC
C cluster energies
      RW(KLUPA+JLUPEC) = ECLU(IPOI(1))
      RW(KLUPA+JLUPEC+1) = ECLU(IPOI(2))
      RW(KLUPA+JLUPES) = ECLU2(IPOI(1))
      RW(KLUPA+JLUPES+1) = ECLU2(IPOI(2))
C get theta, phi of most energetic LCAL cluster on both sides
      CALL VZERO(ISTRK,2)
      CALL VZERO(THCLU,2)
      CALL VZERO(PHCLU,2)
      CALL VZERO(MADDR,2)
      CALL VZERO(XWIDT,2)
      CALL VZERO(YWIDT,2)
      CALL VZERO(XYCOR,2)
      CALL VZERO(XLOC,6)
      CALL VZERO(XLOC2,6)
      CALL VZERO(MADD2,2)
      DO 350 ISIDE = 1, 2
        ICL = NEMAX(ISIDE)
        IF (ICL .NE. 0) THEN
          THCLU(ISIDE) = RTABL(JLIDT,ICL,JLIDTC)
          IF (THCLU(ISIDE) .GT. PIBY2) THCLU(ISIDE) = THCLU(ISIDE) - PI
          THCLU(ISIDE) = THCLU(ISIDE) * 1000.
          PHCLU(ISIDE) = RTABL(JLIDT,ICL,JLIDPC)
          IF (PHCLU(ISIDE) .GT. PI) PHCLU(ISIDE) = PHCLU(ISIDE) - TWOPI
          PHCLU(ISIDE) = PHCLU(ISIDE) * RADEG
          ISTRK(ISIDE) = ITABL(JLIDT,ICL,JLIDST)
          XWIDT(ISIDE) = RTABL(JLIDT,ICL,JLIDXX)
          YWIDT(ISIDE) = RTABL(JLIDT,ICL,JLIDYY)
          XYCOR(ISIDE) = RTABL(JLIDT,ICL,JLIDXY)
          E1CLU(ISIDE) = RTABL(JLIDT,ICL,JLIDE1)
          E2CLU(ISIDE) = RTABL(JLIDT,ICL,JLIDE2)
          IF(JLOCL.GT.0) THEN
            XLOC(1,ISIDE) = RTABL(JLOCL,ICL,JLOCXL)
            XLOC(2,ISIDE) = RTABL(JLOCL,ICL,JLOCYL)
            XLOC(3,ISIDE) = RTABL(JLOCL,ICL,JLOCZL)
          ENDIF
C search address of highest energetic tower
          IF (JLSDA .NE. 0) THEN
            EMX = 0.0
            DO 340 ITOW = 1, LROWS(JLSDA)
              ILCL = ITABL(JLSDA,ITOW,JLSDLC)
              IF (ILCL .NE. ITABL(JLIDT,ICL,JLIDLO))        GOTO 340
              EN = RTABL(JLSDA,ITOW,JLSDEN) +
     +          RTABL(JLSDA,ITOW,JLSDEN+1) + RTABL(JLSDA,ITOW,JLSDEN+2)
              IF (EN .GT. EMX) THEN
                MADDR(ISIDE) = ITABL(JLSDA,ITOW,JLSDAD)
                EMX = EN
              ENDIF
  340       CONTINUE
          ENDIF
        ENDIF
        ICL = NEMX2(ISIDE)
        IF (ICL .NE. 0) THEN
          IF(JLOCL.GT.0) THEN
            XLOC2(1,ISIDE) = RTABL(JLOCL,ICL,JLOCXL)
            XLOC2(2,ISIDE) = RTABL(JLOCL,ICL,JLOCYL)
            XLOC2(3,ISIDE) = RTABL(JLOCL,ICL,JLOCZL)
          ENDIF
C search address of highest energetic tower
          IF (JLSDA .NE. 0) THEN
            EMX = 0.0
            DO 345 ITOW = 1, LROWS(JLSDA)
              ILCL = ITABL(JLSDA,ITOW,JLSDLC)
              IF (ILCL .NE. ITABL(JLIDT,ICL,JLIDLO))        GOTO 345
              EN = RTABL(JLSDA,ITOW,JLSDEN) +
     +          RTABL(JLSDA,ITOW,JLSDEN+1) + RTABL(JLSDA,ITOW,JLSDEN+2)
              IF (EN .GT. EMX) THEN
                MADD2(ISIDE) = ITABL(JLSDA,ITOW,JLSDAD)
                EMX = EN
              ENDIF
  345       CONTINUE
          ENDIF
        ENDIF
  350 CONTINUE
C wire energies
      CALL VZERO(EWIR,2)
      IF (JPLPD .NE. 0) THEN
        DO 360 IWIR = 1, LROWS(JPLPD)
          IADDR = ITABL(JPLPD,IWIR,JPLPAD)
          IMODU = IADDR/64 + 1
          IF (IMODU .LE. 0 .OR. IMODU .GE. 5)               GOTO 360
          ISIDE = 2 - (IMODU-1)/2
          IPLAN = MOD(IADDR,64)
          IF (IPLAN .LE. 0 .OR. IPLAN .GE. 39)              GOTO 360
          IF (IMODU .EQ. LCMOD(MADDR(ISIDE))) THEN
            EWIR(ISIDE) = EWIR(ISIDE) +
     +        UNITEW*REAL(ITABL(JPLPD,IWIR,JPLPEN))
          ENDIF
  360   CONTINUE
      ENDIF
      RW(KLUPA+JLUPEW) = EWIR(IPOI(1))
      RW(KLUPA+JLUPEW+1) = EWIR(IPOI(2))
C cluster thetas, cluster phis
      RW(KLUPA+JLUPTC) = THCLU(IPOI(1))
      RW(KLUPA+JLUPPC) = PHCLU(IPOI(1))
      RW(KLUPA+JLUPTC+1) = THCLU(IPOI(2))
      RW(KLUPA+JLUPPC+1) = PHCLU(IPOI(2))
C cluster x, y coordinates (global)
      ZREF = RTABL(JLCRE,1,JLCRZC)
      SREF = RTABL(JLCRE,1,JLCRSR)
      XCLU(1) =  ZREF * TAN(THCLU(1)/1000.) * COS(PHCLU(1)*DEGRA)
      YCLU(1) =  ZREF * TAN(THCLU(1)/1000.) * SIN(PHCLU(1)*DEGRA)
      XCLU(2) = -ZREF * TAN(THCLU(2)/1000.) * COS(PHCLU(2)*DEGRA)
      YCLU(2) = -ZREF * TAN(THCLU(2)/1000.) * SIN(PHCLU(2)*DEGRA)
      RW(KLUPA+JLUPXC) = XCLU(IPOI(1))
      RW(KLUPA+JLUPYC) = YCLU(IPOI(1))
      RW(KLUPA+JLUPXC+1) = XCLU(IPOI(2))
      RW(KLUPA+JLUPYC+1) = YCLU(IPOI(2))
C cluster x, y coordinates (local)
      IF (XLOC(3,1) .GT. 200.0) THEN
        XLOC(1,1) = XLOC(1,1) * ZREF / XLOC(3,1)
        XLOC(2,1) = XLOC(2,1) * ZREF / XLOC(3,1)
      ENDIF
      IF (XLOC(3,2) .LT. -200.0) THEN
        XLOC(1,2) = XLOC(1,2) * (-ZREF) / XLOC(3,2)
        XLOC(2,2) = XLOC(2,2) * (-ZREF) / XLOC(3,2)
      ENDIF
      IF (XLOC2(3,1) .GT. 200.0) THEN
        XLOC2(1,1) = XLOC2(1,1) * ZREF / XLOC2(3,1)
        XLOC2(2,1) = XLOC2(2,1) * ZREF / XLOC2(3,1)
      ENDIF
      IF (XLOC2(3,2) .LT. -200.0) THEN
        XLOC2(1,2) = XLOC2(1,2) * (-ZREF) / XLOC2(3,2)
        XLOC2(2,2) = XLOC2(2,2) * (-ZREF) / XLOC2(3,2)
      ENDIF
      RW(KLUPA+JLUPXL) = XLOC(1,IPOI(1))
      RW(KLUPA+JLUPYL) = XLOC(2,IPOI(1))
      RW(KLUPA+JLUPXL+1) = XLOC(1,IPOI(2))
      RW(KLUPA+JLUPYL+1) = XLOC(2,IPOI(2))
      RW(KLUPA+JLUPXS) = XLOC2(1,IPOI(1))
      RW(KLUPA+JLUPYS) = XLOC2(2,IPOI(1))
      RW(KLUPA+JLUPXS+1) = XLOC2(1,IPOI(2))
      RW(KLUPA+JLUPYS+1) = XLOC2(2,IPOI(2))
C Energy fractions
      RW(KLUPA+JLUPE1) = E1CLU(IPOI(1))
      RW(KLUPA+JLUPE2) = E2CLU(IPOI(1))
      RW(KLUPA+JLUPE1+1) = E1CLU(IPOI(2))
      RW(KLUPA+JLUPE2+1) = E2CLU(IPOI(2))
C Energy asymmetries
      IADDR = MADDR(IPOI(1))
      CALL LASYM(IADDR,AX1,AY1)
      RW(KLUPA+JLUPAX) = AX1
      RW(KLUPA+JLUPAY) = AY1
      IADDR = MADDR(IPOI(2))
      CALL LASYM(IADDR,AX2,AY2)
      RW(KLUPA+JLUPAX+1) = AX2
      RW(KLUPA+JLUPAY+1) = AY2
*/----------------------------------------------------------
C shower width
      RW(KLUPA+JLUPXX) = XWIDT(IPOI(1))
      RW(KLUPA+JLUPYY) = YWIDT(IPOI(1))
      RW(KLUPA+JLUPXY) = XYCOR(IPOI(1))
      RW(KLUPA+JLUPXX+1) = XWIDT(IPOI(2))
      RW(KLUPA+JLUPYY+1) = YWIDT(IPOI(2))
      RW(KLUPA+JLUPXY+1) = XYCOR(IPOI(2))
C X and Y of centroid in each storey
      DO 370 ISIDE = 1, 2
        ICL = NEMAX(ISIDE)
        IF (ICL .LE. 0)                                     GOTO 370
        ISIG = 3 - 2*ISIDE
        IOFF = 1
        IF (ISIDE .EQ. IPOI(1)) IOFF = 0
        ICLU = ITABL(JLIDT,ICL,JLIDLO)
        THS = RTABL(JLCLU,ICLU,JLCLT1)
        PHS = RTABL(JLCLU,ICLU,JLCLF1)
        ZEFF = RTABL(JLCRE,1,JLCRZ1) * 2.892 / SREF
        X1 = FLOAT(ISIG) * ZEFF * COS(PHS) * TAN(THS)
        Y1 = FLOAT(ISIG) * ZEFF * SIN(PHS) * TAN(THS)
        THS = RTABL(JLCLU,ICLU,JLCLT2)
        PHS = RTABL(JLCLU,ICLU,JLCLF2)
        ZEFF = RTABL(JLCRE,1,JLCRZ2) * 2.985 / SREF
        X2 = FLOAT(ISIG) * ZEFF * COS(PHS) * TAN(THS)
        Y2 = FLOAT(ISIG) * ZEFF * SIN(PHS) * TAN(THS)
        THS = RTABL(JLCLU,ICLU,JLCLT3)
        PHS = RTABL(JLCLU,ICLU,JLCLF3)
        ZEFF = RTABL(JLCRE,1,JLCRZ3) * 3.136 / SREF
        X3 = FLOAT(ISIG) * ZEFF * COS(PHS) * TAN(THS)
        Y3 = FLOAT(ISIG) * ZEFF * SIN(PHS) * TAN(THS)
C cluster x,y averages
        RW(KLUPA+JLUPXA+IOFF) = X1 - 2.*X2 + X3
        RW(KLUPA+JLUPYA+IOFF) = Y1 - 2.*Y2 + Y3
C cluster x,y differences
        RW(KLUPA+JLUPXD+IOFF) = X1 - X3
        RW(KLUPA+JLUPYD+IOFF) = Y1 - Y3
  370 CONTINUE
C addresses of most energetic LCAL towers
      IW(KLUPA+JLUPAD) = MADDR(IPOI(1))
      IW(KLUPA+JLUPAD+1) = MADDR(IPOI(2))
C addresses of intersection tower of highest energetic nonets
      IADDR = MADDR(IPOI(1))
      IF (IADDR .NE. 0) CALL LACCEP(8,IADDR,FAIL)
      IW(KLUPA+JLUPAI) = IADDR
      IADDR = MADDR(IPOI(2))
      IF (IADDR .NE. 0) CALL LACCEP(8,IADDR,FAIL)
      IW(KLUPA+JLUPAI+1) = IADDR
      IADDR = MADD2(IPOI(1))
      IF (IADDR .NE. 0) CALL LACCEP(8,IADDR,FAIL)
      IW(KLUPA+JLUPAS) = IADDR
      IADDR = MADD2(IPOI(2))
      IF (IADDR .NE. 0) CALL LACCEP(8,IADDR,FAIL)
      IW(KLUPA+JLUPAS+1) = IADDR
C count SATR coordinates on both sides
      CALL VZERO(ICOOR,2)
      IF (JPSCO .NE. 0) THEN
        DO 380 ICO = 1, LROWS(JPSCO)
          ISIDE = ITABL(JPSCO,ICO,JPSCSI) + 1
          IF (ISIDE .LE. 0 .OR. ISIDE .GE. 3)               GOTO 380
          ICOOR(ISIDE) = ICOOR(ISIDE) + 64
  380   CONTINUE
      ENDIF
C get properties of tracks
      CALL VZERO(CH2TR,2)
      CALL VZERO(THTRK,2)
      CALL VZERO(PHTRK,2)
      CALL VZERO(X0TRK,2)
      CALL VZERO(Y0TRK,2)
      IF (JSFTR .NE. 0) THEN
        DO 390 ISIDE = 1, 2
          IST = ISTRK(ISIDE)
C if there was no LCAL cluster on that side, find best track on each
C side
          IF (IST .EQ. 0) THEN
            IF (NEMAX(ISIDE) .EQ. 0) THEN
              IQFMX = -1
              PRBMX = -1.
              DO 385 ITR = 1, LROWS(JSFTR)
                THTR = RTABL(JSFTR,ITR,JSFTTH)
                IS = INT(THTR/PIBY2) + 1
                IF (IS .NE. ISIDE)                          GOTO 385
                NDF = ITABL(JSFTR,ITR,JSFTNF)
                IQF = ITABL(JSFTR,ITR,JSFTQF)
                CH2 = RTABL(JSFTR,ITR,JSFTC2)
                IF (CH2 .LT. 0.) CH2 = 1.E-8
                IF (NDF .GT. 0) THEN
                  PRB = PROB(CH2,NDF)
                ELSE
                  PRB = 0.
                ENDIF
                IF (IQF .GT. IQFMX .OR.
     +             (IQF .EQ. IQFMX .AND. PRB .GT. PRBMX)) THEN
                  IST = ITR
                  IQFMX = IQF
                  PRBMX = PRB
                ENDIF
  385         CONTINUE
            ENDIF
          ENDIF
          IF (IST .NE. 0) THEN
            ICOOR(ISIDE) = ICOOR(ISIDE) +
     +        ITABL(JSFTR,IST,JSFTNF)*4 +
     +        ITABL(JSFTR,IST,JSFTQF)
            CH2TR(ISIDE) = RTABL(JSFTR,IST,JSFTC2)
            THETA = RTABL(JSFTR,IST,JSFTTH)
            IF (THETA .GT. PIBY2) THETA = THETA - PI
            THTRK(ISIDE) = THETA * 1000.
            PHI = RTABL(JSFTR,IST,JSFTPH)
            IF (PHI .GT. PI) PHI = PHI - TWOPI
            PHTRK(ISIDE) = PHI * RADEG
            X0TRK(ISIDE) = RTABL(JSFTR,IST,JSFTX0)
            Y0TRK(ISIDE) = RTABL(JSFTR,IST,JSFTY0)
          ENDIF
  390   CONTINUE
      ENDIF
C track information words
      IW(KLUPA+JLUPIT) = ICOOR(IPOI(1))
      IW(KLUPA+JLUPIT+1) = ICOOR(IPOI(2))
C track fit chi squares
      RW(KLUPA+JLUPCT) = CH2TR(IPOI(1))
      RW(KLUPA+JLUPCT+1) = CH2TR(IPOI(2))
C track thetas, phis
      RW(KLUPA+JLUPTT) = THTRK(IPOI(1))
      RW(KLUPA+JLUPPT) = PHTRK(IPOI(1))
      RW(KLUPA+JLUPTT+1) = THTRK(IPOI(2))
      RW(KLUPA+JLUPPT+1) = PHTRK(IPOI(2))
C track vertex x,y positions
      RW(KLUPA+JLUPXT) = X0TRK(IPOI(1))
      RW(KLUPA+JLUPYT) = Y0TRK(IPOI(1))
      RW(KLUPA+JLUPXT+1) = X0TRK(IPOI(2))
      RW(KLUPA+JLUPYT+1) = Y0TRK(IPOI(2))
C bank filled
      GOTO 999
C----------------------------------------------------------------------
  902 WRITE (LOUTRL,'(A)') ' LUPACR --> NO SPACE FOR NEW BANK LUPA'
      IER = 2
      GOTO 999
  998 IER = -1
      GOTO 999
  999 CONTINUE
      END
#endif
