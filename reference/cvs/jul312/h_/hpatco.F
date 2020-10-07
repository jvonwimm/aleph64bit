      SUBROUTINE HPATCO(NPATT)
C*************************************************************
C! Fills the HPCO bank                                       *
C!                                                           *
C! Author  : G. BAGLIESI  880202                             *
C!                                                           *
C! VERSION 1.0                                               *
C!                                                           *
C! INPUT   HTPA, HTUB BANKS                                  *
C! OUTPUT  HPCO, HPDS BANKS                                  *
C!                                                           *
C! NPAT0=Number  of patterns                                 *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "rflags.h"
#include "hwbnkw.h"
#include "hpcojj.h"
#include "hpdsjj.h"
#include "htpajj.h"
#include "htubjj.h"
#include "alcons.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "hcgeom.h"
      DIMENSION CHIT(24),CMIN(23),CMAX(23), SCX(2),SCY(2),SHIT(2),
     +          CENX(2),CENY(2),NXMOD(24)
      LOGICAL FIRST
C
      PARAMETER(GEVSTR=0.182)
      DATA FIRST/.TRUE./
#include "bmacro.h"
      IF(NPATT.EQ.0) GOTO 999
      NHTPX=NAMIND('HTPA')
      NHTUX=NAMIND('HTUB')
      IF(FIRST) THEN
         FIRST=.FALSE.
         IHPDHW = 0
      ENDIF
C
C  HTUB is linked
C
      IHTUB=IW(NHTUX)
      IF(IHTUB.EQ.0) THEN
         CALL RERROR('HPATCO',1,'HTUB BANK NOT FOUND')
         GOTO 90
      ENDIF
C
C  The HTPA bank is linked
C
      IHTPA = IW(NHTPX)
      IF(IHTPA.EQ.0) THEN
         CALL RERROR('HPATCO',2,'HTPA BANKS NOT FOUND')
         GOTO 90
      ENDIF
C
C  HPCO and HPDS are added to the event list
C
      CALL BLIST(IW,'E+','HPCOHPDS')
      LWORD    = LHPCOA
      LDPCO    = LWORD*NPATT+LMHLEN
      CALL AUBOS('HPCO',0,LDPCO,IHPCO,IGARB)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('HPATCO',3,'INSUFF. SPACE TO CREATE HPCO')
         GOTO 90
      ENDIF
      IF(IGARB.EQ.1) THEN
         IHTPA=IW(NHTPX)
         IHTUB=IW(NHTUX)
      ENDIF
      IW(IHPCO + LMHCOL) = LWORD
      IW(IHPCO + LMHROW) = 0
      NPADO         = 0
C
C  The pointer IHPDHW of the temporary WORK BANK is zeroed
C  The bank will fill  HPDS
C
      NRPDS = 0
C
C  Loop on PATTERNS
C
   30 CONTINUE
      NPADO = NPADO + 1
C
C  Sum variables are set to 0
C
      DO 35 II=1,2
         SCX(II)  = 0.
         SCY(II)  = 0.
         SHIT(II) = 0.
   35 CONTINUE
      DO 40 II=1,23
         CMIN(II) =  9999.
         CMAX(II) = -9999.
         CHIT(II) =  0.
   40 CONTINUE
      INEXT = KNEXT(IHPCO)
      IW(IHPCO + LMHROW) = IW(IHPCO + LMHROW) + 1
      IPATT = LROWS(IHTPA)
C
C  LOOP inside the PATTERN to decide the kind of border
C
      MMIN=100
      MMAX=0
      DO 51 IJ=1,24
         NXMOD(IJ) = 0
   51 CONTINUE
      DO 50 JI=1,IPATT
         LCUR = ITABL(IHTPA,JI,JHTPLR)
         MODBA = ITABL(IHTUB,LCUR,JHTUMN)
         IF(MODBA.GT.MMAX) MMAX=MODBA
         IF(MODBA.LT.MMIN) MMIN=MODBA
         NXMOD(MODBA) = NXMOD(MODBA) + 1
   50 CONTINUE
      NPMOD = 0
      DO 52 IJ=1,24
         IF(NXMOD(IJ).GT.0) NPMOD = NPMOD + 1
   52 CONTINUE
      ISUBC = ITABL(IHTUB,LCUR,JHTUSN)
      MDIF= MMAX-MMIN
      IF(ISUBC.EQ.2.AND.NPMOD.GT.2) THEN
         IF(JDBDRF(JULHC).GE.1)
     +       CALL REPORT('HPATCO','PATTERN TOO LARGE FOUND',0)
C
C  Here we try to extract the relevant information for pattern too large
C
         IDIR = -1
         COR1 =  0.
         COR2 =  0.
         CENX0=  0.
         CENY0=  0.
         INTP =  0
         DMAX =  0.
         DEN  =  0.
         IBORD = MDIF + 1
         SHITX = 0.
         IPL = 100
         LPL = -1
         DO 117 JIJ=1,IPATT
            LCUR = ITABL(IHTPA,JIJ,JHTPLR)
            CLUW = RTABL(IHTUB,LCUR,JHTUCW)
            ILAY = ITABL(IHTUB,LCUR,JHTULN)
            SHITX = SHITX + CLUW
            IF(ILAY.LT.IPL) IPL = ILAY
            IF(ILAY.GT.LPL) LPL = ILAY
  117    CONTINUE
         COR1 =  HCLARA(1) + (HCIRTH+HCTUTH)*FLOAT(LPL-IPL)
         PHI0 = PI/12.*(FLOAT(MMIN)+FLOAT(NPMOD)/2.)
         IF(PHI0.GE.2.*PI) PHI0 = PHI0 - 2.*PI
         COR2 = PHI0
         NHIT = INT((SHITX+0.001)/HCSABL)
         EDIG = SHITX*GEVSTR/HCSABL
         CALL WBANK(IW,IHPDHW,LMHLEN,*110)
         IW(IHPDHW + LMHCOL) = LHPDSA
         IW(IHPDHW + LMHROW) = 0
         GOTO 118
      ENDIF
C
C  Looking for the STRIP DIREC. INDEX
C
      IDIR = 0
      IF(ISUBC.EQ.2) THEN
         IDIR = 1
      ELSE
         IF(MODBA.EQ.1.OR.MODBA.EQ.6) IDIR = 2
         IF(MODBA.EQ.3.OR.MODBA.EQ.4) IDIR = 3
         IF(MODBA.EQ.2) IDIR = 4
         IF(MODBA.EQ.5) IDIR = 5
      ENDIF
C
C  Each case is treated in his proper way
C
      IF(ISUBC.EQ.2) THEN
C
C  BARREL
C
         NPLMO = 23
         IF(MMIN.EQ.1.AND.MMAX.EQ.24) THEN
            MMIN=24
            MMAX=1
         ENDIF
      ELSE
C
C  ENDCAP
C
         NPLMO = 22
         IF(MMIN.EQ.1.AND.MMAX.EQ.6) THEN
            MMIN=6
            MMAX=1
         ENDIF
      ENDIF
C
C
C  IBORD = 2 : PATTERN between two modules, then IMODA=1
C  means odd semimodule, otherways IMODA=2
C
      IBORD=1
      IF(MOD(MMIN,2).EQ.1.AND.ISUBC.EQ.2.AND.MMIN.NE.MMAX) IBORD=2
      IPL = 100
      LPL = -1
      DO 60 JI=1,IPATT
         LCUR = ITABL(IHTPA,JI,JHTPLR)
         MODBA = ITABL(IHTUB,LCUR,JHTUMN)
         ILAY =  ITABL(IHTUB,LCUR,JHTULN)
         XHIT =  RTABL(IHTUB,LCUR,JHTULC)
         CLUW =  RTABL(IHTUB,LCUR,JHTUCW)
         ISEGN = 1
         IF(MOD(MODBA,2).EQ.0) ISEGN = -1
         IMODA = 1
         IF(IBORD.EQ.2.AND.MOD(MODBA,2).EQ.0) IMODA = 2
         SCX(IMODA) = SCX(IMODA) + CLUW*XHIT*FLOAT(ISEGN)
         SCY(IMODA) = SCY(IMODA) + CLUW*FLOAT(ILAY)
         SHIT(IMODA) = SHIT(IMODA) + CLUW
         CHIT(ILAY) = CHIT(ILAY) + CLUW
         IF(IBORD.EQ.1) THEN
            IF(ISEGN.EQ.1) THEN
               XMIN= XHIT - CLUW/2.
               XMAX= XHIT + CLUW/2.
            ENDIF
            IF(ISEGN.EQ.-1) THEN
               XMAX = -(XHIT - CLUW/2.)
               XMIN = -(XHIT + CLUW/2.)
            ENDIF
         ELSE IF(IBORD.EQ.2) THEN
            IF(IMODA.EQ.1) THEN
               XMIN= HCLAWI(ILAY) - XHIT - CLUW/2.
               XMAX= HCLAWI(ILAY) - XHIT + CLUW/2.
            ENDIF
            IF(IMODA.EQ.2) THEN
               XMAX= -(HCLAWI(ILAY) - XHIT - CLUW/2.)
               XMIN= -(HCLAWI(ILAY) - XHIT + CLUW/2.)
            ENDIF
         ENDIF
         IF(XMIN.LT.CMIN(ILAY)) CMIN(ILAY) = XMIN
         IF(XMAX.GT.CMAX(ILAY)) CMAX(ILAY) = XMAX
         IF(ILAY.LT.IPL) IPL = ILAY
         IF(ILAY.GT.LPL) LPL = ILAY
   60 CONTINUE
      NHIT = INT((SHIT(1)+SHIT(2)+0.001)/HCSABL)
      EDIG = (SHIT(1)+SHIT(2))*GEVSTR/HCSABL
      DO 70 JD=1,IBORD
         CENX(JD) = SCX(JD)/SHIT(JD)
         CENY(JD) = SCY(JD)/SHIT(JD)
   70 CONTINUE
      DEN=0.
      DMAX = 0.
      INTP = 24
      DSUM = 0.
      CALL WBANK(IW,IHPDHW,LHPDSA*NPLMO+LMHLEN,*110)
      IW(IHPDHW + LMHCOL) = LHPDSA
      IW(IHPDHW + LMHROW) = 0
      DO 80 JJ=1,NPLMO
         IF(JJ.LT.NPLMO) THEN
           IF (INTP.EQ. 24.AND.CHIT(JJ).GT.2.
     &       .AND. CHIT(JJ+1).GT.2.) INTP = JJ
         ENDIF
         IF(INT(CHIT(JJ)).GT.0) THEN
            DIST = CMAX(JJ) - CMIN(JJ)
            IF(CHIT(JJ).GT.DIST) CHIT(JJ) = DIST
            DSUM = DSUM + DIST
            DEN  = DEN + CHIT(JJ)
            IF(DIST.GT.DMAX) DMAX=DIST
C
C  IHPDHW (working bank) is filled
C
            ICNEX = KNEXT(IHPDHW)
            IW(ICNEX + JHPDNL) = JJ
            RW(ICNEX + JHPDFL) = CHIT(JJ)
            RW(ICNEX + JHPDMD) = DIST
            IW(IHPDHW + LMHROW) = IW(IHPDHW + LMHROW) + 1
         ENDIF
   80 CONTINUE
      DEN = DEN/DSUM*100.
      IF(DEN.GT.100.) DEN = 100.
C
C  IHPDHW is reduced to his proper size and then copied to HPDS
C
 118  CONTINUE
      NRPDS = NRPDS + 1
      CALL WBANK(IW,IHPDHW,KNEXT(IHPDHW)-IHPDHW,*110)
      CALL BKFRW(IW,'HPDS',NRPDS,IW,IHPDHW,*110)
C
C   Now we have all the necessary to fill HPCO rows
C
C
C   Variables independend of the local geometry
C
      IW(INEXT + JHPCDI ) = IDIR
      RW(INEXT + JHPCDE ) = EDIG
      IW(INEXT + JHPCND ) = NHIT
      IW(INEXT + JHPCIP ) = INTP
      IW(INEXT + JHPCFP ) = IPL
      IW(INEXT + JHPCLP ) = LPL
      RW(INEXT + JHPCMD ) = DMAX
      RW(INEXT + JHPCPD ) = DEN
      IW(INEXT + JHPCBF ) = IBORD
      IW(INEXT + JHPCHT ) = IW(IHTPA - 2)
      IW(INEXT + JHPCHP ) = NRPDS
C
C  Variables dependend of the geometry
C
      IF(ISUBC.EQ.2.AND.IDIR.NE.-1) THEN
C
C BARREL
C
         IF(IBORD.EQ.1) THEN
            YCOR= (CENY(1)-1.)*(HCIRTH+HCTUTH) + HCLARA(1)
            COR1 = SQRT(YCOR** 2+ CENX(1)**2)
            PHI0 = ATAN(CENX(1)/YCOR)
            PHI0 = PHI0 + PI/6.*FLOAT(MMIN/2)
            IF(PHI0.GE.2.*PI) PHI0 = PHI0 - 2.*PI
            COR2 = PHI0
            CENX0 = CENX(1)
            CENY0 = (CENY(1)-1.)*(HCIRTH+HCTUTH)
         ELSE IF(IBORD.EQ.2) THEN
C
C  Barrel external border
C
            POI1 = SHIT(1)/(SHIT(1) + SHIT(2))
            POI2 = SHIT(2)/(SHIT(1) + SHIT(2))
            YCOR1 = (CENY(1)-1.)*(HCIRTH+HCTUTH) + HCLARA(1)
            YCOR2 = (CENY(2)-1.)*(HCIRTH+HCTUTH) + HCLARA(1)
            RCO1 = SQRT(YCOR1** 2+ CENX(1)**2)
            RCO2 = SQRT(YCOR2** 2+ CENX(2)**2)
            COR1 = RCO1*POI1 + RCO2*POI2
            PHI1 = ATAN(CENX(1)/YCOR1)
            PHI1 = PHI1 + PI/12.*FLOAT(MMIN-1)
            PHI2 = ATAN(CENX(2)/YCOR2)
            PHI2 = PHI2 + PI/12.*FLOAT(MMAX)
            COR2 = PHI1*POI1 + PHI2*POI2
            IF(COR2.GE.2.*PI) COR2 = COR2 - 2.*PI
            CENY0 = YCOR1*POI1 + YCOR2*POI2 - HCLARA(1)
            PHIC = PI/12.*FLOAT(MMAX-1)
            CENX0 = COR1*(COR2-PHIC)
         ENDIF
      ELSE IF(IDIR.NE.-1) THEN
C
C ENDCAP
C
         COR1 = (CENY(1)-1.)*(HCIRTH+HCTUTH)+HCZMIN(2)+HCIRTH
         IF(ISUBC.EQ.3) COR1 = - COR1
         COR2 = CENX(1)
         IF(IDIR.EQ.4.OR.IDIR.EQ.5) COR2 = ABS(COR2)
         CENX0 = CENX(1)
         CENY0 = (CENY(1)-1.)*(HCIRTH+HCTUTH)
      ENDIF
C
C  Filling
C
      RW(INEXT + JHPCC1 ) = COR1
      RW(INEXT + JHPCC2 ) = COR2
      RW(INEXT + JHPCCX ) = CENX0
      RW(INEXT + JHPCCY ) = CENY0
C
   85 CONTINUE
      IHTPA = IW(IHTPA-1)
      IF(IHTPA.NE.0) GOTO 30
   90 CONTINUE
      CALL WDROP(IW,IHPDHW)
      RETURN
  110 CONTINUE
      CALL RERROR('HPATCO',4,'INSUFF. SPACE FOR WBANK')
      CALL WDROP(IW,IHPDHW)
  999 RETURN
      END
#endif
