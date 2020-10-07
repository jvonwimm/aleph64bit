      FUNCTION TBLOCH(BGIN)
C
C----------------------------------------------------------------------
C! Bethe-Bloch parameterization of TPC dE/dx
C! See the note CERN-EP/87-97 by OPAL for details.
C!
C!   Created by Robert P. Johnson     22-AUG-1988
C!
C!   Inputs    : BG        /R      beta*gamma
C!               POIMIN    /R      Poisson minimum (from common GAMMA)
C!   Outputs   : TBLOCH    /R      dE/dx with min. ion. = 1.0 * POIMIN
C!
C!   Modifications:
C!      1.  Any particle below Beta-gamma of 0.1 will produce lots of
C!          ionization.  At BG = 0.1 we have about 35*Minimum ionizing.
C!          Since the TPC electronics saturate at 10*minimum ionizing
C!          anyway,  particles with BG < 0.1 are now treated as though
C!          their BG = 0.1.     DFC 21-OCT-88.
C!      2.  Tune the dE/dx parameterization.   Dave Casper 6 Nov 1992
C!      3.  Tune the dE/dx parameterization again. Dave Casper 30 Feb 94
C---------------------------------------------------------------------
C
C
C  TPCONS contains physical constants for TPC simulation
C
      COMMON /DELTA/ CDELTA,DEMIN,DEMAX,DELCLU,RADFAC,CYLFAC
      PARAMETER (MXGAMV = 8)
      COMMON /TGAMM/ GAMVAL(MXGAMV),GAMLOG(MXGAMV),POIFAC(MXGAMV),
     &               POIMAX,POIMIN,CFERMI,CA,CB,POIRAT,POICON
      PARAMETER (MXBINC = 20)
      COMMON /CLUST/ EBINC(MXBINC),CONCLU,WRKFUN,MXCL,CKNMIN,CFANO,CRUTH
     &              ,POWERC
      COMMON /AVALA/ THETA,ETHETA
      COMMON /TPTIME/ NTMXSH,NTMXNO,NTMXAN,NTMXDI,NTSCAN,NTBNAS,NTBAPD
      COMMON /TPELEC/ TPRPAR,TPRSER,TPCFET,TCFEED,TMVPEL,TSIGMX,NTPBIT
C
      DATA MM/3/,RM/3./,TL10/4.605170186/
      DATA XI/0.0925/,RK/8.5/,AA/0.05/,XA/2.05/,P/2.2/
      DATA BGMIN/0.1/
C
      IF (BGIN .GT. 0.1) THEN
         BG = BGIN
      ELSE
         BG = BGMIN
      ENDIF
C
      X0  = XA - (AA*(TL10/(RM*AA))**(RM/(RM-1.0)))/TL10
      X1  = X0 + (TL10/(RM*AA))**(1./(RM-1.0))
      BGL = ALOG10(BG)
      B2  = BG**2/(1.0+BG**2)
      B   = SQRT(B2)
      BP  = B**P
C
      IF (BGL.LT.X0) THEN
        DEL = 0.
      ELSEIF (BGL.LT.X1) THEN
        DEL = TL10*(BGL-XA)+AA*(X1-BGL)**MM
      ELSE
        DEL = TL10*(BGL-XA)
      ENDIF
C
      TBLOCH = (XI/BP)*(RK+TL10*BGL-BP-DEL)*POIMIN
      IF(TBLOCH .LT. 0.) THEN
        WRITE(6,100) TBLOCH,BG,BGL,BP,DEL
        TBLOCH = -TBLOCH
      ENDIF
C
  999 RETURN
C --------------------------------------------------------------
  100 FORMAT(//1X,'+++TBLOCH+++ Warning : POISAV value < 0.'/
     .       1X,'        Call expert...                  '/
     .       1X,'    Local variables in TBLOCH : '/
     .       1X,'     TBLOCH   = ',E12.6/
     .       1X,'     BG       = ',E12.6/
     .       1X,'     BGL      = ',E12.6/
     .       1X,'     BP       = ',E12.6/
     .       1X,'     DEL      = ',E12.6)
      END
