      SUBROUTINE UFVDMS( FIELD,
     +                   OMEGA, TANL, PHI0, D0, Z0, RI, RO, QMS )
C!-------------------------------------------------------------------
C! GET MULTIPLE SCATTERING IN VDET AND BEAM-PIPE IN A REALISTIC
C! APPROXIMATION FOR THE VDET GEOMETRY
C!
C!    AUTHOR:   T. LOHSE   8.12.1990
C!              G. Taylor 23.9.1992
C!              - Add option of allowing a more detailed treatment of th
C!                multiple scattering in the Vdet to be provided
C!                through a VDMS bank 0
C!
C!    INPUT:  FIELD  =  MAGNETIC FIELD IN KG (SINGLE PRECISION)
C!               THE REST IS DOUBLE PRECISION
C!            OMEGA  =  CURVATURE OF TRACK  (MUST BE .NE. 0)
C!            TANL   =  TAN(LAMBDA)
C!            PHI0   =  PHI-ANGLE OF TRACK AT ORIGIN
C!            D0     =  IMPACT PARAMETER OF TRACK
C!            Z0     =  Z AT TRACK ORIGIN
C!            RI     =  INNER RADIUS TO GO TO
C!                      IF NEGATIVE, FIT STOPS AT ABS(RI)
C!            RO     =  OUTER RADIUS TO START AT
C!            QMS    =  2*2 MULTIPLE SCATTERING MATRIX AT RO
C!    OUTPUT: QMS    ===>  CHANGED TO MULTIPLE SCATTERING MATRIX
C!                         UP TO RI
C!
C!       ****    EVERYTHING DOUBLE PRECISION    ****
C!
C!-------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! multiple-scattering constants in VDET,ITC,TPC
      COMMON /VRLDCOM/  UKRITC,UKSITC,UKRTPC,UKSTPC,UKSPITC,UKSPTPC,
     &                  UKRVAC,UKSVAC,UKZICA,UKRIICA,UKROICA,UKSICA,
     &                  UKZOCA,UKRIOCA,UKROOCA,UKSOCA
      REAL UKRITC,UKSITC,UKRTPC,UKSTPC,UKSPITC,UKSPTPC,UKRVAC,UKSVAC,
     &     UKZICA,UKRIICA,UKROICA,UKSICA,UKZOCA,UKRIOCA,UKROOCA,UKSOCA
C
C  New bank VDMS for VDET multiple scattering
C
      INTEGER LVDMSA,JVDMWI,JVDMFL,JVDMRA,JVDMUC,JVDMWC
      INTEGER JVDMPV,JVDMPU,JVDMPW,JVDMCU,JVDMSG
      PARAMETER(JVDMWI=1,JVDMFL=2,JVDMRA=3,JVDMUC=4,JVDMWC=5,JVDMPV=6,
     +          JVDMPU=7,JVDMPW=8,JVDMCU=9,JVDMSG=10,LVDMSA=10)
C
      DOUBLE PRECISION   OMEGA, TANL, PHI0, D0, Z0, RI, RO, QMS(2,2)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
C----LOCAL VARIABLES
C
      DOUBLE PRECISION DPI, DS, S1, R1, PH1, RH, RF, PHIF, THET, TERM,
     +                 RMAX, S, SL, SH, XH, YH, ZH, SMID, DELS, SI, R2,
     +                 SCACO, PHIN, THICK, UWID, ZWID, TILT, DELTAS,
     +                 Z1, SC, RC, XC, YC, SC1, RC1, XC1, YC1, SCF,
     +                 SCACAP
      DOUBLE PRECISION RAD,RADIUS,SXY,XMS,COSV,RVAC
C
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
      SXY(RADIUS)= 2.*DASIN(0.5*OMEGA*DSQRT(DABS((RADIUS**2-D0**2)
     &               /(1.-OMEGA*D0))))/OMEGA
C ----------------------------------------------------------------
C-
C get the multiple scattering  material description from the database
C RETURN if VRLD bank is missing - job should STOP
C-
      CALL VRLDGT( IER)
      IF(IER.LT.0) RETURN
C
      SCACO = 1000./0.29979/FIELD
      SCACO = 0.0141 * SCACO
      SCACO = (SCACO*OMEGA)**2/(1.+TANL**2)
C
      KVDMS=NLINK('VDMS',0)
      IF(KVDMS.GT.0) THEN
C do a more detailed treatment starting from the VDMS 0 bank
C This treatment also uses a better description of multiple
C scattering in 'thick' materials as given in the particle data book
C ie .0141 -> .0136*(1.+.038*log(x/X0))
c
       R1 = RO
       RMAX = DABS( D0 - 2. / OMEGA )
       IF ( R1 .GT. RMAX ) R1 = RMAX - 0.0001
       RI = ABS(RI)
       R2 = RI
       IF ( R2 .LT. DABS(D0) ) R2 = DABS(D0)
       IF ( R2 .GT. RMAX ) R2 = RMAX - 0.0001
C
C-----arc-length to inner and outer radii
C
       SI = SXY(R2)
       IF(DABS(0.5*OMEGA*DSQRT(DABS((R1**2-D0**2)
     &     /(1.-OMEGA*D0)))).GT.0.9999999) RETURN
       S1 = SXY(R1)
C
C assume for now that the 'VDMS' is ordered in decreasing radii
C
       DO 10 I=1,LROWS(KVDMS)
        IFLAG=ITABL(KVDMS,I,JVDMFL)
        IF(IFLAG.NE.0) THEN
         RAD=RTABL(KVDMS,I,JVDMRA)
         XMS=RTABL(KVDMS,I,JVDMSG)
         IF(XMS.LT.0.00001) XMS=.00001
         COSV=RTABL(KVDMS,I,JVDMPV)
C
C-----check if inner radius is reached
C
         IF ( R1 .LE. R2 ) RETURN
         IF ( R1 .LE. D0 ) RETURN
C
C-----arc-length to outer radius
C
         S1=SXY(R1)
         IF(RAD.LE.R1.AND.RAD.GT.R2) THEN
          SMID=SXY(RAD)
C
C---assume a tiny but finite width of the cylinder in order to avoid
C---numerical problems
C
          IF ( SMID .GT. 0.00005 ) SMID = SMID - 0.00005
C
C---transport to inner radius (drift)
C
          DS = S1 - SMID
          DS = DS * DSQRT( 1. + TANL**2 )
          QMS(1,1) = QMS(1,1) + DS*(QMS(1,2)+QMS(2,1))+DS**2*QMS(2,2)
          QMS(1,2) = QMS(1,2) + DS*QMS(2,2)
          QMS(2,1) = QMS(2,1) + DS*QMS(2,2)
C
C---add multiple scattering at inner radius
C .964 = 1.36/14.1
C
        QMS(2,2)=QMS(2,2)+.964*((1.+.038*dlog(xms/cosv)))*XMS*SCACO/COSV
C
C---update the outer radius to inner surface of this layer
C
          S1 = SMID
          R1 = DSQRT( D0**2 + 4./OMEGA**2*(1.-D0*OMEGA)*
     +            DSIN(OMEGA*SMID/2.)**2 )
         ENDIF
        ENDIF
   10  CONTINUE
C
C---track passed through vdet
C---now transport to inner radius, take care of beam pipe too
C
       IF (R1.GE.UKRVAC .AND. R2.LT.UKRVAC)  THEN
C
C---transport to beam pipe first (drift)
C
        IF ( UKRVAC .LE. ABS(D0) ) THEN
         S = 0.
        ELSE
         RVAC=UKRVAC
         S=SXY(RVAC)
        ENDIF
        DS = ( S1 - S ) * DSQRT( 1. + TANL**2 )
        QMS(1,1) = QMS(1,1) + DS*(QMS(1,2)+QMS(2,1))+DS**2*QMS(2,2)
        QMS(1,2) = QMS(1,2) + DS*QMS(2,2)
        QMS(2,1) = QMS(2,1) + DS*QMS(2,2)
C
C---add ms in beam pipe
C
        IF ( S .GT. 0. )
     +    QMS(2,2) = QMS(2,2) +
     +     .964*((1.+.038*DLOG(UKSVAC*DSQRT(1.+TANL**2))))*
     +       UKSVAC*SCACO*DSQRT(1.+TANL**2)
     +      *OMEGA*UKRVAC/DSIN(OMEGA*S)/(1.-OMEGA*D0)
        R1 = UKRVAC
        S1 = S
       ENDIF
C
C---now transport to inner radius (drift)
C
       IF ( R2 .LE. ABS(D0) ) THEN
        S = 0.
       ELSE
        S=SXY(R2)
       ENDIF
       DS = ( S1 - S ) * DSQRT( 1. + TANL**2 )
       QMS(1,1) = QMS(1,1) + DS*(QMS(1,2)+QMS(2,1))+DS**2*QMS(2,2)
       QMS(1,2) = QMS(1,2) + DS*QMS(2,2)
       QMS(2,1) = QMS(2,1) + DS*QMS(2,2)
      ELSE
       CALL ALTELL('UFVDMS no VDMS bank -> no ms in VDET',0,'RETURN')
      ENDIF
      RETURN
      END
