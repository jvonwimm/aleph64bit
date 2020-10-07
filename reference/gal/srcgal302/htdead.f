              SUBROUTINE HTDEAD(XTBIN,DXTBI,ITUB,IDEAD)
C*****************************************************************
CKEY HCAL DEAD TUBE / INTERNAL
C - G.Catanesi - 890813
C                900211 - set XTBOU for encap part
C!  Checks if a TRACK ELEMENT  is in the active area of a HCAL tube
C!
C! if a fraction of the track element is outside the active area
C! this fraction is killed and x and dx recomputed
C!
C!                    Author : G.Catanesi 15/5/89
C!                    Mod by: Andrea Venturi & L.Silvestris 24/6/92
C!                    to take in account the shift of tubes in barrel...
C!
C!       INPUT:
C!                 XTBIN /R = x coordinate in the Plane R.S.
C!                         of the track element baricenter
C!                 DXTBI/R = projection in the wire direction
C!                 ITUB/I  = tube #
C!          ==>    becareful the input argument XTBIN and DXTBI
C!                 can be modified by the routine
C!       OUTPUT:
C!                 XTBIN /R = x coordinate in the Plane R.S.
C!                          of the track element baricenter
C!                 DXTBI/R = projection in the wire direction
C!                 IDEAD/I = Flag if = 0 Track in the active area
C!                                   = 1 Track in the dead area
C!
C*******************************************************************
      SAVE
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      COMMON/HCGEGA/ HCSMTH,HCIRTH,HCLSLA,HCTUTH, NHCINL,NHCOUL,NHCTRE,
     +HCPHOF,NHCTU1(LHCNL), HCLARA(LHCNL),HCLAWI(LHCNL),IHCREG(LHCTR),
     +HCZMIN(LPHC),HCZMAX(LPHC),HCRMIN(LPHC), HCRMAX(LPHC),HCTIRF(LPHC),
     +NHCPLA(LPHC), HCWINO(LPHC),HCLTNO(LPHC)
C
C
      IDEAD = 0
C
C   Evaluate eightfold and double_eightfold number
C
      CALL HNEIGH(ITUB,IHCIPL,IHCPOR,IHEIF,IDHEI)
      IF(IHEIF.EQ.0)THEN
         IDEAD = 1
         GOTO 99
      ENDIF
C
C   Evaluate the tube active length
       IS = SIGN(1.,XTBIN)
       XLEN = HTULEN(ITUB,IHCIPL,IHCMOD,IHCPOR,IS)
       IF (XLEN.EQ.0) THEN
          IDEAD = 1
          GOTO 99
       ELSE
C
C      the tube is active then..
          DXX=DXTBI/2.
          IF(IHCPOR.EQ.LPBAR)THEN
C
C          Barrel case
             XBAR = XLEN/2.
             XTBOU = ABS(XTBIN)
C           track element is totally inside active area
             IF ((XTBOU+DXX) .LT. XBAR) GOTO 10
C           track element partially inside active area
             IF((XTBOU-DXX) .LT. XBAR)THEN
                XBOR  = XBAR - XTBOU
                DXX = (DXX + XBOR)/2.
                XTBOU  = XBAR - DXX
C           track element outside active area
             ELSEIF ((XTBOU-DXX) .GE. XBAR) THEN
                IDEAD = 1
                GOTO 99
             ENDIF
C
C         the track element is totally or partially in the active area
 10          XTBIN = SIGN(XTBOU,XTBIN)
             DXTBI = DXX*2
C ------------
          ELSE
C          EndCaps Case
             XMIN = HTSTEC(ITUB,IHCIPL,IDHEI)
             XMAX = XMIN + XLEN
             XTBOU = XTBIN
C
C           track element totally inside active area
             IF(((XTBOU-DXX).GE.XMIN) .AND.
     &                     ((XTBOU-DXX).LE.XMIN)) GOTO 20
C           track element partially inside active area
C         .... in the internal part
             IF(((XTBOU+DXX).GE.XMIN) .AND.
     &                     ((XTBOU-DXX).LE.XMIN))THEN
                XBOR  = XMIN - XTBOU
                DXX= (DXX+ XBOR)/2.
                XTBOU  = XMIN - DXX
C         .... in the external part
             ELSEIF(((XTBOU+DXX).GE.XMAX) .AND.
     &                    ((XTBOU-DXX).LE.XMAX))THEN
                XBOR  = XMAX - XTBOU
                DXX= (DXX + XBOR)/2.
                XTBOU  = XMAX - DXX
C           track element outside active area
             ELSEIF (((XTBOU+DXX).LT.XMIN) .OR.
     &                         ((XTBOU-DXX).GT.XMAX)) THEN
                IDEAD = 1
                GOTO 99
             ENDIF
C
C         the track element is totally or partially in the active area
 20          IF(XTBOU.LT.0.)XTBOU = XMIN
             XTBIN = XTBOU
             DXTBI = MAX (0.,DXX*2)
          ENDIF
C ------------
       ENDIF
C
 99    CONTINUE
       END
