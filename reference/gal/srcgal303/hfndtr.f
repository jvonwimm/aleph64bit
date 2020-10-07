      SUBROUTINE HFNDTR(COOR,IPOR,ITOW,ITRI)
C---------------------------------------------------------------
C
C!         Evaluate theta phi trigger adress
C!
C!                         Author: G.Catanesi 25/08/88
C!
C! - Input:
C!         COOR(1-3)/R  : X,Y,Z in the ALEPH system
C!         IPOR /I      :portion #
C!         ITOW(1-3)/I  : nphi,ntheta,nstack
C! - Output :
C!         ITRI(1-2)/I  : nphi trigger, ntheta trigger
C!
C!  - Called by :HCCRTO
C!  - Calls     :HCCYL
C ---------------------------------------------------
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
      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      PARAMETER (LPHCTR= 12,LPHCTF=24,LHCRT=62)
      COMMON /HCTRIG/ NHCEPR,NHCBPR,NHCETR,NHCBTR , IHCTRG(LHCRT),NHCBTS
     +(LHCRT),NHCETS(LHCRT) , IYHCFI(LPHCTF),IXHCFI(LPHCTR),IXHCSE
     +(LPHCTR) , MHCETR,MHCBTR

      DIMENSION COOR(*), ITOW(*), ITRI(*)
C-------------------------------------------------------
C
      CALL HCCYL(COOR,R,THETA,PHI)
C
C
      IF (IPOR.EQ.LPBAR) THEN
         PHIR    = TWOPI/NHCBPR
         ITRI(2) = NHCBTS(ITOW(2))
      ELSE
         PHIR    = TWOPI/NHCEPR
         ITRI(2) = NHCETS(ITOW(2))
      ENDIF
C
      ITRI(1) = INT (PHI/PHIR) + 1
C
      RETURN
      END
