      SUBROUTINE HCFITU(IPOR,IPL,YY,DY,ITUBF,NTUB,YPART,NTUSP,WSPAC)
C------------------------------------------------------------------
C
C! find the tubes intersected by the track element
C!       is usable also with DY=0. (shower)
C!
C!          Author   : G.Catanesi  86/12/05
C!          Modified : G.Catanesi  87/10/21,88/05/01
C!                     F.Ranjard   88/12/12
C!
C!          input  :
C!                 - YY/R           : coordinate normal to the wire dir.
C!                                   (Plane R.S.)
C!                 - DY/R           : track element projection
C!                 - IPOR/I         : portion number
C!                 - IPL/I          : plane number
C!          output :
C!                 - ITUBF/R        :  # of first tube hit
C!                 - NTUB/I         : # of tubes hit
C!                 - YPART(1:NTUB)  :  % of the element track in
C!                                      the tube  j
C!                 - NTUSP/I        : if=I the Ith tube hit (1:NTUB)is
C!                                    a spacer
C!                 - WSPAC/R        : width of the spacer found (if any)
C!
C!    calls     : HCTKSP  from this .HLB
C!
C -------------------------------------------------------
      SAVE
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      REAL YPART(*)
      INTEGER HCLATU
C --------------------------------------------------------------------
      NTUB = 0
      IDEAD = 0
      ITUBF = 0
      NTUSP = 0
      YPART(1) = 1.
C - set HCBM thickness of outer wall, HCSM tube width
      HCBM = HCDRBM(IPOR)
      HCSM = HCSAMP(IPOR)
C
C - return if track element in the outer wall
      IF (YY.LE.HCBM) RETURN
C
C verify if the boundaries are out of range
C
      IF ((YY-DY/2.).LT.0.) THEN
         DY = DY/2. + YY
         YY = DY/2.
      ENDIF
C
C Take in account spacers in barrel and iron frames in End-Cap
      IF (ICHCJO(3).EQ.0) THEN
         YYY = YY
         DYY = DY
         CALL HCTKSP(IPOR,IPL,YYY,DYY,NSP,NTUSP,IDEAD)
         YY = YYY
         DY = DYY
         IF(IDEAD.EQ.1)RETURN
      ENDIF
C - set lower and upper edge of the redefined track element
C   (YY and DY can be modified by HCTKSP)
      YMIN = MAX (YY-DY/2. - HCBM , 0.)
      YMAX = MAX (YY+DY/2. - HCBM , 0.)
C.
C - compute 1st tube hit, RETURN if out of range
      LASTU = HCLATU(IPOR,IPL)
      ITUBF = INT(YMIN/HCSM)+1
      IF (ITUBF .GT. LASTU) RETURN
C
C - compute last tube hit, take care of spacer if any, limit last tube
C   to the maximum # of tubes in the layer
      IF (NTUSP.EQ.0)THEN
         ITUBL = INT(YMAX/HCSM)+1
      ELSE
         WSPAC = HCDPSP(IPOR,IPL,NSP)
         ITUBL = INT((YMAX - WSPAC)/HCSM)+1
         DY = DY - WSPAC
      ENDIF
      ITUBL = MIN (ITUBL,LASTU)
C.
C - set total # of tubes hit
      NTUB = ITUBL-ITUBF+1
C
C - RETURN if only 1 tube hit or if point element (shower)
      IF (NTUB.LE.1 .OR. DY.LE.1.E-6) RETURN
C
C - determine which part of the track element goes through each tube
      YMOD = MOD(YMAX,HCSM)
      IF(YMOD.GT.DY.AND.NTUB.EQ.2) THEN
       YMOD=0.
      ENDIF
      YPERC = YMOD/DY
      YPART(NTUB)=YPERC
      YPART(1) = 1. - YPERC
      IF (NTUB.GT.2) THEN
         DO 10 LL=2,NTUB-1
            YPART(LL)=HCSM/DY
   10    CONTINUE
         YPART(1) = 1.-YPART(NTUB)-(NTUB-2)*HCSM/DY
      ENDIF
      DO I=1,NTUB
       IF(YPART(I).LT.0..OR.YPART(I).GT.1.) THEN
        DO J=1,NTUB
          YPART(J)=0.
        ENDDO
        YPART(1)=1.
       ENDIF
      ENDDO
C
      END
