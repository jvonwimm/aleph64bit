      REAL FUNCTION HCSTEN(NST)
C.--------------------------------------------------------------------
C.                                       032486  G.Catanesi
C!       return the energy of the streamers
C!       Calls to : HISRAN from Cernlib
C ------------------------------------------------------
      COMMON /HCCONG/ HCTUAC,HCSTDT,HCADCE,HADCMX,HCPFAC,
     &                HCTINS,RHBAMN,ZHECMN,ZHBAMX,HSTREA,HSTUST,
     +                NHCFSS,HCFSS1,HCFSS2,HCFLSS(100)
     &               ,HTLEMX,HCTEFF(3),HPINDU
C
C -------------------------------------------------------------------
      HCSTEN = 0.
      DO 20 J=1,NST
   10    CALL HISRAN(HCFLSS,NHCFSS,HCFSS1,HCFSS2,XRAN)
         IF(XRAN.GT.HADCMX) GOTO 10
         HCSTEN=HCSTEN+XRAN*(HCADCE)
C
   20 CONTINUE
      RETURN
      END
