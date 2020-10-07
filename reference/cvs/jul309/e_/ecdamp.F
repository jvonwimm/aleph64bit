      SUBROUTINE ECDAMP (ITOW,NAMP)
C----------------------------------------------------------------------
C!  - Compute amplificator number
C!
C!                       AUTHOR : M.N MINARD 25.11.90
C!
C!    Input  ITOW : Tower number
C!    Output NAMP : amplificator number
C?
C!======================================================================
#ifndef DOC
      DIMENSION DROW (6) , DCOL (5)
      DIMENSION IPAM1(16),IPAM2(16), IPAM3(16),IPAM4(16)
      DATA IPAM1 / 101 , 901 , 1301 , 1701 , 2101, 2501
     & , 2709 , 3005, 3301 , 3509 , 3805 , 4101 , 4301 ,
     &   4501 , 4701 , 4901 /
      DATA IPAM2 / 804 ,1208 , 1608 , 2008 , 2408, 2708
     & , 3004 , 3212, 3508 , 3804 , 4012 , 4216 , 4416 ,
     &   4616 , 4816 , 5016 /
      DATA DROW / 0 , 8 , 24 , 40 , 50 , 178 /
      DATA DCOL / 8 ,16 , 24 , 32 , 32 /
C
C     Decode Theta Phi index
      NAMP = 0
C
      ITETA  = IBITS (ITOW , 16 , 8 ) - 1
      IPHI   = IBITS ( ITOW , 2 , 9 ) - 1
C
C     Calculate row
C
      IMOD0 = 0
      IF ( ITETA.GE.DROW(6)) THEN
         IMOD0 = 2
         ITETA = 228 - ITETA - 1
         GO TO 5
      ENDIF
      IF ( ITETA.GE.DROW(5)) THEN
         IMOD0 = 1
         ITETA = ITETA - DROW(5)
         GO TO 5
      ENDIF
 5    CONTINUE
C
C-    Solve Barrel case
C
      IF ( IMOD0.EQ.1) THEN
         IMOD = IPHI/32
         IAMP =  ITETA / 2
         IAMP = IAMP*2 +    (IPHI-32*IMOD)/16 + 1
         NAMP = IAMP + (IMOD0*12+IMOD+1)*10000
      ELSE
C
C-    Solve petal case
C
        ISID = 1
        IAMP = 0
        DO 10 IT = 1, 4
        IF ( ITETA.GE.DROW(IT).AND.ITETA.LT.DROW(IT+1))THEN
          NCOL = DCOL(IT)
          IMOD = (IPHI+NCOL/2)/NCOL
          IPHIR = IPHI+NCOL/2-(IMOD-1)*NCOL
          IF ( IMOD.EQ.13) THEN
               IMOD = 1
               ISID = 1
          ELSE
               IF (IPHIR/NCOL.GE.1) ISID=2
          ENDIF

          IAMP = MOD(IPHI,NCOL/2) + 1
          IF ( ISID.EQ.1) THEN
          IAMP = ITETA*100 + IAMP
          ELSE
          IAMP = NCOL/2 - IAMP +2
          IAMP = ITETA*100 + IAMP
          ENDIF
          ENDIF
 10     CONTINUE
        IAZ = 0
        DO 20 IA = 1, 16
        IF ( IAMP.GE.IPAM1(IA).AND.IAMP.LE.IPAM2(IA)) THEN
          IAZ = IA
        ENDIF
 20     CONTINUE
      NAMP =(IMOD0*12 + IMOD + 1) * 10000 + IAZ  + (ISID-1)*16
      ENDIF
      RETURN
      END
#endif
