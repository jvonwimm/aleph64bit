      SUBROUTINE HGETDB
C------------------------------------------------------------
C
C!          Fill the HCGEGA common
C!
C!          Author : G.Catanesi 88/04/18
C!
C      Called by: HRDGAL    from this .HLB
C!   Calls    : HGETGA from Alephlib
C!
C -------------------------------------------------
      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      COMMON/HCGEGA/ HCSMTH,HCIRTH,HCLSLA,HCTUTH, NHCINL,NHCOUL,NHCTRE,
     +HCPHOF,NHCTU1(LHCNL), HCLARA(LHCNL),HCLAWI(LHCNL),IHCREG(LHCTR),
     +HCZMIN(LPHC),HCZMAX(LPHC),HCRMIN(LPHC), HCRMAX(LPHC),HCTIRF(LPHC),
     +NHCPLA(LPHC), HCWINO(LPHC),HCLTNO(LPHC)
      COMMON /HCCONG/ HCTUAC,HCSTDT,HCADCE,HADCMX,HCPFAC,
     &                HCTINS,RHBAMN,ZHECMN,ZHBAMX,HSTREA,HSTUST,
     +                NHCFSS,HCFSS1,HCFSS2,HCFLSS(100)
     &               ,HTLEMX,HCTEFF(3),HPINDU
C
      PARAMETER (NWORDS=8+3*LHCNL+LHCTR+8*LPHC)
      DIMENSION ARRAY(NWORDS)
C
C---------------------------------------------------------------
C
      CALL HGETGA(ARRAY)
C
      HCSMTH = ARRAY(1)
      HCIRTH = ARRAY(2)
      HCLSLA = ARRAY(3)
      HCTUTH = ARRAY(4)
      NHCINL = INT(ARRAY(5))
      NHCOUL = INT(ARRAY(6))
      NHCTRE = INT(ARRAY(7))
      HCPHOF = ARRAY(8)
C
      INDEX = 8
C
      DO 10 J=1,LHCNL
         NHCTU1(J) = INT(ARRAY(INDEX +J))
         HCLARA(J) = ARRAY(INDEX+LHCNL +J)
         HCLAWI(J) = ARRAY(INDEX+2*LHCNL +J)
   10 CONTINUE
C
      INDEX = INDEX +3*LHCNL
C
      DO 20 J=1,LHCTR
         IHCREG(J) = INT(ARRAY(INDEX +J))
   20 CONTINUE
C
      INDEX = INDEX + LHCTR
C
      DO 30 J=1,LPHC
         HCZMIN(J) = ARRAY(INDEX+J)
         HCZMAX(J) = ARRAY(INDEX+LPHC+J)
         HCRMIN(J) = ARRAY(INDEX+2*LPHC+J)
         HCRMAX(J) = ARRAY(INDEX+3*LPHC+J)
         HCWINO(J) = ARRAY(INDEX+4*LPHC+J)
         HCLTNO(J) = ARRAY(INDEX+5*LPHC+J)
         HCTIRF(J) = ARRAY(INDEX+6*LPHC+J)
         NHCPLA(J) = INT(ARRAY(INDEX+7*LPHC+J))
   30 CONTINUE
C
C - set few numers which are not on the data base
C
      HCPFAC = 0.1
      HCTINS = 1.
      RHBAMN = 300.
      ZHECMN = 315.
      ZHBAMX = 362.
      HSTUST = HCTUAC / HSTREA
C
C
      RETURN
      END
