      SUBROUTINE CAPSTO (IGTRA)
C ------------------------------------------------------------------
C       M.N Minard 880509
C - fill bank CAPA for geantino with track# IGTRA
C - called by CAPGET                                from this .HLB
C - call   ALTELL                                   from ALEPHLIB
C ========================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JCAPST=1,JCAPPT=2,JCAPTE=3,JCAPTX=4,JCAPTL=5,JCAPEM=6,
     +          JCAPEA=7,JCAPEB=8,JCAPA1=9,JCAPER=10,JCAPEF=11,
     +          JCAPED=12,JCAPEZ=13,JCAPHE=14,JCAPHA=15,JCAPHB=16,
     +          JCAPHD=17,JCAPHP=18,JCAPGA=19,JCAPSX=20,JCAPSA=21,
     +          JCAPNA=22,LCAPAA=22)
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
C -------------------------------------------------------------------
C
C - it is a geantino, IF the bank CAPA,NR=IGTRA does not exist THEN erro
      JCAPA = NLINK ('CAPA',IGTRA)
      IF (JCAPA.EQ.0) THEN
         CALL ALBOS ('CAPSTO: bank CAPA is missing ',11,'NEXT')
      ELSE
         KCAPA = JCAPA + LMHLEN
C        fill bank 'CAPA' using common /CAPANO/
         IW(KCAPA+JCAPST) = NATGER
         IW(KCAPA+JCAPPT) = METHCO
         RW(KCAPA+JCAPTE) = TOTNRJ
         RW(KCAPA+JCAPEM) = EMGNRJ
         RW(KCAPA+JCAPEA) = EMALFA
         RW(KCAPA+JCAPEB) = EMBETA
         RW(KCAPA+JCAPA1) = EMALM1
         RW(KCAPA+JCAPER) = EMAEXP
         RW(KCAPA+JCAPEF) = EMFACT
         RW(KCAPA+JCAPED) = EMUDP0
         RW(KCAPA+JCAPEZ) = EMUDP2
         RW(KCAPA+JCAPHE) = HADNRJ
         RW(KCAPA+JCAPHA) = HALPHA
         RW(KCAPA+JCAPHB) = HABETA
         RW(KCAPA+JCAPHD) = HADRAY
         RW(KCAPA+JCAPHP) = HAPUIS
         IW(KCAPA+JCAPNA) = INTCHA(EMNAME)
         RW(KCAPA+JCAPSX) = SMAXLR
         RW(KCAPA+JCAPSA) = SMAXLA
      ENDIF
      END
