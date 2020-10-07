      SUBROUTINE TPAVAL(JCL,IRET)
C-------------------------------------------------------------------
C! Fast sim : Simulate the avalanche of one cluster at a TPC anode
C! wire.
C
C  Called from: T2TEER
C  Calls:       none
C
C  Inputs:      Passed:   --JCL,        cluster number on the wire
C               /FASTER/: --AVTIM(JCL), avalanche time for the JCLth
C                                       cluster
C                         --NELE(JCL),  number of electrons forming
C                                       the avalanche in JCLth cluster
C                         --SIGT(JCL),  width of the charge time distri-
C                                       tion in the JCLth cluster
C               /TPCOND/  --TPANBN,     the length of the analog signal
C                                       time bin
C                         --AMPLIT,     average size of avalanche from
C                                       one electron
C               /TPCONS/  --NTMXAN,     the largest allowed time bin
C  Outputs:     /FASTER/  --IBIN(IB),   IB=1,2  affected time bins
C                         --NAVBIN(IB), IB=1,2  charge in bins
C               /CHANNL/  --NAVELE,     mean charge in the 2 bins
C  P. Janot  05/05/88
C
C  Revised:
C
C  D. Casper  03/10/92      Put in correct fluctuations in avalanche
C                           and model the distribution of charge
C                           in time intelligently by fitting the first
C                           three moments of the arrival time.
C
C  D. Casper  22/02/94      Fix bug in treatment of 3rd moment.
C-----------------------------------------------------------------------
C
C  CHANNL carries the analog signals on all channels due to one
C  wire avalanche
C
C  Wire avalanche information
C  NAVELE     -- The charge from an avalanche
C  IBIN1  -- The first bin to get charge
C
      COMMON / AVALAN / NAVELE,IBIN1
C
C  Long pad coupling information
C  MPPUL   -- The array containing the coupled avalanches
C  NAMP   -- Pad 'name' generating this pulse
C            = (pad row-1)*150 + pad number
C
      PARAMETER (MXPHIT=15)
      COMMON / PADPUL / MPPUL(MXPHIT),NAMP(MXPHIT)
C
C  Trigger pad coupling information
C  MTPUL   -- The array containing the coupled avalanches
C  NAMT   -- Pad 'name' generating this pulse
C            = (tpad row# - 1)*(max # tpads/row) + tpad number
C
      PARAMETER (MXTHIT=2)
      COMMON / TRGPUL / MTPUL(MXTHIT),NAMT(MXTHIT)
C
C  FASTER : variables used in fast simulation
C
      COMMON / LANDAU / NITLAN,NITIND,INTWRD
      COMMON / EXBEFF / XPROP,TTTT,XXXX(1001),XSHFT(50)
      COMMON / EVT / IEVNT,ISECT
      COMMON / T3TR / JSEGT,NSEGT,ITYPE,WIRRAD(4),WIRPHI(4)
     &               ,AVTIM(4),NELE(4),NCL,SIGT(4)
      COMMON / TPSG / CC(3),XX(3),TOTDIS,XLEN,ISTY,IE
      COMMON / XBIN / IBIN(4),NAVBIN(4),NB
      DIMENSION XEX(4,4)
      PARAMETER (SQ2=1.4142136,SQ3=1.7320508)
C
C  TPCOND  conditions under which this simulation
C  will be performed
C
      COMMON /DEBUGS/ NTPCDD,NCALDD,NTPCDT,NCALDT,NTPCDA,NCALDA,
     &                NTPCDC,NCALDC,NTPCDS,NCALDS,NTPCDE,NCALDE,
     &                NTPCDI,NCALDI,NTPCSA,NCALSA,NTPCDR,NCALDR,
     &                LTDEBU
      LOGICAL LTDEBU
      COMMON /SIMLEV/ ILEVEL
      CHARACTER*4 ILEVEL
      COMMON /GENRUN/ NUMRUN,MXEVNT,NFEVNT,INSEED(3),LEVPRO
      COMMON /RFILES/ TRKFIL,DIGFIL,HISFIL
      CHARACTER*64 TRKFIL,DIGFIL,HISFIL
      COMMON /TLFLAG/ LTWDIG,LTPDIG,LTTDIG,LWREDC,FTPC90,LPRGEO,
     &                LHISST,LTPCSA,LRDN32,REPIO,WEPIO,LDROP,LWRITE
      COMMON /TRANSP/ MXTRAN,CFIELD,BCFGEV,BCFMEV,
     &                        DRFVEL,SIGMA,SIGTR,ITRCON
      COMMON /TPCLOK/ TPANBN,TPDGBN,NLSHAP,NSHPOF
      COMMON /AVLNCH/ NPOLYA,AMPLIT,GRANNO(1000)
      COMMON /COUPCN/ CUTOFF,NCPAD,EFFCP,SIGW,SIGH,HAXCUT
      COMMON /TGCPCN/ TREFCP,SIGR,SIGARC,RAXCUT,TCSCUT
      COMMON /DEFAUL/ PEDDEF,SPEDEF,SGADEF,SDIDEF,WPSCAL,NWSMAX,THRZTW,
     &                LTHRSH,NPRESP,NPOSTS,MINLEN,
     &                LTHRS2,NPRES2,NPOST2,MINLE2
      COMMON /SHAOPT/ WIRNRM,PADNRM,TRGNRM
C
      LOGICAL LTWDIG,LTPDIG,LTTDIG,LPRGEO,
     &        LWREDC,LTPCSA,LHISST,FTPC90,LRND32,
     &        REPIO,WEPIO,LDROP,LWRITE
C
      LOGICAL LTDIGT(3)
      EQUIVALENCE (LTWDIG,LTDIGT(1))
C
      REAL FACNRM(3)
      EQUIVALENCE (WIRNRM,FACNRM(1))
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
      COMMON / HISCOM / IHDEDX,IHTRAN,IHAVAL,IHCOUP,IHTRCP,IHBOS,IHTOT
      PARAMETER (MAXTAB = 50000)
      COMMON /POLTAB/ZZUSE(MAXTAB),NTAB
      LOGICAL LDBA1,LDBA3
      DATA ICALL/0/
C
      ICALL = ICALL + 1
C
C  Set the return code. IRET = 1 for invalid time bins.
C
      IRET = 0
C
C  Debug levels
C
      LDBA1 = (NTPCDA .GE. 1 .AND. ICALL .LE. NCALDA)
      LDBA3 = (NTPCDA .GE. 3 .AND. ICALL .LE. NCALDA)
C
C  Loop over the electrons in one cluster, getting the total charge
C  produced after amplification and various moments of the time
C  structure.  If only one or two incident electrons, then simplify this
C  procedure, using the actual charges and times of the electron(s).
C
      NB = 0
      NAVBIN(1) = 0
      NAVBIN(2) = 0
      IBIN(1) = 0
      IBIN(2) = 0
      IBIN(3) = 0
      SUMQ = 0.
      SUMT = 0.
      SUMTSQ = 0.
      SUMTCU = 0.
      IF(NELE(JCL).GT.2)THEN
          DO 10 IELEC = 1, NELE(JCL)
              ITABLE = NTAB * RNDM(IELEC) + 1
              QAVAL = ZZUSE(ITABLE)
              IPOINT = 1000*RNDM(IELEC) + 1
              FL1 = GRANNO(IPOINT)
              FL1 = FL1 + (RNDM(FL1) - 0.5) * SIGT(JCL + 2)
              SUMQ = SUMQ + QAVAL
              SUMT = SUMT + QAVAL * FL1
              FLSQ = FL1 * FL1
              SUMTSQ = SUMTSQ + QAVAL * FLSQ
              FLCU = FLSQ * FL1
              SUMTCU = SUMTCU + QAVAL * FLCU
10        CONTINUE
          IF(SUMQ.LE.0.)THEN
              IRET = 1
              RETURN
          ENDIF
C
C  Calculate the first three moments
C
          TAVE = SUMT/SUMQ
          SIG2 = (SUMQ*SUMTSQ - SUMT**2)/SUMQ**2
          IF(SIG2.LE.0.)THEN
              NB = 1
              NAVBIN(1) = SUMQ
              IBIN(1) = INT((AVTIM(JCL)+TAVE*SIGT(JCL))/TPANBN) + 1
              IBIN1 = IBIN(1)
              IF(IBIN1.LE.0 .OR. IBIN1.GT.NTMXAN) IRET = 1
              RETURN
          ENDIF
          SKEW = (SUMTCU*SUMQ**2 - 3*SUMT*SUMTSQ*SUMQ
     1            + 2*SUMT**3)/SUMQ**3
          G3SQ = SKEW**2/SIG2**3
C
C  Now reduce all the incoming clusters into two clusters which have
C  the same total charge, mean, variance, and skewness (in time) as
C  the original charge distribution.  Also note that by generating
C  the charge multiplication from a Polya distribution above we ensure
C  the correct charge resolution.
C
          DQ = SUMQ*SQRT(G3SQ/(G3SQ + 4.))
          IF(SKEW .LT. 0.) DQ = -DQ
          DT = - SQRT(SIG2 * (G3SQ + 4.))
          T1 = TAVE + (SUMQ - DQ)*DT/(2*SUMQ)
          T2 = TAVE - (SUMQ + DQ)*DT/(2*SUMQ)
          Q1 = (SUMQ + DQ)/2.
          Q2 = SUMQ - Q1
          NB = 2
          IBIN(1) = INT((AVTIM(JCL)+T1*SIGT(JCL))/TPANBN) + 1
          IBIN(2) = INT((AVTIM(JCL)+T2*SIGT(JCL))/TPANBN) + 1
          IBIN1 = INT((AVTIM(JCL)+TAVE*SIGT(JCL))/TPANBN) + 1
          NAVBIN(1) = Q1
          NAVBIN(2) = Q2
          NTOELE = NAVBIN(1) + NAVBIN(2)
C
C   For long pulses, the above procedure does not work well - due to the
C   short length of the electronics response function, a long pulse will
C   split into two separate pulses, one early and one late.  To avoid th
C   problem, we can add a third pulse in the center...
C
          IF(ABS(IBIN(1)-IBIN(2)).GE.8)THEN
            Q0 = (TAVE - T1)*(Q2-Q1)/(T2-T1) + Q1
            QQ0 = Q0 * SUMQ / (SUMQ + Q0)
            QCOR = SUMQ / (SUMQ - QQ0)
            SIG2 = SIG2 * QCOR
            G3SQ = G3SQ / QCOR
            SUMQ = SUMQ - QQ0
            DQ = SUMQ*SQRT(G3SQ/(G3SQ + 4.))
            IF(SKEW.LT.0.) DQ = -DQ
            DT = - SQRT(SIG2 * (G3SQ + 4.))
            T1 = TAVE + (SUMQ - DQ)*DT/(2*SUMQ)
            T2 = TAVE - (SUMQ + DQ)*DT/(2*SUMQ)
            Q1 = (SUMQ + DQ)/2.
            Q2 = SUMQ - Q1
C
C   Multiply by the average multiplication factor to get charge
C   on the wire
C
            NB = 0
            NTOELE = 0
            IF(INT(Q1).GT.0)THEN
             NB = NB + 1
             NAVBIN(NB) = Q1
             NTOELE = NTOELE + NAVBIN(NB)
             IBIN(NB) = INT((AVTIM(JCL)+T1*SIGT(JCL))/TPANBN) + 1
            ENDIF
            IF(INT(QQ0).GT.0)THEN
             NB = NB + 1
             NAVBIN(NB) = QQ0
             NTOELE = NTOELE + NAVBIN(NB)
             IBIN(NB) = INT((AVTIM(JCL)+TAVE*SIGT(JCL))/TPANBN) + 1
            ENDIF
            IF(INT(Q2).GT.0)THEN
             NB = NB + 1
             NAVBIN(NB) = Q2
             NTOELE = NTOELE + NAVBIN(NB)
             IBIN(NB) = INT((AVTIM(JCL)+T2*SIGT(JCL))/TPANBN) + 1
            ENDIF
          ENDIF
      ELSE IF(NELE(JCL).EQ.2)THEN
          ITABLE = NTAB * RNDM(IELEC) + 1
          Q1 = ZZUSE(ITABLE)
          ITABLE = NTAB * RNDM(IELEC+1) + 1
          Q2 = ZZUSE(ITABLE)
          CALL RANNOR(T1,T2)
          T1 = T1 + (RNDM(T1) - 0.5) * SIGT(JCL + 2)
          T2 = T2 + (RNDM(T2) - 0.5) * SIGT(JCL + 2)
C   Multiply by the average multiplication factor to get charge
C   on the wire
          NAVBIN(1) = Q1
          NAVBIN(2) = Q2
          NTOELE = NAVBIN(1) + NAVBIN(2)
C
C  Determine the time bins of the avalanches and make sure they
C  are legal values.
C
          IBIN(1) = INT((AVTIM(JCL)+T1*SIGT(JCL))/TPANBN) + 1
          IBIN(2) = INT((AVTIM(JCL)+T2*SIGT(JCL))/TPANBN) + 1
          TAVE = (T1*Q1+T2*Q2)/(Q1+Q2)
          IBIN1 = INT((AVTIM(JCL)+TAVE*SIGT(JCL))/TPANBN) + 1
          NB = 2
      ELSE IF(NELE(JCL).EQ.1)THEN
          ITABLE = NTAB * RNDM(IELEC) + 1
          Q1 = ZZUSE(ITABLE)
          CALL RANNOR(T1,T2)
          T1 = T1 + (RNDM(T1) - 0.5) * SIGT(JCL + 2)
C   Multiply by the average multiplication factor to get charge
C   on the wire
          NAVBIN(1) = Q1
          NTOELE = NAVBIN(1)
C
C  Determine the time bins of the avalanches and make sure they
C  are legal values.
C
          IBIN(1) = INT((AVTIM(JCL)+T1*SIGT(JCL))/TPANBN) + 1
          IBIN1 = IBIN(1)
          NB = 1
      ENDIF
C
      IF(LDBA3) WRITE(6,100) NELE(JCL),NTOELE
      IF(LDBA1) CALL HF1(IHAVAL+1,ALOG10(FLOAT(NTOELE)),1.)
      IF(IBIN1.LE.0 .OR. IBIN1 .GE. NTMXAN) THEN
        IRET = 1
        IF(LDBA1) WRITE(6,202) IBIN1
        RETURN
      ENDIF
      IF(LDBA3) WRITE(6,103) (IBIN(IB),NAVBIN(IB),IB=1,NB)
      RETURN
C------------------------------------------------------------------
100   FORMAT(//,10X,'Start avalanche process '/
     &       1X,'Number of electrons in avalanche : ',I4/
     &       1X,'Total electrons deposited        : ',I10)
103   FORMAT(1X,'Time bin : ',I4,', Released charge : ',I10)
202   FORMAT(/,' +++ TPAVAL +++ ILLEGAL BIN VALUE : ',I12)
      END
