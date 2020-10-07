*DK asredc
      SUBROUTINE ASREDC
C --------------------------------------------------------------------
C. - F.RANJARD - 850200                  last change - 860108
C! Read BOS data cards
C. Check consistency of flags set.
C.
C. - modified by : F.Ranjard - 911002
C                  add SICAL in print list
C                  add setting of SICAL run conditions
C
C. - called from    ASIRUN                                from this .HLB
C. - calls          BREADC, MREADC, NDROP, CHAINT         from BOS lib
C.                  NPRNT                                 from BOS lib
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "iocom.h"
#include "alfgeo.h"
#include "bcs.h"
      CHARACTER*4 TPRIN(LPRI)
      CHARACTER*4 XINP, ZKAR, TKEYS(LPRO), CHAINT
      DATA TPRIN /'VDET','ITC ','TPC ','ECAL','LCAL','SATR','HCAL'
     &           ,'MUON','SICA','UNDF'
     &           ,'TRIG','RDST','UNDF','OUTP','INPU','KINE','TREL'
     &           ,'DRAW','GEOM','PART'/
      DATA TKEYS/ 'KINE','TRAC','HITS','DIGI','TRIG','RDST'/
C ----------------------------------------------------------------------
C              SET LOCAL FLAGS
      KARD = 0
      NRND  = 0
C
C             READ A DATA CARD AND DECODE THE KEYWORD
    1 CONTINUE
      IF (KARD.NE.0) KARD = NDROP(XINP,NR)
      KARD = MREADC(IW)
      IF (KARD.EQ.0) GOTO 70
      XINP = CHAINT(IW(KARD-3))
      NR   = IW(KARD-2)
C
C             DECODE THE DATA CARD
      IF(XINP.EQ.'DEBU') GOTO 11
      IF(XINP.EQ.'DISP') GOTO 12
      IF(XINP.EQ.'TCUT') GOTO 13
      IF(XINP.EQ.'STAT') GOTO 14
      IF(XINP.EQ.'PRIN') GOTO 15
      IF(XINP.EQ.'GEOM') GOTO 17
      IF(XINP.EQ.'HIST') GOTO 20
      IF(XINP.EQ.'KINE') GOTO 21
      IF(XINP.EQ.'RUNC') GOTO 23
      IF(XINP.EQ.'FXXX') GOTO 30
      IF(XINP.EQ.'PROC') GOTO 31
      IF(XINP.EQ.'TPCS') GOTO 32
      IF(XINP.EQ.'DATE') GOTO 33
      IF(XINP.EQ.'SETS') GOTO 35
      IF(XINP.EQ.'RNDM') GOTO 36
      IF(XINP.EQ.'RUN ') GOTO 37
      IF(XINP.EQ.'FIDU') GOTO 60
      IF(XINP.EQ.'END ') GOTO 70
C
C     Private data cards - do not drop it
      KARD = 0
      GOTO 1
C
C            EVENTS TO BE DEBUGGED : DEBUG  unit# / n1   n2
C                                    if unit# = 0 or absent , debug
C                                    is written on LOUTIO set to 6 in
C                                    ASINIT
C                                     n1  1st event to be debugged
C                                     n2  last event to be debugged
C                                     n3  frequency to print last rando m #
   11 IDB1JO=IW(KARD+1)
      IDB2JO=IW(KARD+2)
      IDB3JO=IW(KARD+3)
      IF (NR.GT.0) LOUTIO=NR
C     set the BOS output unit to LOUTIO
      IW(6) = LOUTIO
      GOTO 1
C
C            EVENTS TO BE DISPLAYED : DISPLAY  unit# / n1  n2
C                                     if unit# = 0 NO graphics
C                                     if unit# > 0 graphic is made on
C                                     this unit#
C                                      n1  1st event to be displayed
C                                      n2  last event to be displayed
C                  to suppress calls to graphic routine: DISPLAY 0/
   12 CONTINUE
      LGRAIO = NR
      IF (LGRAIO .GT. 0) THEN
        IDS1JO = IW(KARD+1)
        IDS2JO = IW(KARD+2)
      ENDIF
      GOTO 1
C
C               MOMEMTUM CUTS USED DURING TRACKING : TCUT p1  p2 ...
C                                                    with n.le.LTCUT
C                                                    and p = track. cut
   13 CONTINUE
      DO 131 N=1,IW(KARD)
        IF(N.LE.LTCUT) TCUTJO(N)=RW(KARD+N)
  131 CONTINUE
      GOTO 1
C
C              BIN SIZES OF PSEUDO-HISTO : STAT   n1   n2  n3
C                                          n1  # of track/event
C                                          n2  # of 'primaries'
C                                          n2  stack size
   14 CONTINUE
      DO 141 I=1,IW(KARD)
        IF(I.LE.LST2 .AND. IW(KARD+I).GT.0) MBINJO(I)=IW(KARD+I)
  141 CONTINUE
      GOTO 1
C
C             SELECTED PRINT OUT : PRINT  n1  n2  n3   .....
C                                  if n .le. LPRI then IPRIJO(N)=1
   15 DO 151 I=1,IW(KARD)
        J=IW(KARD+I)
        IF (J.GT.0 .AND. J.LE.LPRI) THEN
          IPRIJO(J) = 1
        ELSE
          ZKAR = CHAINT (J)
          DO 152 K=1,LPRI
            IF (ZKAR .NE. TPRIN(K)) GOTO 152
            IPRIJO(K) = 1
            GOTO 151
  152     CONTINUE
        ENDIF
  151 CONTINUE
      GOTO 1
C
C              GEOMETRY LEVEL : GEOM  'comp1'  lv1   'comp2'  lv2  ...
C                               'compi'  component name starting by
C                                        the 2-letter code : VD, IT,
C                                        TP, EC, LC, SA, HC, MU, SI,
C                                        BP, QU, PM, CO
C                                        do NOT remove SATR/SAMBA
C                                lvi     geometry level required for
C                                        this component: a number in
C                                        the range [0,3]
C                                        if a component is not mentionned
C                                        it receives a level by default:
C                                         lv = 1 in ASINIT
   17 CONTINUE
      DO 171 I=1,IW(KARD),2
        ZKAR = CHAINT(IW(KARD+I))
        DO 172 J=1,LGEO
          IF(ZKAR(1:2).EQ.TGEOJO(J)) GOTO 173
  172   CONTINUE
        GOTO 179
  173   ISET = J
        LVELJO(ISET)=IW(KARD+I+1)
        IF (LVELJO(ISET).LT.0.OR.LVELJO(ISET).GT.3) GOTO 179
  171 CONTINUE
      GOTO 1
  179 CONTINUE
      KARD = NPRNT (XINP,NR)
      CALL ALTELL ('ASREDC: wrong comp. or geo.level ',0,'STOP')
C
C             HISTOGRAMS : HISTO  n1  n2   n3   ...
C                          if n .le. LHIS  then  FHISJO(n)=.T.
   20 DO 201 I=1,IW(KARD)
        J = IW(KARD+I)
        IF (J.GT.0 .AND. J.LE.LHIS) THEN
          FHISJO(J) = .TRUE.
          MHISJO = 1
        ELSE
          ZKAR = CHAINT(J)
          DO 202 K=1,LHIS
            IF (ZKAR .NE. TPRIN(K)) GOTO 202
            FHISJO(K) = .TRUE.
            MHISJO = 1
            GOTO 201
  202     CONTINUE
        ENDIF
  201 CONTINUE
      GOTO 1
C
C              KINEMATIC : KINE  'name'   svx   svy   svz   p4   p5  ...
C                         'name'   kinematic type  'USER'  'PART'
C                          when name=PART the following parameters are required
C                          svx     sigma  x-vertex
C                          svy            y-vertex
C                          svz            z-vertex
C                          p4      particule type
C                          p5,p6   momentum range
C                          p7,p8   cosine(theta) range
   21 CONTINUE
      TKINJO=CHAINT(IW(KARD+1))
      CONTINUE
      DO 211 I=2,IW(KARD)
        IF(I-1.LE.LKINP) BKINJO(I-1)=RW(KARD+I)
  211 CONTINUE
      NKINJO = IW(KARD)-1
      GOTO 1
C
C             RUN CONDITIONS : RUNC   'det1'  p1   p2 ....
C                              'deti'    detector name starting with the
C                                        2-letter code : VD, IT, TP, EC,
C                                        LC, SA, HC, MU
C                               if n.le.LCxx  then ICxxJO(n)=p
   23 CONTINUE
C   decode detector name
      ZKAR = CHAINT(IW(KARD+1))
C   set detector run condition flags
      DO 231 I=2,IW(KARD)
        IF (ZKAR(1:2).EQ.'VD') THEN
          IF(I-1.LE.LCVD) ICVDJO(I-1) = IW(KARD+I)
        ELSEIF (ZKAR(1:2).EQ.'IT') THEN
          IF (I-1.LE.LCIT) ICITJO(I-1) = IW(KARD+I)
        ELSEIF (ZKAR(1:2).EQ.'TP') THEN
          IF (I-1.LE.LCTP) ICTPJO(I-1) = IW(KARD+I)
        ELSEIF (ZKAR(1:2).EQ.'EC') THEN
          IF (I-1.LE.LCEC) ICECJO(I-1) = IW(KARD+I)
        ELSEIF (ZKAR(1:2).EQ.'LC') THEN
          IF (I-1.LE.LCLC) ICLCJO(I-1) = IW(KARD+I)
        ELSEIF (ZKAR(1:2).EQ.'LT') THEN
          IF (I-1.LE.LCSA) ICSAJO(I-1) = IW(KARD+I)
        ELSEIF (ZKAR(1:2).EQ.'HC') THEN
          IF (I-1.LE.LCHC) ICHCJO(I-1) = IW(KARD+I)
        ELSEIF (ZKAR(1:2).EQ.'MU') THEN
          IF (I-1.LE.LCMU) ICMUJO(I-1) = IW(KARD+I)
        ELSEIF (ZKAR(1:2).EQ.'SI') THEN
          IF (I-1.LE.LCSI) ICSIJO(I-1) = IW(KARD+I)
        ENDIF
  231 CONTINUE
      GOTO 1
C
C             FXXX output : FXXX  'flag1'   'flag2'  cut
C                           flag1 : DRTK if tracks must be dropped (def)
C                                   KPTK if tracks must be kept
C                           flag2 : DRSH if showers must be dropped (def
C                                   KPSH if showers must be kept
C                           flag3 : CUTF if momemtum cut is following
C                                   must be followed by a real number:
C                           cut   : momemtum cut (=0.1 by def.)
C
   30 CONTINUE
      FXXXJO = .TRUE.
      IF (IW(KARD) .GT. 0) CALL FYREDC (FXTKJO,FXSHJO,CUTFJO)
      GOTO 1
C
C             EXECUTE PROCESSORS : PROCESS  'name1'   'name2'  .....
C                                  'name' must belong to the TKEYS list
   31 MPROJO=IW(KARD)
      DO 311 I=1,IW(KARD)
        TPROJO(I) = CHAINT(IW(KARD+I))
        DO 312 J=1,LPRO
          IF(TPROJO(I).EQ.TKEYS(J)) IPROJO(J)=1
  312   CONTINUE
  311 CONTINUE
      GOTO 1
C
C                  TPCSIM run
   32 CONTINUE
      ICTPJO(7) = 1
      GOTO 1
C
C              DATE CHOSEN FOR GEOMETRY(SURVEY FILE)
C                DATE :  period number = yyyymm
C                        period = yy stands for 19yy
C                        period = yy stands for 19yy01
C                        period = yymm stands for 19yymm
   33 CONTINUE
      IF (IW(KARD+1)/10000 .GE. 19) THEN
        IDATJO = IW(KARD+1)
      ELSEIF (IW(KARD+1) .EQ. 0) THEN
        IDATJO = 200001
      ELSEIF (IW(KARD+1) .EQ. 1) THEN
        IDATJO = 200101
      ELSE
        I = MOD (IW(KARD+1),10000)
        IF (I .LT. 100) THEN
          IDATJO = (1900+IW(KARD+1))*100+1
        ELSEIF (I .LE. 3000) THEN
          IDATJO = IW(KARD+1)*100+1
        ELSE
          IDATJO = 190000+IW(KARD+1)
        ENDIF
      ENDIF 
      GOTO 1
C
C              DETECTOR SETS : SETS   'name1'   'name2'   'name3' ...
C                              'name' of the detectors for which PROcess
C                              of HITS , DIGI modules are required.
C                              'name' must start with the 2 letter-code
C                              of a det.: VD, IT, TP, EC, LC, SA, HC, MU
   35 DO 351 I=1,IW(KARD)
        IF(MSETJO.GE.LSET) GOTO 1
        MSETJO=MSETJO+1
        TSETJO(MSETJO)=CHAINT(IW(KARD+I))
  351 CONTINUE
      DO 352 I=1,LDET
        DO 3521 M=1,MSETJO
          IF(TGEOJO(I).EQ.TSETJO(M)(1:2)) GOTO 3522
 3521   CONTINUE
        IDETJO(I) = 0
        GOTO 352
 3522   IDETJO(I) = I
  352 CONTINUE
      GOTO 1
C
C             Random generator : RNDM process # / n1  n2  n3
C                                process # : integer in the range [1,6]
C                                            for modules KINE, TRAC,
C                                            HITS, DIGI, TRIG, RDST
C                                   n1..n3  are integers
C                                   which will be used as roots for the
C                                   random generator of the 1st event.
C                                   there is 1 to 3 roots per module
C                                   depending of the random number generator
C                                   used: RNDM    1 root
C                                         RANECU  2 roots
C                                         RANMAR  3 roots
   36 NRND = MAX (NR,1)
      IF(NRND.GE.LPRO) GOTO 1
      DO 361 I=1,IW(KARD)
        IRNDJO(I,NRND) = IW(KARD+I)
  361 CONTINUE
      GOTO 1
C
C                RUN  :  RUN   n1    'title'  (up to 60 char.)
   37 IRUNJO=IW(KARD+1)
      J1 = 1
      DO J=2,IW(KARD)
         TRUNJO(J1:J1+3) = CHAINT(IW(KARD+J))
         J1 = J1+4
      ENDDO
      GOTO 1
C
C - FIDUCIAL VOLUME : FIDU  p1   p2   p3   p4
C                      p1   max. radius of ALEPH
C                      p2   max. z
C                      p3   mag. field
C                      p4   beam energy in center of mass
   60 CONTINUE
      ALRMAX=RW(KARD+1)
      ALZMAX=RW(KARD+2)
      ALFIEL=RW(KARD+3)
      ALECMS=RW(KARD+4)
      GOTO 1
C
C ----------  LAST DATA CARD : check consistency of flags set ----------
C
   70 WRITE(LOUTIO,801)XINP
  801 FORMAT(/1X,'+++ASREDC+++ END OF DATA CARDS ',A8)
C
      IF (LVELJO(6) .EQ. 0) THEN
         LVELJO(6) = 1
         WRITE(LOUTIO,*) ' +++ASREDC+++ DO NOT REMOVE SATR/SAMBA - ',
     &                   ' GEOM SATR 0 has been cancelled.'
      ENDIF
C
      JTIME = IW(NAMIND('TIME'))
      IF (JTIME.EQ.0) THEN
         TIMLJO = 10.
      ELSEIF (IW(JTIME).EQ.0) THEN
         TIMLJO = 10.
      ELSE
         TIMLJO = IW(JTIME+1)
         IF (TIMLJO.LT.1.) TIMLJO = RW(JTIME+1)
      ENDIF
C
      IF (IW(NAMIND('FILI')).GT.0) MGETJO = 1
      IF (IW(NAMIND('FILO')).GT.0) MSAVJO = 1
C
      IF (MGETJO.EQ.0) THEN
         JNEVT = IW(NAMIND('NEVT'))
         IF (JNEVT.EQ.0) THEN
            CALL ALTELL ('ASREDC: no input file and no NEVT card',
     &                    0,'STOP')
         ELSEIF (IW(JNEVT).EQ.2) THEN
            IW(JNEVT+1) = -IW(JNEVT+2)
            JNEVT = NBANK('NEVT',0,1)
         ENDIF
      ENDIF
C
  999 RETURN
      END
#endif
