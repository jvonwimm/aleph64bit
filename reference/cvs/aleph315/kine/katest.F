#if defined(KINETEST)
      PROGRAM KINGAL
C--------------------------------------------------------------------
C - B. Bloch-Devaux sept. 87                     F.Ranjard - 871116
C! Main program to run KINGAL in a stand alone mode
C
CKEY KINE KINGAL MAIN / INTERNAL
C   The program calls 3 interface routines to be provided by the user
C     -ASKUSI    at initialisation step
C     -ASKUSE    at event process step
C     -USCJOB    at end of job step
C
C   a set of these suboutines follows the PROGRAM KINGAL in this deck
C   and can be selected with the historian flag KINETEST.
C   This example uses LUND as the user event generator.
C   The FORTRAN file can be extracted from the ALEPHLIB using
C   Historian:
C     $ @HISTOR P=ALE:ALEPHLIB.HLB,C=KINGAL.FOR,Q
C       *DF KINETEST
C       *E KATEST
C       ctrl Z
C     $
C
C   In order to extract only the main program, use the historian flag
C   KMAIN as below:
C     $ @HISTOR P=ALE:ALEPHLIB.HLB,C=KAMAIN.FOR,Q
C       *DF KINETEST,KMAIN
C       *E KATEST
C       ctrl Z
C     $
C   Other examples can be found in decks KAEXA1, KAEXA2, KAEXA3.
C   The code can be accessed using the Historian flags KINETEST.
C--------------------------------------------------------------------
#ifndef DOC
      PARAMETER (LMHLEN=2, LMHROW=2, LMHCOL=1)
      PARAMETER (LBCS=50000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LHB=20000)
      COMMON //  HB(LHB)
      EXTERNAL KMRUNH,KMKRUN
      INTEGER KMRUNH,KMKRUN
C
C  INITIALIZATION            *********************
C
C 1. General facilities initialization
C
C   LBASE = data base logical unit # ( 4 is the default DAF unit )
C   LCARD = data card logical unit # (= 0 means "no data card")
C                                       7 is default
C   LOUT  = printout logical unit # ( = 0 means "no printout")
C                                       6 is default
C   LWRT  = BOS write logical unit # ( = 0 means "no file written")
C                                       0 is default
C   FMT   = BOS write format ('EPIO' means "EPIO format")
C                            ( ' '  means NATIVE format )
C
      PARAMETER (LCTIT=48, LWTIT=LCTIT/4)
      CHARACTER*48 NTIT
      CHARACTER*4 FMT,CHAINT
#include "kipara.h"
      DATA IDPR,WEIT /0,1./
      DATA LBASE,LCARD,LOUT,LWRT /4,7,6,0/
      DATA FMT /' '/
C ------------------------------------------------------------------------------
      CALL KINIT (LBCS,LBASE,LCARD,LOUT,LWRT,FMT)
C
C   Initialise Hbook Package
C
      CALL HLIMIT(LHB)
C
C 2. Run's specific parameters initialization
C
C   BANK RUNH and KRUN
C   NRUN = run #
C   NEXP = experiment # (mandatory 1001 for monte carlo)
C   NRTP = run type (10000 - 99999 for user needs)
C   IGCOD = generator code (has to be the same as the generator code
C                          number given in the library)
C   NOTR  = notracking marking word (the default value has to be taken)
C   NTIT  = run title
C
C      suggested RUN card ( as GALEPH ) RUN  NRUN  'TITLE'
C
      NRUN = LRUN
      NEXP = LEXP
      NOTR = LNOTRK
      NTIT = LTITL
      IGCOD = LUCOD
      JRUN = NLINK('RUN ',0)
      IF(JRUN.NE.0) THEN
         NRUN = IW(JRUN+1)
         NKAR = IW(JRUN)-1
         J=1
         NKMAX = MIN(LWTIT,NKAR)
         DO 10 I=1,NKMAX
            NTIT(J:J+3) = CHAINT(IW(JRUN+1+I))
            J=J+4
   10    CONTINUE
      ENDIF
C  TRIG        n1    n2
C    Suggested data card ( as GAleph)
C
      IEV1 = 1
      IEV2 = 100
      JTRIG = NLINK('TRIG',0)
      IF(JTRIG.NE.0) THEN
         IEV1 = IW(JTRIG+1)
         IEV2 = IW(JTRIG+2)
      ENDIF
C  DEBUG lout / ndeb1  ndeb2
      IDB1 = 0
      IDB2 = 0
      NADEB = NAMIND('DEBU')
      JDEBU = IW(NADEB)
      IF(JDEBU.NE.0) THEN
         IDB1 = IW(JDEBU+1)
         IDB2 = IW(JDEBU+2)
         LOUT = IW(JDEBU-2)
      ENDIF
C  SAVE lwrt / fmt
      NASAV = NAMIND('SAVE')
      JSAVE = IW(NASAV)
      IF(JSAVE.NE.0) THEN
         LWRT = IW(JSAVE-2)
         FMT = CHAINT(IW(JSAVE+1))
      ENDIF
C
C  IF DATA CARDS HAVE BEEN READ, RECALL KINIT
C
      IF(LCARD.GT.0) CALL KINIT (LBCS,LBASE,LCARD,LOUT,LWRT,FMT)
C
C 3. Generator initialization
C
      CALL ASKUSI(IGCOD)
      NRTP = IGCOD+10000
C
C  REDEFINE SOME RUNH AND KRUN PARAMETERS
C
      NBK = KMRUNH (NRUN,NEXP,NRTP)
      NBK = KMKRUN (IGCOD,NOTR,NTIT)
C
C  PERFORM GARBAGE COLLECTION
C
      CALL BGARB(IW)
C
C  EVENT GENERATION          *********************
C
      DO 20 IEVT = IEV1,IEV2
C
         CALL ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C
C  if DEBUG required
         IF(IEVT.GE.IDB1 .AND. IEVT.LE. IDB2) THEN
            WRITE(IW(6),'(/1X,''IEVT,IDPR,ISTA,NTRK,NVRT,ECMS,WEIT'',
     +         5I10,F10.4,E10.5)') IEVT,IDPR,ISTA,NTRK,NVRT,ECMS,WEIT
            CALL PRKINE
            WRITE (LOUT,'(/1X,''+++KATEST+++ KHIS bank print out'')')
            JKHIS = NLINK ('KHIS',0)
            NROW = IW(JKHIS+LMHROW)
            M1 = 1
 19         M2 = MIN (M1+9,NROW)
            WRITE (LOUT,'(/1X,10I10)') (LLL,LLL=M1,M2)
            WRITE (LOUT,'(1X ,10I10)') (IW(JKHIS+LMHLEN+LLL),LLL=M1,M2)
            M1 = M1+10
            IF (M1.LE.NROW) GOTO 19
         ENDIF
C
         MSTAT = 0
         IF(ISTA.EQ.0) MSTAT = 1
         CALL KEEVT(IEVT,MSTAT,NVRT,NTRK,WEIT,IDPR,ECMS,LWRT)
C
   20 CONTINUE
C
C  END OF GENERATION         *********************
C
      CALL USCJOB
C
C  KINGAL END OF JOB
      NEVT = IEV2 - IEV1 + 1
      CALL KEJOB(LWRT,NEVT)
C
      CALL HPHST(0)
C
      STOP
      END
#ifndef KMAIN
      SUBROUTINE ASKUSI(IGCOD)
C ------------------------------------------------------------------
C - F.Ranjard - 870604
CKEY KINE KINGAL EXAMPLE / USER
C! Example of an initialization routine of a user generator
C  in this example LUND is the user generator
C  ASKUSI is a GALEPH dummy routine called at initialization time
C  in GALEPH when the data card KINE  'USER' is selected.
C ------------------------------------------------------------------
#include "lundcom.h"
#include "bcs.h"
      PARAMETER (LPDEC=48)
      INTEGER NODEC(LPDEC)
C    IGCOD is 0 for LUND
      PARAMETER ( IGCO  =  0)
C -----------------------------------------------------------------
C
C   Return generator code IGCOD
C
      IGCOD = IGCO
C -- Set MYGEN parameters by data cards
C    since MYGEN is LUND I can use the library routine KXLUCO
      CALL KXLUCO (LUPAR)
C
C -- complete PART bank with MYGEN particles
C    since MYGEN is LUND I can use the library routine KXLUPA
      CALL KXLUPA (IPART,IKLIN)
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''
     +                 ,2I3)') IPART,IKLIN
         GOTO 20
      ENDIF
C
C - Print PART and KLIN banks
      CALL PRPART
C
C -- get list of MYGEN particle# which should not be decayed
C    in MYGEN because they are decayed in GALEPH.
C    the routines uses the KLIN bank and fills the user array
C    NODEC in the range [1-LPDEC]
      MXDEC = KNODEC (NODEC,LPDEC)
      MXDEC = MIN (MXDEC,LPDEC)
C
C -- inhibit decays in MYGEN
C    If the user has set some decay channels by data cards they will
C    will not be overwritten (i.e. the user has a data card IDB 23 / 'ON'
C    then the PI0 will be decayed in LUND)
C    BUT BE CAREFULL in this case, to fill properly the secondary
C    vertices, if not the PI0 will be decayed in the generator AND
C    in GEANT during tracking.
      IF (MXDEC .GT. 0) THEN
         DO 10 I=1,MXDEC
            IF (NODEC(I).GT.0) THEN
               JIDB = NLINK('IDB ',NODEC(I))
               IF (JIDB .EQ. 0) IDBLU3(NODEC(I)) = 0
            ENDIF
   10    CONTINUE
      ENDIF
C
   20 CONTINUE
      END
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C ------------------------------------------------------------
C - F.Ranjard - 870604
CKEY KINE KINGAL EXAMPLE /USER
C! Example of a user routine called at each event to get an
C  event from the user generator and to transfer the information
C  into VERT and KINE banks.
C  the example uses LUND as user generator.
C  the routine ASKUSE is a dummy routine of the GALEPH library.
C  It is called when using the data card KINE  'USER'
C
C     structure : subroutine
C     output arguments :
C          IDPR   : process identification if meanigful
C          ISTA   : status flag ( 0 means ok), use it to reject
C                   unwanted events
C          NTRK   : number of tracks generated and kept
C                  (i.e. # KINE banks  written)
C          NVRT   : number of vertices generated
C                   (i.e. # VERT banks written)
C          ECMS   : center of mass energy for the event (may be
C                   different from nominal cms energy)
C          WEIT   : event weight ( not 1 if a weighting method is used)
C -----------------------------------------------------------------
      REAL VRTX(4),SVERT(3)
C
#include "bcs.h"
#include "lundcom.h"
C     IFL      : LUND flavour , set to 0 by default, can be changed
C     ECM      : nominal cms energy
C     SVERT    : vertex smearing, set to 0. by default, can be changed
      DATA IFL,ECM / 0,90./ , SVERT /3*0./
      DATA NLUND /0/
      DATA IFI / 0/
#include "bmacro.h"
#include "kmacro.h"
C ------------------------------------------------------------------
C
      ISTA = 0
C
C  1. get an event from the generator
C
      IF ( IFI . EQ . 0 ) THEN
C - get the LUND flavour IFL if given on data card
      IF (NLUND .EQ. 0) THEN
         NLUND = NAMIND ('LUND')
         JLUND = IW(NLUND)
         IF (JLUND .NE. 0) THEN
            IFL = IW(JLUND+1)
            ECM  =  RW(JLUND+2)
         ENDIF
C - make use of a smearing of the vertex
C   if it is given
         NSVER = NAMIND ('SVER')
         JSVER = IW(NSVER)
         IF (JSVER .NE. 0) THEN
            SVERT(1) = RW(JSVER+1)
            SVERT(2) = RW(JSVER+2)
            SVERT(3) = RW(JSVER+3)
         ENDIF
      ENDIF
      IFI  =  IFI + 1
      ENDIF
C - set the process identification IDPR = IFL
      IDPR = IFL
C - set the cms energy for this event ECMS = ECM
      ECMS = ECM
C
C - get an event from LUND generator
      CALL LUEEVT (IFL,ECMS)
C
C  2. fill BOS banks
C
C - get the primary vertex
      CALL RANNOR (RX,RY)
      CALL RANNOR (RZ,DUM)
      VRTX(1) = RX*SVERT(1)
      VRTX(2) = RY*SVERT(2)
      VRTX(3) = RZ*SVERT(3)
      VRTX(4) = 0.
C
C     in this example primary particles (the ones which have
C     produced the main vertex) are not kept. To do it look at
C     the KINGAL writeup.
C
C      Call my own routine to fill the VERT and KINE banks
C      calling the function KBVERT and KBKINE
C      in this example the secondary vertices are not propagated
C      decay of particles decayed in GEANT should be inhibited
******   CALL MYKLUN (VRTX,ISTA,NVRT,NTRK)
C
C      or
C      Call the specific routine KXLUAL to fill BOS banks
C      the secondary vertices are propagated.
      CALL KXLUAL (VRTX,ISTA,NVRT,NTRK)
C
      END
      SUBROUTINE MYKLUN (VRTX,ISTA,NVRT,NTRK)
C ----------------------------------------------------------------------
C! Transfer LUND event to BOS banks
CKEY KINE KINGAL EXAMPLE  /USER
C  in this example LUND is the generator, the secondary vertices
C  are not propagated.
C. - calls        KBVERT, KBKINE, KGPART, KIBPRS     from ALEPHLIB
C --------------------------------------------------------------------
      EXTERNAL KBKINE,KBVERT,KGPART
      REAL VRTX(*),PXYZE(4)
#include "alcons.h"
#include "bcs.h"
#include "lundcom.h"
#include "bmacro.h"
C ----------------------------------------------------------------------
C
C - book and fill the main vertex bank: VERT,NR=NVRT=1
C   from a track origin # 0
      NVRT = 1
      JVERT = KBVERT (NVRT,VRTX,0)
      IF (JVERT .EQ. 0) THEN
         WRITE (IW(6),'(1X,'' bug in KBVERT '',I3)') JVERT
         ISTA = -1
         GOTO 20
      ENDIF
C
C -   Store LUND event in ALEPH banks
C
      NTRK = 0
      DO 10 J=1,NPARLU
C     Lund particle # IC
         IC=KODELU(J,2)
         IF(KODELU(J,1).LT. 0.OR. KODELU(J,1).GT.1000) GO TO 10
C     Get the Aleph particle# JT of the Lund particle# IC
         JT=KGPART (IC)
         IF(JT.LE.0) GO TO 10
C
         NTRK = NTRK + 1
C        transfer track momentum to local array
         PXYZE(1)=PARTLU(J,1)
         PXYZE(2)=PARTLU(J,2)
         PXYZE(3)=PARTLU(J,3)
C        do not give the particle mass , KBKINE will do it
         PXYZE(4)=0.
C
         JKINE = KBKINE (NTRK,PXYZE,JT,NVRT)
         IF (JKINE .LE. 0) THEN
            WRITE (IW(6),'(1X,''bug in KBKINE'',I3)') JKINE
            ISTA = -2
            GOTO 20
         ENDIF
   10 CONTINUE
C
C - end
      ISTA = 0
C
   20 RETURN
      END
      SUBROUTINE USCJOB
C-------------------------------------------------------------------
C! End of job routine
CKEY KINE KINGAL EXAMPLE   /USER
C
C   To be filled by user to print any relevant info
C
C------------------------------------------------------------------
      RETURN
      END
#endif
#endif
#endif
