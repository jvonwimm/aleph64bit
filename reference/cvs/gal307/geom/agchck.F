*DK agchck
      SUBROUTINE AGCHCK
C-------------------------------------------------------------------
C  B. Bloch-Devaux   _  850619
C         modified 7 septembre 87 for DATA BASE usage
C         modified November 15 , 1990 for geometry setup usage
C!   Check validity of geometry data and selected detectors
C!  and set up array IGEOJO in common JOBCOM according to selected
C!      detectors
C!     Fills FMGEOM common with boundaries of field regions
C
C - modified by : F.Ranjard - 911002
C                 change COIL component # to 13 instead of 9
C - modified by : B.Bloch-Devaux - 911015
C                 handle SATR or SCAL depending on setup number
C - modified by : B.Bloch-Devaux - 920110
C                 handle more beam pipes.....
C
C    Called by AGEOME                  from this .HLB
C-------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alfgeo.h"
#include "iocom.h"
#include "jobcom.h"
#include "alcons.h"
#include "agecom.h"
#include "vvoljj.h"
#include "fmgeom.h"
#include "lcnamc.h"
#include "ljjpar.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "hcgega.h"
#include "munamc.h"
#include "mjjpar.h"
      EXTERNAL NAMIND
      DIMENSION PLAN(4,8),CORN(3,12)
      CHARACTER*16 VOLN
      DIMENSION VOLN(2)
C
      INTEGER ALGTDB, GTSTUP
      DATA VOLN       /'B external      ','E external      '/
#include "bmacro.h"
C ----------------------------------------------------------------------
C
C - Get banks for passive components
C
      IRET = ALGTDB (LRDBIO,'COG1QUG1PMG1',IRUNJO)
      IF (IRET .EQ. 0) THEN
         CALL ALTELL ('AGCHCK: missing bank ',0,'STOP')
      ENDIF
C
      DO 1 I=1,LGEO
 1      IGEOJO(I)=0
      DO 2 I=1,NLIMZ
 2      AGLIMZ(I)=0.
      DO 3 I=1,NLIMR
 3      AGLIMR(I)=0.
C
C   Check if FIDU card  read in
C
      IF (ALRMAX.LE.0. .OR. ALZMAX.LE.0.) THEN
         CALL ALTELL ('AGCHCK: wrong or missing FIDU card...STOP....'
     1   ,0,'STOP')
      ENDIF
C
C  Check Beam Pipe definition
C
      IBPST = GTSTUP ('BP',1)
      IRET = ALGTDB (LRDBIO,'BPG1',IBPST)
      KBP  = IW(NAMIND('BPG1'))
      IF (KBP.EQ.0) THEN
         CALL ALTELL ('AGCHCK: BPG1 bank is missing ',0,'STOP')
      ENDIF
      LBRO = MIN(14,LROWS (KBP))
      IF (IBPST.GE.3) LBRO = MIN(9,LROWS(KBP))
C
C   Check against fiducial volume
C
      ZMAX=0.
      RMAX=0.
      DO 5 I=1,LBRO
        RMAX=MAX(RMAX,RTABL(KBP,I,8))
        RMAX=MAX(RMAX,RTABL(KBP,I,9))
        ZMAX=MAX(ZMAX,RTABL(KBP,I,10))
   5  CONTINUE
      AGLIMR(1)=RMAX
      ZMAX=MIN(ZMAX,ALZMAX)
      AGLIMZ(2)=ZMAX
      AGLIMZ(5)=ZMAX
      IF (RMAX.GT.ALRMAX) THEN
         CALL ALTELL ('AGCHCK: inconsistent BPIP and FIDU radius ',0,
     &                'STOP')
      ENDIF
      AGLIMR(2)=RMAX
      AGLIMR(3)=RMAX
      IF ( IBPST.GE.2) THEN
C     BPIP 91, 92, .....
         ZMAX=0.
         RMAX=0.
         DO 6 I=LBRO+1,LROWS(KBP)
           RMAX=MAX(RMAX,RTABL(KBP,I,8))
           RMAX=MAX(RMAX,RTABL(KBP,I,9))
           ZMAX=MAX(ZMAX,RTABL(KBP,I,10))
6        CONTINUE
         AGLIMR(9)=RMAX
         ZMAX=MIN(ZMAX,ALZMAX)
         AGLIMZ(2)=ZMAX
         IF (RMAX.GT.ALRMAX) THEN
            CALL ALTELL ('AGCHCK: inconsistent BPIP and FIDU radius ',0,
     &                   'STOP')
         ENDIF
      ENDIF
      IGEOJO(10) = IBPST
C
C     If detector geometry defined ,then
C    Check detector against fiducial volume
C
C    Vertex detector
C
      IVSTP = GTSTUP ('VD',1)
      IRET = ALGTDB (LRDBIO,'VVOL',IVSTP)
      JVVOL = IW(NAMIND('VVOL'))
      IF (JVVOL.EQ.0) THEN
         CALL ALTELL ('AGCHCK : VVOL bank is missing ',0,'STOP')
      ELSE
         NAME = INTCHA('VDET')
         DO 10 IROW = 1,LROWS(JVVOL)
            IF (NAME .EQ. ITABL(JVVOL,IROW,JVVOVO)) GOTO 12
 10      CONTINUE
         CALL ALTELL ('AGCHCK: VDET volume missing from VVOL',0,
     &                   'STOP')
 12      VDRALO = RTABL (JVVOL,IROW,JVVOPA)
         VDRAHI = RTABL (JVVOL,IROW,JVVOPA+1)
         VDZSIZ = RTABL (JVVOL,IROW,JVVOPA+2)
      ENDIF
C
      IF (VDRALO.LT.AGLIMR(1)) THEN
         CALL ALTELL ('AGCHCK: VDET hits beam pipe',0,'STOP')
      ENDIF
      IF (VDRAHI.GT.ALRMAX .OR. VDZSIZ.GT.ALZMAX) THEN
         CALL ALTELL ('AGCHCK: VDET dim. out-of-range',7,'RETURN')
         GOTO 11
      ENDIF
      AGLIMR(2) = AMAX1(AGLIMR(2),VDRAHI)
      AGLIMZ(1) = AMAX1(AGLIMZ(1),VDZSIZ)
      IGEOJO(1) = IVSTP
      AGLIMR(3)=AGLIMR(2)
      AGLIMR(5)=AGLIMR(3)
  11  CONTINUE
C
C   ITC detector
C
      IISTP = GTSTUP ('IT',1)
      KIT  = IW(NAMIND('IGEO'))
      DO 20 I=1,LROWS(KIT)
         IF (RTABL(KIT,I,8).LE.0.) GO TO 21
         IF (RTABL(KIT,I,9).GT.ALRMAX) GO TO 21
         IF (RTABL(KIT,I,11).GT.ALZMAX) GO TO 21
         AGLIMR(3)=MAX(RTABL(KIT,I,9),AGLIMR(3))
         AGLIMZ(1)=MAX(RTABL(KIT,I,11),AGLIMZ(1))
  20  CONTINUE
      IGEOJO(2)=IISTP
      AGLIMR(5)=AGLIMR(3)
      AGLIMZ(6)=RTABL(KIT,4,11)
  21  CONTINUE
C
C   TPC detector
C
      IF (RTPCMN-DRTPMN.LE.0.) GO TO 31
      IF (RTPCMN-DRTPMN.GT.ALRMAX) GO TO 31
      IF (RTPCMX+DRTPMX.GT.ALRMAX) GO TO 31
      IF (ZTPCMX+DZTPMX.GT.ALZMAX) GO TO 31
      AGLIMR(3)=MAX (RTPCMN-DRTPMN,AGLIMR(3))
      AGLIMZ(1)=MAX (ZTPCMX+DZTPMX,AGLIMZ(1))
      AGLIMR(5)=MAX (RTPCMX+DRTPMX,AGLIMR(3))
      IGEOJO(3)=1
  31  CONTINUE
C
C ECAL detector
C
C          End cap
         CALL EVOLPL(VOLN(2),1,0,LEP,PLAN)
         CALL EVOLCR(VOLN(2),LEP,PLAN,LEC,CORN)
         IF (CORN(3,2).LE.0.) GO TO 41
         IF (CORN(3,2).GT.ALZMAX ) GO TO 41
         IF (CORN(3,1).GT.ALZMAX ) GO TO 41
         AGLIMZ(2)=MAX (AGLIMZ(2),CORN(3,1) )
         IF (ABS(CORN(2,1)).GT.ALRMAX ) GO TO 41
         IF (ABS(CORN(2,5)).GT.ALRMAX ) GO TO 41
         IF (ABS(CORN(2,9)).GT.ALRMAX ) GO TO 41
C           Barrel
         CALL EVOLPL(VOLN(1),2,0,LEP,PLAN)
         CALL EVOLCR(VOLN(1),LEP,PLAN,LEC,CORN)
         AGLIMZ(1)=MAX (AGLIMZ(1),ABS(CORN(3,1)))
         AGLIMR(6)=MAX (AGLIMR(5),ABS(CORN(2,5)))
         AGLIMR(5)=MAX (AGLIMR(5),ABS(CORN(2,1)))
         IGEOJO(4)=1
  41  CONTINUE
C
C  LCAL detector
C
C   Get geometry bank pointers
C
      ILSTP = GTSTUP ('LC',1)
      KLCAL = IW(NALCAL)
      KLMTY = IW(NALMTY)
      KLSCO = IW(NALSCO)
C
      NSUBC = ITABL(KLCAL,1,JLCANS)
      DO 50 I=1,NSUBC
         ABSZ = ABS(RTABL(KLSCO,I,JLSCRP+2))
         IF (ABSZ.LE.0.) GO TO 51
         IF (ABSZ.GT.ALZMAX ) GO TO 51
         CENZ = ABSZ+RTABL(KLCAL,1,JLCAZL)
         IF (CENZ.GT.ALZMAX ) GO TO 51
         AGLIMZ(2)=MAX (AGLIMZ(2),CENZ)
  50  CONTINUE
      IF (RTABL(KLCAL,1,JLCARI).GT.ALRMAX ) GO TO 51
      IF (RTABL(KLCAL,1,JLCARO).GT.ALRMAX ) GO TO 51
      AGLIMR(6)=MAX(AGLIMR(6),RTABL(KLCAL,1,JLCARI))
      AGLIMR(6)=MAX(AGLIMR(6),RTABL(KLCAL,1,JLCARO))
      IGEOJO(5)=ILSTP
  51  CONTINUE
C
C  SATR detector or SAMBA
C
      ISSTP = GTSTUP ('PM',1)
      IF ( ISSTP.GT.0) THEN
C     SAMBA
         IGEOJO(6)=ISSTP+1
      ELSE
C     SATR  
         KLA=IW(NAMIND('LALI'))
         KLS=IW(NAMIND('LSLO'))
         KSA=IW(NAMIND('SATR'))
         IF (KSA.GT.0 .AND. KLA.GT.0 .AND. KLS.GT.0) THEN
            KSB=KSA+LMHLEN
            DO I=1,4
              Z = RTABL(KLS,I,7)+RTABL(KLA,I,6)
              IF (ABS(Z).GT.ALZMAX ) GO TO 61
              AGLIMZ(2)=MAX(AGLIMZ(2),ABS(Z))
            ENDDO
            IF (RW(KSB+9).GT.ALRMAX ) GO TO 61
            IF (RW(KSB+10).GT.ALRMAX ) GO TO 61
            AGLIMR(6)=MAX(AGLIMR(6),RW(KSB+10))
            IGEOJO(6)=1
         ENDIF
      ENDIF 
  61  CONTINUE
C
C  SCAL detector
C
      ISITP = GTSTUP ('SI',1)
      IF ( ISITP.LE.0) GO TO 62
      IGEOJO(9)=ISITP
  62  CONTINUE
C
C    HCAL   detector
C
      DO 70 I=2,3
         IF (HCRMIN(I).LE.0.) GO TO 71
         IF (HCRMIN(I).GT.ALRMAX ) GO TO 71
         IF (HCRMAX(I).GT.ALRMAX ) GO TO 71
         IF (HCZMIN(I).GT.ALZMAX ) GO TO 71
         IF (HCZMAX(I).GT.ALZMAX ) GO TO 71
         AGLIMR(4)=MAX (AGLIMR(4),HCRMIN(I))
         AGLIMZ(4)=MAX(AGLIMZ(2),HCZMAX(I))
  70  CONTINUE
         AGLIMZ(2)=MAX (AGLIMZ(2),HCZMIN(2))
         AGLIMR(6)=MAX (AGLIMR(6),HCRMAX(2))
C
      IF (HCRMIN(1).LE.0.) GO TO 71
      IF (HCRMIN(1).GT.ALRMAX) GO TO 71
      IF (HCRMAX(1).GT.ALRMAX) GO TO 71
      IF (HCZMAX(1) .GT.ALZMAX) GO TO 71
      AGLIMR(7)=MAX (HCRMIN(1),AGLIMR(7))
      AGLIMZ(3)=MAX (HCZMAX(1),AGLIMZ(3))
      AGLIMR(8)=MAX (HCRMAX(1)/COS(PI/12.),AGLIMR(7))
      IGEOJO(7)=1
  71  CONTINUE
C
C          MUON detector
C
#include "muiwna.h"
C
C       Check barrel modules
C
      IF( RTABL(JMBAG,1,JMBALE)*0.5 .GT. ALZMAX )GOTO 89
      RITNG = RTABL(JMBAG,1,JMBAR1)-0.5*RTABL(JMBAG,1,JMBATH)
      ROTNG = RTABL(JMBAG,1,JMBAR2)-0.5*RTABL(JMBAG,1,JMBATH)
      IF(RITNG .GT. ALRMAX )GOTO 89
      IF(ROTNG .GT. ALRMAX )GOTO 89
C
C       Check middle angle modules
C
      RMIN=ALRMAX
      ZOUT = RTABL(JMMSG,13,JMMSZC) + 0.5*RTABL(JMMTG,6,JMMTZB)
      IF( ZOUT .GT. ALZMAX )GOTO 89
      NSLOT = ITABL(JMMAG,1,JMMANS)
      DO 81 I=1,NSLOT
         IF( RTABL(JMMSG,I,JMMSRC) .GT. ALRMAX )GOTO 89
         RMIN=MIN(RMIN,RTABL(JMMSG,I,JMMSRC))
   81 CONTINUE
      AGLIMR(8)=MAX(RMIN,AGLIMR(8))
C
C       Check endcap modules
C
      THCK = RTABL(JMECG,1,JMECTH)
      ZOUT = RTABL(JMESG,5,JMESZC) - RTABL(JMECG,1,JMECZE) + THCK
      IF( ZOUT .GT. ALZMAX )GOTO 89
      ZIN = RTABL(JMESG,1,JMESZC) - RTABL(JMECG,1,JMECZI)
      AGLIMZ(4)=MAX(AGLIMZ(4),ZIN)
C
      IGEOJO(8) = 1
   89 CONTINUE
C
C COIL detector
C
      KCO  = IW(NAMIND('COG1'))
      IF (RTABL(KCO,1,8).GT.ALRMAX) GO TO 91
      IF (RTABL(KCO,1,8)+RTABL(KCO,1,9).GT.ALRMAX) GO TO 91
      IF (RTABL(KCO,1,10)+RTABL(KCO,1,11).GT.ALZMAX) GO TO 91
      AGLIMR(6)=MAX(RTABL(KCO,1,8),AGLIMR(6))
      AGLIMR(7)=MAX(RTABL(KCO,1,8)+RTABL(KCO,1,9),AGLIMR(7))
      AGLIMZ(3)=MAX(RTABL(KCO,1,10)+RTABL(KCO,1,11),AGLIMZ(3))
      IGEOJO(13)=1
  91  CONTINUE
C
C QUUA detector
C
      KQU  = IW(NAMIND('QUG1'))
      LQRO = LROWS (KQU)
      DO 101 L=1,LQRO
        IF(RTABL(KQU,L,8).LE.0. ) GO TO 111
        IF (RTABL(KQU,L,9).GT.ALRMAX) GO TO 111
        IF (RTABL(KQU,L,10).GT.ALZMAX) GO TO 111
        AGLIMR(4)=MAX(AGLIMR(4),RTABL(KQU,L,9))
 101  CONTINUE
      IGEOJO(11)=1
 111  CONTINUE
C
C Set up passive material ( can be cancelled by a GEOM 'PMAT' 0 card)
C PMAT represents ECAL/HCAL electronics boxes. 
C Nothing to do with SAMBA
C
      IGEOJO(12) = 1
C
C   Check now if detector was not suppressed by GEOM 0  card
C   Do NOT remove SATR/SAMBA material(I=6), it is used by LCAL
C   if the card GEOM 'SATR' 0 is present ASREDC removed it
      DO 200 I=1,LGEO
      IF (LVELJO(I).EQ.0) IGEOJO(I)=0
 200  CONTINUE
C
C   Load PMG1 bank if necesary
C
      KPM = IW(NAMIND('PMG1'))
      IF ( KPM.EQ.0) IGEOJO(12)=0
C
C   fill /FMGEOM/ common to be used in connection with mag field
C
      FMRMIN(1)=0.
      FMRMAX(1)=ABS(CORN(2,1))
      FMZMIN(1)=0.
      FMZMAX(1)=ZTPCMX+DZTPMX
      FMRMIN(2)=RTABL(KCO,2,8)
      FMRMAX(2)=HCRMIN(1)
      FMZMIN(2)=0.
      FMZMAX(2)=HCZMIN(2)
      FMRMIN(3)=HCRMAX(2)
      FMRMAX(3)=HCRMIN(1)
      FMZMIN(3)=HCZMIN(2)
      FMZMAX(3)=HCZMAX(1)
      FMRMIN(4)=HCRMIN(1)
      FMRMAX(4)=FMRMIN(4)+86.9
      FMZMIN(4)=0.
      FMZMAX(4)=HCZMAX(1)
      FMRMIN(5)=FMRMAX(4)
      FMRMAX(5)=HCRMAX(1)
      FMZMIN(5)=0.
      FMZMAX(5)=HCZMAX(1)
      FMRMIN(6)=HCRMIN(2)
      FMRMAX(6)=FMRMIN(3)
      FMZMIN(6)=HCZMIN(2)
      FMZMAX(6)=HCZMAX(1)
      FMRMIN(7)=HCRMIN(3)
      FMRMAX(7)=HCRMAX(3)
      FMZMIN(7)=FMZMAX(6)
      FMZMAX(7)=FMZMIN(7)+55.4
      FMRMIN(8)=HCRMIN(3)
      FMRMAX(8)=FMRMAX(7)
      FMZMIN(8)=FMZMAX(7)
      FMZMAX(8)=HCZMAX(3)
      FMRMIN(9)=0.
      FMRMAX(9)=HCRMIN(2)
      FMZMIN(9)=HCZMIN(2)
      FMZMAX(9)=HCZMAX(3)
      FMRMIN(10)=FMRMAX(1)
      FMRMAX(10)=FMRMIN(2)
      FMZMIN(10)=0.
      FMZMAX(10)=FMZMAX(1)
      FMRMIN(11)=0.
      FMRMAX(11)=FMRMAX(10)
      FMZMIN(11)=FMZMAX(1)
      FMZMAX(11)=HCZMIN(2)
C - debug
      IF(IPRIJO(19).EQ.1) THEN
         WRITE (LOUTIO,805) ALRMAX,ALZMAX,IGEOJO,AGLIMR,AGLIMZ,
     &   (I,I=1,LNRG),(FMRMIN(I),I=1,LNRG),(FMRMAX(I),I=1,LNRG),
     &   (FMZMIN(I),I=1,LNRG), (FMZMAX(I),I=1,LNRG)
      ENDIF
C - end
 805  FORMAT(/1X,'+++ AGCHCK +++                ++++++++++++++++++',
     & ' GEOMETRY SET UP ++++++++++++++++++++++++++++++++',/
     &' According to the fiducial volume defined (R,Z) CM',9X,2F10.4,/
     &' and the geometry found, ',
     &' the following detectors will be set up with ',
     &  'the given setup code ( 0 = OFF)'/
     &  '    VD   IT   TP   EC   LC   SA   HC   MU   SI   BP   QU  ' ,
     &  'PM   CO'/ 1X,13I5//
     &  ' artificial boundaries may be set up as (R(9) , Z(5) )'/
     &  ' R(1:9) ',3X,9F8.3/' Z(1:6) ',3X,6F8.3 //
     &  ' Limits for Field Regions will be set up as :' /
     &  ' Region # ',11I10/' Rmin     ',11F10.2/' Rmax     ',11F10.2/
     &  ' Zmin     ',11F10.2/' Zmax     ',11F10.2/
     &  ' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++',
     &  ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      END
#endif
