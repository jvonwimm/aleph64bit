      SUBROUTINE AUNPCK(LIST,ULIST,IER)
C-------------------------------------------------------------------
CKEY ALPHARD POT UNPACK
C   Created  by : D.SCHLATTER          11-AUG-1988
C   modified by : F.Ranjard - 881200
C                             920224 to call TPDVEL at new run
C
C! Unpacks the banks  (POT,DST...  format to JULIA format )
C  Needs database ADBSCONS. Will be opened if not yet opened.
C
C   Inputs:  ULIST:  character string for unpacking of banks:
C                    'AL '  all banks are unpacked but no
C                           coordinate sorting.
C                    'VD '
C                    'IT '  only ITC
C                    'TP '  only TPC
C                    'TE '  only dE/dx
C                    'EC '  Ecal ( electron id. )
C                    'HC '
C                    'MU '
C                    'LC '
C                    'SA '
C                    'FI '  track fits
C                    'SO '  to sort coordinates in phi
C                           to redo pattern recognition
C                    'CR '  cal-object relationship banks
C                    '   '  NO unpacking
C              Example:  ULIST='IT TP EC HA '
C                        ULIST='AL  SO '
C
C                LIST  = BOS list of bank names
C                        if LIST(2:2) .eq. '-' then
C                           POT banks are dropped.
C
C   OUTPUT:      IER   = 0  successful unpacking
C                        -1 OK but garbage collection
C                        1  at least 1 POT bank does not exist
C                        2  not enough space
C                        >2 TPC internal error
C                        >90cannot find data base constants
C                'S'list    contains the list of created banks
C                           it is dropped and reset to '0' in AREAD
C
C   Calls:     JUNIDB,ABRUEV,ERDDAF,ECDFRD,HRDDAF,TRDDAF,TPDVEL,
C              IRDDAF,PITCOJ,FPTOJ,TPTOJ,PHSTOJ,PHMADJ,PEIDTJ,AUNPUS
C
C----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      CHARACTER*(*) LIST
      CHARACTER*(*) ULIST
      CHARACTER*80 UNLIST,UOLIST
      CHARACTER*3 UNAM(12)
      CHARACTER*1 RLIST
      LOGICAL FIRST,NEWRUN
      LOGICAL FLAGS(15),NOINI(15)
      REAL DVA(3),DVB(3)
      DATA FIRST/.TRUE./
      DATA FLAGS /15*.FALSE./
C   NU= number of detectors to unpack corresponding to 'AL'
C   excluding  'SO'rt !!
      DATA IROLD/0/,NU/10/
      DATA UNAM/'VD ','IT ','TP ','EC ','HC ',
     & 'MU ','LC ','SA ','TE ','FI ','SO ','AL '/
#include "bmacro.h"
C----------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        UOLIST = ' '
        LRCONS=JUNIDB(0)
C           ITC
        NAPIDI=NAMIND('PIDI')
C           TPC
        NAPTCO=NAMIND('PTCO')
        NAPTBC=NAMIND('PTBC')
        NAPTNC=NAMIND('PTNC')
C            DEDX
        NAPTEX=NAMIND('PTEX')
C           TRACKS
        NAPFRT=NAMIND('PFRT')
        NAPFRF=NAMIND('PFRF')
C             ECAL
        NAPEID=NAMIND('PEID')
C             HCAL
        NAPHST=NAMIND('PHST')
C            HCAL/MUON
        NAPHMA=NAMIND('PHMA')
      ENDIF
C
C - Decode ULIST if different from previous call ====================
      IF (ULIST .NE. UOLIST) THEN
        UOLIST = ULIST
        LE=LNBLNK(ULIST)
C
C     which banks should be unpacked ?
C
        UNLIST=ULIST(1:LE)//' '
        DO 1 I=1,15
          NOINI(I)=.TRUE.
          FLAGS(I)=.FALSE.
          IF(I.GT.NU) GOTO 1
          I2=INDEX(UNLIST,'AL ')
          IF(INDEX(UNLIST,UNAM(I)).NE.0 .OR. I2.NE.0)  FLAGS(I)=.TRUE.
    1   CONTINUE
C
C        set special flags for TPC
        IF(INDEX(UNLIST,'SO ').NE.0) FLAGS(11)=.TRUE.
        IF(FLAGS(3)) THEN
C             bank TPCO
          FLAGS(13)=.TRUE.
C             bank TBCO
          FLAGS(14)=.TRUE.
        ENDIF
C             dE/dx : bank TEXS
        FLAGS(15)=FLAGS(9)
      ENDIF
C
C - new run ?   ===================================================
C
      CALL ABRUEV (IRUNRC,IEVT)
      NEWRUN=.FALSE.
      IF (IROLD.NE.IRUNRC) THEN
        IROLD=IRUNRC
        NEWRUN=.TRUE.
C      get the magnetic field at begin of run
        FIELD = ALFIEL (0)
      ENDIF
C
C     initialize the ITC geometry
      IF(FLAGS(2)) THEN
        IF(NEWRUN.OR.NOINI(2)) THEN
          NOINI(2)=.FALSE.
          CALL IRDDAF(IRUNRC,IRET)
          IF (IRET.EQ.0) THEN
            IER=92
C            GOTO 999
          ENDIF
        ENDIF
      ENDIF
C
C     initialize the TPC geometry
      IF(FLAGS(3)) THEN
        IF(NEWRUN.OR.NOINI(3)) THEN
          NOINI(3)=.FALSE.
          CALL TRDDAF(LRCONS,IRUNRC,IRET)
          IF (IRET.EQ.0) THEN
            IER=93
            GOTO 999
          ENDIF
C        get TPC drift velocity for this run
          CALL TPDVEL ('POT',DVA,DVB,IRET)
          IF (IRET.NE.0) THEN
            IER = 93
            GOTO 999
          ENDIF
        ENDIF
      ENDIF
C
C     initialize the ECAL geometry
      IF(FLAGS(4)) THEN
        IF(NEWRUN.OR.NOINI(4)) THEN
          NOINI(4)=.FALSE.
          CALL ERDDAF (LRCONS,IRUNRC,IRET)
          IF (IRET.EQ.0) THEN
            IER=94
            GOTO 999
          ELSE
            CALL ECDFRD
          ENDIF
        ENDIF
      ENDIF
C
C     initialize the HCAL geometry
      IF(FLAGS(5)) THEN
        IF(NEWRUN.OR.NOINI(5)) THEN
          NOINI(5)=.FALSE.
          CALL HRDDAF (LRCONS,IRUNRC,IRET)
          IF (IRET.EQ.0) THEN
            IER=95
            GOTO 999
          ENDIF
        ENDIF
      ENDIF
C
C -  Unpack the global Fit banks ==================================
C
      IF(FLAGS(10).AND.IW(NAPFRF).NE.0) THEN
        CALL FPTOJ (LIST,IER)
        IF (IER .GE. 2) RETURN
        CALL PITMAJ(LIST,FIELD,IER)
        IF (IER .EQ. 2) RETURN
      ENDIF
C
C - Unpack  ITC coordinates  =======================================
C
      IF(FLAGS(2).AND.IW(NAPIDI).NE.0) THEN
        CALL PITCOJ( LIST, IER )
      ENDIF
C
C - Unpack  TPC coordinates  =====================================
C
      IF((FLAGS(13) .AND.
     &      (IW(NAPTCO).NE.0 .OR. IW(NAPTNC).NE.0)) .OR.
     &              (FLAGS(14).AND.IW(NAPTBC).NE.0) .OR.
     &              (FLAGS(15).AND.IW(NAPTEX).NE.0)) THEN
        CALL TPTOJ( FLAGS(13),FLAGS(11),LIST,IER )
        IF (IER .GE. 2) RETURN
C
      ENDIF
C
C - Unpack ECAL  ===================================================
C
      IF (FLAGS(4).AND.IW(NAPEID).NE.0) THEN
        CALL PEIDTJ (LIST,IER)
        IF (IER .GE. 2) RETURN
      ENDIF
C
C - Unpack HCAL  ===================================================
C
      IF(FLAGS(5).AND.IW(NAPHST).NE.0) THEN
        CALL PHSTOJ (LIST,IER)
        IF (IER .GE. 2) RETURN
      ENDIF
C
      IF((FLAGS(5).OR.FLAGS(6)).AND.IW(NAPHMA).NE.0) THEN
        CALL PHMADJ (LIST,IER)
        IF (IER .GE. 2) RETURN
      ENDIF
C
C - Unpack PCRL ( cal-object relationships ) ========================
C
      IF(INDEX(ULIST,'CR ').NE.0) THEN
        CALL PCRLTJ(IER)
      ENDIF
C
C  -   USER Unpack   ================================================
C
      CALL AUNPUS( LIST, ULIST )

  999 CONTINUE
      END
#endif
