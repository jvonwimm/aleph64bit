      SUBROUTINE EHTRKE(ISCMP,MODUL)
C.----------------------------------------------------------------
C Y.Karyotakis M.Rumpf Dec 85
C! Track elem. signal steering
C    INPUT : ISCMP  1 = end cap z>0  2 = Barrel  3 = End cap z<0
C          : MODUL   module number
C      - Called by ECHIT
C     Calls EHSIMT,EHCHAR,EHSITW
C.----------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
      PARAMETER (LSTCK=3,LPHI=384,LTHET=228)
      PARAMETER (LWPLA=45,LMODU=12,LCOMP=3)
      PARAMETER (NTHSG=12,NPHSG=24)
C
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)
      EXTERNAL EFNDPL
      INTEGER  EFNDPL
C
      PARAMETER (NMGTR = LWPLA)
      DIMENSION PSGTR(3,NMGTR),DRGTR(3,NMGTR),SGTR(NMGTR),IPLGT(NMGTR)
      DIMENSION QSGTR(3,NMGTR),POSIG(3,NMGTR)
C
      CHARACTER * 5 TYGEO
      TYGEO = 'ALEPH'
      NECONT(2) = NECONT(2) + 1
C
C     If it is a hadron or muon cut track segment
C     Compute intersections with virtual wire planes
C
      IF(ITRKEL(4).GT.3)  THEN
C
         CALL EHSIMT(TRKELE(1),TRKELE(4),TRKELE(11),ISCMP,MODUL,NMGTR,
     &            NGTR,PSGTR,QSGTR,DRGTR,SGTR,IPLGT)
C
         IF (NGTR.EQ.0)                  RETURN
C
C check that the plane numbers are in the range 1 - 45 .
C
         DO 5  J = 1,NGTR
           SGTR(J) = ABS( SGTR(J) )
           IF(IPLGT(J) .LT. 1 .OR. IPLGT(J) .GT. LWPLA ) THEN
             NECONT(5) = NECONT(5) + 1
             RETURN
           ENDIF
    5    CONTINUE
         DO 3 I=1,NGTR
         DO 4 J=1,3
         POSIG(J,I) = (PSGTR(J,I) + QSGTR(J,I)) * .5
    4    CONTINUE
    3    CONTINUE
C
      ELSE
          NGTR = 1
          DO 2  I=1,3
             POSIG(I,1) = (TRKELE(I) + TRKNXT(I)) * .5
    2     CONTINUE
          SGTR(1) = TRKELE(11)
C
C in about 1 event / 1000 there was a plane 46
C
          IPLGT(1) = EFNDPL(ISCMP,MODUL,POSIG(1,1),TYGEO)
          IF(IPLGT(1).LE.0.OR.IPLGT(1).GT.LWPLA)     THEN
            NECONT(5) = NECONT(5) + 1
            RETURN
          ENDIF
      ENDIF
C
C  Loop over TRACK segments to deposit energy
C
      CALL EHCHAR (POSIG,SGTR,IPLGT,NGTR)
C
C  Allocate signals to towers and wires - Update output banks
C
      CALL EHSITW(ISCMP,MODUL,TYGEO)
C
      END
