      INTEGER FUNCTION VDEXCY(ILAY,TPAR,ZBUF,GPOS0,IPHI,IWAFF)
C----------------------------------------------------------------------
C!  Extrapolate a given track to all a Vdet cylinder
CKEY VDET TRACK
C!
C!   Author   :- A. Bonissent March 1995
C!   Inputs:
C!
C!        ILAY       - layer
C!        TPAR       - 5 Track parameters, as in FRFT
C!        ZBUF       - Quantity added to Vdet for X acceptance
C!
C!   Outputs:
C!        GPOS0(3)   - 3D position of track intersection with Vdet cyl.
C!        IPHI       - face number of above
C!        IWAFf      - wafer in face of above
C!
C!        VDEXCY     - return code. 0=OK
C!                                  1=track does not intersect
C!
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
      REAL TPAR(*),GPOS0(3),ZBUF
      INTEGER ILAY,JFAC,IPHI(2),IWAFF(2),JPHI
      INTEGER ILL
C
      REAL PHI0,DPHI
      REAL RFAC(2),XYZ1(3),RFID,VPHIT
      REAL FULLU,FULLW,FULUW(3)
      INTEGER IRET,NFAC(2)
      LOGICAL FIRST
C
      INTEGER VNRFAC,VXYZFC,VWFFND,VFADIM,VJFACI
      REAL VDOTN
C
      PARAMETER (RFID = 1.0)
      REAL PHIOV(15,2)
      INTEGER IFOV(15,2,2)
      REAL XYZF(3,2),XOVL(2,2,15)
      INTEGER IOVL, I, KFAC, IWFF
      REAL CMAX, DIST
      DATA FIRST /.TRUE./
      SAVE RFAC,PHI0,DPHI,NFAC,FULLU,FULLW
      IF(FIRST)THEN
         FIRST=.FALSE.
         DO ILL=1,2
           JFAC=1+(ILL-1)*VNRFAC(1)
           IRET = VXYZFC(JFAC,XYZ1)
           RFAC(ILL) = SQRT(XYZ1(1)**2+XYZ1(2)**2)
C
           NFAC(ILL) = VNRFAC(ILL)
           DO IOVL=1,NFAC(ILL)
             DO I=1,2
               KFAC = IOVL+I-1
               IF(KFAC.GT.NFAC(ILL))KFAC=1
               IFOV(IOVL,I,ILL) = KFAC
               IRET = VJFACI(ILL,KFAC,JFAC)
               IRET = VXYZFC(JFAC,XYZF(1,I))
             ENDDO
             CALL VADD(XYZF(1,1),XYZF(1,2),XOVL(1,ILL,IOVL),2)
           ENDDO
         ENDDO
         IRET = VFADIM(FULUW)
         FULLU = FULUW(2)*.5
         FULLW = FULUW(3)*.5 + ZBUF
      ENDIF
      VDEXCY=1
C
C  Don't check tracks whose impact parameter is larger than the
C  VDET radius minus a small amount (~1 cm) to preclude the case
C  where the cylinder approximation is too crude.
C
      IF(ABS(TPAR(4)).GE.RFAC(ILAY)-RFID)GO TO 999
C
C  Crude extrapolation to a cylinder
C
      CALL VTXRAW (TPAR,RFAC(ILAY),GPOS0,VPHIT,IRET)
C
C  Reject this layer if there's an error, or
C  if abs(z) is outside the face
C
      IF ((IRET .NE. 0) .OR. (ABS(GPOS0(3)) .GT. FULLW)) GO TO 999
C
C  Decide which wafers to extrapolate to.  First find the phi, then
C  the Z. Find the closest two faces VDOTN is the cosine of the angle
C  between the track point and a given overlap
C
      CMAX = 0.
      DO IOVL=1,NFAC(ILAY)
         DIST = VDOTN(GPOS0,XOVL(1,ILAY,IOVL),2)
         IF(DIST.GT.CMAX)THEN
            IPHI(1) = IFOV(IOVL,1,ILAY)
            IPHI(2) = IFOV(IOVL,2,ILAY)
            CMAX = DIST
         ENDIF
      ENDDO
C
C  Determine the wafer (Z).
C
      IWFF = VWFFND(GPOS0(3))
      IWAFF(1) = IWFF
      IWAFF(2) = IWFF
      VDEXCY=0
  999 CONTINUE
      RETURN
      END
#endif
