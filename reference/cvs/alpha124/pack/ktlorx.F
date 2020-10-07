      FUNCTION KTLORX (ITK,ICL,IFL,ITKRST)
CKEY KINEMATICS /INTERNAL
C----------------------------------------------------------------------
C! Boost ITK into the rest frame of ITKRST.
C                                                    H.Albrecht  2.1.89
C Mod. I.Tomalin 9.5.97 : Allows boost of tracks with zero attributes.
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DATA IERR /0/
      JR(J) = KOQVEC + KTLORX  * KCQVEC + J
      J1(J) = KOQVEC + ITKRST * KCQVEC + J
      J2(J) = KOQVEC + ITK    * KCQVEC + J
C
      IF (ITK .LE. KLUST)  GO TO 30
      IPC = IW(J2(JQVEPA))
      IFR = IW(J2(JQVECL))
C
C       new track :
C
   30 IFL = 0
      IF (KTDROP .NE. 0)  THEN
        KTLORX = KTDROP
        KTDROP = IW(JR(JQVENP))
      ELSE
        KTLORX = KFFRT
        KFFRT = KFFRT + 1
        IF (KFFRT .GE. KLFRT)  CALL QSBANK ('QVEC', KFFRT+200)
      ENDIF
C
C       mass > 0 ?
C
      IF (RW(J1(JQVEQM)) .LT. 1.E-9)  GO TO 60
C
C       basic attributes :
C
      RW(JR(JQVEQM)) = RW(J2(JQVEQM))
      RW(JR(JQVECH)) = RW(J2(JQVECH))
      P1P2 = RW(J1(JQVEQX)) * RW(J2(JQVEQX)) +
     &       RW(J1(JQVEQY)) * RW(J2(JQVEQY)) +
     &       RW(J1(JQVEQZ)) * RW(J2(JQVEQZ))
      DELB2 = (RW(J1(JQVEQP))/RW(J1(JQVEQE)))**2 + 
     &        (RW(J2(JQVEQP))/RW(J2(JQVEQE)))**2 -
     &         2.0*P1P2/(RW(J1(JQVEQE))*RW(J2(JQVEQE)))
C
      IF (DELB2.GT.1.0E-6) THEN
        RW(JR(JQVEQE)) = (RW(J1(JQVEQE)) * RW(J2(JQVEQE)) - P1P2) /
     &    RW(J1(JQVEQM))
        FAC = - (RW(JR(JQVEQE)) + RW(J2(JQVEQE))) /
     &          (RW(J1(JQVEQE)) + RW(J1(JQVEQM)))
        RW(JR(JQVEQX)) = RW(J2(JQVEQX)) + FAC * RW(J1(JQVEQX))
        RW(JR(JQVEQY)) = RW(J2(JQVEQY)) + FAC * RW(J1(JQVEQY))
        RW(JR(JQVEQZ)) = RW(J2(JQVEQZ)) + FAC * RW(J1(JQVEQZ))
        RW(JR(JQVEQP)) = SQRT (RW(JR(JQVEQX))**2 + RW(JR(JQVEQY))**2 +
     &    RW(JR(JQVEQZ))**2)
      ELSE
        RW(JR(JQVEQX)) = RW(J2(JQVEQX)) * 1.E-9
        RW(JR(JQVEQY)) = RW(J2(JQVEQY)) * 1.E-9
        RW(JR(JQVEQZ)) = RW(J2(JQVEQZ)) * 1.E-9
        RW(JR(JQVEQP)) = RW(J2(JQVEQP)) * 1.E-9
        RW(JR(JQVEQE)) = RW(J2(JQVEQM))
      ENDIF
C
      IF (ITK .LE. KLUST)  GO TO 90
C
C       all boosted tracks from the same class ?
C
      IF (KCLAFR(ICL) .EQ. 0)  THEN
        KCLAFR(ICL) = IFR
        IF (IFR .GT. 0) KCLARM(ICL) = KCLARM(IFR)
      ELSE
        IF (IFR .NE. KCLAFR(ICL))  CALL QMTERM
     &      ('_KTLORX_ Particles from different classes mixed up')
      ENDIF
C
C       particle code, class :
C
      IW(JR(JQVECL)) = ICL
      IW(JR(JQVEPA)) = IPC
      IF (IPC .NE. 0)  THEN
        IFP = IW(KOQPLI+IPC)
        IF (IFP .EQ. 0)  IFP = KFPADR (IPC)
        IW(JR(JQVENP)) =
     &      IW(KOQFPA+IFP*KCQFPA+ICL)
        IW(KOQFPA+IFP*KCQFPA+ICL) = KTLORX
      ELSE
        IW(JR(JQVENP)) = 0
      ENDIF
C
C       KSAME pointer :
C
      IW(JR(JQVESP)) = IW(J2(JQVESP))
      IW(J2(JQVESP)) = KTLORX
C
C       daughter - mother - daughter relation; match :
C
      IW(JR(JQVEND)) = 0
      IW(JR(JQVENO)) = 0
      IW(JR(JQVENM)) = IW(J2(JQVENM))
      IW(JR(JQVEML)) = IW(J2(JQVEML))
C
C        bit masks :
C
      DO 9 IB=1,KLOCKM
        IW(JR(JQVEBM+IB-1)) = IW(J2(JQVEBM+IB-1))
9     CONTINUE
      IW(JR(JQVELK)) = IW(J2(JQVELK))
C
C       vertices :
C
      IW(JR(JQVESC)) = IW(J2(JQVESC))
      IW(JR(JQVEOV)) = IW(J2(JQVEOV))
      IW(JR(JQVEEV)) = IW(J2(JQVEEV))
      RW(JR(JQVEDB)) = RW(J2(JQVEDB))
      RW(JR(JQVEZB)) = RW(J2(JQVEZB))
      RW(JR(JQVESD)) = RW(J2(JQVESD))
      RW(JR(JQVESZ)) = RW(J2(JQVESZ))
      RW(JR(JQVECB)) = RW(J2(JQVECB))
C
C       other pointers etc. :
C
      IW(JR(JQVEKS)) = IW(J2(JQVEKS))
      IW(JR(JQVETN)) = IW(J2(JQVETN))
      IW(JR(JQVEQD)) = IW(J2(JQVEQD))
C
C       error matrix : not yet implemented.
C
      RW(JR(JQVEEM)) = -1.
      RW(JR(JQVECF)) = RW(J2(JQVECF))
      GO TO 90
C
C       Mass = 0 :
C
   60 IERR = IERR + 1
      IF (IERR .LE. 10)  CALL QWMESE
     &  ('_QVLORX_ Lorentz boost : beta close to 1')
      CALL QVZERO (KTLORX)
      KTLORX = 0
C
   90 END
#endif
