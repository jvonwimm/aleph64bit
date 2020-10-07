C!    Common for VSLT data: VDET slots
C ----------------------------------------------------------------------
      INTEGER NSLOTS, JJLAYF, ISSFLG
      REAL PHIOFF
C
      COMMON / VSLTCO / NSLOTS, JJLAYF(NVFMAX), PHIOFF(NVFMAX),
     >                  ISSFLG(NVFMAX)
C
#if defined(DOC)
      NSLOTS        Total number of slots in VDET
      JJLAYF(JFAC)  Layer number for slot/face JFAC
      PHIOFF(JFAC)  phi (rad) of face normal (outward from z axis)
      ISSFLG(JFAC)  Face serial number for slot JFAC (=0 if empty)
#endif
