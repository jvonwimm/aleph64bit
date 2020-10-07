      PARAMETER (MHASA=4)
      COMMON /SALIGC/XSHISA(MHASA),YSHISA(MHASA),ZSHISA(MHASA),
     +               YZANSA(MHASA),ZXANSA(MHASA),XYANSA(MHASA)
      REAL XSHISA,YSHISA,ZSHISA,YZANSA,ZXANSA,XYANSA
#if defined(DOC)
C
C!         SATR alignment constants
C
C MHASA    : number of SATR half modules
C XSHISAi  : translation of half module i in x-direction
C YSHISAi  : translation of half module i in y-direction
C ZSHISAi  : translation of half module i in z-direction
C YZANSAi  : angle of half module i between ideal and real yz-plane
C ZXANSAi  : angle of half module i between ideal and real zx-plane
C XYANSAi  : angle of half module i between ideal and real xy-plane
#endif
