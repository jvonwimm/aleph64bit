C! VDET JULIA geometry constants
      INTEGER JVDL, JGEO, JSLOM, JSLOI, JWAFN
      PARAMETER (JVDL=2,JGEO=2,JSLOM=15,JSLOI=12,JWAFN=4)
      INTEGER NPSLLY,JSLOGM
      REAL VJRIOL,VJBIGZ,VJPHIZ,VJFPHI,VJUACT,VJWACT,VJZOFF
      COMMON /VDJGEO/  NPSLLY(JVDL),JSLOGM(JSLOM,JVDL),
     &                 VJRIOL(JVDL),VJBIGZ,VJPHIZ(JVDL),VJFPHI(JVDL),
     &                 VJWACT(JGEO),VJUACT(JGEO),VJZOFF(JWAFN,JGEO)
#if defined(DOC)
      JVDL       : number of VDET layers L=1,JVDL
      JGEO       : number of geometries (Pisa and Munich) J=1,JGEO
      JSLOM      : mximum number of slots in a layer N=1,JSLOM
      JWAFN      : number of wafers in a face I=1,JWAFN
      NPSLLY(L)  : number of slots along phi in layer L
      JSLOGM(N,L): geometry type of slot N in layer L
      VJRIOL(L)  : radius of first face of layer L
      VJBIGZ     : oversized length in zed
      VJPHIZ(L)  : phi angle sustended by a face in layer L
      VJFPHI(L)  : phi at the center of the 1st wafer of layer L
      VJWACT(J)  : active area on the phi side for wafer geometry J
      VJUACT(J)  : active area on the zed side for wafer geometry J
      VJZOFF(I,J): Z offset of wafer I in geometry J
#endif
