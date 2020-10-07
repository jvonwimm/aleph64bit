C!   VDET Unconnected, extra channels; Face-module content
C ------------------------------------------------------------
      INTEGER VUECH, VEXCH, VIGBM
      INTEGER MAXFACE
      PARAMETER(MAXFACE=40)
      CHARACTER*4 FACEC
      INTEGER FACEN,MODNEG,MODPOS
c
      COMMON/VDUEFC/VUECH(2),VEXCH(2),VIGBM,
     >      FACEN(MAXFACE),FACEC(MAXFACE),
     >      MODNEG(MAXFACE),MODPOS(MAXFACE)
#if defined(DOC)
*      VUECH(2) unconnected channels for z, rphi
*             normally VUECH = 32,2
*      VEXCH(2)  extra channels at start of readout
*             normally VEXCH = 1,1
*      VIGBM  ignore bond map flag
*      FACEN(MAXFACE)    face serial number
*      FACEC(MAXFACE)    face character name
*      MODNEG(MAXFACE)   serial number of module for z<0 (B side)
*      MODPOS(MAXFACE)   serial number of module for z>0 (A side)
#endif
