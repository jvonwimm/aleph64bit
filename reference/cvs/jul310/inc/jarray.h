C!  Common used in Jet algorithm
      PARAMETER (NCMAX=3000)
      COMMON / JARRAY / NINDMX,XARRAY(4,NCMAX),IARRAY(3,NCMAX)
#if defined(DOC)
C-    NINDMX  number of elements in XARRAY
C-    XARRAY(I,K) for each K element I=1,4 : px ,py ,pz ,E
C-    IARRAY(I,K)                    I=1 element type
C-                                   I=2 element range in jet algorithm
#endif
