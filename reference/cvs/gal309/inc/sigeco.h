*CD sigeco
      COMMON/SIGACO/NMODSI,NRBNSI,NPBNSI,NZBNSI,Z0SNSI(2),
     $              ZWIDSI,ZWFRSI,ZWFLSI,ZWLASI,ZWRFSI,ZWRLSI,OVLPSI
#if defined(DOC)
C    NMODSI   Number of Modules
C    NRBNSI   Number of radial bins per module
C    NPBNSI   Number of Phi bins per module
C    NZBNSI   Number of z layers per module
C    Z0SNSI(2)absolute z position of first active plane
C    ZWIDSI   width between two successive z planes
C    ZWFRSI   width in front of first module
C    ZWFLSI   width in front of first Si layer
C    ZWLASI   width of last module ( #12)
C    ZWRFSI   Number of radiation length before first Si layer
C    ZWRLSI   Number of radiation length per module
C    OVLPSI   overlap between two successive crystals
#endif
