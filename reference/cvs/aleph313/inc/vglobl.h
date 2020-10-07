C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
      INTEGER VDERR, VDOK
      PARAMETER (VDERR = -1)
      PARAMETER (VDOK  = 1)
C
C     Labels for views:
C
      INTEGER VVIEWZ, VVIEWP
      PARAMETER (VVIEWZ = 1)
      PARAMETER (VVIEWP = 2)
C
C     Fixed VDET geometry parameters:
C
      INTEGER NVLAYR, NVMODF, NVVIEW, NPROMM, IROMAX
      PARAMETER (NVLAYR = 2)
      PARAMETER (NVMODF = 2)
      PARAMETER (NVVIEW = 2)
      PARAMETER (NPROMM = 1)
      PARAMETER (IROMAX = 4)
C
C     Array dimensions:
C
      INTEGER NVWMMX, NVWFMX, NVFLMX, NVFMAX, NVMMAX, NVWMAX
      INTEGER NVZRMX, NVPRMX
      PARAMETER (NVWMMX = 3)
      PARAMETER (NVWFMX = NVWMMX*NVMODF)
      PARAMETER (NVFLMX = 15)
      PARAMETER (NVFMAX = 24)
      PARAMETER (NVMMAX = NVFMAX*NVMODF)
      PARAMETER (NVWMAX = NVFMAX*NVWFMX)
      PARAMETER (NVZRMX = NVFMAX*IROMAX)
      PARAMETER (NVPRMX = NVMMAX*NPROMM)
C
#if defined(DOC)
      VDERR       Error return code
      VDOK        Normal return code

      VVIEWZ      Array index for z view
      VVIEWP      Array index for r-phi view

      NVLAYR      Number of VDET layers
      NVMODF      Number of modules per face
      NVVIEW      Number of views
      NPROMM      Number of readout modules per module in r-phi view
      IROMAX      Maximum number of readout modules per face (z)

      NVWMMX      Maximum number of wafers per module
      NVWFMX      Maximum number of wafers per face
      NVFLMX      Maximum number of faces per layer
      NVFMAX      Maximum number of faces
      NVMMAX      Maximum number of modules
      NVWMAX      Maximum number of wafers
      NVZRMX      Maximum number of readout modules, z view
      NVPRMX      Maximum number of readout modules, r-phi view
#endif
