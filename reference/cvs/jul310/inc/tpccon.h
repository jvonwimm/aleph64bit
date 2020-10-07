      COMMON /TPCCON/ TVDRFT(2),BFIELD,BCFGEV,TPRWDT,TSIGMD,TOMTAU,
     1                TPRFN2(LTPDRO),
     2                JPEDST(LTSECT,2),TVOFFS(LTSECT),
     3                TXDRFT(2),TYDRFT(2),TVOFS0(LTSECT),TPCCLK
#if defined(DOC)
C
C!      Constants for reconstruction of TPC data
C
C      TVDRFT      = drift velocity in buckets/ns for end A and
C                    end B (z component only)
C      TXDRFT      = drift velocity component in x direction
C      TYDRFT      = drift velocity component in y direction
C      TVOFFS      = constant offset for conversion from buckets to
C                    drift length:
C                        drift length= (buckets-TVOFFS) * TVDRFT
C      BFIELD      = magnetic field (in KGauss)
C      BCFGEV      = magnetic field  (in GeV )
C      TPRWDT      = expected pad reponse width for zero drift
C      TSIGMD      = z dependence of pad response width
C      TOMTAU      = omega*tau/B
C      TPRFN2(irow)= pad response width from 3-pad cluster
C      JPEDST(is,j)= TPD pedestals per sector for pads and wires
C--------------------------------------------------------------------
#endif
