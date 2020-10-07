C! keep content of RUNCARTS.LIST file
C! keep content of RUNCARTS.LIST file
      PARAMETER(MXLRUN=2500,MXSEG=6,MXTYP=5,LK7=9)
      INTEGER K7LRUN
C - 22500=MXLRUN*LK7 (9 characters per K7 for MXLRUN runs)
      CHARACTER*22500 K7CART
C - 5=MXTYP the number of various data types ('RPDMN')
      CHARACTER*5 K7TYPE
      COMMON/ALK7COM/ K7SEG,K7LINE(MXSEG),K7LRUN(MXLRUN,MXSEG),
     &                K7CART(MXTYP,MXSEG),K7TYPE
#if defined(DOC)
      MXLRUN          maximum number of runs in a segment
      MXSEG           maximum number of segments
      MXTYPE          maximum number of data types
      K7SEG           number of segments used by RUNCARTS.LIST
      K7LINE (is)     number of runs in  segment # is
      K7LRUN (il,is)  run number at line # il in segment # is
      K7CART (it,is)  list of cartridges for data type # it in
                      segment # is
      K7TYPE          list of data types 'RPDMN'
#endif
