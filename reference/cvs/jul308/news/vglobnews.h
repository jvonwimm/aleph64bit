C! 1st entry in vglob_set
 ! JULIA 305
    VFTALL,VGBRAN,VGBRUT,VGFEVT,VGTCUT,VMCHIP,VMCLLD : fix variable 
             type inconsistencies in function calls, for Linux    (A.Waananen)

 ! JULIA 304
    VFTALL : Tracks identified as originating at a secondary vertex 
             have their errors and energy loss propagated only to the 
             corresponding point.                                  (D.Casper)

 *JULIA 303 correction file no 7
    VFTALL : Do not generate "Fatal" error if no track banks found (D.Casper)

 *JULIA 303 correction file no 3
    VGLINK : Create FXTR if it doesn't exist; primarily to 
             facilitate ALPHA interface                     (D.Casper)

 ! JULIA 303
    New package in JULIA to do VDET global pattern recognition in the context
    of the tracking upgrade. The package is called as an option in JULIA, by
    adding the steering card VGLO.

    IMPORTANT NOTE. This software is currently not officially supported in
    Aleph, since its authors (P.Rensing, J-F.Pusztaszeri) have left and noone
    has been found to take over responsibility.

