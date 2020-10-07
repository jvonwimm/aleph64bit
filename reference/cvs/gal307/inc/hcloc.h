*CD hcloc
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
#if defined(DOC)
C! Local Variables for HCAL
        HCPDIP Theta shower angle (in the shower system)
        HCPPHI Phi shower angle (in the shower system)
        HCPACX director cosine ( X axis ,shower system)
        HCPACY director cosine ( Y axis ,shower system)
        HCPACZ director cosine ( Z axis ,shower system)
        HCPAX0 starting X-position of shower
        HCPAY0 starting Y-position of shower
        HCPAZ0 starting Z-position of shower
        FHADRC  if .TRUE. is an hadronic particle shower
        FHCPRJ  if .TRUE. spot projection on active plane has been found
        Geo. quantities related to the track element spatial position in
        IHCPOR portion # for current track element (or spot)
        IHCMOD module # for current track element (or spot)
        IHCIPL plane # for current track element (or spot)
#endif
