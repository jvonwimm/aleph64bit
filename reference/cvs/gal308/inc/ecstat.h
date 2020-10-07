*CD ecstat
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)
#if defined(DOC)
   ALL$ Statistics for ECAL
      NECONT
          1  Total number of events which deposit energy in ECAL
          2  Average number of GEANT  track elements per event
          3  Average number of Shower track elements per event
          4  Average number of fired towers in Ecal   per event
          5  Total number of track elem. out of planes geometrical def.
          6  Total number of track elem. out of columns    "        "
          7  Total number of track elem. out of rows       "        "

      ECCONT
          1  Deposited energy for current event in stack 1
          2        "                "              stack 2
          3        "                "              stack 3
          4  <energy>                          for stack 1
          5        "                "          for stack 2
          6        "                "          for stack 3
          7  <e>               stack 1+2+3
          8  <e**2>                 "
          9  Delta = sqrt((<e**2> - <e>**2)/<e>)  (Gev)
#endif
