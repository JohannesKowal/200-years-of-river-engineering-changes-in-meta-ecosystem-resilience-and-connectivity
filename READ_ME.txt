~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~

LINKS_XXXX.csv directed links (connections) between nodes in the habitat network corresponding to the year XXXX

  IN_FID           source node identifier (same as the identification of the corresponding reach (spatial line feature))
  NEAR_FID         target node identifier (same as the identification of the corresponding reach (spatial line feature))
  TYPE             link type ("SL" = sluice; "DAM" = dam; "FP" = fish pass; "LINK" = confluences or upstream/downstream link between reaches)
  pass_u	   upstream passability (if TYPE = "LINK" then pass_u = 1)
  pass_d           downstream passability (if TYPE = "LINK" then pass_d = 1)

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~


NODES_XXXX.shp: individual reaches/nodes (spatial line features) of the habitat network corresponding to the year XXXX
  
  FID              reach/node identifier
  WB_TYPE          reach type according to Amoros et al. 1987 (later adapted by Hohensinner et al. 2011)
  LENGTH           reach length [m]
  IMP	           indicates whether a reach is impounded (IMP = 1) or not (IMP = 0)
  SECTION	   indicates whether a reach is part of the "Old Danube" side channel (SECTION = "OD") or not (SECTION = "GENERAL")

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~


SAMPLES.shp: presence/absence data set for taxa (where available) used for the definition of habitat suitability clusters (SCs)

  ID_SMPL           observation identifier 
  GROUP             functional group ("FI" = flying macroinvertebrate; "NFI" = non-flying macroinvertebrate; "PLANT" = plant; "FISH" = fish; "AMPG" = amphibians)
  DISP		    dispersal mode ("MDM" = medium distance migrator; "SDM" = short distance migrator; "PD" = passive disperser; "EUC" = dispersing more independently of the network structure)
  GENUS		    taxonomic genus
  SPECIES	    taxonomic species
  PRES		    indicates whether a taxon was present (1) or absent (0) at a site during the time of sampling
  YEAR		    year of sampling

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~


CONVERSION.csv: translation of reach types according to Chovanec et al. 2005 into reach types according to Amoros et al. 1987 (later adapted by Hohensinner et al. 2011)
 
  AMOROS           reach type according to Amoros et al. 1987 (later adapted by Hohensinner et al. 2011)
  FHI	           reach types according to Chovanec et al. 2005 (floodplain habitat index)
  IMP	           indicates whether a reach is impounded (IMP = 1) or not (IMP = 0)

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~

SC_TAXA.csv: habitat suitability of reach types (Chovanec et al. 2005) for different taxa

  ID_TAXA          taxon identifier
  H1		   habitat suitability (0 = low; 10 = high) for H1 reaches
  H2               habitat suitability (0 = low; 10 = high) for H2 reaches
  H3               habitat suitability (0 = low; 10 = high) for H3 reaches
  H4               habitat suitability (0 = low; 10 = high) for H4 reaches
  H5               habitat suitability (0 = low; 10 = high) for H5 reaches
  GENUS		   taxonomic genus
  SPECIES	   taxonomic species
  GROUP            functional group ("FI" = flying macroinvertebrate; "NFI" = non-flying macroinvertebrate; "PLANT" = plant; "FISH" = fish; "AMPG" = amphibians)
  DISP		   dispersal mode ("MDM" = medium distance migrator; "SDM" = short distance migrator; "PD" = passive disperser; "EUC" = dispersing more independently of the network structure)

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~

WIDTHS.csv: average widths of the riverbed for different reach types

  YEAR             the year for which average widths were calculated
  WB_TYPE          reach type according to Amoros et al. 1987 (later adapted by Hohensinner et al. 2011)
  IMP	           indicates whether average widths were calculated for impounded (IMP = 1) or non-impounded (IMP = 0) reaches
  SECTION	   indicates whether average widths were calculated for reaches that are part of the "Old Danube" side channel (SECTION = "OD") or not (SECTION = "GENERAL")
  WIDTH            average width [m]

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~

K_WSS_SC.csv: within sum of squares versus the number of clusters used to define habitat suitability clusters (SCs)

  K                number of clusters
  WSS              within sum of squares

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~

K_WSS_CC.csv: within sum of squares versus the number of clusters used to delineate connectivity clusters (CCs)

  K                number of clusters
  WSS              within sum of squares

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~

CCI_MASTER.csv: catchment connectivity indices (CCI) 

  ID               index identifier
  CCI              NUM divided by DEN; catchment connectivity index (normalized; see Baldan et al. 2022)          
  NUM              catchment connectivity index (unscaled); this version was used in this study to allow for better comparison between years
  DEN              denominator used for normalization of the catchment connectivity index
  YEAR             the year for which the CCI was calculated
  CONF_ID          CCI configuration identifier
  DISP             dispersal mode of the CCI configuration
  SC               habitat suitability cluster of the CCI configuration

~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~

RCI_MASTER.shp: reach connectivity indices (RCI)

  ID               index identifier
  RCI              NUM divided by DEN; reach connectivity index (normalized; see Baldan et al. 2022)
  NUM              reach connectivity index (unscaled); this version was used in this study to allow for better comparison between years
  DEN              denominator used for normalization of the reach connectivity index
  DEN_G            denominator relevant for the normalization of the reach connectivity index of each connected sub-network (graph)
  FID              reach/node identifier
  ID_G             connected sub-network (graph) identifier within networks corresponding to individual years
  YEAR             the year for which the RCI was calculated
  CONF_ID          RCI configuration identifier
  DISP             dispersal mode of the RCI configuration
  SC               habitat suitability cluster of the RCI configuration
  CC               connectivity cluster (CC)
  WEIGHT           reach/node weight
  WB_TYPE          reach type according to Amoros et al. 1987 (later adapted by Hohensinner et al. 2011)
  
~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~~ ><{{{°> ~~~~~ ><{{{°> ~~~~~
