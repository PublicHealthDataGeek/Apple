#############################################################################
# Obtaining data from OS MasterMap Highways Network
#
# This code imports the relevent layers from the MasterMap dataset.
# The data is limited to London using ONS dataset.
# XYZ data is converted to XY
#
# - road nodes that reprsent junctions are identified
#  ???????
# aggregated data is saved to github but other data is not


library(tidyverse)
library(sf)
library(mapview)

# set mapview options so that matches crs
mapviewOptions(native.crs = TRUE, legend = FALSE)



# Obtain list of layers within mastermap
st_layers("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb")

# Available layers:
#                                           layer_name        geometry_type features fields
# 1                        AccessRestriction_NetworkRef                   NA    44104      4
# 2                                   AccessRestriction                Point    44104      6
# 3                         AccessRestriction_Inclusion                   NA    44151      3
# 4                         AccessRestriction_Exemption                   NA    39868      3
# 5                                        TQ_NamedDate                   NA     1206      3
# 6                                         TQ_NamedDay                   NA     1681      4
# 7                                        TQ_TimeRange                   NA     1092      6
# 8                                    TQ_Day_Time_Join                   NA     1170      4
# 9                                    TQ_Date_Day_Join                   NA     1326      3
# 10                                TQ_Parent_Date_Join                   NA     1230      2
# 11                                     TQ_NamedPeriod                   NA      155      4
# 12                                       TQ_NamedTime                   NA       78      5
# 13                                       TQ_DateRange                   NA       24      6
# 14                                     ConnectingLink 3D Multi Line String  1434725      8
# 15                                     ConnectingNode             3D Point  1329440      4
# 16                                  HighwayDedication    Multi Line String  3695856     13
# 17                                  Network_Reference                   NA  9453809      2
# 18                                          FerryLink 3D Multi Line String      130     11
# 19                                          FerryNode             3D Point      215      6
# 20                                      FerryTerminal                   NA      215     13
# 21                                  Hazard_NetworkRef                   NA    27319      6
# 22                                             Hazard                   NA    15898      6
# 23                 Hazard_NodeReference_LinkReference                   NA       27      2
# 24                                 Maintenance_noGeom                   NA  1200714     13
# 25                                  Maintenance_point          Multi Point    46893     13
# 26                                   Maintenance_line    Multi Line String    15957     14
# 27                                   Maintenance_area        Multi Polygon      143     15
# 28                                               Path                   NA    25913      9
# 29                                  Path_Ref_PathLink                   NA   102668      2
# 30                        PathLnk_AlternateIdentifier 3D Multi Line String   460003      4
# 31                               PathLink_FormsPartOf                   NA   624030      3
# 32                                           PathLink 3D Multi Line String  2293382     24
# 33                                           PathNode             3D Point  2690842      7
# 34                               Reinstatement_noGeom                   NA  1316803      9
# 35                                Reinstatement_point          Multi Point   133933      9
# 36                                 Reinstatement_line    Multi Line String    17258     10
# 37                                 Reinstatement_area        Multi Polygon      190     11
# 38                             RestrictionForVehicles                Point    11620     15
# 39                  RestrictionForVehicles_NetworkRef                   NA    11620      5
# 40 RestrictionForVehicles_NodeReference_LinkReference                   NA     1138      2
# 41                                    Road_NetworkRef                   NA  4031757      2
# 42                                               Road                   NA   810082     20
# 43                                       roadJunction                   NA     2997      9
# 44                                   roadJunctionNode                   NA    17375      2
# 45                               RoadLink_TopoTOIDRef                   NA 11480739      2
# 46                               RoadLink_FormsPartOf                   NA  9128544      3
# 47                                           RoadLink 3D Multi Line String  5062741     38
# 48                       RoadLink_AlternateIdentifier                   NA  3966048      3
# 49                                           RoadNode             3D Point  4289045     14
# 50                           RoadNode_RelatedRoadArea                   NA  4308448      2
# 51                            SpecialDesignation_line    Multi Line String    41643     12
# 52                          SpecialDesignation_noGeom                   NA   270694     11
# 53                           SpecialDesignation_point          Multi Point   246467     11
# 54                            SpecialDesignation_area        Multi Polygon     2320     13
# 55                                  Street_NetworkRef                   NA  5816045      2
# 56                                             Street    Multi Line String  1410593     47
# 57                                          Structure                Point   699706      6
# 58                               Structure_NetworkRef                   NA   699706      5
# 59              Structure_NodeReference_LinkReference                   NA    11980      2
# 60                         TurnRestriction_NetworkRef                   NA  1051619      4
# 61                                    TurnRestriction                   NA   832430      5
# 62                          TurnRestriction_Exemption                   NA     1587      3
# 63                          TurnRestriction_inclusion                   NA       27      3



#########################
# Import road_link data #
#########################


# NB LOOKING AT MAPS WILL NEED TO LIMIT NODES TO ONES THAT ARE JUNCTIONS _ ALSO 
# ? also need to limits roads to something tooo????


os_road_link = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                layer = "RoadLink")
# Simple feature collection with 5062741 features and 38 fields
# geometry type:  MULTILINESTRING
# dimension:      XYZ
# bbox:           xmin: 9123 ymin: 7757.525 xmax: 655563.6 ymax: 1216649
# z_range:        zmin: -21.1 zmax: 1090.7
# projected CRS:  OSGB 1936 / British National Grid

# Identify variables
names(os_road_link)
# [1] "TOID"                           "identifier"                    
# [3] "identifierVersionId"            "beginLifespanVersion"          
# [5] "fictitious"                     "validFrom"                     
# [7] "reasonForChange"                "roadClassification"            
# [9] "routeHierarchy"                 "formOfWay"                     
# [11] "trunkRoad"                      "primaryRoute"                  
# [13] "roadClassificationNumber"       "roadName1"                     
# [15] "roadName2"                      "roadName1_Language"            
# [17] "roadName2_Language"             "operationalState"              
# [19] "provenance"                     "directionality"                
# [21] "length"                         "matchStatus"                   
# [23] "alternateIdentifier1"           "alternateIdentifier2"          
# [25] "alternateIdentifier3"           "alternateIdentifier4"          
# [27] "alternateIdentifier5"           "startGradeSeparation"          
# [29] "endGradeSeparation"             "roadStructure"                 
# [31] "cycleFacility"                  "roadWidthMinimum"              
# [33] "roadWidthAverage"               "elevationGainInDirection"      
# [35] "elevationGainOppositeDirection" "startNode"                     
# [37] "endNode"                        "SHAPE_Length"                  
# [39] "SHAPE"                  


str(os_road_link)


# Need to limit to Greater London area

# import May 2020 ONS LA boundary data (required for NA management)
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")


# Spatially join road link data to London Boroughs using inner join 
# (ie only keep observations that are in both)
lon_os_road_link = st_join(os_road_link, lon_lad_2020, left = FALSE)

# Get rid of Z aspect in geometry (keep just X&Y)
lon_os_road_link = st_zm(lon_os_road_link, drop = T, what = 'ZM')

# Save road link data - not on github
#saveRDS(lon_os_road_link, file = "/home/bananafan/Documents/PhD/Datasets/lon_os_road_link")

# Examine London road dataset
# lon_os_road_link = readRDS(file = "/home/bananafan/Documents/PhD/Datasets/lon_os_road_link")
str(lon_os_road_link) # 292652 obs. of  49 variables

# limit to city of London
city = lon_os_road_link %>%
  filter(BOROUGH == "City of London") # n = 1443

mapview(city)
tmap::qtm(city)
tmap::qtm(lon_os_road_link)



####################################
# Import road_node data            #
# - aim to find junction locations #
####################################

# Read in road node data
os_road_node = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                       layer = "RoadNode")

# Reading layer `RoadNode' from data source `/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb' using driver `OpenFileGDB'
# Simple feature collection with 4289045 features and 14 fields
# geometry type:  POINT
# dimension:      XYZ
# bbox:           xmin: 9242 ymin: 7757.525 xmax: 655563.6 ymax: 1216649
# z_range:        zmin: -21.1 zmax: 1076.1
# projected CRS:  OSGB 1936 / British National Grid

# Identify variables
names(os_road_node)
# [1] "TOID"                 "identifier"          
# [3] "beginLifespanVersion" "validFrom"           
# [5] "formOfRoadNode"       "classification"      
# [7] "access"               "junctionNumber1"     
# [9] "junctionNumber2"      "junctionName1_text"  
# [11] "junctionName1_lang"   "junctionName2_text"  
# [13] "junctionName2_lang"   "reasonForChange"     
# [15] "SHAPE" 

# Obtain London data
# 1) Spatially join road link data to London Boroughs using inner join 
# (ie only keep observations that are in both)
lon_os_road_node = st_join(os_road_node, lon_lad_2020, left = FALSE)

# 2) Get rid of Z aspect in geometry (keep just X&Y)
lon_os_road_node = st_zm(lon_os_road_node, drop = T, what = 'ZM')

str(lon_os_road_node) # 231304 obs. of  25 variables

# 3) Save london road nodes dataset - not on github
saveRDS(lon_os_road_node, file = "/home/bananafan/Documents/PhD/Datasets/lon_os_road_node")

# Data wrangle node dataset to figure out how to use it
tmap::qtm(lon_os_road_node)

unique(os_road_node$formOfRoadNode)  # identify unique types of RoadNodes
# [1] "roadEnd"             "junction"            "pseudoNode"          "roundabout"         
# [5] "enclosedTrafficArea"

# formofRoadNode codes: 
# Enclosed traffic area = The road node is situated inside and/or represents an enclosed traffic area.A traffic area isan area with no internal structure of legally defined driving directions. At least two roads are connected to the area.
# Junction = Three or more road links intersect at the road node.
# pseudo node = Exactly two road links connect to the road node.
# road end = Only one road link connects to the road node. It signifies the end of a road.
# roundabout = The road node represents or is a part of a roundabout.

# enclosed traffic areas are things like parking lots.  

# wanting just Junctions as denominator for ASLs and signals. 
# ASLs are found at junctions/intersections - see TAL 8/93

# Create dataset of junctions
lon_os_road_nodes_junctions = lon_os_road_node %>%
  filter(formOfRoadNode == "junction")

# Count number of junctions by Borough
lon_os_junctions_borough_count = lon_os_road_nodes_junctions %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# Save junction counts - saved on github as aggregated data
saveRDS(lon_os_junctions_borough_count, 
        file = "/home/bananafan/Documents/PhD/Paper1/data/lon_os_mm_junction_borough_counts")

# OLD CODE: Code checking that junction is the right value I want - yes it is
  # # limit to city of London to get smaller dataset
  # city_n = lon_os_road_node %>%
  #   filter(BOROUGH == "City of London") # n = 1001
  # mapview(city_n)
  # tmap::qtm(city_n)
  # 
  # city_j = city_n %>%
  #   filter(formOfRoadNode == "junction")
  # city_p = city_n %>%
  #   filter(formOfRoadNode == "pseudoNode")
  # mapview(city_p$SHAPE, color = "red", cex = 1) + mapview(city_j$SHAPE, color = "green", cex = 1)



###############################################################
# Import structures data                                      #
# - to see if have details of traffic lights - but it doesnt! #
###############################################################

os_structures = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                       layer = "Structure")
# Simple feature collection with 699706 features and 6 fields (with 290361 geometries empty)
# geometry type:  POINT
# dimension:      XY
# bbox:           xmin: 63153.38 ymin: 8099.057 xmax: 655472.2 ymax: 1215182
# projected CRS:  OSGB 1936 / British National Grid

names(os_structures)
# [1] "TOID"                 "localId"              "beginLifespanVersion" "structure"           
# [5] "description"          "reasonForChange"      "SHAPE"  


# Spatially join road link data to London Boroughs using inner join 
# (ie only keep observations that are in both)
lon_os_structures = st_join(os_structures, lon_lad_2020, left = FALSE)

str(lon_os_structures) # 231304 obs. of  25 variables

 # Identify variables
unique(lon_os_structures$structure)
# [1] "Bridge Over Road"               "Gate"                          
# [3] "Rising Bollards"                "Moveable Barrier"              
# [5] "Level Crossing Unbarriered"     "Level Crossing Fully Barriered"
# [7] "Toll Indicator"                 "Level Crossing Part Barriered" 
# [9] "Structure"                      "Bridge Under Road" 