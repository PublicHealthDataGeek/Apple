#############################################################################
# Obtaining data from OS MasterMap Highways Network
#
# This code imports the relevent layers from the MasterMap dataset.
# The data is limited to London using ONS dataset.
# XYZ data is converted to XY
#
# - road nodes that represent junctions are identified
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

# 1) Read in road link layer
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


# 2) Limit Road Link data to London Boroughs only

# import May 2020 ONS LA boundary data
lon_lad_2020 = readRDS(file = "./map_data/lon_LAD_boundaries_May_2020_BFE.Rds")

# Create spatial object for all 33 boroughs
london_union = st_union(lon_lad_2020) 

# Limit road links to within the Outer London Boundary
lon_os_road_link = st_intersection(os_road_link, london_union)

# Create object with just Borough names and geometry
lon_boroughs = lon_lad_2020 %>%
  select(c("BOROUGH", "geometry"))  

# Spatially join London road link data to London Borough names 
lon_os_road_link = st_join(lon_os_road_link, lon_boroughs)

# Remove Z aspect in geometry (keep just X&Y)
lon_os_road_link = st_zm(lon_os_road_link, drop = T, what = 'ZM')

# Save road link data - not on github
#saveRDS(lon_os_road_link, file = "/home/bananafan/Documents/PhD/Datasets/lon_os_road_link")

# Examine London road dataset
str(lon_os_road_link) # 290664 obs. of  40 variables



unique(lon_os_road_link$roadClassification)
# [1] "A Road"                "Unclassified"          "Classified Unnumbered"
# [4] "Unknown"               "Not Classified"        "B Road"               
# [7] "Motorway"   

# Not Classifed = Roads that have not been assigned a road classification at national or local level by a designation authority.
# Unknown = The classification of the road is unknown because the RoadLink is not a Motorway, A or B road and the RoadLink has not been matched to the National Street Gazetteer.

unique(lon_os_road_link$routeHierarchy)
# [1] "A Road Primary"                   "Minor Road"                      
# [3] "A Road"                           "Local Road"                      
# [5] "Restricted Local Access Road"     "B Road"                          
# [7] "Restricted Secondary Access Road" "Secondary Access Road"           
# [9] "Local Access Road"                "B Road Primary"                  
# [11] "Motorway" 
# Local access road = A road intended for the start or end of a journey, not intended for through traffic but will be openly accessible.
# Restricted local access road = A road intended for the start or end of a journey, not intended for through traffic andwill have a restriction on who can use it
# Secondary Access Road = A road that provides alternate/secondary access to property or land not intended for throughtraffic
# Restricted Secondary Access Road - as above plus restricted

unique(lon_os_road_link$operationalState)
# [1] "Open"               "Under Construction"

unique(lon_os_road_link$formOfWay)
# [1] "Dual Carriageway"                "Single Carriageway"             
# [3] "Traffic Island Link"             "Slip Road"                      
# [5] "Traffic Island Link At Junction" "Roundabout"                     
# [7] "Shared Use Carriageway"          "Enclosed Traffic Area"          
# [9] "Track"                           "Guided Busway"                  
# [11] "Layby"

unique(lon_os_road_link$directionality)
# [1] "inOppositeDirection" "bothDirections"      "inDirection" 

summary(lon_os_road_link$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.01   24.38   46.07   68.55   84.02 3735.77 

# roadWidthMinimum and roadWidthAverage are character in metres

unique(lon_os_road_link$cycleFacility)
# [1] ""                                       "Unknown Type Of Cycle Route Along Road"

###########################
# Data wrangling
#

# 1) Factor appropriate columns
columns2factor = c("roadClassification", "routeHierarchy", "formOfWay", "directionality",
           "cycleFacility", "operationalState")

lon_os_road_link = lon_os_road_link %>%
  mutate_at(columns2factor, as.factor)


# 2) Convert road width into numeric
#??? DO I WANT TO DO THIS
# lon_os_road_link$roadWidthMinimum = as.numeric(lon_os_road_link$roadWidthMinimum)
# above doesnt work, turns them all into NAS
#will have to drop m from cell then convert to numeric



##############
# Examine London data

# a) Road classification
lon_road_class_count = lon_os_road_link %>%
  st_drop_geometry() %>%
  select(roadClassification) %>%
  group_by(roadClassification) %>%
  summarise(count = n())

# roadClassification     count
# * <chr>                  <int>
# 1 A Road                 42486
# 2 B Road                 10683
# 3 Classified Unnumbered  12316
# 4 Motorway                 420
# 5 Not Classified         19376
# 6 Unclassified          148140
# 7 Unknown                57243

# b) Route hierachy
lon_route_hierachy_count = lon_os_road_link %>%
  st_drop_geometry() %>%
  select(routeHierarchy) %>%
  group_by(routeHierarchy) %>%
  summarise(count = n())

# routeHierarchy                    count
# * <fct>                             <int>
# 1 A Road                            31728
# 2 A Road Primary                    10758
# 3 B Road                            10678
# 4 B Road Primary                        5
# 5 Local Access Road                  3761
# 6 Local Road                       145551
# 7 Minor Road                        29395
# 8 Motorway                            420
# 9 Restricted Local Access Road      33678
# 10 Restricted Secondary Access Road  16980
# 11 Secondary Access Road              7710 


motorway_total_length = lon_os_road_link %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  filter(roadClassification == "Motorway") %>%
  select(length) %>%
  summarise(total = sum(length))

motorway_total_length_test = test %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  filter(roadClassification == "Motorway") %>%
  select(length) %>%
  summarise(total = sum(length))

lon_motorways = lon_os_road_link %>%
  filter(roadClassification == "Motorway") 
mapview(lon_motorways, zcol = "identifier") + mapview(lon_lad_2020)

lon_a_roads = lon_os_road_link %>%
  filter(roadClassification == "A Road")
lon_a_road_length = lon_a_roads %>%
  st_drop_geometry() %>%
  group_by(BOROUGH) %>%
  select(length) %>%
  summarise(total = sum(length))

mapview(lon_a_roads, lwd = 0.5) + mapview(lon_lad_2020, alpha.regions = 0.1,lwd = 0.5)
 


# Road Classification:
# Source: https://www.gov.uk/government/publications/guidance-on-road-classification-and-the-primary-route-network/guidance-on-road-classification-and-the-primary-route-network
# A roads – major roads intended to provide large-scale transport links within or between areas
# B roads – roads intended to connect different areas, and to feed traffic between A roads and smaller roads on the network
# classified unnumbered – smaller roads intended to connect together unclassified roads with A and B roads, and often linking a housing estate or a village to the rest of the network. Similar to ‘minor roads’ on an Ordnance Survey map and sometimes known unofficially as C roads
# unclassified – local roads intended for local traffic. The vast majority (60%) of roads in the UK fall within this category

# not classified - Roads that have not been assigned a road classification at national or local level by a designation authority
# Unknown - The classification of the road is unknown because the RoadLink is not a Motorway, A or B road and the RoadLink has not been matched to the National Street Gazetteer. 





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

#############################
# Access restriction layers #
#############################

# 1) AccessRestriction
# location and type of restriction plus vehicle it applies to 

os_access_rest = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                        layer = "AccessRestriction") 

# Reading layer `AccessRestriction' from data source `/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb' using driver `OpenFileGDB'
# Simple feature collection with 44104 features and 6 fields
# geometry type:  POINT
# dimension:      XY
# bbox:           xmin: 70629 ymin: 10495.87 xmax: 655548 ymax: 1215648
# projected CRS:  OSGB 1936 / British National Grid

names(os_access_rest)
# [1] "TOID"                 "localId"              "beginLifespanVersion" "restriction"         
# [5] "reasonForChange"      "trafficSign"          "SHAPE"    

unique(os_access_rest$restriction)
#[1] "forbiddenLegally"     "physicallyImpossible" "private" 

unique(os_access_rest$trafficSign)
# [1] "No goods vehicles over 7.5T"               "Unsuitable for long vehicles"             
# [3] "Unsuitable for heavy goods vehicles"       "No goods vehicles over 17T"               
# [5] "No goods vehicles over 18T"                "Unsuitable for coaches"                   
# [7] "No goods vehicles over 5T"                 "No goods vehicles over 17.5T"             
# [9] "Unsuitable for motor vehicles"             "Unsuitable for towed caravans"            
# [11] "Unsuitable for wide vehicles"              "Unsuitable for large vehicles"            
# [13] "No goods vehicles over 3.5T"               "No goods vehicles over 3T"                
# [15] "No goods vehicles over 16.5T"              "Unsuitable for buses"                     
# [17] "Unsuitable for through traffic"            "Unsuitable for articulated vehicles"      
# [19] "No Entry"                                  "No motor vehicles"                        
# [21] "No vehicles"                               "Buses only"                               
# [23] "No buses"                                  "Pedal cycles and motor cycles only"       
# [25] "Pedal cycles only"                         "No abnormal loads"                        
# [27] "Local buses only"                          "No heavy goods vehicles"                  
# [29] "No light goods vehicles"                   "Buses and taxis only"                     
# [31] "Buses, pedal cycles and taxis only"        "Pedal cycles, local buses and taxis only" 
# [33] "No solo motorcycles"                       "Taxis only"                               
# [35] "No cycling"                                "Buses and pedal cycles only"              
# [37] "Local buses, school buses and taxis only"  "Buses, pedal cycles, motor cycles and tax"
# [39] "No towed caravans"                         "No wide loads"                            
# [41] "School buses only"                         "No motor vehicles except solo motor cycle"
# [43] "No buses, No goods vehicles over 7.5T"     "No vehicles carrying explosives or flamma"
# [45] "No vehicles carrying dangerous goods"      "Non-statutory wide loads only"            
# [47] "Tramcars only"                             "Non-statutory residents and guests only"  
# [49] "No tracked vehicles"                       "Motor cycles only"                        
# [51] "No buses, No heavy goods vehicles"         "Non-statutory no vehicles"                
# [53] "No buses, No light goods vehicles, No sol" "No articulated vehicles"                  
# [55] "No goods vehicles over 33T"                "Buses and tramcars only"                  
# [57] "Local buses and school buses only"  

# 2) AccessRestriction_Inclusion  Types of vehicle or use that the restriction applies to.
os_access_rest_inc = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                         layer = "AccessRestriction_Inclusion") 
# tibble df no geometry
# 44151 obs of 3 varialbes
names(os_access_rest_inc)

names(os_access_rest_inc)
#[1] "TOID"          "inclusion"     "inclusionType"

unique(os_access_rest_inc$inclusion)
# [1] "Goods Vehicles Exceeding 7.5T"  "Long Vehicles"                 
# [3] "Heavy Goods Vehicles"           "Goods Vehicles Exceeding 17T"  
# [5] "Goods Vehicles Exceeding 18T"   "Coaches"                       
# [7] "Goods Vehicles Exceeding 5T"    "Goods vehicles Exceeding 17.5T"
# [9] "Motor Vehicles"                 "Towed Caravans"                
# [11] "Wide Vehicles"                  "Large Vehicles"                
# [13] "Goods Vehicles Exceeding 3.5T"  "Goods Vehicles Exceeding 3T"   
# [15] "Goods vehicles Exceeding 16.5T" "Buses"                         
# [17] "Through Traffic"                "Articulated Vehicles"          
# [19] "All Vehicles"                   "Loading And Unloading"         
# [21] "Abnormal Loads"                 "Light Goods Vehicles"          
# [23] "Motor Cycles"                   "Pedal Cycles"                  
# [25] "Wide Loads"                     "Explosives"                    
# [27] "Inflammables"                   "Dangerous Goods"               
# [29] "Tracked Vehicles"               "Access"                        
# [31] "Goods vehicles Exceeding 33T"   "Authorised Vehicles" 

unique(os_access_rest_inc$inclusionType)
#[1] "vehicle" "use"     "load" 


# 3) AccessRestriction_Exemption- Types of vehicle or use that are exempt from the restriction

os_access_rest_exem = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                              layer = "AccessRestriction_Exemption") 
# tibble

names(os_access_rest_exem)
# [1] "TOID"          "exemption"     "exemptionType"

unique(os_access_rest_exem$exemption)
# [1] "Loading And Unloading"         "Access"                       
# [3] "Permit Holders"                "Local Buses"                  
# [5] "Buses"                         "Authorised Vehicles"          
# [7] "Taxis"                         "Pedal Cycles"                 
# [9] "Motor Cycles"                  "Emergency Vehicles"           
# [11] "School Buses"                  "Goods Vehicles"               
# [13] "Emergency Access"              "Service Vehicles"             
# [15] "Heavy Goods Vehicles"          "Fuel Tankers"                 
# [17] "Disabled"                      "Coaches"                      
# [19] "Abnormal Loads"                "Tramcars"                     
# [21] "Escorted Traffic"              "Guided Buses"                 
# [23] "Wide Loads"                    "Residents And Guests"         
# [25] "Access To Off Street Premises" "Works Traffic"                
# [27] "Articulated Vehicles"          "Public Transport"             
# [29] "Mopeds"      

unique(os_access_rest_exem$exemptionType)
# [1] "use"     "vehicle" "load"   


#############################
# Turn Restriction layers #
#############################
# Turn restrictions are a restriction based upon a vehicle manoeuvre. Thistype of 
# restriction includes prohibitive driving instructions, mandatory driving instruction and implicit restrictions.
# Prohibited instructions are indicated by road signs within a red circle,
# Mandatory driving instructions indicated by road signs within a blue circle

# 1) Turn Restriction
os_turn_rest = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                        layer = "TurnRestriction") 
# tibble - no shape
# 832430 obs. of  5 variables:
  
names(os_turn_rest)
# [1] "TOID"                 "identifier"           "beginLifespanVersion"
# [4] "restriction"          "reasonForChange" 

unique(os_turn_rest$restriction)
#[1] "One Way"        "No Turn"        "Mandatory Turn"

unique(os_turn_rest$beginLifespanVersion)
# NB on 30 dates.... 

unique(os_turn_rest$reasonForChange)
# [1] "New"                 "Modified Attributes"


# 2) TurnRestriction_Exemption 
os_turn_rest_exemp = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                       layer = "TurnRestriction_Exemption") 
#tibble
# 1587 obs. of  3 variables:

names(os_turn_rest_exemp)
# [1] "TOID"          "exemption"     "exemptionType"

unique(os_turn_rest_exemp$exemption)
# [1] "Pedal Cycles"          "Buses"                 "Taxis"                
# [4] "Local Buses"           "Motor Cycles"          "Heavy Goods Vehicles" 
# [7] "Loading And Unloading" "Emergency Vehicles"    "Access"               
# [10] "Authorised Vehicles"   "Emergency Access"      "Coaches"              
# [13] "Vehicles Under 7.5T"   "Public Transport"      "Permit Holders"       
# [16] "Service Vehicles"      "School Buses" 

#THEREFORE CAN IDENTIFY WHICH TURNS HAVE PEDAL CYCLE EXEMPTION.  

turn_count_exemp_veh_type = os_turn_rest_exemp %>%
  group_by(exemption) %>%
  summarise(count = n())

# -> Pedal Cycles 678  ? seems low.  are these new turn exemptions?  









#############################
# Highway Dedication layers #
#############################

# Highway dedication provides an indication of the type of Highway user who has access to that particular 
# section of the Highway.
# There can only be one Highway Dedication type applied to the geometry at any given date or time.

# Higwhay dedication
os_access_ded = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                         layer = "HighwayDedication") 

# Reading layer `HighwayDedication' from data source `/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb' using driver `OpenFileGDB'
# Simple feature collection with 3695856 features and 13 fields
# geometry type:  MULTILINESTRING
# dimension:      XY
# bbox:           xmin: 80699.01 ymin: 6441.14 xmax: 655562.7 ymax: 657489
# projected CRS:  OSGB 1936 / British National Grid

names(os_access_ded)
# [1] "id"                   "identifier"           "beginLifespanVersion" "validFrom"           
# [5] "reasonForChange"      "dedication"           "publicRightOfWay"     "nationalCycleRoute"  
# [9] "quietRoute"           "obstruction"          "planningOrder"        "worksProhibited"     
# [13] "SHAPE_Length"         "SHAPE" 

unique(os_access_ded$dedication)
# [1] "All Vehicles"                        "Pedestrian Way Or Footpath"         
# [3] "No Dedication Or Dedication Unknown" "Cycle Track Or Cycle Way"           
# [5] "Motorway"                            "Byway Open To All Traffic"          
# [7] "Bridleway"                           "Restricted Byway"  

unique(os_access_ded$publicRightOfWay)
# 0-1

unique(os_access_ded$planningOrder)
# 0 NA -1



RestrictionForVehicles - tro

#############################
# Maintenance layers #
#############################


#### Maintenance layes - provides inforamtion about whether the road is maintatined at public expences
#  (if prospectively maintainable it will be going into public maintenance)
# 24                                      Maintenance_noGeom                   NA  1200714     13
# 25                                  Maintenance_point          Multi Point    46893     13
# 26                                   Maintenance_line    Multi Line String    15957     14
# 27                                   Maintenance_area        Multi Polygon      143     15


# Turn restrictions are a restriction based upon a vehicle manoeuvre. Thistype of 
# restriction includes prohibitive driving instructions, mandatory driving instruction and implicit restrictions.
# Prohibited instructions are indicated by road signs within a red circle,
# Mandatory driving instructions indicated by road signs within a blue circle

# 1) Maintenance_line
os_main_line = st_read("/home/bananafan/Downloads/os_highways/MasterMap Highways Network_3984103/Highways_Data_March19.gdb",
                       layer = "Maintenance_line") 

# Simple feature collection with 15957 features and 14 fields
# geometry type:  MULTILINESTRING
# dimension:      XY
# bbox:           xmin: 222282.2 ymin: 36610 xmax: 655502.7 ymax: 622941
# projected CRS:  OSGB 1936 / British National Grid

names(os_main_line)
# [1] "ID"                        "localId"                  
# [3] "beginLifespanVersion"      "validFrom"                
# [5] "maintenanceResponsibility" "partialReference"         
# [7] "highwayAuthorityName"      "highwayAuthorityID"       
# [9] "maintenanceAuthorityName"  "maintenanceAuthorityID"   
# [11] "referenceToStreetUSRN"     "reasonForChange"          
# [13] "locationDescription"       "SHAPE_Length"             
# [15] "SHAPE"  

unique(os_main_line$maintenanceResponsibility)
# 1] "Maintainable At Public Expense"                            
# [2] "Not Maintained At Public Expense"                          
# [3] "Prospectively Maintainable At Public Expense"              
# [4] "Maintenance Responsibility Is To Another Highway Authority"

# Spatially join road maintainance data to London Boroughs using inner join 
# (ie only keep observations that are in both)
lon_os_main_line = st_join(os_main_line, lon_lad_2020, left = FALSE)
unique(lon_os_main_line$maintenanceAuthorityName)
unique(lon_os_main_line$highwayAuthorityName)



















###OLD code

# limit to city of London
city = lon_os_road_link %>%
  filter(BOROUGH == "City of London") # n = 1443

mapview(city)
tmap::qtm(city)
tmap::qtm(lon_os_road_link)
