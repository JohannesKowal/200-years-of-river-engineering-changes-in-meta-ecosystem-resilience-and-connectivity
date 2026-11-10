# SETUP ####

set.seed(42)

## PACKAGES ####

Sys.setenv(LANGUAGE='en')
BIN <- normalizePath(file.path(Sys.getenv("APPDATA"), "TinyTeX", "bin", "windows"))
Sys.setenv(PATH = paste(BIN, Sys.getenv("PATH"), sep = ";"))

packages <- c(
  "readxl", "data.table", "tidyverse", "igraph", "sf", "terra", "raster",
  "future.apply", "vegan", "cluster", "factoextra", "riverconn",
  "ggnewscale", "ggtext", "paletteer", "plotly", "ggspatial", "ggfx", "ggridges", "patchwork", "elevatr",
  "grid", "png", "ragg", "latex2exp", "xdvir"
)

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## FUNCTIONS ####

index_calculation_2 <- function (graph_list, param_list, year){
  
  INDICES <- list()
  
  for (config in 1:nrow(param_list)) {
    
    gc()
    
    print("#####")
    print(paste0("Processing year ", year, " and configuration nr. ", config, ": ", param_list$DIST[config], " - ", param_list$SC[config], " - ", param_list$index_type[config]))
    print("#####")
    
    Sys.sleep(1)
    
    INDICES[[config]] <- list() 
    
    for (sub_graph in 1:length(graph_list)) {
      
      print(paste0("Processing graph ", sub_graph, " of ", length(graph_list)))
      
      graph <- graph_list[[sub_graph]]
      
      if (nrow(vertex_attr(graph) %>% as_tibble()) > 1 &
          param_list$DISP[config] != "PD" &
          param_list$index_type[config] == "full") {
        
        INDICES[[config]][[sub_graph]] <- riverconn::index_calculation(
          
          graph                  = graph,
          index_type             = param_list$index_type[config],
          dir_fragmentation_type = param_list$dir_fragmentation_type[config],
          pass_u                 = param_list$pass_u[config],
          pass_d                 = param_list$pass_d[config],
          B_ij_flag              = param_list$B_ij_flag[config],
          c_ij_flag              = param_list$c_ij_flag[config],
          dir_distance_type      = param_list$dir_distance_type[config],
          disp_type              = param_list$disp_type[config],
          param_l                = c(as.numeric(strsplit(param_list$param_l[config], ",")[[1]])),
          weight                 = param_list$weight[config],
          field_B                = param_list$DIST[config],
        ) %>%
          
          cbind(param_list[config,],
                "name"    = NA,
                "CONF_ID" = config,
                "ID_G"    = sub_graph
          ) %>%
          dplyr::select(CONF_ID, ID_G, name, everything())
        
      }
      
      if (nrow(vertex_attr(graph) %>% as_tibble()) > 1 &
          param_list$DISP[config] == "PD" &
          param_list$index_type[config] == "full") {
        
        INDICES[[config]][[sub_graph]] <- riverconn::index_calculation(
          
          graph                  = graph,
          index_type             = param_list$index_type[config],
          dir_fragmentation_type = param_list$dir_fragmentation_type[config],
          pass_u                 = param_list$pass_u[config],
          pass_d                 = param_list$pass_d[config],
          param_u                = param_list$param_u[config],
          param_d                = param_list$param_d[config],
          B_ij_flag              = param_list$B_ij_flag[config],
          c_ij_flag              = param_list$c_ij_flag[config],
          dir_distance_type      = param_list$dir_distance_type[config],
          disp_type              = param_list$disp_type[config],
          weight                 = param_list$weight[config],
          field_B                = param_list$DIST[config],
        ) %>%
          
          cbind(param_list[config,],
                "name"    = NA,
                "CONF_ID" = config,
                "ID_G"    = sub_graph
          ) %>%
          dplyr::select(CONF_ID, ID_G, name, everything())
        
      }
      
      if (nrow(vertex_attr(graph) %>% as_tibble()) > 1 &
          param_list$DISP[config] != "PD" &
          param_list$index_type[config] == "reach") {
        
        INDICES_TEMP <- riverconn::index_calculation(
          
          graph                  = graph,
          index_type             = param_list$index_type[config],
          index_mode             = "to",
          dir_fragmentation_type = param_list$dir_fragmentation_type[config],
          pass_u                 = param_list$pass_u[config],
          pass_d                 = param_list$pass_d[config],
          param_u                = param_list$param_u[config],
          param_d                = param_list$param_d[config],
          B_ij_flag              = param_list$B_ij_flag[config],
          c_ij_flag              = param_list$c_ij_flag[config],
          dir_distance_type      = param_list$dir_distance_type[config],
          disp_type              = param_list$disp_type[config],
          param_l                = c(as.numeric(strsplit(param_list$param_l[config], ",")[[1]])),
          weight                 = param_list$weight[config],
          field_B                = param_list$DIST[config],
        )
        
        INDICES_TEMP <- INDICES_TEMP %>%
          cbind(param_list[rep(config, nrow(INDICES_TEMP)),],
                "CONF_ID" = config,
                "ID_G"    = sub_graph
          ) %>%
          dplyr::select(CONF_ID, ID_G, name, everything())
        
        INDICES[[config]][[sub_graph]] <- INDICES_TEMP
        
      }
      
      if (nrow(vertex_attr(graph) %>% as_tibble()) > 1 &
          param_list$DISP[config] == "PD" &
          param_list$index_type[config] == "reach") {
        
        INDICES_TEMP <- riverconn::index_calculation(
          
          graph                  = graph,
          index_type             = param_list$index_type[config],
          index_mode             = "to",
          dir_fragmentation_type = param_list$dir_fragmentation_type[config],
          pass_u                 = param_list$pass_u[config],
          pass_d                 = param_list$pass_d[config],
          param_u                = param_list$param_u[config],
          param_d                = param_list$param_d[config],
          B_ij_flag              = param_list$B_ij_flag[config],
          c_ij_flag              = param_list$c_ij_flag[config],
          dir_distance_type      = param_list$dir_distance_type[config],
          disp_type              = param_list$disp_type[config],
          weight                 = param_list$weight[config],
          field_B                = param_list$DIST[config],
        )
        
        INDICES_TEMP <- INDICES_TEMP %>%
          cbind(param_list[rep(config, nrow(INDICES_TEMP)),],
                "CONF_ID" = config,
                "ID_G"    = sub_graph
          ) %>%
          dplyr::select(CONF_ID, ID_G, name, everything())
        
        INDICES[[config]][[sub_graph]] <- INDICES_TEMP
        
      } 
      
      if (nrow(vertex_attr(graph) %>% as_tibble()) == 1 &
          param_list$index_type[config] == "full") {
        
        INDICES[[config]][[sub_graph]] <- data.frame("num"   = vertex_attr(graph) %>% as_tibble() %>% pull(param_list$weight[config]),
                                                     "den"   = vertex_attr(graph) %>% as_tibble() %>% pull(param_list$weight[config]),
                                                     "index" = 1) %>%
          cbind(param_list[config,],
                "name"    = NA,
                "CONF_ID" = config,
                "ID_G"    = sub_graph
          ) %>%
          dplyr::select(CONF_ID, ID_G, name, everything())
      }
      
      if (nrow(vertex_attr(graph) %>% as_tibble()) == 1 &
          param_list$index_type[config] == "reach") {
        
        INDICES[[config]][[sub_graph]] <- data.frame("name"  = vertex_attr(graph) %>% as_tibble() %>% pull(name),
                                                     "num"   = vertex_attr(graph) %>% as_tibble() %>% pull(param_list$weight[config]),
                                                     "den"   = vertex_attr(graph) %>% as_tibble() %>% pull(param_list$weight[config]),
                                                     "index" = 1) %>%
          cbind(param_list[config,],
                "CONF_ID" = config,
                "ID_G"    = sub_graph
          ) %>%
          dplyr::select(CONF_ID, ID_G, name, everything())
      }
      
    }
  }
  return(INDICES)
}

add_identity_edges <- function(ml) {
  
  
  LAYER_NAMES <- layers_ml(ml)
  N_LAYERS    <- length(LAYER_NAMES)
  
  for (i in 1:(N_LAYERS - 1)) {
    for (j in (i + 1):N_LAYERS) {
      
      ACTORS   <- actors_ml(ml, layers = LAYER_NAMES[i])$actor
      ACTORS_2 <- actors_ml(ml, layers = LAYER_NAMES[j])$actor
      
      COMMON_ACTORS <- intersect(ACTORS, ACTORS_2)
      
      if (length(COMMON_ACTORS) > 0) {
        
        EDGES_FWD <- data.frame(
          actor1 = COMMON_ACTORS,
          layer1 = LAYER_NAMES[i],
          actor2 = COMMON_ACTORS,
          layer2 = LAYER_NAMES[j],
          stringsAsFactors = FALSE
        )
        
        EDGES_REV <- data.frame(
          actor1 = COMMON_ACTORS,
          layer1 = LAYER_NAMES[j],
          actor2 = COMMON_ACTORS,
          layer2 = LAYER_NAMES[i],
          stringsAsFactors = FALSE
        )
        
        add_edges_ml(ml, EDGES_FWD)
        add_edges_ml(ml, EDGES_REV)
        
      }
    }
  }
  
  return(ml)
}

# ~~~~~~~~~~~~~~~~~~~~ ####       
# ~~~~~~ ><{{{°> ~~~~~ ####
# ~~~~~~~~~~~~~~~~~~~~ ####       
# PREP DATA ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## SC (WEIGHTS) ####

SC_TAXA    <- read_csv("./WEIGHTS/SC_TAXA.csv")
CONVERSION <- read_csv("./WEIGHTS/CONVERSION.csv")

K_WSS_SC <- fviz_nbclust(SC_TAXA %>%
                           dplyr::select(-GENUS, -SPECIES, -DISP, -GROUP) %>%
                           column_to_rownames("ID_TAXA") %>%
                           unique(),
                         FUNcluster = cluster::pam,
                         method = "wss",
                         k.max = 10)

K_WSS_SC <- K_WSS_SC$data %>%
  as_tibble() %>%
  rename(K = clusters, WSS = y)

write_csv(K_WSS_SC, "./RESULTS/K_WSS_SC.csv")

CLUSTER_PAM <- cluster::pam(SC_TAXA %>%
                              dplyr::select(-GENUS, -SPECIES, -DISP, -GROUP) %>%
                              column_to_rownames("ID_TAXA")  %>%
                              unique(),
                            k = 5)
SC_TAXA <- SC_TAXA %>%
  left_join(SC_TAXA %>%
              dplyr::select(-GENUS, -SPECIES, -DISP, -GROUP) %>%
              column_to_rownames("ID_TAXA")  %>%
              unique() %>%
              cbind(SC = CLUSTER_PAM$clustering),
            by = c("H1", "H2", "H3", "H4", "H5")) %>%
  mutate(SC = case_when(
    SC == 1 ~ 1,
    SC == 2 ~ 2,
    SC == 3 ~ 4,
    SC == 4 ~ 3,
    SC == 5 ~ 5,
    TRUE    ~ SC
  )
  )

SC_TAXA_MEDIAN <- SC_TAXA %>%
  dplyr::select(-GENUS, -SPECIES, -DISP, -GROUP) %>%
  column_to_rownames("ID_TAXA")  %>%
  group_by(SC) %>%
  summarise(across(starts_with("H"), ~ median(.)))

SC <- SC_TAXA_MEDIAN %>%
  rowwise() %>%
  mutate(H1_H2 = mean(c(H1, H2)),
         H3_H4 = mean(c(H3, H4))) %>%
  column_to_rownames("SC") %>%
  t() %>%
  as_tibble(rownames = "FHI") %>%
  left_join(CONVERSION, by = "FHI") %>%
  drop_na() %>%
  rename(WB_TYPE = "AMOROS")

SC_TAXA <- SC_TAXA %>%
  left_join(SC_TAXA_MEDIAN %>% rename_with(~ paste0(., "_MEDIAN"), starts_with("H")), by = "SC") %>%
  mutate(H1_ERROR = abs(H1 - H1_MEDIAN),
         H2_ERROR = abs(H2 - H2_MEDIAN),
         H3_ERROR = abs(H3 - H3_MEDIAN),
         H4_ERROR = abs(H4 - H4_MEDIAN),
         H5_ERROR = abs(H5 - H5_MEDIAN)) %>%
  rowwise() %>%
  mutate(ERROR_CUM = sum(H1_ERROR, H2_ERROR, H3_ERROR, H4_ERROR, H5_ERROR),
         ERROR_MAX = max(H1_ERROR, H2_ERROR, H3_ERROR, H4_ERROR, H5_ERROR)) %>%
  ungroup()

## WIDTH (WEIGHTS) ####

WIDTHS <- read_csv("./WEIGHTS/WIDTHS.csv")

## NODES ####

NODES_1817 <- st_read("./NODES/NODES_1817/NODES_1817.shp")
NODES_1857 <- st_read("./NODES/NODES_1857/NODES_1857.shp")
NODES_1875 <- st_read("./NODES/NODES_1875/NODES_1875.shp")
NODES_1910 <- st_read("./NODES/NODES_1910/NODES_1910.shp")
NODES_1965 <- st_read("./NODES/NODES_1965/NODES_1965.shp")
NODES_1996 <- st_read("./NODES/NODES_1996/NODES_1996.shp")
NODES_2022 <- st_read("./NODES/NODES_2022/NODES_2022.shp")

NODES_1817 <- NODES_1817 %>%
  left_join(SC %>% dplyr::select(-FHI, -"5"), by = c("IMP", "WB_TYPE")) %>%
  rename(SC1 = "1",
         SC2 = "2",
         SC3 = "3",
         SC4 = "4") %>%
  mutate(across(starts_with("SC"), ~ ifelse(is.na(.), 0, .)))

NODES_1857 <- NODES_1857 %>%
  left_join(SC %>% dplyr::select(-FHI, -"5"), by = c("IMP", "WB_TYPE")) %>%
  rename(SC1 = "1",
         SC2 = "2",
         SC3 = "3",
         SC4 = "4") %>%
  mutate(across(starts_with("SC"), ~ ifelse(is.na(.), 0, .)))

NODES_1875 <- NODES_1875 %>% 
  left_join(SC %>% dplyr::select(-FHI, -"5"), by = c("IMP", "WB_TYPE")) %>%
  rename(SC1 = "1",
         SC2 = "2",
         SC3 = "3",
         SC4 = "4") %>%
  mutate(across(starts_with("SC"), ~ ifelse(is.na(.), 0, .)))

NODES_1910 <- NODES_1910 %>% 
  left_join(SC %>% dplyr::select(-FHI, -"5"), by = c("IMP", "WB_TYPE")) %>%
  rename(SC1 = "1",
         SC2 = "2",
         SC3 = "3",
         SC4 = "4") %>%
  mutate(across(starts_with("SC"), ~ ifelse(is.na(.), 0, .)))

NODES_1965 <- NODES_1965 %>% 
  left_join(SC %>% dplyr::select(-FHI, -"5"), by = c("IMP", "WB_TYPE")) %>%
  rename(SC1 = "1",
         SC2 = "2",
         SC3 = "3",
         SC4 = "4") %>%
  mutate(across(starts_with("SC"), ~ ifelse(is.na(.), 0, .)))

NODES_1996 <- NODES_1996 %>% 
  left_join(SC %>% dplyr::select(-FHI, -"5"), by = c("IMP", "WB_TYPE")) %>%
  rename(SC1 = "1",
         SC2 = "2",
         SC3 = "3",
         SC4 = "4") %>%
  mutate(across(starts_with("SC"), ~ ifelse(is.na(.), 0, .)))

NODES_2022 <- NODES_2022 %>% 
  left_join(SC %>% dplyr::select(-FHI, -"5"), by = c("IMP", "WB_TYPE")) %>%
  rename(SC1 = "1",
         SC2 = "2",
         SC3 = "3",
         SC4 = "4") %>%
  mutate(across(starts_with("SC"), ~ ifelse(is.na(.), 0, .)))
  
TYPE_CONNECTIVITY <- tibble(
  WB_TYPE = unique(c(
    NODES_1817$WB_TYPE,
    NODES_1857$WB_TYPE,
    NODES_1875$WB_TYPE,
    NODES_1910$WB_TYPE,
    NODES_1965$WB_TYPE,
    NODES_1996$WB_TYPE,
    NODES_2022$WB_TYPE
  ))) %>%
  arrange(WB_TYPE) %>%
  mutate(
    WL_UP = case_when(
      WB_TYPE == "eua" ~ "<LW",
      WB_TYPE == "eub" ~ "<LW-LW+",
      WB_TYPE == "pab" ~ "SMW-HW",
      WB_TYPE == "tri" ~ "<LW",
      WB_TYPE == "paa" ~ "MW-SMW",
      WB_TYPE == "ple" ~ "SMW-HW",
      WB_TYPE == "art" ~ "BW-HW"
    ),
    
    WL_DOWN = case_when(
      WB_TYPE == "eua" ~ "<LW",
      WB_TYPE == "eub" ~ "<LW-LW+",
      WB_TYPE == "pab" ~ "<LW-SMW",
      WB_TYPE == "tri" ~ "<LW",
      WB_TYPE == "paa" ~ "<LW-SMW",
      WB_TYPE == "ple" ~ "SMW-BW",
      WB_TYPE == "art" ~ "BW-HW"
    )
  ) %>%
  
  mutate(DAYS_CON_DOWN = NA,
         DAYS_CON_UP   = NA) %>%
  
  mutate(
    DAYS_CON_DOWN = case_when(
      WL_DOWN == "<LW" ~ 365,
      WL_DOWN == "<LW-LW+" ~ 357,
      WL_DOWN == "SMW-HW" ~ 77,
      WL_DOWN == "MW-SMW" ~ 174,
      WL_DOWN == "BW-HW" ~ 4,
      WL_DOWN == "<LW-SMW" ~ 252,
      WL_DOWN == "SMW-BW" ~ 76),
    
    
    
    
    DAYS_CON_UP = case_when(
      WL_UP == "<LW" ~ 365,
      WL_UP == "<LW-LW+" ~ 357,
      WL_UP == "SMW-HW" ~ 77,
      WL_UP == "MW-SMW" ~ 174,
      WL_UP == "BW-HW" ~ 4,
      WL_UP == "<LW-SMW" ~ 252,
      WL_UP == "SMW-BW" ~ 76
    )
  ) %>%
  
  drop_na()

## LINKS ####

LINKS_1817 <- read_csv("./LINKS/LINKS_1817.csv")
LINKS_1857 <- read_csv("./LINKS/LINKS_1857.csv")
LINKS_1875 <- read_csv("./LINKS/LINKS_1875.csv")
LINKS_1910 <- read_csv("./LINKS/LINKS_1910.csv")
LINKS_1965 <- read_csv("./LINKS/LINKS_1965.csv")
LINKS_1996 <- read_csv("./LINKS/LINKS_1996.csv")
LINKS_2022 <- read_csv("./LINKS/LINKS_2022.csv")

## SAMPLES ####

SAMPLES <- st_read("./SAMPLES/SAMPLES.shp")

## GENERATE GRAPHS ####
### 1817 ####

NODE_WEIGHTS_1817 <- NODES_1817 %>%
  
  mutate(YEAR = 1817) %>%
  left_join(WIDTHS %>%
              dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
            by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
  relocate(WIDTH, .after = LENGTH) %>%
  mutate(WIDTH = ifelse(WB_TYPE == "CO", 0, WIDTH)) %>%
  
  mutate(AREA = LENGTH * WIDTH) %>%
  mutate(AREA = ifelse(AREA == 0, 0, log(AREA))) %>%
  mutate(across(c(starts_with("SC")), ~ (./10))) %>%
  mutate(WSC1 = AREA * SC1^2,
         WSC2 = AREA * SC2^2,
         WSC3 = AREA * SC3^2,
         WSC4 = AREA * SC4^2) %>%
  
  mutate(name = as.character(FID)) %>%
  
  left_join(TYPE_CONNECTIVITY %>%
              dplyr::select(WB_TYPE, DAYS_CON_UP), by = "WB_TYPE") %>%
  
  mutate(DSC1 = ifelse(LENGTH == 0, 0, (LENGTH/((SC1^2) + 10e-6))),
         DSC2 = ifelse(LENGTH == 0, 0, (LENGTH/((SC2)   + 10e-6))),
         DSC3 = ifelse(LENGTH == 0, 0, (LENGTH/((SC3)   + 10e-6))),
         DSC4 = ifelse(LENGTH == 0, 0, (LENGTH/((SC4)   + 10e-6)))) %>%
  
  dplyr::select(name, starts_with(c("DSC", "WSC"))) %>%
  st_drop_geometry()

GRAPH_1817 <- graph_from_data_frame(
  
  d = LINKS_1817 %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)),
  
  vertices = NODE_WEIGHTS_1817,
  
  directed = T)

GRAPH_1817_DECOMP_TEMP <- decompose(GRAPH_1817)
GRAPH_1817_DECOMP      <- list()
GRAPH_1817_DECOMP[[1]] <- 1:length(GRAPH_1817_DECOMP_TEMP)
GRAPH_1817_DECOMP      <- c(GRAPH_1817_DECOMP, GRAPH_1817_DECOMP_TEMP)

### 1857 ####

NODE_WEIGHTS_1857 <- NODES_1857 %>%
  
  mutate(YEAR = 1857) %>%
  left_join(WIDTHS %>%
              dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
            by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
  relocate(WIDTH, .after = LENGTH) %>%
  mutate(WIDTH = ifelse(WB_TYPE == "CO", 0, WIDTH)) %>%
  
  mutate(AREA = LENGTH * WIDTH) %>%
  mutate(AREA = ifelse(AREA == 0, 0, log(AREA))) %>%
  mutate(across(c(starts_with("SC")), ~ (./10))) %>%
  mutate(WSC1 = AREA * SC1^2,
         WSC2 = AREA * SC2^2,
         WSC3 = AREA * SC3^2,
         WSC4 = AREA * SC4^2) %>%
  
  mutate(name = as.character(FID)) %>%
  
  left_join(TYPE_CONNECTIVITY %>%
              dplyr::select(WB_TYPE, DAYS_CON_UP), by = "WB_TYPE") %>%
  
  mutate(DSC1 = ifelse(LENGTH == 0, 0, (LENGTH/((SC1^2) + 10e-6))),
         DSC2 = ifelse(LENGTH == 0, 0, (LENGTH/((SC2)   + 10e-6))),
         DSC3 = ifelse(LENGTH == 0, 0, (LENGTH/((SC3)   + 10e-6))),
         DSC4 = ifelse(LENGTH == 0, 0, (LENGTH/((SC4)   + 10e-6)))) %>%
  
  dplyr::select(name, starts_with(c("DSC", "WSC"))) %>%
  st_drop_geometry()

GRAPH_1857 <- graph_from_data_frame(
  
  d = LINKS_1857 %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)),
  
  vertices = NODE_WEIGHTS_1857,
  
  directed = T)

GRAPH_1857_DECOMP_TEMP <- decompose(GRAPH_1857)
GRAPH_1857_DECOMP      <- list()
GRAPH_1857_DECOMP[[1]] <- 1:length(GRAPH_1857_DECOMP_TEMP)
GRAPH_1857_DECOMP      <- c(GRAPH_1857_DECOMP, GRAPH_1857_DECOMP_TEMP)

### 1875 ####

NODE_WEIGHTS_1875 <- NODES_1875 %>%
  
  mutate(YEAR = 1875) %>%
  left_join(WIDTHS %>%
              dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
            by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
  relocate(WIDTH, .after = LENGTH) %>%
  mutate(WIDTH = ifelse(WB_TYPE == "CO", 0, WIDTH)) %>%
  
  mutate(AREA = LENGTH * WIDTH) %>%
  mutate(AREA = ifelse(AREA == 0, 0, log(AREA))) %>%
  mutate(across(c(starts_with("SC")), ~ (./10))) %>%
  mutate(WSC1 = AREA * SC1^2,
         WSC2 = AREA * SC2^2,
         WSC3 = AREA * SC3^2,
         WSC4 = AREA * SC4^2) %>%
  
  mutate(name = as.character(FID)) %>%
  
  left_join(TYPE_CONNECTIVITY %>%
              dplyr::select(WB_TYPE, DAYS_CON_UP), by = "WB_TYPE") %>%
  
  mutate(DSC1 = ifelse(LENGTH == 0, 0, (LENGTH/((SC1^2) + 10e-6))),
         DSC2 = ifelse(LENGTH == 0, 0, (LENGTH/((SC2)   + 10e-6))),
         DSC3 = ifelse(LENGTH == 0, 0, (LENGTH/((SC3)   + 10e-6))),
         DSC4 = ifelse(LENGTH == 0, 0, (LENGTH/((SC4)   + 10e-6)))) %>%
  
  dplyr::select(name, starts_with(c("DSC", "WSC"))) %>%
  st_drop_geometry()

GRAPH_1875 <- graph_from_data_frame(
  
  d = LINKS_1875 %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)),
  
  vertices = NODE_WEIGHTS_1875,
  
  directed = T)

GRAPH_1875_DECOMP_TEMP <- decompose(GRAPH_1875)
GRAPH_1875_DECOMP      <- list()
GRAPH_1875_DECOMP[[1]] <- 1:length(GRAPH_1875_DECOMP_TEMP)
GRAPH_1875_DECOMP      <- c(GRAPH_1875_DECOMP, GRAPH_1875_DECOMP_TEMP)

### 1910 ####

NODE_WEIGHTS_1910 <- NODES_1910 %>%
  
  mutate(YEAR = 1910) %>%
  left_join(WIDTHS %>%
              dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
            by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
  relocate(WIDTH, .after = LENGTH) %>%
  mutate(WIDTH = ifelse(WB_TYPE == "CO", 0, WIDTH)) %>%
  
  mutate(AREA = LENGTH * WIDTH) %>%
  mutate(AREA = ifelse(AREA == 0, 0, log(AREA))) %>%
  mutate(across(c(starts_with("SC")), ~ (./10))) %>%
  mutate(WSC1 = AREA * SC1^2,
         WSC2 = AREA * SC2^2,
         WSC3 = AREA * SC3^2,
         WSC4 = AREA * SC4^2) %>%
  
  mutate(name = as.character(FID)) %>%
  
  left_join(TYPE_CONNECTIVITY %>%
              dplyr::select(WB_TYPE, DAYS_CON_UP), by = "WB_TYPE") %>%
  
  mutate(DSC1 = ifelse(LENGTH == 0, 0, (LENGTH/((SC1^2) + 10e-6))),
         DSC2 = ifelse(LENGTH == 0, 0, (LENGTH/((SC2)   + 10e-6))),
         DSC3 = ifelse(LENGTH == 0, 0, (LENGTH/((SC3)   + 10e-6))),
         DSC4 = ifelse(LENGTH == 0, 0, (LENGTH/((SC4)   + 10e-6)))) %>%
  
  dplyr::select(name, starts_with(c("DSC", "WSC"))) %>%
  st_drop_geometry()

GRAPH_1910 <- graph_from_data_frame(
  
  d = LINKS_1910 %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)),
  
  vertices = NODE_WEIGHTS_1910,
  
  directed = T)

GRAPH_1910_DECOMP_TEMP <- decompose(GRAPH_1910)
GRAPH_1910_DECOMP      <- list()
GRAPH_1910_DECOMP[[1]] <- 1:length(GRAPH_1910_DECOMP_TEMP)
GRAPH_1910_DECOMP      <- c(GRAPH_1910_DECOMP, GRAPH_1910_DECOMP_TEMP)

### 1965 ####

NODE_WEIGHTS_1965 <- NODES_1965 %>%
  
  mutate(YEAR = 1965) %>%
  left_join(WIDTHS %>%
              dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
            by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
  relocate(WIDTH, .after = LENGTH) %>%
  mutate(WIDTH = ifelse(WB_TYPE == "CO", 0, WIDTH)) %>%
  
  mutate(AREA = LENGTH * WIDTH) %>%
  mutate(AREA = ifelse(AREA == 0, 0, log(AREA))) %>%
  mutate(across(c(starts_with("SC")), ~ (./10))) %>%
  mutate(WSC1 = AREA * SC1^2,
         WSC2 = AREA * SC2^2,
         WSC3 = AREA * SC3^2,
         WSC4 = AREA * SC4^2) %>%
  
  mutate(name = as.character(FID)) %>%
  
  left_join(TYPE_CONNECTIVITY %>%
              dplyr::select(WB_TYPE, DAYS_CON_UP), by = "WB_TYPE") %>%
  
  mutate(DSC1 = ifelse(LENGTH == 0, 0, (LENGTH/((SC1^2) + 10e-6))),
         DSC2 = ifelse(LENGTH == 0, 0, (LENGTH/((SC2)   + 10e-6))),
         DSC3 = ifelse(LENGTH == 0, 0, (LENGTH/((SC3)   + 10e-6))),
         DSC4 = ifelse(LENGTH == 0, 0, (LENGTH/((SC4)   + 10e-6)))) %>%
  
  dplyr::select(name, starts_with(c("DSC", "WSC"))) %>%
  st_drop_geometry()

GRAPH_1965 <- graph_from_data_frame(
  
  d = LINKS_1965 %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)),
  
  vertices = NODE_WEIGHTS_1965,
  
  directed = T)

GRAPH_1965_DECOMP_TEMP <- decompose(GRAPH_1965)
GRAPH_1965_DECOMP      <- list()
GRAPH_1965_DECOMP[[1]] <- 1:length(GRAPH_1965_DECOMP_TEMP)
GRAPH_1965_DECOMP      <- c(GRAPH_1965_DECOMP, GRAPH_1965_DECOMP_TEMP)

### 1996 ####

NODE_WEIGHTS_1996 <- NODES_1996 %>%
  
  mutate(YEAR = 1996) %>%
  left_join(WIDTHS %>%
              dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
            by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
  relocate(WIDTH, .after = LENGTH) %>%
  mutate(WIDTH = ifelse(WB_TYPE == "CO", 0, WIDTH)) %>%
  
  mutate(AREA = LENGTH * WIDTH) %>%
  mutate(AREA = ifelse(AREA == 0, 0, log(AREA))) %>%
  mutate(across(c(starts_with("SC")), ~ (./10))) %>%
  mutate(WSC1 = AREA * SC1^2,
         WSC2 = AREA * SC2^2,
         WSC3 = AREA * SC3^2,
         WSC4 = AREA * SC4^2) %>%
  
  mutate(name = as.character(FID)) %>%
  
  left_join(TYPE_CONNECTIVITY %>%
              dplyr::select(WB_TYPE, DAYS_CON_UP), by = "WB_TYPE") %>%
  
  mutate(DSC1 = ifelse(LENGTH == 0, 0, (LENGTH/((SC1^2) + 10e-6))),
         DSC2 = ifelse(LENGTH == 0, 0, (LENGTH/((SC2)   + 10e-6))),
         DSC3 = ifelse(LENGTH == 0, 0, (LENGTH/((SC3)   + 10e-6))),
         DSC4 = ifelse(LENGTH == 0, 0, (LENGTH/((SC4)   + 10e-6)))) %>%
  
  dplyr::select(name, starts_with(c("DSC", "WSC"))) %>%
  st_drop_geometry()

GRAPH_1996 <- graph_from_data_frame(
  
  d = LINKS_1996 %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)),
  
  vertices = NODE_WEIGHTS_1996,
  
  directed = T)

GRAPH_1996_DECOMP_TEMP <- decompose(GRAPH_1996)
GRAPH_1996_DECOMP      <- list()
GRAPH_1996_DECOMP[[1]] <- 1:length(GRAPH_1996_DECOMP_TEMP)
GRAPH_1996_DECOMP      <- c(GRAPH_1996_DECOMP, GRAPH_1996_DECOMP_TEMP)

### 2022 ####

NODE_WEIGHTS_2022 <- NODES_2022 %>%
  
  mutate(YEAR = 2022) %>%
  left_join(WIDTHS %>%
              dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
            by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
  relocate(WIDTH, .after = LENGTH) %>%
  mutate(WIDTH = ifelse(WB_TYPE == "CO", 0, WIDTH)) %>%
  
  mutate(AREA = LENGTH * WIDTH) %>%
  mutate(AREA = ifelse(AREA == 0, 0, log(AREA))) %>%
  mutate(across(c(starts_with("SC")), ~ (./10))) %>%
  mutate(WSC1 = AREA * SC1^2,
         WSC2 = AREA * SC2^2,
         WSC3 = AREA * SC3^2,
         WSC4 = AREA * SC4^2) %>%
  
  mutate(name = as.character(FID)) %>%
  
  left_join(TYPE_CONNECTIVITY %>%
              dplyr::select(WB_TYPE, DAYS_CON_UP), by = "WB_TYPE") %>%
  
  mutate(DSC1 = ifelse(LENGTH == 0, 0, (LENGTH/((SC1^2) + 10e-6))),
         DSC2 = ifelse(LENGTH == 0, 0, (LENGTH/((SC2)   + 10e-6))),
         DSC3 = ifelse(LENGTH == 0, 0, (LENGTH/((SC3)   + 10e-6))),
         DSC4 = ifelse(LENGTH == 0, 0, (LENGTH/((SC4)   + 10e-6)))) %>%
  
  dplyr::select(name, starts_with(c("DSC", "WSC"))) %>%
  st_drop_geometry()

GRAPH_2022 <- graph_from_data_frame(
  
  d = LINKS_2022 %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)),
  
  vertices = NODE_WEIGHTS_2022,
  
  directed = T)

GRAPH_2022_DECOMP_TEMP <- decompose(GRAPH_2022)
GRAPH_2022_DECOMP      <- list()
GRAPH_2022_DECOMP[[1]] <- 1:length(GRAPH_2022_DECOMP_TEMP)
GRAPH_2022_DECOMP      <- c(GRAPH_2022_DECOMP, GRAPH_2022_DECOMP_TEMP)

## CONCATENATE GRAPHS ####

### 1817 ####

NODE_WEIGHTS_1817_CONCAT <-
  rbind(NODE_WEIGHTS_1817 %>%
          mutate(name = as.numeric(name) + 1 +  max(as.numeric(NODE_WEIGHTS_1817$name)) * 1) %>%
          mutate(name = as.character(name)),
        NODE_WEIGHTS_1817,
        NODE_WEIGHTS_1817 %>%
          mutate(name = as.numeric(name) + 2 +  max(as.numeric(NODE_WEIGHTS_1817$name)) * 2) %>%
          mutate(name = as.character(name))
  )

LINKS_1817_CONCAT <-
  rbind(LINKS_1817 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1817$name)) * 1) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 1 +  max(as.numeric(NODE_WEIGHTS_1817$name)) * 1),
        LINKS_1817,
        LINKS_1817 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 2 +  max(as.numeric(NODE_WEIGHTS_1817$name)) * 2) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 2 +  max(as.numeric(NODE_WEIGHTS_1817$name)) * 2)
  )

UPSTREAM_NODES   <- c(4448, 4473)
DOWNSTREAM_NODES <- c(2509, 2494)

CONCAT_LINKS <- LINKS_1817_CONCAT %>%
  dplyr::select(IN_FID, NEAR_FID) %>%
  slice(1:4) %>%
  mutate(across(everything(), ~ NA))

CONCAT_LINKS$IN_FID   <- c(DOWNSTREAM_NODES, DOWNSTREAM_NODES)
CONCAT_LINKS$NEAR_FID <- c(UPSTREAM_NODES, UPSTREAM_NODES)

CONCAT_LINKS <- CONCAT_LINKS %>%
  mutate(IN_FID   = as.character(IN_FID),
         NEAR_FID = as.character(NEAR_FID)) %>%
  mutate(pass_u   = 1,
         pass_d   = 1,
         TYPE     = "LINK") %>%
  rename(FROM = IN_FID, TO = NEAR_FID)

CONCAT_LINKS$FROM <- c(DOWNSTREAM_NODES,
                       DOWNSTREAM_NODES + 1 +  max(as.numeric(NODE_WEIGHTS_1817$name)) * 1)

CONCAT_LINKS$TO   <- c(UPSTREAM_NODES   + 2 +  max(as.numeric(NODE_WEIGHTS_1817$name)) * 2,
                       UPSTREAM_NODES)

GRAPH_1817_CONCAT <- graph_from_data_frame(
  
  d = LINKS_1817_CONCAT %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)) %>%
    rbind(CONCAT_LINKS),
  
  vertices = NODE_WEIGHTS_1817_CONCAT,
  
  directed = T)

GRAPH_1817_DECOMP_TEMP_CONCAT <- decompose(GRAPH_1817_CONCAT)
GRAPH_1817_DECOMP_CONCAT <- list()
GRAPH_1817_DECOMP_CONCAT[[1]] <- 1:length(GRAPH_1817_DECOMP_TEMP_CONCAT)
GRAPH_1817_DECOMP_CONCAT <- c(GRAPH_1817_DECOMP_CONCAT, GRAPH_1817_DECOMP_TEMP_CONCAT)

### 1857 ####

NODE_WEIGHTS_1857_CONCAT <-
  rbind(NODE_WEIGHTS_1857 %>%
          mutate(name = as.numeric(name) + 1 +  max(as.numeric(NODE_WEIGHTS_1857$name)) * 1) %>%
          mutate(name = as.character(name)),
        NODE_WEIGHTS_1857,
        NODE_WEIGHTS_1857 %>%
          mutate(name = as.numeric(name) + 2 +  max(as.numeric(NODE_WEIGHTS_1857$name)) * 2) %>%
          mutate(name = as.character(name))
  )

LINKS_1857_CONCAT <-
  rbind(LINKS_1857 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1857$name)) * 1) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 1 +  max(as.numeric(NODE_WEIGHTS_1857$name)) * 1),
        LINKS_1857,
        LINKS_1857 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 2 +  max(as.numeric(NODE_WEIGHTS_1857$name)) * 2) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 2 +  max(as.numeric(NODE_WEIGHTS_1857$name)) * 2)
  )

UPSTREAM_NODES   <- c(3571, 3580)
DOWNSTREAM_NODES <- c(7525, 1615)

CONCAT_LINKS <- LINKS_1857_CONCAT %>%
  dplyr::select(IN_FID, NEAR_FID) %>%
  slice(1:4) %>%
  mutate(across(everything(), ~ NA))

CONCAT_LINKS$IN_FID   <- c(DOWNSTREAM_NODES, DOWNSTREAM_NODES)
CONCAT_LINKS$NEAR_FID <- c(UPSTREAM_NODES, UPSTREAM_NODES)

CONCAT_LINKS <- CONCAT_LINKS %>%
  mutate(IN_FID   = as.character(IN_FID),
         NEAR_FID = as.character(NEAR_FID)) %>%
  mutate(pass_u   = 1,
         pass_d   = 1,
         TYPE     = "LINK") %>%
  rename(FROM = IN_FID, TO = NEAR_FID)

CONCAT_LINKS$FROM <- c(DOWNSTREAM_NODES,
                       DOWNSTREAM_NODES + 1 +  max(as.numeric(NODE_WEIGHTS_1857$name)) * 1)
CONCAT_LINKS$TO   <- c(UPSTREAM_NODES   + 2 +  max(as.numeric(NODE_WEIGHTS_1857$name)) * 2,
                       UPSTREAM_NODES)

GRAPH_1857_CONCAT <- graph_from_data_frame(
  d = LINKS_1857_CONCAT %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)) %>%
    rbind(CONCAT_LINKS),
  
  vertices = NODE_WEIGHTS_1857_CONCAT,
  
  directed = T)

GRAPH_1857_DECOMP_TEMP_CONCAT <- decompose(GRAPH_1857_CONCAT)
GRAPH_1857_DECOMP_CONCAT <- list()
GRAPH_1857_DECOMP_CONCAT[[1]] <- 1:length(GRAPH_1857_DECOMP_TEMP_CONCAT)
GRAPH_1857_DECOMP_CONCAT <- c(GRAPH_1857_DECOMP_CONCAT, GRAPH_1857_DECOMP_TEMP_CONCAT)

### 1875 ####

NODE_WEIGHTS_1875_CONCAT <- NODE_WEIGHTS_1875 %>%
  rbind(NODE_WEIGHTS_1875 %>%
          mutate(name = as.numeric(name) + 1 +  max(as.numeric(NODE_WEIGHTS_1875$name)) * 1) %>%
          mutate(name = as.character(name)),
        NODE_WEIGHTS_1875 %>%
          mutate(name = as.numeric(name) + 2 +  max(as.numeric(NODE_WEIGHTS_1875$name)) * 2) %>%
          mutate(name = as.character(name))
  )

LINKS_1875_CONCAT <- LINKS_1875 %>%
  rbind(LINKS_1875 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1875$name)) * 1) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1875$name)) * 1),
        LINKS_1875 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 2 + max(as.numeric(NODE_WEIGHTS_1875$name)) * 2) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 2 + max(as.numeric(NODE_WEIGHTS_1875$name)) * 2)
  )

UPSTREAM_NODES   <- c(3169, 3195)
DOWNSTREAM_NODES <- c(2066, 1956)

CONCAT_LINKS <- LINKS_1875_CONCAT %>%
  dplyr::select(IN_FID, NEAR_FID) %>%
  slice(1:4) %>%
  mutate(across(everything(), ~ NA))

CONCAT_LINKS$IN_FID   <- c(DOWNSTREAM_NODES, DOWNSTREAM_NODES)
CONCAT_LINKS$NEAR_FID <- c(UPSTREAM_NODES, UPSTREAM_NODES)

CONCAT_LINKS <- CONCAT_LINKS %>%
  mutate(IN_FID   = as.character(IN_FID),
         NEAR_FID = as.character(NEAR_FID)) %>%  
  mutate(pass_u   = 1,
         pass_d   = 1,
         TYPE     = "LINK") %>%
  rename(FROM = IN_FID, TO = NEAR_FID)

CONCAT_LINKS$FROM <- c(DOWNSTREAM_NODES,
                       DOWNSTREAM_NODES + 1 +  max(as.numeric(NODE_WEIGHTS_1875$name)) * 1)
CONCAT_LINKS$TO   <- c(UPSTREAM_NODES   + 2 +  max(as.numeric(NODE_WEIGHTS_1875$name)) * 2,
                       UPSTREAM_NODES)

GRAPH_1875_CONCAT <- graph_from_data_frame(
  d = LINKS_1875_CONCAT %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)) %>%
    rbind(CONCAT_LINKS),
  
  vertices = NODE_WEIGHTS_1875_CONCAT,
  
  directed = T)

GRAPH_1875_DECOMP_TEMP_CONCAT <- decompose(GRAPH_1875_CONCAT)
GRAPH_1875_DECOMP_CONCAT <- list()
GRAPH_1875_DECOMP_CONCAT[[1]] <- 1:length(GRAPH_1875_DECOMP_TEMP_CONCAT)
GRAPH_1875_DECOMP_CONCAT <- c(GRAPH_1875_DECOMP_CONCAT, GRAPH_1875_DECOMP_TEMP_CONCAT)

### 1910 ####

NODE_WEIGHTS_1910_CONCAT <- NODE_WEIGHTS_1910 %>%
  rbind(NODE_WEIGHTS_1910 %>%
          mutate(name = as.numeric(name) + 1 +  max(as.numeric(NODE_WEIGHTS_1910$name)) * 1) %>%
          mutate(name = as.character(name)),
        NODE_WEIGHTS_1910 %>%
          mutate(name = as.numeric(name) + 2 +  max(as.numeric(NODE_WEIGHTS_1910$name)) * 2) %>%
          mutate(name = as.character(name))
  )

LINKS_1910_CONCAT <- LINKS_1910 %>%
  rbind(LINKS_1910 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1910$name)) * 1) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1910$name)) * 1),
        LINKS_1910 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 2 + max(as.numeric(NODE_WEIGHTS_1910$name)) * 2) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 2 + max(as.numeric(NODE_WEIGHTS_1910$name)) * 2)
  )

UPSTREAM_NODES   <- c(3979, 4017)
DOWNSTREAM_NODES <- c(2336, 2302)

CONCAT_LINKS <- LINKS_1910_CONCAT %>%
  dplyr::select(IN_FID, NEAR_FID) %>%
  slice(1:4) %>%
  mutate(across(everything(), ~ NA))

CONCAT_LINKS$IN_FID   <- c(DOWNSTREAM_NODES, DOWNSTREAM_NODES)
CONCAT_LINKS$NEAR_FID <- c(UPSTREAM_NODES, UPSTREAM_NODES)

CONCAT_LINKS <- CONCAT_LINKS %>%
  mutate(IN_FID   = as.character(IN_FID),
         NEAR_FID = as.character(NEAR_FID)) %>%  
  mutate(pass_u   = 1,
         pass_d   = 1,
         TYPE     = "LINK") %>%
  rename(FROM = IN_FID, TO = NEAR_FID)

CONCAT_LINKS$FROM <- c(DOWNSTREAM_NODES,
                       DOWNSTREAM_NODES + 1 +  max(as.numeric(NODE_WEIGHTS_1910$name)) * 1)
CONCAT_LINKS$TO   <- c(UPSTREAM_NODES   + 2 +  max(as.numeric(NODE_WEIGHTS_1910$name)) * 2,
                       UPSTREAM_NODES)

GRAPH_1910_CONCAT <- graph_from_data_frame(
  d = LINKS_1910_CONCAT %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)) %>%
    rbind(CONCAT_LINKS),
  
  vertices = NODE_WEIGHTS_1910_CONCAT,
  
  directed = T)

GRAPH_1910_DECOMP_TEMP_CONCAT <- decompose(GRAPH_1910_CONCAT)
GRAPH_1910_DECOMP_CONCAT <- list()
GRAPH_1910_DECOMP_CONCAT[[1]] <- 1:length(GRAPH_1910_DECOMP_TEMP_CONCAT)
GRAPH_1910_DECOMP_CONCAT <- c(GRAPH_1910_DECOMP_CONCAT, GRAPH_1910_DECOMP_TEMP_CONCAT)

### 1965 ####

NODE_WEIGHTS_1965_CONCAT <- NODE_WEIGHTS_1965 %>%
  rbind(NODE_WEIGHTS_1965 %>%
          mutate(name = as.numeric(name) + 1 +  max(as.numeric(NODE_WEIGHTS_1965$name)) * 1) %>%
          mutate(name = as.character(name)),
        NODE_WEIGHTS_1965 %>%
          mutate(name = as.numeric(name) + 2 +  max(as.numeric(NODE_WEIGHTS_1965$name)) * 2) %>%
          mutate(name = as.character(name))
  )

LINKS_1965_CONCAT <- LINKS_1965 %>%
  rbind(LINKS_1965 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1965$name)) * 1) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1965$name)) * 1),
        LINKS_1965 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 2 + max(as.numeric(NODE_WEIGHTS_1965$name)) * 2) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 2 + max(as.numeric(NODE_WEIGHTS_1965$name)) * 2)
  )

UPSTREAM_NODES   <- c(2234, 2302)
DOWNSTREAM_NODES <- c(1782, 1772)

CONCAT_LINKS <- LINKS_1965_CONCAT %>%
  dplyr::select(IN_FID, NEAR_FID) %>%
  slice(1:4) %>%
  mutate(across(everything(), ~ NA))

CONCAT_LINKS$IN_FID   <- c(DOWNSTREAM_NODES, DOWNSTREAM_NODES)
CONCAT_LINKS$NEAR_FID <- c(UPSTREAM_NODES, UPSTREAM_NODES)

CONCAT_LINKS <- CONCAT_LINKS %>%
  mutate(IN_FID   = as.character(IN_FID),
         NEAR_FID = as.character(NEAR_FID)) %>%  
  mutate(pass_u   = 1,
         pass_d   = 1,
         TYPE     = "LINK") %>%
  rename(FROM = IN_FID, TO = NEAR_FID)

CONCAT_LINKS$FROM <- c(DOWNSTREAM_NODES,
                       DOWNSTREAM_NODES + 1 +  max(as.numeric(NODE_WEIGHTS_1965$name)) * 1)
CONCAT_LINKS$TO   <- c(UPSTREAM_NODES   + 2 +  max(as.numeric(NODE_WEIGHTS_1965$name)) * 2,
                       UPSTREAM_NODES)

GRAPH_1965_CONCAT <- graph_from_data_frame(
  d = LINKS_1965_CONCAT %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)) %>%
    rbind(CONCAT_LINKS),
  
  vertices = NODE_WEIGHTS_1965_CONCAT,
  
  directed = T)

GRAPH_1965_DECOMP_TEMP_CONCAT <- decompose(GRAPH_1965_CONCAT)
GRAPH_1965_DECOMP_CONCAT <- list()
GRAPH_1965_DECOMP_CONCAT[[1]] <- 1:length(GRAPH_1965_DECOMP_TEMP_CONCAT)
GRAPH_1965_DECOMP_CONCAT <- c(GRAPH_1965_DECOMP_CONCAT, GRAPH_1965_DECOMP_TEMP_CONCAT)

### 1996 ####

NODE_WEIGHTS_1996_CONCAT <- NODE_WEIGHTS_1996 %>%
  rbind(NODE_WEIGHTS_1996 %>%
          mutate(name = as.numeric(name) + 1 +  max(as.numeric(NODE_WEIGHTS_1996$name)) * 1) %>%
          mutate(name = as.character(name)),
        NODE_WEIGHTS_1996 %>%
          mutate(name = as.numeric(name) + 2 +  max(as.numeric(NODE_WEIGHTS_1996$name)) * 2) %>%
          mutate(name = as.character(name))
  )

LINKS_1996_CONCAT <- LINKS_1996 %>%
  rbind(LINKS_1996 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1996$name)) * 1) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 1 + max(as.numeric(NODE_WEIGHTS_1996$name)) * 1),
        LINKS_1996 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 2 + max(as.numeric(NODE_WEIGHTS_1996$name)) * 2) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 2 + max(as.numeric(NODE_WEIGHTS_1996$name)) * 2)
  )

# ggplotly(ggplot(NODES_1996)+
#   geom_sf(aes(text = FID)))

UPSTREAM_NODES   <- c(2135, 2164)
DOWNSTREAM_NODES <- c(1599, 1589)

CONCAT_LINKS <- LINKS_1996_CONCAT %>%
  dplyr::select(IN_FID, NEAR_FID) %>%
  slice(1:4) %>%
  mutate(across(everything(), ~ NA))

CONCAT_LINKS$IN_FID   <- c(DOWNSTREAM_NODES, DOWNSTREAM_NODES)
CONCAT_LINKS$NEAR_FID <- c(UPSTREAM_NODES, UPSTREAM_NODES)

CONCAT_LINKS <- CONCAT_LINKS %>%
  mutate(IN_FID   = as.character(IN_FID),
         NEAR_FID = as.character(NEAR_FID)) %>%  
  mutate(pass_u   = 1,
         pass_d   = 1,
         TYPE     = "LINK") %>%
  rename(FROM = IN_FID, TO = NEAR_FID)

CONCAT_LINKS$FROM <- c(DOWNSTREAM_NODES,
                       DOWNSTREAM_NODES + 1 +  max(as.numeric(NODE_WEIGHTS_1996$name)) * 1)
CONCAT_LINKS$TO   <- c(UPSTREAM_NODES   + 2 +  max(as.numeric(NODE_WEIGHTS_1996$name)) * 2,
                       UPSTREAM_NODES)

GRAPH_1996_CONCAT <- graph_from_data_frame(
  d = LINKS_1996_CONCAT %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)) %>%
    rbind(CONCAT_LINKS),
  
  vertices = NODE_WEIGHTS_1996_CONCAT,
  
  directed = T)

GRAPH_1996_DECOMP_TEMP_CONCAT <- decompose(GRAPH_1996_CONCAT)
GRAPH_1996_DECOMP_CONCAT <- list()
GRAPH_1996_DECOMP_CONCAT[[1]] <- 1:length(GRAPH_1996_DECOMP_TEMP_CONCAT)
GRAPH_1996_DECOMP_CONCAT <- c(GRAPH_1996_DECOMP_CONCAT, GRAPH_1996_DECOMP_TEMP_CONCAT)

# is_connected(GRAPH_1996_DECOMP_TEMP_CONCAT[[1]])

### 2022 ####

NODE_WEIGHTS_2022_CONCAT <- NODE_WEIGHTS_2022 %>%
  rbind(NODE_WEIGHTS_2022 %>%
          mutate(name = as.numeric(name) + 1 +  max(as.numeric(NODE_WEIGHTS_2022$name)) * 1) %>%
          mutate(name = as.character(name)),
        NODE_WEIGHTS_2022 %>%
          mutate(name = as.numeric(name) + 2 +  max(as.numeric(NODE_WEIGHTS_2022$name)) * 2) %>%
          mutate(name = as.character(name))
  )

LINKS_2022_CONCAT <- LINKS_2022 %>%
  rbind(LINKS_2022 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 1 + max(as.numeric(NODE_WEIGHTS_2022$name)) * 1) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 1 + max(as.numeric(NODE_WEIGHTS_2022$name)) * 1),
        LINKS_2022 %>%
          mutate(IN_FID = as.numeric(IN_FID) + 2 + max(as.numeric(NODE_WEIGHTS_2022$name)) * 2) %>%
          mutate(NEAR_FID = as.numeric(NEAR_FID) + 2 + max(as.numeric(NODE_WEIGHTS_2022$name)) * 2)
  )

UPSTREAM_NODES   <- c(2001, 2002)
DOWNSTREAM_NODES <- c(1343, 1434)

CONCAT_LINKS <- LINKS_2022_CONCAT %>%
  dplyr::select(IN_FID, NEAR_FID) %>%
  slice(1:4) %>%
  mutate(across(everything(), ~ NA))

CONCAT_LINKS$IN_FID   <- c(DOWNSTREAM_NODES, DOWNSTREAM_NODES)
CONCAT_LINKS$NEAR_FID <- c(UPSTREAM_NODES, UPSTREAM_NODES)

CONCAT_LINKS <- CONCAT_LINKS %>%
  mutate(IN_FID   = as.character(IN_FID),
         NEAR_FID = as.character(NEAR_FID)) %>%  
  mutate(pass_u   = 1,
         pass_d   = 1,
         TYPE     = "LINK") %>%
  rename(FROM = IN_FID, TO = NEAR_FID)

CONCAT_LINKS$FROM <- c(DOWNSTREAM_NODES,
                       DOWNSTREAM_NODES + 1 +  max(as.numeric(NODE_WEIGHTS_2022$name)) * 1)
CONCAT_LINKS$TO   <- c(UPSTREAM_NODES   + 2 +  max(as.numeric(NODE_WEIGHTS_2022$name)) * 2,
                       UPSTREAM_NODES)

GRAPH_2022_CONCAT <- graph_from_data_frame(
  d = LINKS_2022_CONCAT %>%
    rename(FROM = IN_FID, TO = NEAR_FID) %>%
    mutate(FROM = as.character(FROM),
           TO = as.character(TO)) %>%
    rbind(CONCAT_LINKS),
  
  vertices = NODE_WEIGHTS_2022_CONCAT,
  
  directed = T)

GRAPH_2022_DECOMP_TEMP_CONCAT <- decompose(GRAPH_2022_CONCAT)
GRAPH_2022_DECOMP_CONCAT <- list()
GRAPH_2022_DECOMP_CONCAT[[1]] <- 1:length(GRAPH_2022_DECOMP_TEMP_CONCAT)
GRAPH_2022_DECOMP_CONCAT <- c(GRAPH_2022_DECOMP_CONCAT, GRAPH_2022_DECOMP_TEMP_CONCAT)

## GRAPH LIST ####

GRAPHS <- list(GRAPH_1817_DECOMP, GRAPH_1857_DECOMP, GRAPH_1875_DECOMP, GRAPH_1910_DECOMP, GRAPH_1965_DECOMP, GRAPH_1996_DECOMP, GRAPH_2022_DECOMP)
GRAPHS <- list(GRAPHS, c(1817, 1857, 1875, 1910, 1965, 1996, 2022) %>% as.character())

GRAPHS_CONCAT <- list(GRAPH_1817_DECOMP_CONCAT, GRAPH_1857_DECOMP_CONCAT, GRAPH_1875_DECOMP_CONCAT, GRAPH_1910_DECOMP_CONCAT, GRAPH_1965_DECOMP_CONCAT, GRAPH_1996_DECOMP_CONCAT, GRAPH_2022_DECOMP_CONCAT)
GRAPHS_CONCAT <- list(GRAPHS_CONCAT, c(1817, 1857, 1875, 1910, 1965, 1996, 2022) %>% as.character())

# ~~~~~~~~~~~~~~~~~~~~ ####       
# ~~~~~~ ><{{{°> ~~~~~ ####
# ~~~~~~~~~~~~~~~~~~~~ ####       
# CALCULATE INDICES ####
## CONFIGURATION PARAMETERS ####

PARAMS_MASTER <- tibble(DISP                   = c(rep("MDM", 4), rep("SDM", 4), rep("PD", 4)),
                        SC                     = rep(c("1", "2", "3", "4"), 3),
                        DIST                   = rep(c("DSC1", "DSC2", "DSC3", "DSC4"), 3),
                        nodes_id               = rep("name", 4*3),
                        index_type             = rep("full", 4*3),
                        dir_fragmentation_type = rep("asymmetric", 4*3),
                        pass_u                 = rep("pass_u", 4*3),
                        pass_d                 = rep("pass_d", 4*3),
                        B_ij_flag              = rep(TRUE, 4*3),
                        c_ij_flag              = rep(TRUE, 4*3),
                        dir_distance_type      = c(rep("symmetric", 8), rep("asymmetric", 4)),
                        disp_type              = c(rep("leptokurtic", 8), rep("exponential", 4)))

PARAMS_MASTER <- PARAMS_MASTER %>%
  rbind(PARAMS_MASTER %>% mutate(index_type = "reach")) %>%
  mutate(weight  = paste("WSC", SC, sep = ""),
         param_u = ifelse(DISP == "PD", 0, NA),
         param_d = ifelse(DISP == "PD", 0.05^(1/2000), NA),

         param_l = case_when(

           DISP == "PD" ~ NA,
           DISP == "MDM" ~ "2600, 74200, .5",
           DISP == "SDM" ~ "260, 7420, .5")
  )

## NETWORK DISTANCES ####
### CCI ####

PARAMS <- PARAMS_MASTER %>%
  filter(!(DISP == "MDM" &
             !SC %in% c(1,2)) &
           index_type == "full")

#### 1817 ####

INDICES_1817_LIST <- index_calculation_2(GRAPHS[[1]][[1]][-1], PARAMS, 1817)
gc()

INDICES_1817 <- do.call(rbind, lapply(INDICES_1817_LIST, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1817)
CCI_1817     <- INDICES_1817 %>% filter(index_type ==  "full") %>% dplyr::select(-name)

#### 1857 ####

INDICES_1857_LIST <- index_calculation_2(GRAPHS[[1]][[2]][-1], PARAMS, 1857)
gc()

INDICES_1857 <- do.call(rbind, lapply(INDICES_1857_LIST, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1857)
CCI_1857     <- INDICES_1857 %>% filter(index_type ==  "full") %>% dplyr::select(-name)

#### 1875 ####

INDICES_1875_LIST <- index_calculation_2(GRAPHS[[1]][[3]][-1], PARAMS, 1875)
gc()

INDICES_1875 <- do.call(rbind, lapply(INDICES_1875_LIST, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1875)
CCI_1875     <- INDICES_1875 %>% filter(index_type ==  "full") %>% dplyr::select(-name)

#### 1910 ####

INDICES_1910_LIST <- index_calculation_2(GRAPHS[[1]][[4]][-1], PARAMS, 1910)
gc()

INDICES_1910 <- do.call(rbind, lapply(INDICES_1910_LIST, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1910)
CCI_1910     <- INDICES_1910 %>% filter(index_type ==  "full") %>% dplyr::select(-name)

#### 1965 ####

INDICES_1965_LIST <- index_calculation_2(GRAPHS[[1]][[5]][-1], PARAMS, 1965)
gc()

INDICES_1965 <- do.call(rbind, lapply(INDICES_1965_LIST, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1965)
CCI_1965     <- INDICES_1965 %>% filter(index_type ==  "full") %>% dplyr::select(-name)

#### 1996 ####

INDICES_1996_LIST <- index_calculation_2(GRAPHS[[1]][[6]][-1], PARAMS, 1996)
gc()

INDICES_1996 <- do.call(rbind, lapply(INDICES_1996_LIST, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1996)
CCI_1996     <- INDICES_1996 %>% filter(index_type ==  "full") %>% dplyr::select(-name)

#### 2022 ####

INDICES_2022_LIST <- index_calculation_2(GRAPHS[[1]][[7]][-1], PARAMS, 2022)
gc()

INDICES_2022 <- do.call(rbind, lapply(INDICES_2022_LIST, function(x) do.call(rbind, x))) %>% mutate(YEAR = 2022)
CCI_2022     <- INDICES_2022 %>% filter(index_type ==  "full") %>% dplyr::select(-name)

#### COMBINE YEARS ####

CCI <- rbind(
  CCI_1817,
  CCI_1857,
  CCI_1875,
  CCI_1910,
  CCI_1965,
  CCI_1996,
  CCI_2022
)

CCI_PROCESSED <- CCI %>%

  group_by(YEAR, CONF_ID) %>%
  summarise(num = sum(num),
            den = sum(den),
            across(names(PARAMS), first),
            .groups = "drop") %>%
  ungroup() %>%
  rename(NUM = num,
         DEN = den) %>%
  mutate(CCI = NUM/DEN) %>%
  dplyr::select(CCI, NUM, DEN, YEAR, CONF_ID, names(PARAMS)) %>%
  dplyr::select(-weight)

### RCI ####

PARAMS <- PARAMS_MASTER %>%
  filter(!(DISP == "MDM" &
             !SC %in% c(1,2)) &
           index_type == "reach")

#### 1817 ####

INDICES_1817_LIST_CONCAT <- index_calculation_2(GRAPHS_CONCAT[[1]][[1]][-1], PARAMS, 1817)

INDICES_1817_CONCAT <- do.call(rbind, lapply(INDICES_1817_LIST_CONCAT, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1817)
RCI_1817_CONCAT     <- INDICES_1817_CONCAT %>% filter(index_type == "reach")

#### 1857 ####

INDICES_1857_LIST_CONCAT <- index_calculation_2(GRAPHS_CONCAT[[1]][[2]][-1], PARAMS, 1857)

INDICES_1857_CONCAT <- do.call(rbind, lapply(INDICES_1857_LIST_CONCAT, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1857)
RCI_1857_CONCAT     <- INDICES_1857_CONCAT %>% filter(index_type == "reach")

#### 1875 ####

INDICES_1875_LIST_CONCAT <- index_calculation_2(GRAPHS_CONCAT[[1]][[3]][-1], PARAMS, 1875)

INDICES_1875_CONCAT <- do.call(rbind, lapply(INDICES_1875_LIST_CONCAT, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1875)
RCI_1875_CONCAT     <- INDICES_1875_CONCAT %>% filter(index_type == "reach")

#### 1910 ####

INDICES_1910_LIST_CONCAT <- index_calculation_2(GRAPHS_CONCAT[[1]][[4]][-1], PARAMS, 1910)

INDICES_1910_CONCAT <- do.call(rbind, lapply(INDICES_1910_LIST_CONCAT, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1910)
RCI_1910_CONCAT     <- INDICES_1910_CONCAT %>% filter(index_type == "reach")

#### 1965 ####

INDICES_1965_LIST_CONCAT <- index_calculation_2(GRAPHS_CONCAT[[1]][[5]][-1], PARAMS, 1965)

INDICES_1965_CONCAT <- do.call(rbind, lapply(INDICES_1965_LIST_CONCAT, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1965)
RCI_1965_CONCAT     <- INDICES_1965_CONCAT %>% filter(index_type == "reach")

#### 1996 ####

INDICES_1996_LIST_CONCAT <- index_calculation_2(GRAPHS_CONCAT[[1]][[6]][-1], PARAMS, 1996)

INDICES_1996_CONCAT <- do.call(rbind, lapply(INDICES_1996_LIST_CONCAT, function(x) do.call(rbind, x))) %>% mutate(YEAR = 1996)
RCI_1996_CONCAT     <- INDICES_1996_CONCAT %>% filter(index_type == "reach")

#### 2022 ####

INDICES_2022_LIST_CONCAT <- index_calculation_2(GRAPHS_CONCAT[[1]][[7]][-1], PARAMS, 2022)

INDICES_2022_CONCAT <- do.call(rbind, lapply(INDICES_2022_LIST_CONCAT, function(x) do.call(rbind, x))) %>% mutate(YEAR = 2022)
RCI_2022_CONCAT     <- INDICES_2022_CONCAT %>% filter(index_type == "reach")

#### COMBINE YEARS ####

RCI_CONCAT <- rbind(
  RCI_1817_CONCAT,
  RCI_1857_CONCAT,
  RCI_1875_CONCAT,
  RCI_1910_CONCAT,
  RCI_1965_CONCAT,
  RCI_1996_CONCAT,
  RCI_2022_CONCAT
)

RCI_PROCESSED_CONCAT <- RCI_CONCAT %>%
  rename(DEN_G = den,
         FID = name,
         NUM = num)

RCI_DEN_CONCAT <- RCI_PROCESSED_CONCAT %>%

  group_by(YEAR, CONF_ID) %>%
  summarise("DEN" = sum(DEN_G))

RCI_PROCESSED_CONCAT <- RCI_PROCESSED_CONCAT %>%
  left_join(RCI_DEN_CONCAT, by = c("YEAR", "CONF_ID")) %>%
  mutate(RCI = NUM/DEN) %>%
  dplyr::select(RCI, NUM, DEN, DEN_G, FID, ID_G, YEAR, CONF_ID, names(PARAMS))

CONCAT_KEY <- tibble(
  YEAR     = c(1817, 1857, 1875, 1910, 1965, 1996, 2022),
  NR_NODES = c(nrow(NODES_1817), nrow(NODES_1857), nrow(NODES_1875), nrow(NODES_1910),
               nrow(NODES_1965), nrow(NODES_1996), nrow(NODES_2022))
)

RCI_PROCESSED_CONCAT <- RCI_PROCESSED_CONCAT %>%
  left_join(CONCAT_KEY, by = "YEAR") %>%
  mutate(FID = as.numeric(FID)) %>%
  filter(FID <= NR_NODES-1)

WEIGHTS <- rbind(
  NODE_WEIGHTS_1817 %>% mutate(YEAR = 1817),
  NODE_WEIGHTS_1857 %>% mutate(YEAR = 1857),
  NODE_WEIGHTS_1875 %>% mutate(YEAR = 1875),
  NODE_WEIGHTS_1910 %>% mutate(YEAR = 1910),
  NODE_WEIGHTS_1965 %>% mutate(YEAR = 1965),
  NODE_WEIGHTS_1996 %>% mutate(YEAR = 1996),
  NODE_WEIGHTS_2022 %>% mutate(YEAR = 2022)
) %>%
  dplyr::select(-contains("DSC")) %>%
  pivot_longer(cols = c("WSC1", "WSC2", "WSC3", "WSC4"),
               names_to = "SC",
               values_to = "WEIGHT") %>%
  mutate(name = as.numeric(name),
         SC = str_sub(SC, -1)) %>%
  rename(FID = name) %>%

  left_join(rbind(
    NODES_1817 %>% mutate(YEAR = 1817, FID = as.numeric(FID)) %>% dplyr::select(FID, YEAR, WB_TYPE),
    NODES_1857 %>% mutate(YEAR = 1857, FID = as.numeric(FID)) %>% dplyr::select(FID, YEAR, WB_TYPE),
    NODES_1875 %>% mutate(YEAR = 1875, FID = as.numeric(FID)) %>% dplyr::select(FID, YEAR, WB_TYPE),
    NODES_1910 %>% mutate(YEAR = 1910, FID = as.numeric(FID)) %>% dplyr::select(FID, YEAR, WB_TYPE),
    NODES_1965 %>% mutate(YEAR = 1965, FID = as.numeric(FID)) %>% dplyr::select(FID, YEAR, WB_TYPE),
    NODES_1996 %>% mutate(YEAR = 1996, FID = as.numeric(FID)) %>% dplyr::select(FID, YEAR, WB_TYPE),
    NODES_2022 %>% mutate(YEAR = 2022, FID = as.numeric(FID)) %>% dplyr::select(FID, YEAR, WB_TYPE)
  ), by = c("FID", "YEAR")) %>%
  dplyr::relocate(geometry, .after = last_col())

RCI_PROCESSED_CONCAT <- RCI_PROCESSED_CONCAT %>%

  dplyr::select(-contains(c("flag", "pass", "index_type", "nodes_id", "dir", "param", "disp_type", "weight", "NR_NODES"))) %>%
  left_join(WEIGHTS, by = c("FID", "YEAR", "SC")) %>%
  st_as_sf() %>%
  mutate(ID = row_number()) %>%
  dplyr::select(ID, everything())

## EUCLIDEAN DISTANCES ####
### RCI ####
#### 1817 ####

NODE_1817_CENTROID <- NODES_1817 %>%
  st_line_sample(sample = 0.5) %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  cbind(NODES_1817 %>% st_drop_geometry()) %>%
  left_join(NODE_WEIGHTS_1817, by = c("FID" = "name")) %>%
  rename(geometry = x) %>%
  st_as_sf()

COORDS        <- st_coordinates(NODE_1817_CENTROID)
DIST_MAT      <- as.matrix(dist(COORDS))
WEIGHTS       <- paste0("WSC", 1:4)
TR            <- 2e+3
KERNEL_FACTOR <- 0.05^(1/TR)

for (w in WEIGHTS) {
  cat("Processing", w, "\n")
  W_VALS <- NODE_1817_CENTROID[[w]]
  diag(DIST_MAT) <- NA
  NODE_1817_CENTROID[[paste0("RCI_EUC_", w)]] <- rowSums(W_VALS * (KERNEL_FACTOR^DIST_MAT), na.rm = TRUE)
}

EUC_1817 <- NODE_1817_CENTROID %>%
  st_drop_geometry() %>%
  dplyr::select(FID, starts_with("RCI")) %>%
  mutate(YEAR = 1817) %>%
  relocate(YEAR, .after = FID)

#### 1857 ####

NODE_1857_CENTROID <- NODES_1857 %>%
  st_line_sample(sample = 0.5) %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  cbind(NODES_1857 %>% st_drop_geometry()) %>%
  left_join(NODE_WEIGHTS_1857, by = c("FID" = "name")) %>%
  rename(geometry = x) %>%
  st_as_sf()

COORDS        <- st_coordinates(NODE_1857_CENTROID)
DIST_MAT      <- as.matrix(dist(COORDS))
WEIGHTS       <- paste0("WSC", 1:4)
TR            <- 2e+3
KERNEL_FACTOR <- 0.05^(1/TR)

for (w in WEIGHTS) {
  cat("Processing", w, "\n")
  W_VALS <- NODE_1857_CENTROID[[w]]
  diag(DIST_MAT) <- NA
  NODE_1857_CENTROID[[paste0("RCI_EUC_", w)]] <- rowSums(W_VALS * (KERNEL_FACTOR^DIST_MAT), na.rm = TRUE)
}

EUC_1857 <- NODE_1857_CENTROID %>%
  st_drop_geometry() %>%
  dplyr::select(FID, starts_with("RCI")) %>%
  mutate(YEAR = 1857) %>%
  relocate(YEAR, .after = FID)

#### 1875 ####

NODE_1875_CENTROID <- NODES_1875 %>%
  st_line_sample(sample = 0.5) %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  cbind(NODES_1875 %>% st_drop_geometry()) %>%
  left_join(NODE_WEIGHTS_1875, by = c("FID" = "name")) %>%
  rename(geometry = x) %>%
  st_as_sf()

COORDS        <- st_coordinates(NODE_1875_CENTROID)
DIST_MAT      <- as.matrix(dist(COORDS))
WEIGHTS       <- paste0("WSC", 1:4)
TR            <- 2e+3
KERNEL_FACTOR <- 0.05^(1/TR)

for (w in WEIGHTS) {
  cat("Processing", w, "\n")
  W_VALS <- NODE_1875_CENTROID[[w]]
  diag(DIST_MAT) <- NA
  NODE_1875_CENTROID[[paste0("RCI_EUC_", w)]] <- rowSums(W_VALS * (KERNEL_FACTOR^DIST_MAT), na.rm = TRUE)
}

EUC_1875 <- NODE_1875_CENTROID %>%
  st_drop_geometry() %>%
  dplyr::select(FID, starts_with("RCI")) %>%
  mutate(YEAR = 1875) %>%
  relocate(YEAR, .after = FID)

#### 1910 ####

NODE_1910_CENTROID <- NODES_1910 %>%
  st_line_sample(sample = 0.5) %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  cbind(NODES_1910 %>% st_drop_geometry()) %>%
  left_join(NODE_WEIGHTS_1910, by = c("FID" = "name")) %>%
  rename(geometry = x) %>%
  st_as_sf()

COORDS        <- st_coordinates(NODE_1910_CENTROID)
DIST_MAT      <- as.matrix(dist(COORDS))
WEIGHTS       <- paste0("WSC", 1:4)
TR            <- 2e+3
KERNEL_FACTOR <- 0.05^(1/TR)

for (w in WEIGHTS) {
  cat("Processing", w, "\n")
  W_VALS <- NODE_1910_CENTROID[[w]]
  diag(DIST_MAT) <- NA
  NODE_1910_CENTROID[[paste0("RCI_EUC_", w)]] <- rowSums(W_VALS * (KERNEL_FACTOR^DIST_MAT), na.rm = TRUE)
}

EUC_1910 <- NODE_1910_CENTROID %>%
  st_drop_geometry() %>%
  dplyr::select(FID, starts_with("RCI")) %>%
  mutate(YEAR = 1910) %>%
  relocate(YEAR, .after = FID)

#### 1965 ####

NODE_1965_CENTROID <- NODES_1965 %>%
  st_line_sample(sample = 0.5) %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  cbind(NODES_1965 %>% st_drop_geometry()) %>%
  left_join(NODE_WEIGHTS_1965, by = c("FID" = "name")) %>%
  rename(geometry = x) %>%
  st_as_sf()

COORDS        <- st_coordinates(NODE_1965_CENTROID)
DIST_MAT      <- as.matrix(dist(COORDS))
WEIGHTS       <- paste0("WSC", 1:4)
TR            <- 2e+3
KERNEL_FACTOR <- 0.05^(1/TR)

for (w in WEIGHTS) {
  cat("Processing", w, "\n")
  W_VALS <- NODE_1965_CENTROID[[w]]
  diag(DIST_MAT) <- NA
  NODE_1965_CENTROID[[paste0("RCI_EUC_", w)]] <- rowSums(W_VALS * (KERNEL_FACTOR^DIST_MAT), na.rm = TRUE)
}

EUC_1965 <- NODE_1965_CENTROID %>%
  st_drop_geometry() %>%
  dplyr::select(FID, starts_with("RCI")) %>%
  mutate(YEAR = 1965) %>%
  relocate(YEAR, .after = FID)

#### 1996 ####

NODE_1996_CENTROID <- NODES_1996 %>%
  st_line_sample(sample = 0.5) %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  cbind(NODES_1996 %>% st_drop_geometry()) %>%
  left_join(NODE_WEIGHTS_1996, by = c("FID" = "name")) %>%
  rename(geometry = x) %>%
  st_as_sf()

COORDS        <- st_coordinates(NODE_1996_CENTROID)
DIST_MAT      <- as.matrix(dist(COORDS))
WEIGHTS       <- paste0("WSC", 1:4)
TR            <- 2e+3
KERNEL_FACTOR <- 0.05^(1/TR)

for (w in WEIGHTS) {
  cat("Processing", w, "\n")
  W_VALS <- NODE_1996_CENTROID[[w]]
  diag(DIST_MAT) <- NA
  NODE_1996_CENTROID[[paste0("RCI_EUC_", w)]] <- rowSums(W_VALS * (KERNEL_FACTOR^DIST_MAT), na.rm = TRUE)
}

EUC_1996 <- NODE_1996_CENTROID %>%
  st_drop_geometry() %>%
  dplyr::select(FID, starts_with("RCI")) %>%
  mutate(YEAR = 1996) %>%
  relocate(YEAR, .after = FID)

#### 2022 ####

NODE_2022_CENTROID <- NODES_2022 %>%
  st_line_sample(sample = 0.5) %>%
  st_cast("POINT") %>%
  st_as_sf() %>%
  cbind(NODES_2022 %>% st_drop_geometry()) %>%
  left_join(NODE_WEIGHTS_2022, by = c("FID" = "name")) %>%
  rename(geometry = x) %>%
  st_as_sf()

COORDS        <- st_coordinates(NODE_2022_CENTROID)
DIST_MAT      <- as.matrix(dist(COORDS))
WEIGHTS       <- paste0("WSC", 1:4)
TR            <- 2e+3
KERNEL_FACTOR <- 0.05^(1/TR)

for (w in WEIGHTS) {
  cat("Processing", w, "\n")
  W_VALS <- NODE_2022_CENTROID[[w]]
  diag(DIST_MAT) <- NA
  NODE_2022_CENTROID[[paste0("RCI_EUC_", w)]] <- rowSums(W_VALS * (KERNEL_FACTOR^DIST_MAT), na.rm = TRUE)
}

EUC_2022 <- NODE_2022_CENTROID %>%
  st_drop_geometry() %>%
  dplyr::select(FID, starts_with("RCI")) %>%
  mutate(YEAR = 2022) %>%
  relocate(YEAR, .after = FID)

#### COMBINE YEARS ####

RCI_EUC <- rbind(

  EUC_1817,
  EUC_1857,
  EUC_1875,
  EUC_1910,
  EUC_1965,
  EUC_1996,
  EUC_2022

) %>%

  set_names(c("FID", "YEAR", c(1:4))) %>%
  pivot_longer(cols = as.character(c(1:4)), names_to = "SC", values_to = "NUM")

### CCI ####

CCI_EUC <- list()

for (year in c(1817, 1857, 1875, 1910, 1965, 1996, 2022)) {

  cat("Processing year:", year, "\n")

  NODE_CENTROID <- get(paste0("NODE_", year, "_CENTROID"))
  COORDS        <- st_coordinates(NODE_CENTROID)
  DIST_MATRIX   <- as.matrix(dist(COORDS))
  KERNEL_FACTOR <- 0.05^(1/TR)

  CCI_EUC_WIDE <- RCI_EUC %>%
    filter(YEAR == year) %>%
    mutate(SC = paste0("SC", SC)) %>%
    pivot_wider(names_from = SC, values_from = NUM)

  for (sc in paste0("SC", 1:4)) {

    cat("Processing: ", sc, "\n")

    W <- CCI_EUC_WIDE[[sc]]
    CCI_VAL <- sum(outer(W, W) * (KERNEL_FACTOR^DIST_MATRIX))
    CCI_EUC[[paste0("CCI_", sc, "_", year)]] <- data.frame(
      YEAR = year,
      SC = sc,
      NUM = CCI_VAL
    )
  }
}

CCI_EUC <- do.call(rbind, CCI_EUC)

## RCI MASTER TABLE ####

RCI_EUC_TEMP <- RCI_PROCESSED_CONCAT %>%
  filter(DISP == "PD") %>%
  mutate(DISP =  "EUC",
         ID             = row_number() + nrow(RCI_PROCESSED_CONCAT)) %>%
  mutate(CONF_ID = case_when(

    SC == 1 ~ nrow(PARAMS_MASTER %>% filter(index_type == "reach")) + 1,
    SC == 2 ~ nrow(PARAMS_MASTER %>% filter(index_type == "reach")) + 2,
    SC == 3 ~ nrow(PARAMS_MASTER %>% filter(index_type == "reach")) + 3,
    SC == 4 ~ nrow(PARAMS_MASTER %>% filter(index_type == "reach")) + 4

  )) %>%
  dplyr::select(-RCI, -NUM) %>%
  left_join(RCI_EUC %>% mutate(FID = as.numeric(FID)), by = c("FID", "YEAR", "SC")) %>%
  mutate(RCI = NUM/DEN) %>%
  dplyr::select(ID, RCI, NUM, everything())

RCI_MASTER <- RCI_PROCESSED_CONCAT %>%
  rbind(RCI_EUC_TEMP) %>%
  st_as_sf() %>%
  dplyr::select(-DIST)

## CCI MASTER TABLE ####

CCI_NET <- CCI_PROCESSED %>%
  dplyr::select(CCI, intersect(names(CCI_PROCESSED), names(RCI_MASTER)))

CCI_EUC <- CCI_NET %>%
  filter(DISP == "PD") %>%
  mutate(DISP = "EUC") %>%
  mutate(CONF_ID = case_when(

    SC == 1 ~ nrow(PARAMS_MASTER %>% filter(index_type == "reach")) + 1,
    SC == 2 ~ nrow(PARAMS_MASTER %>% filter(index_type == "reach")) + 2,
    SC == 3 ~ nrow(PARAMS_MASTER %>% filter(index_type == "reach")) + 3,
    SC == 4 ~ nrow(PARAMS_MASTER %>% filter(index_type == "reach")) + 4

  )) %>%
  dplyr::select(-CCI, -NUM) %>%
  left_join(CCI_EUC %>%
              mutate(SC = str_sub(SC, -1, -1)), by = c("YEAR", "SC")) %>%
  mutate(CCI = NUM/DEN) %>%
  dplyr::select(CCI, NUM, everything())

CCI_MASTER <- CCI_NET %>%
  rbind(CCI_EUC) %>%
  mutate(ID = row_number()) %>%
  relocate(ID, .before = CCI) %>%
  dplyr::select(-DIST)

# ~~~~~~~~~~~~~~~~~~~~ ####
# ~~~~~~ ><{{{°> ~~~~~ ####
# ~~~~~~~~~~~~~~~~~~~~ ####
# VALIDATION ####

RCI_MASTER_2022 <- RCI_MASTER %>%
  filter(YEAR == 2022 & WB_TYPE != "CO")

VALIDATION <- SAMPLES %>%
  left_join(SC_TAXA %>%
              dplyr::select(-ID_TAXA, -DISP, -GROUP), by = c("GENUS", "SPECIES")) %>%
  filter(ERROR_CUM <= 5 & ERROR_MAX <= 2) %>%
  filter(SC != 5) %>%
  st_join(RCI_MASTER_2022 %>%
            dplyr::select(FID, geometry) %>%
            unique() %>%
            rename(FID_RCI = FID), join = st_nearest_feature) %>%
  relocate(FID_RCI, .after = ID_SMPL) %>%
  left_join(RCI_MASTER_2022 %>%
              st_drop_geometry() %>%
              rename(FID_RCI = FID) %>%
              mutate(SC = as.numeric(SC)) %>%
              dplyr::select(FID_RCI, SC, WEIGHT, DISP, WB_TYPE, RCI, NUM, DEN),
            by = c("FID_RCI", "SC", "DISP")) %>%
  relocate(geometry, .after = last_col()) %>%
  mutate(PRES = ifelse(PRES == 1, "Present", "Absent")) %>%
  mutate(SC   = factor(SC, levels = c(1:4)),
         PRES = factor(PRES, levels = rev(c("Absent", "Present"))),
         DISP = factor(DISP, levels = c("EUC", "PD", "SDM", "MDM"))) %>%
  group_by(DISP, SC) %>%
  mutate(NUM = scales::rescale(NUM),
         RCI = scales::rescale(RCI))

SIGNIFICANCE <- VALIDATION %>%
  st_drop_geometry() %>%
  dplyr::select(NUM, DISP, SC, PRES) %>%
  drop_na() %>%
  mutate(NUM = as.numeric(NUM)) %>%
  group_by(DISP, SC, PRES) %>%
  mutate(GROUP_SIZE = n()) %>%
  ungroup()

GROUP_SIZES <- SIGNIFICANCE %>%
  group_by(DISP, SC, PRES) %>%
  summarise(N = n(), .groups = "drop") %>%
  pivot_wider(names_from = PRES, values_from = N, names_prefix = "N_", values_fill = 0)

set.seed(42)
SIGNIFICANCE <- SIGNIFICANCE %>%
  group_by(DISP, SC) %>%
  mutate(MIN_SIZE = min(GROUP_SIZE)) %>%
  ungroup() %>%
  group_by(DISP, SC, PRES) %>%
  group_modify(~ slice_sample(.x, n = .x$MIN_SIZE[1])) %>%
  ungroup() %>%
  group_by(DISP, SC) %>%
  summarise(test = list(wilcox.test(NUM ~ PRES, alternative = "greater", exact = FALSE) %>% broom::tidy()), .groups = "drop") %>%
  unnest(test) %>%
  dplyr::select(DISP, SC, p.value) %>%
  mutate(LEVEL = case_when(
    p.value < 0.001 ~ "***",
    p.value <= 0.01 ~ "**",
    p.value <= 0.05 ~ "*",
    p.value <= 0.1 ~ " ",
    TRUE ~ " "
  )) %>%
  rename(P = p.value) %>%
  left_join(GROUP_SIZES, by = c("DISP", "SC"))

write_csv(SIGNIFICANCE, "./RESULTS/SIGNIFICANCE.csv")
st_write(VALIDATION, "./RESULTS/VALIDATION/VALIDATION.shp", append = FALSE)

# ~~~~~~~~~~~~~~~~~~~~ ####
# ~~~~~~ ><{{{°> ~~~~~ ####
# ~~~~~~~~~~~~~~~~~~~~ ####
# CC (CLUSTERS) ####

RCI_CLUSTER <- RCI_MASTER %>%
  st_drop_geometry() %>%
  filter(WB_TYPE != "CO") %>%
  dplyr::select(FID, YEAR, NUM, DISP, SC) %>%
  left_join(SIGNIFICANCE %>% dplyr::select(-LEVEL, -starts_with("N")),
            by = c("DISP", "SC")) %>%
  filter(P < .05) %>%
  dplyr::select(-P) %>%
  group_by(YEAR, FID) %>%
  mutate(SC = paste0("SC", SC)) %>%
  pivot_wider(names_from = c("SC", "DISP"),
              values_from = NUM) %>%
  ungroup() %>%
  mutate(ID = row_number()) %>%
  dplyr::select(ID, everything())

COR                 <- cor(RCI_CLUSTER %>% dplyr::select(starts_with("SC")), method = "kendall")
COR[abs(COR) <= .7] <- "ok"
diag(COR)           <- "ok"
REDUNDANT           <- c()
COR

REDUNDANT          <- c("SC3_PD")
REDUNDANT_i        <- which(colnames(COR) %in% REDUNDANT)
COR                <- COR[-REDUNDANT_i, -REDUNDANT_i]
COR

REDUNDANT          <- c(REDUNDANT, c("SC1_PD"))
REDUNDANT_i        <- which(colnames(COR) %in% REDUNDANT)
COR                <- COR[-REDUNDANT_i, -REDUNDANT_i]
COR

REDUNDANT          <- c(REDUNDANT, c("SC1_SDM"))
REDUNDANT_i        <- which(colnames(COR) %in% REDUNDANT)
COR                <- COR[-REDUNDANT_i, -REDUNDANT_i]
COR

REDUNDANT          <- c(REDUNDANT, c("SC4_PD"))
REDUNDANT_i        <- which(colnames(COR) %in% REDUNDANT)
COR                <- COR[-REDUNDANT_i, -REDUNDANT_i]
COR

REDUNDANT          <- c(REDUNDANT, c("SC3_SDM"))
REDUNDANT_i        <- which(colnames(COR) %in% REDUNDANT)
COR                <- COR[-REDUNDANT_i, -REDUNDANT_i]
COR

K_WSS_CC <- fviz_nbclust(RCI_CLUSTER %>%
                           dplyr::select(-FID, -YEAR, -contains(REDUNDANT)) %>%
                           column_to_rownames("ID") %>%
                           mutate(across(everything(), ~ scale(.x))) %>%
                           mutate(across(everything(), ~ log(.x+2))),
                         FUNcluster = cluster::clara,
                         method = "wss",
                         k.max = 10,
                         nboot = 25,
                         verbose = TRUE)

K_WSS_CC <- K_WSS_CC$data %>% as_tibble() %>% rename(K = clusters, WSS = y)

write_csv(K_WSS_CC, "./RESULTS/K_WSS_CC.csv")

CC <- cluster::clara(RCI_CLUSTER %>%
                       dplyr::select(-FID, -YEAR, -contains(REDUNDANT)) %>%
                       column_to_rownames("ID") %>%
                       mutate(across(everything(), ~ scale(.x))) %>%
                       mutate(across(everything(), ~ log(.x+2))),
                     k = 4)

RCI_MASTER <- RCI_MASTER %>%
  left_join(RCI_CLUSTER %>%
              dplyr::select(FID, YEAR) %>%
              cbind(CC = CC$clustering),
            by = c("FID", "YEAR")) %>%
  relocate(CC, .after = SC) %>%
  mutate(CC = factor(CC, levels = c(1,2,3,4)))

CLUSTER_VARIABLES <- RCI_CLUSTER %>%
  dplyr::select(starts_with("SC") & !all_of(REDUNDANT)) %>%
  names()

CLUSTER_VARIABLES <- tibble(TEMP = CLUSTER_VARIABLES) %>%
  separate(TEMP, into = c("SC", "DISP"), sep = "_") %>%
  mutate(SC    = str_sub(SC, -1, -1),
         LABEL = "(Clustering variable)")

st_write(RCI_MASTER, "./RESULTS/RCI_MASTER/RCI_MASTER.shp", append = FALSE)
write_csv(CLUSTER_VARIABLES, "./RESULTS/CLUSTER_VARIABLES.csv")

# ~~~~~~~~~~~~~~~~~~~~ ####
# ~~~~~~ ><{{{°> ~~~~~ ####
# ~~~~~~~~~~~~~~~~~~~~ ####
# RESILIENCE ####

## MULTILAYER GRAPHS ####

LAYERS  <- list()
YEARS   <- as.character(c(1817, 1857, 1875, 1910, 1965, 1996, 2022))

DISP    <- c("EUC", "PD", "SDM", "MDM")
SC      <- c("SC1", "SC2", "SC3", "SC4")

CONFIGS <- tidyr::crossing(DISP, SC) %>%
  unite("CONFIGS", DISP, SC, sep = "_") %>%
  filter(!CONFIGS %in% c("MDM_SC3", "MDM_SC4", "SDM_SC4")) %>%
  pull(CONFIGS)

MULTI_GRAPHS <- list()
EDGE_WEIGHTS <- tibble(YEAR = numeric(), CONF = character(), TYPE = character(), MEAN = numeric())

GRAPHS_SINGLE <- list(

  "1817" = GRAPH_1817,
  "1857" = GRAPH_1857,
  "1875" = GRAPH_1875,
  "1910" = GRAPH_1910,
  "1965" = GRAPH_1965,
  "1996" = GRAPH_1996,
  "2022" = GRAPH_2022

)

NODES  <- list(

  "1817" = NODES_1817,
  "1857" = NODES_1857,
  "1875" = NODES_1875,
  "1910" = NODES_1910,
  "1965" = NODES_1965,
  "1996" = NODES_1996,
  "2022" = NODES_2022

)

RCI_LIM <- RCI_MASTER %>%
  st_drop_geometry() %>%
  filter(WB_TYPE!= "CO") %>%
  dplyr::select(FID, YEAR, SC, DISP, NUM) %>%
  mutate(SC = paste0("SC", SC)) %>%
  mutate(CONFIG = paste(DISP, SC, sep = "_")) %>%
  group_by(CONFIG) %>%
  summarise(RCI_MAX = max(NUM, na.rm = TRUE), .groups = "drop")

DIST_LIM <- tibble(CONFIG = character(), DIST_MAX = numeric())

for (conf in CONFIGS) {

  GLOBAL_MAX <- 0

  for (year in YEARS) {

    G <- GRAPHS_SINGLE[[year]]

    CONF     <- str_split(conf, "_")[[1]][2]
    CONF_INT <- sub(".*(.)$", "\\1", CONF)
    CONF_LAB <- paste("DSC", CONF_INT, sep = "")

    EDGE_NODES <- ends(G, E(G), names = FALSE)

    VERTEX_ATTR <- vertex_attr(G, CONF_LAB)

    WEIGHT <- (0.5 * VERTEX_ATTR[EDGE_NODES[, 1]] + 0.5 * VERTEX_ATTR[EDGE_NODES[, 2]])

    YEAR_MAX <- max(WEIGHT, na.rm = TRUE)

    if (YEAR_MAX > GLOBAL_MAX) {
      GLOBAL_MAX <- YEAR_MAX
    }

  }

  DIST_LIM <- rbind(DIST_LIM, tibble(CONFIG = conf, DIST_MAX = GLOBAL_MAX))

}

for (year in 1:length(YEARS)) {

  cat("Preparing layers for ", YEARS[year], "\n")
  LAYERS[[year]] <- list()

  for (conf in 1:length(CONFIGS)) {

    LAYERS[[year]][[conf]] <- list()
    CONF                   <- str_split(CONFIGS[conf], pattern = "_")[[1]][2]
    CONF_INT               <- sub(".*(.)$", "\\1", CONF)
    CONF_LAB               <- paste("DSC", CONF_INT, sep = "")
    G                      <- GRAPHS_SINGLE[[YEARS[year]]]

    for(att in setdiff(vertex_attr_names(G), c("name", CONF_LAB))) {
      G <- delete_vertex_attr(G, att)
    }

    for (att in edge_attr_names(G)) {
      G <- delete_edge_attr(G, att)
    }

    RCI_MASTER_TEMP <- RCI_MASTER %>%
      st_drop_geometry() %>%
      dplyr::select(FID, YEAR, SC, CC, DISP, NUM) %>%
      mutate(SC     = paste0("SC", SC),
             CONFIG = CONFIGS[conf],
             CC     = ifelse(is.na(CC), 5, CC)) %>%
      filter(
        YEAR == YEARS[year],
        DISP == str_split(CONFIGS[conf], "_")[[1]][1],
        SC   == str_split(CONFIGS[conf], "_")[[1]][2]
      )

    VERTEX_ATTR <- vertex_attr(G) %>%
      as_tibble() %>%
      left_join(RCI_MASTER_TEMP %>% mutate(FID = as.character(FID)), by = c("name" = "FID")) %>%
      dplyr::select(-name)

    EDGE_NODES <- ends(G, E(G), names = FALSE)
    WEIGHT     <- (0.5 * dplyr::pull(VERTEX_ATTR, 1)[EDGE_NODES[, 1]] + 0.5 * dplyr::pull(VERTEX_ATTR, 1)[EDGE_NODES[, 2]])

    CONFIG                      <- CONFIGS[conf]

    RCI_LIM_CURR                <- RCI_LIM %>% filter(CONFIG == CONFIGS[conf])
    RCI                         <- VERTEX_ATTR$NUM
    RCI_S                       <- VERTEX_ATTR$NUM / RCI_LIM_CURR$RCI_MAX
    CC                          <- VERTEX_ATTR$CC
    G                           <- set_vertex_attr(G, name = "RCI", value = RCI)
    G                           <- set_vertex_attr(G, name = "RCI_S", value = RCI_S)
    G                           <- set_vertex_attr(G, name = "CC", value = CC)

    WEIGHT_LIM_CURR             <- DIST_LIM %>% filter(CONFIG == CONFIGS[conf])
    WEIGHT_S                    <- WEIGHT / WEIGHT_LIM_CURR$DIST_MAX
    G                           <- set_edge_attr(G, name = "WEIGHT", value = WEIGHT)
    G                           <- set_edge_attr(G, name = "WEIGHT_S", value = WEIGHT_S)

    G                           <- delete_vertex_attr(G, CONF_LAB)

    DISP_MODE <- str_split(CONFIGS[conf], "_")[[1]][1]

    if (DISP_MODE != "PD") {

      E_DF                       <- igraph::as_data_frame(G, what = "edges")
      E_RECIP                    <- E_DF
      E_RECIP[, c("from", "to")] <- E_DF[, c("to", "from")]

      G <- igraph::graph_from_data_frame(
        d        = rbind(E_DF, E_RECIP),
        directed = TRUE,
        vertices = igraph::as_data_frame(G, what = "vertices")
      )
      G <- igraph::simplify(G, edge.attr.comb = "first")

    }

    LAYERS[[year]][[conf]][[1]] <- YEARS[year]
    LAYERS[[year]][[conf]][[2]] <- CONFIGS[conf]
    LAYERS[[year]][[conf]][[3]] <- G

  }

}

for (year in 1:length(YEARS)) {

  for (conf in 1:length(CONFIGS)) {

    W            <- E(LAYERS[[year]][[conf]][[3]])$WEIGHT
    W_S          <- E(LAYERS[[year]][[conf]][[3]])$WEIGHT_S

    EDGE_WEIGHTS <- bind_rows(
      EDGE_WEIGHTS,

      tibble(YEAR   = as.numeric(YEARS[year]),
             CONF   = CONFIGS[conf],
             TYPE   = "W",
             MEAN   = mean(W[W > 0])),

      tibble(YEAR   = as.numeric(YEARS[year]),
             CONF   = CONFIGS[conf],
             TYPE   = "W_S",
             MEAN   = mean(W[W_S > 0]))
    )

  }
}

for (year in 1:length(YEARS)) {

  cat("Preparing multilayer graph for", YEARS[[year]], "\n")

  MULTI_GRAPHS[[year]]  <- list()
  MULTI_GRAPHS_YEAR     <- NULL
  TOTAL_V               <- 0

  for (conf in 1:length(CONFIGS)) {

    G_CONF          <- LAYERS[[year]][[conf]][[3]]
    V(G_CONF)$LAYER <- CONFIGS[conf]
    V(G_CONF)$REACH <- V(G_CONF)$name
    V(G_CONF)$name  <- paste0(V(G_CONF)$name, "_", CONFIGS[conf])

    if (is.null(MULTI_GRAPHS_YEAR)) {

      MULTI_GRAPHS_YEAR <- G_CONF
      TOTAL_V           <- vcount(G_CONF)

    } else {

      N_CURR            <- vcount(G_CONF)
      CURR_REACH        <- V(G_CONF)$REACH
      MULTI_GRAPHS_YEAR <- igraph::disjoint_union(MULTI_GRAPHS_YEAR, G_CONF)
      PREV_IDX          <- 1:TOTAL_V
      PREV_REACH        <- V(MULTI_GRAPHS_YEAR)$REACH[PREV_IDX]
      MATCH_LIST        <- split(PREV_IDX, PREV_REACH)

      INTER_EDGES_LIST  <- lapply(seq_along(CURR_REACH), function(i) {
        reach_id   <- CURR_REACH[i]
        prev_nodes <- MATCH_LIST[[reach_id]]

        if (!is.null(prev_nodes)) {
          return(cbind(prev_nodes, TOTAL_V + i))
        }

        return(NULL)

      })

      INTER_EDGES       <- do.call(rbind, INTER_EDGES_LIST)
      INTER_EDGES_BOTH  <- rbind(INTER_EDGES, INTER_EDGES[, 2:1])
      MULTI_GRAPHS_YEAR <- igraph::add_edges(MULTI_GRAPHS_YEAR, t(INTER_EDGES_BOTH))
      IDX               <- (ecount(MULTI_GRAPHS_YEAR) - nrow(INTER_EDGES_BOTH) + 1):ecount(MULTI_GRAPHS_YEAR)
      W                 <- filter(EDGE_WEIGHTS, YEAR == YEARS[year], CONF == CONFIGS[conf], TYPE == "W") %>% pull(MEAN)
      W_S               <- filter(EDGE_WEIGHTS, YEAR == YEARS[year], CONF == CONFIGS[conf], TYPE == "W_S") %>% pull(MEAN)

      edge_attr(MULTI_GRAPHS_YEAR, "WEIGHT",   IDX) <- W
      edge_attr(MULTI_GRAPHS_YEAR, "WEIGHT_S", IDX) <- W_S

      TOTAL_V           <- TOTAL_V + N_CURR

    }
  }

  MULTI_GRAPHS[[year]] <- list(
    YEAR = YEARS[year],
    G    = MULTI_GRAPHS_YEAR
  )

}

## METRICS ####
### ROBUSTNESS ####

# For a continuous progress update open the log-file via the windows console:
# powershell -command Get-Content 'ADD_PATH\PROGRESS_LOG_ROBUSTNESS.txt' -Wait

plan(multisession, workers = floor(parallel::detectCores() * 2/3))

N_SRC     <- 100
N_REPS    <- 100
YEARS     <- c(1817, 1857, 1875, 1910, 1965, 1996, 2022)
STEP_SIZE <- 1000
LOG_FILE  <- "PROGRESS_LOG_ROBUSTNESS.txt"

cat("Starting robustness analysis\n", file = LOG_FILE, append = FALSE)

ROBUSTNESS_LIST <- vector("list", length(YEARS))

for (year in seq_along(YEARS)) {

  CURRENT_YEAR <- YEARS[year]
  GRAPH        <- MULTI_GRAPHS[[year]]$G
  N0           <- vcount(GRAPH)
  ATTACK_STEPS <- seq(0, N0, by = STEP_SIZE)
  NUM_STEPS    <- length(ATTACK_STEPS)

  NODE_METADATA <- data.frame(
    INDEX = seq_len(vcount(GRAPH)),
    RCI   = vertex_attr(GRAPH, "RCI_S"),
    LAYER = vertex_attr(GRAPH, "LAYER")
  )

  ATTACK_ORDER_DATA <- NODE_METADATA %>%
    group_by(LAYER) %>%
    mutate(LAYER_RANK = rank(RCI, ties.method = "random")) %>%
    ungroup() %>%
    arrange(LAYER_RANK, LAYER)

  ATTACK_ORDER_TARGETED <- ATTACK_ORDER_DATA$INDEX

  cat(sprintf("\nCalculating robustness for %d\n", CURRENT_YEAR))

  YEAR_RESULTS <- future_lapply(seq_len(N_REPS), function(rep) {

    gc()
    REP_RESULTS  <- vector("list", NUM_STEPS)
    ATTACK_ORDER <- ATTACK_ORDER_TARGETED

    for (step in seq_along(ATTACK_STEPS)) {
      T_STEP_START  <- Sys.time()
      NODES_REMOVED <- ATTACK_STEPS[step]

      if (NODES_REMOVED == 0) {

        G_SUB <- GRAPH

      } else {

        NODES_TO_REMOVE <- ATTACK_ORDER[1:NODES_REMOVED]
        G_SUB <- delete_vertices(GRAPH, NODES_TO_REMOVE)

      }

      N_NODES <- vcount(G_SUB)

      if (N_NODES > 1) {
        SRC <- sample.int(N_NODES, min(N_SRC, N_NODES))

        D <- distances(G_SUB, v = SRC, to = seq_len(N_NODES), weights = E(G_SUB)$WEIGHT_S)

        DIST <- as.vector(D)
        DIST <- DIST[DIST > 0 & is.finite(DIST)]

        EF <- if (length(DIST)) sum(1 / DIST) else NA_real_
      } else {
        EF    <- NA_real_
        DIST <- NA_real_
        SRC   <- integer(0)
      }

      NR_PATHS <- length(SRC) * N_NODES

      REP_RESULTS[[step]] <- list(
        YEAR         = CURRENT_YEAR,
        REPLICATE    = rep,
        REMOVED      = NODES_REMOVED,
        NODES_LEFT   = N_NODES,
        DIST_SUM     = sum(DIST, na.rm = TRUE),
        DIST_MEDIAN  = median(DIST, na.rm = TRUE),
        DIST_MEAN    = mean(DIST, na.rm = TRUE),
        DIST_SD      = sd(DIST, na.rm = TRUE),
        NR_PATHS     = NR_PATHS
      )

      T_STEP_END <- Sys.time()
      STEP_TIME  <- as.numeric(difftime(T_STEP_END, T_STEP_START, units = "secs"))
      cat(
        sprintf("[%s] Year: %d | Rep: %d/%d | Step: %d/%d | Removed: %d | Time: %.2fs\n",
                T_STEP_END, CURRENT_YEAR, rep, N_REPS, step, NUM_STEPS, NODES_REMOVED, STEP_TIME),
        file = LOG_FILE, append = TRUE
      )
    }

    return(rbindlist(REP_RESULTS))

  }, future.seed = TRUE, future.chunk.size = 1)

  ROBUSTNESS_LIST[[year]] <- rbindlist(YEAR_RESULTS)
}

ROBUSTNESS <- rbindlist(ROBUSTNESS_LIST)
write_rds(ROBUSTNESS, "./RESULTS/ROBUSTNESS.rds")

### DISSIMILARITY ####

LAYER_DIS <- list()

for (year in 1:length(YEARS)) {

  cat("Processing", YEARS[year], "\n")

  G        <- MULTI_GRAPHS[[year]]$G
  LAYER_ID <- unique(V(G)$LAYER)
  NODE_IDS <- V(G)$REACH

  RCI_MAT           <- matrix(NA, nrow = length(unique(NODE_IDS)), ncol = length(LAYER_ID))
  rownames(RCI_MAT) <- unique(NODE_IDS)
  colnames(RCI_MAT) <- LAYER_ID

  for (layer in LAYER_ID) {

    i                     <- which(V(G)$LAYER == layer)
    NODES                 <- NODE_IDS[i]
    RCI_MAT[NODES, layer] <- V(G)$RCI_S[i]

  }

  DIS_MAT           <- as.matrix(dist(t(RCI_MAT), method = "euclidean"))
  LAYER_DIS[[year]] <- DIS_MAT

}

DISSIMILARITY <- map2_dfr(
  LAYER_DIS,
  YEARS,
  function(D, year) {
    TIBBLE <- as_tibble(as.table(D), .name_repair = "minimal")
    names(TIBBLE) <- c("CONFIG_1", "CONFIG_2", "DISSIMILARITY")
    TIBBLE %>%
      filter(CONFIG_1 < CONFIG_2) %>%
      mutate(YEAR = year)
  }
)

write_csv(DISSIMILARITY, "./RESULTS/DISSIMILARITY.csv")

### MODULARITY ####

MODULARITY <- tibble(
  YEAR = YEARS,
  MD   = NA
)

for (year in 1:length(YEARS)) {
  
  gc()
  cat("Calculating modularity for ", YEARS[[year]], "\n")
  
  G           <- LAYERS[[year]][[1]][[3]]
  G           <- as_undirected(G, mode = "collapse", edge.attr.comb = "ignore")
  E_DF        <- igraph::as_data_frame(G, what = "edges") %>%
    dplyr::select(from, to)
  
  VERTEX_ATTR <- tibble(name = V(G)$name, CC = as.numeric(V(G)$CC)) %>%
    left_join(NODES[[YEARS[year]]] %>%
                st_drop_geometry() %>%
                mutate(name = as.character(FID)) %>%
                dplyr::select(name, LENGTH),
              by = "name")
  
  CONNECTORS  <- VERTEX_ATTR %>%
    filter(CC == 5) %>%
    pull(name)
  
  G_CON       <- induced_subgraph(G, CONNECTORS)
  CON_GROUPS  <- decompose(G_CON)
  E_CON       <- list()
  
  for (group in 1:length(CON_GROUPS)) {
    
    CON   <- V(CON_GROUPS[[group]])$name
    REACH <- unique(c(E_DF$to[E_DF$from %in% CON], E_DF$from[E_DF$to %in% CON]))
    REACH <- setdiff(REACH, CONNECTORS)
    
    if (length(REACH) > 1) {
      PAIRS          <- combn(REACH, 2)
      E_CON[[group]] <- tibble(from = PAIRS[1, ], to = PAIRS[2, ])
    }
    
  }
  
  E_DIRECT    <- E_DF %>%
    filter(!from %in% CONNECTORS, !to %in% CONNECTORS)
  
  H           <- graph_from_data_frame(
    
    d        = rbind(E_DIRECT, bind_rows(E_CON)),
    
    vertices = VERTEX_ATTR %>% filter(CC != 5),
    
    directed = F)
  
  H           <- igraph::simplify(H)
  
  EDGE_NODES  <- ends(H, E(H), names = FALSE)
  WEIGHT      <- (0.5 * V(H)$LENGTH[EDGE_NODES[, 1]] + 0.5 * V(H)$LENGTH[EDGE_NODES[, 2]])
  
  W_INV               <- 1 / (WEIGHT + 10e-6)
  CC_FROM             <- V(H)$CC[EDGE_NODES[, 1]]
  CC_TO               <- V(H)$CC[EDGE_NODES[, 2]]
  A                   <- sapply(1:4, function(cc) (sum(W_INV[CC_FROM == cc]) + sum(W_INV[CC_TO == cc])) / (2 * sum(W_INV)))
  MD                  <- igraph::modularity(H, membership = V(H)$CC, weights = W_INV, directed = FALSE)
  MODULARITY$MD[year] <- MD / (1 - sum(A^2))
  
}

write_csv(MODULARITY, "./RESULTS/MODULARITY.csv")

# ~~~~~~~~~~~~~~~~~~~~ ####       
# ~~~~~~ ><{{{°> ~~~~~ ####
# ~~~~~~~~~~~~~~~~~~~~ ####       
# PLOTS ####
## DATA PREP ####

YEARS             <- c(1817, 1857, 1875, 1910, 1965, 1996, 2022)

RCI_MASTER        <- st_read("./RESULTS/RCI_MASTER/RCI_MASTER.shp")
CCI_MASTER        <- read_csv("./RESULTS/CCI_MASTER.csv")
ROBUSTNESS        <- read_rds("./RESULTS/ROBUSTNESS.rds")
DISSIMILARITY     <- read_csv("./RESULTS/DISSIMILARITY.csv")
MODULARITY        <- read_csv("./RESULTS/MODULARITY.csv")
SIGNIFICANCE      <- read_csv("./RESULTS/SIGNIFICANCE.csv")
VALIDATION        <- st_read("./RESULTS/VALIDATION/VALIDATION.shp")
CLUSTER_VARIABLES <- read_csv("./RESULTS/CLUSTER_VARIABLES.csv")

K_WSS_SC <- read_csv("./RESULTS/K_WSS_SC.csv")
K_WSS_CC <- read_csv("./RESULTS/K_WSS_CC.csv")

EUROPE_60 <- giscoR::gisco_get_countries(region = "Europe", resolution = "60") %>%
  filter(!ISO3_CODE %in% c("ISL", "FRO", "SJM", "GRL", "CYP")) %>%
  st_transform(crs = 3035) %>%
  st_crop(st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -16, ymin = 33, xmax = 45, ymax = 72), crs = 4326)), 3035))) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  filter(as.numeric(st_area(.)) > 1e9)

AUT_60             <- EUROPE_60[EUROPE_60$ISO3_CODE == "AUT", ] %>% st_transform(crs = 31287)
AUT_10             <- giscoR::gisco_get_countries(country = "Austria", resolution = "10") %>% st_transform(crs = 31287)

BARRIERS <- st_read("./PLOTS/BARRIERS/BARRIERS.shp") %>% st_transform(crs = 31287) %>% filter(STR_TYP == "DAM")
LAKES    <- st_read("./PLOTS/LAKES/LAKES.shp") %>% st_transform(crs = 31287) %>% st_zm() %>% filter(log(Shape_Area) >= 15) %>% st_intersection(AUT_10)
RIVERS   <- st_read("./PLOTS/RIVERS/RIVERS.shp") %>% st_transform(crs = 31287) %>% st_zm() %>% st_intersection(AUT_10)

HABITATS <- NODES_2022 %>%
  filter(WB_TYPE != "CO") %>%
  mutate(WB_TYPE = paste0(WB_TYPE, "_", IMP)) %>%
  mutate(WB_TYPE = ifelse(WB_TYPE == "art_1", "art_0", WB_TYPE))

HABITATS$WB_TYPE <- factor(HABITATS$WB_TYPE,
                           levels = c("eua_0", "eua_1",
                                      "eub_0", "eub_1",
                                      "paa_0",
                                      "pab_0", "pab_1",
                                      "ple_0",
                                      "tri_0",
                                      "art_0"))

DEM_C            <- rast(get_elev_raster(locations = EUROPE_60[EUROPE_60$ISO3_CODE == "AUT", ], z = 7, clip = "bbox"))
DEM_C            <- project(DEM_C, "EPSG:31287", res = 500)
SLOPE_C          <- terrain(DEM_C, v = "slope", unit = "radians")
ASPECT_C         <- terrain(DEM_C, v = "aspect", unit = "radians")
HILL_C           <- shade(SLOPE_C, ASPECT_C, angle = 45, direction = 315)
ELEV_DF_C        <- as.data.frame(DEM_C, xy = TRUE, na.rm = TRUE)
HILL_DF_C        <- as.data.frame(HILL_C, xy = TRUE, na.rm = TRUE)
names(ELEV_DF_C) <- c("X", "Y", "ELEVATION")
names(HILL_DF_C) <- c("X", "Y", "HILLSHADE")

DEM            <- rast(get_elev_raster(locations = AUT_10, z = 7, clip = "bbox"))
DEM            <- project(DEM, "EPSG:31287", res = 500)
SLOPE          <- terrain(DEM, v = "slope", unit = "radians")
ASPECT         <- terrain(DEM, v = "aspect", unit = "radians")
HILL           <- shade(SLOPE, ASPECT, angle = 45, direction = 315)
DEM            <- mask(DEM, vect(AUT_10))
HILL           <- mask(HILL, vect(AUT_10))
ELEV_DF        <- as.data.frame(DEM, xy = TRUE, na.rm = TRUE)
HILL_DF        <- as.data.frame(HILL, xy = TRUE, na.rm = TRUE)
names(ELEV_DF) <- c("X", "Y", "ELEVATION")
names(HILL_DF) <- c("X", "Y", "HILLSHADE")

BBOX       <- st_bbox(NODES_2022)
BBOX       <- st_as_sfc(BBOX)
BBOX       <- st_sf(geometry = BBOX)
BBOX       <- st_transform(BBOX, crs = 31287)
BBOX       <- st_buffer(BBOX, dist = 100)
BBOX       <- st_bbox(BBOX)
BBOX       <- st_as_sfc(BBOX)
BBOX       <- st_sf(geometry = BBOX)

LIMS_C <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -12, ymin = 31, xmax = 34, ymax = 71), crs = 4326)), 31287))

MDM_ICON <- readPNG("./ICONS/MDM.png")
SDM_ICON <- readPNG("./ICONS/SDM.png")
PD_ICON  <- readPNG("./ICONS/PD.png")
EUC_ICON <- readPNG("./ICONS/EUC.png")

GREY_ICON <- function(ICON, ALPHA = .7) {
  ICON[, , 1:3] <- col2rgb("grey40") / 255
  ICON[, , 4]   <- ICON[, , 4] * ALPHA
  ICON
}

DISP_ICONS <- tribble(
  ~DISP, ~ICON,                ~X,  ~Y,
  "MDM", GREY_ICON(MDM_ICON),  .15, .91,
  "SDM", GREY_ICON(SDM_ICON),  .15, .09,
  "PD",  GREY_ICON(PD_ICON),   .85, .09,
  "EUC", GREY_ICON(EUC_ICON),  .85, .91
)

GEOM_ICON <- ggproto("GEOM_ICON", Geom,
                     draw_panel = function(data, panel_params, coord, GROB) GROB
)

ICON_LAYER <- function(ICON, X, Y, HEIGHT, HJUST = .5, VJUST = .5, DATA = tibble(ROW = 1)) {
  layer(
    data        = DATA,
    stat        = StatIdentity,
    position    = PositionIdentity,
    geom        = GEOM_ICON,
    inherit.aes = FALSE,
    params      = list(GROB = rasterGrob(ICON, x = X, y = Y, height = unit(HEIGHT, "npc"), hjust = HJUST, vjust = VJUST, interpolate = TRUE))
  )
}

## SPATIAL ####
### STUDY SITE ####
#### DATA ####

MERGE_IMPOUNDED <- TRUE

HABITAT_KEY <- tribble(
  ~WB_TYPE, ~COLOUR,   ~LABEL,
  "eua_0",  "#468892", "Eupotamon A",
  "eua_1",  "#1CAC1C", "Eupotamon A (impounded)",
  "eub_0",  "#74F3D3", "Eupotamon B",
  "eub_1",  "#12D09E", "Eupotamon B (impounded)",
  "paa_0",  "#751C6D", "Parapotamon A",
  "pab_0",  "#FF3BD5", "Parapotamon B",
  "pab_1",  "#623BC5", "Parapotamon B (impounded)",
  "ple_0",  "#FDC067", "Plesiopotamon",
  "tri_0",  "#9F6619", "Tributary",
  "art_0",  "grey40",  "Artificial"
)

TIMELINE <- tribble(
  ~YEAR, ~EVENT,
  1817,  "Unregulated",
  1857,  "Regulation onset",
  1875,  "First sections regulated",
  1910,  "HQ1 regulation",
  1965,  ">HQ1 regulation (except east of Vienna)",
  1996,  "Max. regulation",
  2022,  "Restoration ongoing"
)

if (MERGE_IMPOUNDED) HABITAT_KEY <- filter(HABITAT_KEY, !grepl("_1$", WB_TYPE))

ELEV_COLOURS <- c("#468892", "#74F3D3", "#751C6D", "#FF3BD5", "#FDC067")
ELEV_BREAKS  <- c(1000, 2000, 3000)

Y_OFFSET <- 17500

shift_y <- function(DATA, DY) {
  st_geometry(DATA) <- st_sfc(st_geometry(DATA) + c(0, DY), crs = st_crs(DATA))
  DATA
}

get_habitats <- function(NODES, YEAR) {
  NODES %>%
    filter(WB_TYPE != "CO") %>%
    mutate(
      WB_TYPE = paste0(WB_TYPE, "_", IMP),
      WB_TYPE = ifelse(WB_TYPE == "art_1" | MERGE_IMPOUNDED, sub("_1$", "_0", WB_TYPE), WB_TYPE),
      WB_TYPE = factor(WB_TYPE, levels = HABITAT_KEY$WB_TYPE),
      YEAR    = YEAR
    ) %>%
    dplyr::select(YEAR, WB_TYPE)
}

NETWORK <- rbind(
  get_habitats(NODES_1817, 1817),
  get_habitats(NODES_2022, 2022) %>% shift_y(-Y_OFFSET)
)

BARRIERS_2022 <- BARRIERS %>%
  st_transform(st_crs(NETWORK)) %>%
  shift_y(-Y_OFFSET)

IMP_WIDTH  <- 2250
HATCH_STEP <- 700

MAIN_CHANNEL <- NODES_2022 %>%
  filter(WB_TYPE == "eua") %>%
  st_union()

IMP_CHANNEL <- NODES_2022 %>%
  filter(WB_TYPE == "eua", IMP == 1) %>%
  st_union()

IMP_ZONES <- IMP_CHANNEL %>%
  st_buffer(IMP_WIDTH) %>%
  st_cast("POLYGON")

blade <- function(POINT) {
  NEAR <- st_coordinates(st_intersection(MAIN_CHANNEL, st_buffer(st_sfc(st_point(POINT), crs = st_crs(MAIN_CHANNEL)), 1000)))[, 1:2]
  FLOW <- prcomp(NEAR)$rotation[, 1]
  SIDE <- c(-FLOW[2], FLOW[1])
  st_polygon(list(rbind(POINT + 20 * FLOW + 2 * IMP_WIDTH * SIDE,
                        POINT - 20 * FLOW + 2 * IMP_WIDTH * SIDE,
                        POINT - 20 * FLOW - 2 * IMP_WIDTH * SIDE,
                        POINT + 20 * FLOW - 2 * IMP_WIDTH * SIDE,
                        POINT + 20 * FLOW + 2 * IMP_WIDTH * SIDE)))
}

zone_ends <- function(ZONE) {
  XY   <- st_coordinates(st_intersection(IMP_CHANNEL, st_sfc(ZONE, crs = st_crs(IMP_CHANNEL))))[, 1:2]
  HULL <- XY[chull(XY), ]
  D    <- as.matrix(dist(HULL))
  HULL[which(D == max(D), arr.ind = TRUE)[1, ], ]
}

MAIN_DAMS <- BARRIERS %>%
  st_transform(st_crs(NODES_2022)) %>%
  filter(as.numeric(st_distance(., IMP_CHANNEL)) < 100)

CUT_POINTS <- rbind(
  st_coordinates(MAIN_DAMS)[, 1:2],
  do.call(rbind, map(IMP_ZONES, zone_ends))
)

CUTS <- map(seq_len(nrow(CUT_POINTS)), ~ blade(CUT_POINTS[.x, ])) %>%
  st_sfc(crs = st_crs(NODES_2022)) %>%
  st_union()

IMPOUNDMENTS <- IMP_ZONES %>%
  st_difference(CUTS) %>%
  st_cast("POLYGON") %>%
  st_sf(geometry = .) %>%
  filter(as.numeric(st_area(.)) > 1e7) %>%
  shift_y(-Y_OFFSET)

hatch <- function(POLYGONS, STEP) {
  BOX <- st_bbox(POLYGONS)
  X   <- c(BOX[["xmin"]], BOX[["xmax"]])
  bind_rows(
    tibble(DIR = 1,  C = seq(BOX[["ymin"]] - BOX[["xmax"]], BOX[["ymax"]] - BOX[["xmin"]], by = STEP)),
    tibble(DIR = -1, C = seq(BOX[["ymin"]] + BOX[["xmin"]], BOX[["ymax"]] + BOX[["xmax"]], by = STEP))
  ) %>%
    pmap(function(DIR, C) st_linestring(cbind(X, C + DIR * X))) %>%
    st_sfc(crs = st_crs(POLYGONS)) %>%
    st_intersection(st_union(POLYGONS)) %>%
    st_collection_extract("LINESTRING")
}

IMP_HATCH <- hatch(IMPOUNDMENTS, HATCH_STEP)

NET_YEARS <- tibble(
  YEAR = c(1817, 2022),
  DY   = c(0, -Y_OFFSET)
)

YEAR_LABELS <- NET_YEARS %>%
  mutate(
    X = 2.8e4,
    Y = 3.375e5 + DY
  )

END_MARKS <- tribble(
  ~X,      ~Y,      ~XEND,  ~YEND,
  -7.25e4, 3.445e5, -7.4e4, 3.47e5,
  4.8e4,   3.354e5, 4.8e4,  3.384e5
) %>%
  expand_grid(DY = NET_YEARS$DY) %>%
  mutate(
    Y    = Y + DY,
    YEND = YEND + DY
  )

#### LAYOUT ####

FIG_W    <- 178
BOX_L    <- .5
BOX_R    <- FIG_W - .5
PAD      <- 3
BOX_GAP  <- 3
GAP      <- 4
SHADOW_L <- 4
SHADOW_B <- 7
KEY      <- 4
KEY_GAP  <- 2
TEXT_PT  <- 6
TITLE_PT <- 8
LABEL_PT <- 3 * .pt
ELEV_H   <- 8.2
TL_H     <- 8.6

text_w <- function(LABEL, PT, FACE = "plain") {
  max(map_dbl(as.list(LABEL), ~ convertWidth(grobWidth(textGrob(.x, gp = gpar(fontsize = PT, fontface = FACE))), "mm", valueOnly = TRUE)))
}

map_frame <- function(BOX, SCALE, X0, Y0, LEFT = 0, BOTTOM = 0, RIGHT = 0, TOP = 0) {
  tibble(
    S    = SCALE,
    XMIN = BOX[["xmin"]] - LEFT / SCALE,
    XMAX = BOX[["xmax"]] + RIGHT / SCALE,
    YMIN = BOX[["ymin"]] - BOTTOM / SCALE,
    YMAX = BOX[["ymax"]] + TOP / SCALE,
    X0   = X0 - LEFT,
    Y0   = Y0 - BOTTOM,
    W    = (XMAX - XMIN) * SCALE,
    H    = (YMAX - YMIN) * SCALE
  )
}

x_mm <- function(FRAME, X) FRAME$X0 + (X - FRAME$XMIN) * FRAME$S
y_mm <- function(FRAME, Y) FRAME$Y0 + (Y - FRAME$YMIN) * FRAME$S

place <- function(PLOT, FRAME) {
  annotation_custom(ggplotGrob(PLOT), xmin = FRAME$X0, xmax = FRAME$X0 + FRAME$W, ymin = FRAME$Y0, ymax = FRAME$Y0 + FRAME$H)
}

AUT_BOX <- st_bbox(AUT_10)
EUR_BOX <- LIMS_C
NET_BOX <- st_bbox(NETWORK)

NET_BOX[["xmin"]] <- min(NET_BOX[["xmin"]], END_MARKS$XEND)
NET_BOX[["xmax"]] <- max(NET_BOX[["xmax"]], END_MARKS$X)

RATIO_AUT <- (AUT_BOX[["xmax"]] - AUT_BOX[["xmin"]]) / (AUT_BOX[["ymax"]] - AUT_BOX[["ymin"]])
RATIO_EUR <- (EUR_BOX[["xmax"]] - EUR_BOX[["xmin"]]) / (EUR_BOX[["ymax"]] - EUR_BOX[["ymin"]])

X_L <- BOX_L + SHADOW_L + 1
X_R <- BOX_R - PAD

Y_TL     <- BOX_L + PAD + TL_H
Y_C_TOP  <- Y_TL + TL_H + PAD
Y_B_BASE <- Y_C_TOP + BOX_GAP

NET_FRAME <- map_frame(NET_BOX, (X_R - X_L) / (NET_BOX[["xmax"]] - NET_BOX[["xmin"]]),
                       X0 = X_L, Y0 = Y_B_BASE + SHADOW_B, LEFT = SHADOW_L, BOTTOM = SHADOW_B, RIGHT = 1, TOP = 1)

Y_B_TOP <- Y_B_BASE + SHADOW_B + (NET_BOX[["ymax"]] - NET_BOX[["ymin"]]) * NET_FRAME$S + PAD

AUT_X0 <- X_L
H_TOP  <- (X_R - AUT_X0 - GAP + RATIO_AUT * ELEV_H) / (RATIO_AUT + RATIO_EUR)
AUT_H  <- H_TOP - ELEV_H
AUT_X1 <- AUT_X0 + RATIO_AUT * AUT_H
EUR_X0 <- AUT_X1 + GAP
Y_A0   <- Y_B_TOP + BOX_GAP + SHADOW_B
Y_ROW  <- Y_A0 + 4.2
Y_A1   <- Y_A0 + H_TOP
FIG_H  <- Y_A1 + PAD + BOX_L

AUT_FRAME <- map_frame(AUT_BOX, AUT_H / (AUT_BOX[["ymax"]] - AUT_BOX[["ymin"]]),
                       X0 = AUT_X0, Y0 = Y_A0 + ELEV_H, LEFT = SHADOW_L, BOTTOM = SHADOW_B, RIGHT = 1, TOP = 1)
EUR_FRAME <- map_frame(EUR_BOX, H_TOP / (EUR_BOX[["ymax"]] - EUR_BOX[["ymin"]]),
                       X0 = EUR_X0, Y0 = Y_A0, LEFT = SHADOW_L, BOTTOM = SHADOW_B)

BOXES <- tribble(
  ~YMIN,              ~YMAX,          ~LABEL,
  Y_B_TOP + BOX_GAP,  FIG_H - BOX_L,  "A",
  Y_B_BASE,           Y_B_TOP,        "B",
  BOX_L,              Y_C_TOP,        "C"
)

SEPARATORS <- tibble(Y = c(Y_C_TOP, Y_B_TOP) + BOX_GAP / 2)

#### MAPS ####

THEME_MAP <- theme_void() +
  theme(
    plot.background = element_blank(),
    plot.margin     = margin(0, 0, 0, 0)
  )

P_EUR <- ggplot() +
  as_reference(
    geom_sf(data = EUROPE_60[!EUROPE_60$ISO3_CODE == "AUT", ], fill = "black", colour = NA),
    id = "aut") +
  with_mask(
    with_shadow(
      geom_sf(data = EUROPE_60[!EUROPE_60$ISO3_CODE %in% c("AUT", "RUS"), ], fill = "grey97", colour = "grey40", linewidth = .3, lineend = "round"),
      x_offset = -60,
      y_offset = 120,
      colour   = "grey80",
      sigma    = 12),
    mask   = ch_alpha("aut"),
    invert = TRUE) +
  geom_raster(data = ELEV_DF_C, aes(x = X, y = Y, fill = ELEVATION)) +
  scale_fill_gradientn(colours = ELEV_COLOURS, na.value = NA, guide = "none") +
  geom_sf(data = EUROPE_60[!EUROPE_60$ISO3_CODE %in% c("AUT", "RUS"), ], fill = "grey97", colour = "grey40", linewidth = .3, lineend = "round") +
  coord_sf(crs = 31287, xlim = c(EUR_FRAME$XMIN, EUR_FRAME$XMAX), ylim = c(EUR_FRAME$YMIN, EUR_FRAME$YMAX), expand = FALSE) +
  THEME_MAP

P_AUT <- ggplot() +
  with_shadow(
    geom_raster(data = ELEV_DF, aes(x = X, y = Y, fill = ELEVATION)),
    x_offset = -60,
    y_offset = 120,
    colour   = "grey80",
    sigma    = 12) +
  scale_fill_gradientn(colours = ELEV_COLOURS, na.value = NA, guide = "none") +
  new_scale_fill() +
  geom_raster(data = HILL_DF, aes(x = X, y = Y, fill = HILLSHADE), alpha = .5) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA, guide = "none") +
  geom_sf(data = LAKES, fill = "white", colour = "white") +
  geom_sf(data = RIVERS %>% filter(FLOZ %in% 2:9), aes(linewidth = FLOZ), lineend = "round", colour = "white") +
  geom_sf(data = BBOX, colour = "grey30", fill = NA, linewidth = .5) +
  scale_linewidth(range = c(.01, .8), guide = "none") +
  coord_sf(crs = 31287, xlim = c(AUT_FRAME$XMIN, AUT_FRAME$XMAX), ylim = c(AUT_FRAME$YMIN, AUT_FRAME$YMAX), expand = FALSE) +
  THEME_MAP

P_NET <- ggplot() +
  geom_sf(data = IMP_HATCH, colour = "grey60", linewidth = .15) +
  geom_sf(data = IMPOUNDMENTS, fill = NA, colour = "grey60", linewidth = .25) +
  with_shadow(
    geom_sf(data = NETWORK, aes(colour = WB_TYPE), linewidth = .25, lineend = "round"),
    x_offset = -60,
    y_offset = 120,
    colour   = "grey80",
    sigma    = 12) +
  geom_segment(data = END_MARKS, aes(x = X, y = Y, xend = XEND, yend = YEND), linewidth = 1, colour = "grey40", lineend = "round") +
  geom_sf(data = BARRIERS_2022, shape = 17, size = 1.5, colour = "grey30") +
  geom_text(data = YEAR_LABELS, aes(x = X, y = Y, label = YEAR), size = 3, fontface = "bold", colour = "grey40") +
  scale_colour_manual(values = setNames(HABITAT_KEY$COLOUR, HABITAT_KEY$WB_TYPE), guide = "none") +
  coord_sf(crs = st_crs(NETWORK), xlim = c(NET_FRAME$XMIN, NET_FRAME$XMAX), ylim = c(NET_FRAME$YMIN, NET_FRAME$YMAX), expand = FALSE) +
  THEME_MAP

#### LEGENDS ####

TL_X0 <- BOX_L + 10
TL_X1 <- BOX_R - 6

TIMELINE_POS <- TIMELINE %>%
  mutate(
    X     = TL_X0 + (YEAR - 1817) / (2022 - 1817) * (TL_X1 - TL_X0),
    SIDE  = if_else(row_number() %% 2 == 1, 1, -1),
    HJUST = case_when(YEAR == 1817 ~ 0, YEAR == 2022 ~ 1, TRUE ~ .5),
    VJUST = if_else(SIDE == 1, 0, 1),
    FILL  = if_else(YEAR %in% NET_YEARS$YEAR, "grey40", "white")
  )

ELEV_TITLE  <- "paste(bold('Elevation'), ' [m]')"
ELEV_LIMITS <- range(ELEV_DF$ELEVATION)
ELEV_X0     <- AUT_X0 + text_w(parse(text = ELEV_TITLE), TITLE_PT) + 2.5
ELEV_BAR    <- matrix(scales::gradient_n_pal(ELEV_COLOURS)(seq(0, 1, length.out = 200)), nrow = 1)

ELEV_TICKS <- tibble(VALUE = ELEV_BREAKS) %>%
  mutate(X = ELEV_X0 + 25 * (VALUE - ELEV_LIMITS[1]) / diff(ELEV_LIMITS)) %>%
  filter(X > ELEV_X0, X < ELEV_X0 + 25)

LEG_X     <- x_mm(NET_FRAME, -5.9e4)
LEG_Y     <- y_mm(NET_FRAME, 3.52e5 - Y_OFFSET)
LEG_ROWS  <- ceiling(nrow(HABITAT_KEY) / 2)
LEG_COL_W <- KEY + KEY_GAP + text_w(HABITAT_KEY$LABEL[1:LEG_ROWS], TEXT_PT) + GAP
LEG_DAM_X <- LEG_X + LEG_COL_W + KEY + KEY_GAP + text_w(c(HABITAT_KEY$LABEL[-(1:LEG_ROWS)], "Main channel impounded"), TEXT_PT) + GAP

HABITAT_LEGEND <- HABITAT_KEY %>%
  mutate(
    X = LEG_X + ((row_number() - 1) %/% LEG_ROWS) * LEG_COL_W,
    Y = LEG_Y - 4.6 - ((row_number() - 1) %% LEG_ROWS) * (KEY + 1)
  )

DAM_Y <- min(HABITAT_LEGEND$Y)
IMP_X <- LEG_X + LEG_COL_W
IMP_Y <- min(HABITAT_LEGEND$Y[HABITAT_LEGEND$X == IMP_X]) - KEY - 1

IMP_KEY <- st_sfc(st_polygon(list(rbind(c(0, 0), c(KEY, 0), c(KEY, KEY), c(0, KEY), c(0, 0)))) + c(IMP_X, IMP_Y - KEY)) %>%
  hatch(STEP = 1) %>%
  st_coordinates() %>%
  as_tibble() %>%
  transmute(X, Y, L = L1)

LEGEND_TITLES <- tribble(
  ~X,        ~Y,          ~LABEL,
  LEG_X,     LEG_Y,       "Habitat"
)

SCALE_BARS <- tribble(
  ~KM, ~STEP, ~S,          ~X0,                                                   ~Y,
  100, 25,    AUT_FRAME$S, AUT_X0 + text_w("100 km", LABEL_PT, "bold") + 2.5,    Y_A1 - 14,
  15,  5,     NET_FRAME$S, x_mm(NET_FRAME, 2.8e4) - 7500 * NET_FRAME$S,          y_mm(NET_FRAME, 3.5e5)
) %>%
  mutate(LABEL = paste(KM, "km"))

NORTH_ARROWS <- SCALE_BARS %>%
  transmute(
    X = X0 + KM * 1000 * S / 2,
    Y = Y + 5.5
  )

SCALE_SEGMENTS <- SCALE_BARS %>%
  mutate(N = KM / STEP) %>%
  uncount(N, .id = "I") %>%
  mutate(
    XMIN = X0 + (I - 1) * STEP * 1000 * S,
    XMAX = XMIN + STEP * 1000 * S,
    FILL = if_else(I %% 2 == 1, "grey40", "white")
  )

NORTH_GROB <- gTree(children = ggspatial::north_arrow_orienteering(line_width = .5, line_col = "black", fill = c("grey40", "white"), text_col = "grey40", text_face = "bold", text_size = 6))

#### PLOTTING ####

P_SITE <- ggplot() +
  geom_segment(data = SEPARATORS, aes(x = BOX_L, xend = BOX_R, y = Y, yend = Y), colour = "grey40", linewidth = 1.2) +
  place(P_NET, NET_FRAME) +
  place(P_AUT, AUT_FRAME) +
  place(P_EUR, EUR_FRAME) +
  geom_text(data = BOXES, aes(x = BOX_L + 2, y = YMAX - 2, label = LABEL), hjust = 0, vjust = 1, size = TITLE_PT / .pt, fontface = "bold", colour = "grey40") +
  annotate("segment", x = TL_X0, xend = TL_X1, y = Y_TL, yend = Y_TL, colour = "grey40", linewidth = .5) +
  geom_point(data = TIMELINE_POS, aes(x = X, y = Y_TL, fill = FILL), shape = 21, size = 1.8, stroke = .5, colour = "grey40") +
  geom_text(data = TIMELINE_POS, aes(x = X, y = Y_TL + SIDE * 2.6, label = YEAR, hjust = HJUST, vjust = VJUST), size = 3, fontface = "bold", colour = "grey40") +
  geom_text(data = TIMELINE_POS, aes(x = X, y = Y_TL + SIDE * 6.4, label = EVENT, hjust = HJUST, vjust = VJUST), size = TEXT_PT / .pt, colour = "grey40") +
  annotate("text", x = AUT_X0, y = Y_ROW, label = ELEV_TITLE, parse = TRUE, hjust = 0, size = TITLE_PT / .pt, colour = "grey40") +
  annotation_raster(ELEV_BAR, xmin = ELEV_X0, xmax = ELEV_X0 + 25, ymin = Y_ROW - 1.5, ymax = Y_ROW + 1.5, interpolate = TRUE) +
  geom_segment(data = ELEV_TICKS, aes(x = X, xend = X, y = Y_ROW + .5, yend = Y_ROW + 1.5), colour = "white", linewidth = .2) +
  geom_segment(data = ELEV_TICKS, aes(x = X, xend = X, y = Y_ROW - .5, yend = Y_ROW - 1.5), colour = "white", linewidth = .2) +
  geom_text(data = ELEV_TICKS, aes(x = X, y = Y_ROW - 4.2, label = VALUE), size = TEXT_PT / .pt, colour = "grey40") +
  geom_text(data = LEGEND_TITLES, aes(x = X, y = Y, label = LABEL), hjust = 0, vjust = 1, size = TITLE_PT / .pt, fontface = "bold", colour = "grey40") +
  geom_rect(data = HABITAT_LEGEND, aes(xmin = X, xmax = X + KEY, ymin = Y - KEY, ymax = Y, fill = COLOUR)) +
  geom_text(data = HABITAT_LEGEND, aes(x = X + KEY + KEY_GAP, y = Y - KEY / 2, label = LABEL), hjust = 0, size = TEXT_PT / .pt, colour = "grey40") +
  geom_path(data = IMP_KEY, aes(x = X, y = Y, group = L), colour = "grey60", linewidth = .15) +
  annotate("rect", xmin = IMP_X, xmax = IMP_X + KEY, ymin = IMP_Y - KEY, ymax = IMP_Y, fill = NA, colour = "grey60", linewidth = .25) +
  annotate("text", x = IMP_X + KEY + KEY_GAP, y = IMP_Y - KEY / 2, label = "Main channel impounded", hjust = 0, size = TEXT_PT / .pt, colour = "grey40") +
  annotate("point", x = LEG_DAM_X + KEY / 2, y = DAM_Y - KEY / 2, shape = 17, size = 1.5, colour = "grey30") +
  annotate("text", x = LEG_DAM_X + KEY + KEY_GAP, y = DAM_Y - KEY / 2, label = "Dam", hjust = 0, size = TEXT_PT / .pt, colour = "grey40") +
  geom_rect(data = SCALE_SEGMENTS, aes(xmin = XMIN, xmax = XMAX, ymin = Y - .5, ymax = Y + .5, fill = FILL), colour = "black", linewidth = .2) +
  geom_text(data = SCALE_BARS, aes(x = X0 - 2.5, y = Y, label = LABEL), hjust = 1, size = 3, fontface = "bold", colour = "grey40") +
  pmap(NORTH_ARROWS, function(X, Y) annotation_custom(NORTH_GROB, xmin = X - 2.5, xmax = X + 2.5, ymin = Y - 2.5, ymax = Y + 2.5)) +
  scale_fill_identity() +
  coord_fixed(xlim = c(0, FIG_W), ylim = c(0, FIG_H), expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(0, 0, 0, 0)
  )

ggsave(P_SITE, filename = "./PLOTS/STUDY_SITE.png", width = FIG_W, height = FIG_H, unit = "mm", dpi = 600)
### CC ####

YEAR_LEVELS                     <- sort(unique(RCI_MASTER$YEAR))
RCI_MASTER_OFFSET               <- RCI_MASTER %>% mutate(YEAR_INDEX  = match(YEAR, YEAR_LEVELS), CC = factor(CC, levels = c(1,2,3,4)))
Y_OFFSET                        <- 17500
Y_OFFSETS                       <- - Y_OFFSET * (RCI_MASTER_OFFSET$YEAR_INDEX - 1)
SHIFTED_GEOM                    <- Map(function(geom, y_shift) {geom + c(0, y_shift)}, st_geometry(RCI_MASTER_OFFSET), Y_OFFSETS)
st_geometry(RCI_MASTER_OFFSET)  <- st_sfc(SHIFTED_GEOM, crs = st_crs(RCI_MASTER))

VIENNA_POLY                     <- st_read("./PLOTS/VIENNA_BOUNDARY/VIENNA_COUNTY_POLY.shp")
VIENNA_POLY_OFFSET              <- st_transform(VIENNA_POLY, crs = st_crs(RCI_MASTER))
Y_OFFSETS                       <- - Y_OFFSET * (length(YEAR_LEVELS) - 1)
SHIFTED_GEOM                    <- Map(function(geom, y_shift) {geom + c(0, y_shift)}, st_geometry(VIENNA_POLY_OFFSET), Y_OFFSETS)
st_geometry(VIENNA_POLY_OFFSET) <- st_sfc(SHIFTED_GEOM, crs = st_crs(VIENNA_POLY_OFFSET))

ADDITIONAL_DATA <- RCI_MASTER_OFFSET %>%

  filter(WB_TYPE != "CO" & DISP == "MDM" & SC == 1) %>%
  dplyr::select(FID, YEAR, CC) %>%

  left_join(

    rbind(NODES_1817 %>%
            st_drop_geometry() %>%
            dplyr::select(FID, LENGTH) %>%
            mutate(YEAR = 1817,
                   FID  = as.numeric(FID)),
          NODES_1857 %>%
            st_drop_geometry() %>%
            dplyr::select(FID, LENGTH) %>%
            mutate(YEAR = 1857,
                   FID  = as.numeric(FID)),
          NODES_1875 %>%
            st_drop_geometry() %>%
            dplyr::select(FID, LENGTH) %>%
            mutate(YEAR = 1875,
                   FID  = as.numeric(FID)),
          NODES_1910 %>%
            st_drop_geometry() %>%
            dplyr::select(FID, LENGTH) %>%
            mutate(YEAR = 1910,
                   FID  = as.numeric(FID)),
          NODES_1965 %>%
            st_drop_geometry() %>%
            dplyr::select(FID, LENGTH) %>%
            mutate(YEAR = 1965,
                   FID  = as.numeric(FID)),
          NODES_1996 %>%
            st_drop_geometry() %>%
            dplyr::select(FID, LENGTH) %>%
            mutate(YEAR = 1996,
                   FID  = as.numeric(FID)),
          NODES_2022 %>%
            st_drop_geometry() %>%
            dplyr::select(FID, LENGTH) %>%
            mutate(YEAR = 2022,
                   FID  = as.numeric(FID))),

    by = c("FID", "YEAR"))

METADATA_CC <- ADDITIONAL_DATA %>%
  st_drop_geometry() %>%
  group_by(YEAR, CC) %>%
  summarise(CUM_LENGTH = sum(LENGTH)) %>%
  mutate(CC = factor(CC, levels = c(4:1)))

METADATA_REACHES <- ADDITIONAL_DATA %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(NR_REACHES = n(), LENGTH = sum(LENGTH)) %>%
  mutate(LENGTH = round(LENGTH/1000, digits = 0)) %>%
  cbind(Y = rev(seq(2.14e5 + 17500 * 2, 2.14e5 + 17500 + 7*17500, length.out = 7))) %>%
  cbind(X = -55000) %>%
  mutate(LABEL = paste0("bold('", LENGTH, "')~' km (n = ", NR_REACHES, ")'"))

P_CC_1 <- ggplot() +

  with_shadow(
    geom_sf(data = VIENNA_POLY_OFFSET, color = "grey40", fill = "grey99"),
    x_offset = 60,
    y_offset = 120,
    colour   = "grey90",
    sigma    = 12
  ) +

  with_shadow(
    geom_sf(data = RCI_MASTER_OFFSET %>%
              filter(WB_TYPE != "CO" & DISP == "MDM" & SC == 1),
            aes(color = CC), lineend = "round", linewidth = .25, show.legend = FALSE),
    x_offset = 60,
    y_offset = 120,
    colour   = "grey90",
    sigma    = 12
  ) +

  geom_point(
    data = tibble(CC = factor(seq(1,4,1), levels = c(1,2,3,4))),
    aes(x = 1e4, y = 3e5, fill = CC),
    shape = 22, size = 5, color = NA, show.legend = TRUE
  ) +

  geom_text(data = tibble(y    = rev(seq(2.15e5 + 17500, 2.15e5 + 7*17500, length.out = 7)),
                          x    = 2.8e4,
                          text = as.character(c(1817, 1857, 1875, 1910, 1965, 1996, 2022))),
            aes(x = x, y = y, label = text),
            size = 3,
            color = "grey40",
            hjust = 0.5,
            fontface = "bold") +

  geom_text(
    data = METADATA_REACHES,
    aes(
      x = X,
      y = Y,
      label = LABEL
    ),
    parse = TRUE,
    size  = 2.5,
    color = "grey40",
    hjust = .5
  ) +

  geom_segment(data = tibble(x_start = -7.25e4,
                             y_start = rev(seq(2.22e5 + 17500, 2.22e5 + 7*17500, length.out = 7)),
                             x_end   = -7.4e4,
                             y_end   = rev(seq(2.245e5 + 17500, 2.245e5 + 7*17500, length.out = 7))),
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               linewidth = 1,
               color = "grey40",
               lineend = "round") +

  geom_segment(data = tibble(x_start = 4.8e4,
                             y_start = rev(seq(2.129e5 + 17500, 2.129e5 + 7*17500, length.out = 7)),
                             x_end   = 4.8e4,
                             y_end   = rev(seq(2.159e5 + 17500, 2.159e5 + 7*17500, length.out = 7))),
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               linewidth = 1,
               color = "grey40",
               lineend = "round") +

  ggspatial::annotation_north_arrow(location = "tr",
                                    height   = unit(5, "mm"),
                                    width    = unit(5, "mm"),
                                    pad_x    = unit(27.5, "mm"),
                                    pad_y    = unit(12, "mm"),
                                    style    = north_arrow_orienteering(text_size = 6,
                                                                        line_width    = .5,
                                                                        text_col      = "grey40",
                                                                        text_face     = "bold",
                                                                        fill          = c("grey40", "white")))+

  ggspatial::annotation_scale(location   = "bl",
                              pad_x      = unit(27, "mm"),
                              pad_y      = unit(8.5, "mm"),
                              text_col   = "grey40", text_cex= 0,
                              line_width = .5,
                              height     = unit(1, "mm"),
                              bar_cols   = c("grey40", "white"))+

  scale_x_continuous(expand = expansion(mult = c(.0025, .018))) +
  scale_y_continuous(expand = expansion(mult = c(.03, 0))) +

  scale_fill_manual(
    values = c("#468892", "#74F3D3","#751C6D",  "#FF3BD5"),
    labels = c("CC1", "CC2", "CC3", "CC4"),
    name = "Connectivity cluster"
  ) +

  scale_color_manual(
    values = c("#468892", "#74F3D3","#751C6D",  "#FF3BD5"),
    guide = "none"
  ) +

  theme_void() +

  guides(
    fill  = guide_legend(
      nrow           = 1,
      byrow          = TRUE,
      label.position = "bottom",
      label.hjust    = 0.5,
      title.position = "left"
    )
  ) +

  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.25, 0.11),
    legend.direction       = "horizontal",
    legend.text            = element_text(size= 6, color = "grey40"),
    legend.title           = element_text(margin = margin(0,10,12,0), face = "bold", size= 8, color = "grey40"),
    legend.key.height      = unit(4,"mm"),
    legend.key.width       = unit(4,"mm"),
    panel.background       = element_rect(fill = "white", color = NA),
    plot.margin            = margin(0,0,0,0)
  ) +

  annotate("text",
           x = -60000, y = 228000,
           label = "30 km",
           size = 3,
           fontface = "bold",
           color = "grey40"
  )

P_CC_2 <- ggplot(METADATA_CC, aes(x = "Total", y = CUM_LENGTH, fill = CC)) +
  geom_col(width = .1) +
  coord_flip() +
  facet_wrap(~YEAR, ncol = 1) +
  scale_fill_manual(values = rev(c("#468892", "#74F3D3","#751C6D",  "#FF3BD5"))) +
  theme_void() +
  theme(
    legend.position       = "none",
    strip.text            = element_blank(),
    panel.background      = element_rect(fill = "transparent", color = NA),
    plot.background       = element_rect(fill = "transparent", color = NA),
    strip.background      = element_rect(fill = "transparent", color = NA),
    legend.background     = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

P_CC <- P_CC_1 +
  inset_element(
    P_CC_2,
    left = 0.085,
    bottom = 0.12,
    right = 0.23,
    top = 0.965
  )

ggsave(P_CC, filename= "./PLOTS/CC_SPATIAL.png", width= 178, height= 200, unit= "mm", dpi = 600)

## NON-SPATIAL ####
### CCI ####

WB_TYP_AREA <- rbind(

  NODES_1817 %>%
    dplyr::select(WB_TYPE, IMP, SECTION, LENGTH) %>%
    mutate(YEAR = 1817) %>%
    left_join(WIDTHS %>%
                dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
              by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
    relocate(WIDTH, .after = LENGTH) %>%
    mutate(WIDTH = replace_na(WIDTH, 0)),

  NODES_1857 %>%
    dplyr::select(WB_TYPE, IMP, SECTION, LENGTH) %>%
    mutate(YEAR = 1857) %>%
    left_join(WIDTHS %>%
                dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
              by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
    relocate(WIDTH, .after = LENGTH) %>%
    mutate(WIDTH = replace_na(WIDTH, 0)),

  NODES_1875 %>%
    dplyr::select(WB_TYPE, IMP, SECTION, LENGTH) %>%
    mutate(YEAR = 1875) %>%
    left_join(WIDTHS %>%
                dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
              by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
    relocate(WIDTH, .after = LENGTH) %>%
    mutate(WIDTH = replace_na(WIDTH, 0)),

  NODES_1910 %>%
    dplyr::select(WB_TYPE, IMP, SECTION, LENGTH) %>%
    mutate(YEAR = 1910) %>%
    left_join(WIDTHS %>%
                dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
              by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
    relocate(WIDTH, .after = LENGTH) %>%
    mutate(WIDTH = replace_na(WIDTH, 0)),

  NODES_1965 %>%
    dplyr::select(WB_TYPE, IMP, SECTION, LENGTH) %>%
    mutate(YEAR = 1965) %>%
    left_join(WIDTHS %>%
                dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
              by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
    relocate(WIDTH, .after = LENGTH) %>%
    mutate(WIDTH = replace_na(WIDTH, 0)),

  NODES_1996 %>%
    dplyr::select(WB_TYPE, IMP, SECTION, LENGTH) %>%
    mutate(YEAR = 1996) %>%
    left_join(WIDTHS %>%
                dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
              by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
    relocate(WIDTH, .after = LENGTH) %>%
    mutate(WIDTH = replace_na(WIDTH, 0)),

  NODES_2022 %>%
    dplyr::select(WB_TYPE, IMP, SECTION, LENGTH) %>%
    mutate(YEAR = 2022) %>%
    left_join(WIDTHS %>%
                dplyr::select(YEAR, WB_TYPE, IMP, SECTION, WIDTH),
              by = c("YEAR", "WB_TYPE", "IMP", "SECTION")) %>%
    relocate(WIDTH, .after = LENGTH) %>%
    mutate(WIDTH = replace_na(WIDTH, 0))

)

WB_TYP_AREA <- WB_TYP_AREA %>%
  st_drop_geometry() %>%
  filter(WB_TYPE != "CO") %>%
  mutate(AREA = WIDTH * LENGTH) %>%
  mutate(IMP = ifelse(WB_TYPE == "art", 0, IMP)) %>%
  group_by(YEAR, WB_TYPE, IMP) %>%
  summarise(AREA   = sum(AREA, na.rm = TRUE),
            WIDTH  = sum(WIDTH, na.rm = TRUE),
            LENGTH = sum(LENGTH, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(WB_TYPE = paste0(WB_TYPE,"_",IMP)) %>%
  dplyr::select(-IMP) %>%
  rbind(

    tibble(YEAR = rep(c(1817, 1857, 1875, 1910, 1965), 2),
           WB_TYPE = c(rep("eua_1", 5), rep("pab_1", 5)),
           AREA = 0,
           WIDTH = 0,
           LENGTH = 0)
  )

MAX_2022 <- WB_TYP_AREA %>%
  filter(YEAR == 1817) %>%
  summarise(MAX_AREA = sum(AREA)) %>%
  pull(MAX_AREA)

WB_TYP_AREA <- WB_TYP_AREA %>%
  mutate(AREA = (AREA/MAX_2022) * 100,
         ALPHA = ifelse(WB_TYPE == "eua_1", .8, .1))

WB_TYP_AREA$WB_TYPE <- factor(WB_TYP_AREA$WB_TYPE,
                              levels = c("eua_0", "eua_1",
                                         "eub_0", "eub_1",
                                         "paa_0",
                                         "pab_0", "pab_1",
                                         "ple_0",
                                         "tri_0",
                                         "art_0"))

CCI_MASTER_PLOT <- CCI_MASTER %>%
  group_by(DISP, SC) %>%
  mutate(NUM = (NUM/max(NUM))*100) %>%
  ungroup()

CCI_BASELINE   <- CCI_MASTER_PLOT %>%
  filter(YEAR == 1817) %>%
  dplyr::select(NUM, DISP, SC) %>%
  mutate(NUM = 100 - NUM) %>%
  rename(NUM_BASE = NUM)

CCI_MASTER_PLOT <- CCI_MASTER_PLOT %>%
  left_join(CCI_BASELINE, by = c("DISP", "SC")) %>%
  mutate(NUM = NUM + NUM_BASE) %>%
  left_join(SIGNIFICANCE %>%
              dplyr::select(DISP, SC, P),
            by = c("DISP", "SC")) %>%
  filter(!is.na(P)) %>%
  mutate(P = ifelse(P < 0.05, "Significant", "Non-significant")) %>%
  mutate(
    SC   = paste0("SC", SC),
    P    = factor(P, levels = c("Significant", "Non-significant")),
    DISP = factor(DISP, levels = rev(c("MDM", "SDM", "PD", "EUC")))
  )

CCI_ICONS <- pmap(DISP_ICONS, function(DISP, ICON, ...) {
  ICON_LAYER(ICON, X = .04, Y = .96, HEIGHT = .2, HJUST = 0, VJUST = 1,
             DATA = tibble(DISP = factor(DISP, levels = levels(CCI_MASTER_PLOT$DISP))))
})

P_CCI <- ggplot(CCI_MASTER_PLOT) +

  geom_area(data = WB_TYP_AREA, aes(x = YEAR, y = AREA, fill = WB_TYPE), alpha = .5) +
  scale_x_continuous(breaks = c(1817, 1857, 1875, 1910, 1965, 1996, 2022),
                     labels = as.character(c(1817, 1857, 1875, 1910, 1965, 1996, 2022))) +
  scale_y_continuous(name = expression(paste(bold("Deviation from baseline"), " [%]")),
                     breaks = c(   0,  50, 100, 150, 200),
                     labels = c(-100, -50,   0,  50, 100)) +
  scale_fill_manual(values = c("#468892", "#1CAC1C",
                               "#74F3D3", "#12D09E",
                               "#751C6D",
                               "#FF3BD5", "#623BC5",
                               "#FDC067",
                               "#9F6619",
                               "grey40"),
                    labels = c(
                      "Eupotamon A",
                      "Eupotamon A (impounded)",
                      "Eupotamon B",
                      "Eupotamon B (impounded)",
                      "Parapotamon A",
                      "Parapotamon B",
                      "Parapotamon B (impounded)",
                      "Plesiopotamon",
                      "Tributary",
                      "Artificial"
                    ),
                    name = "Habitat") +
  geom_hline(yintercept = 100, color = "white", linetype = "solid", linewidth = 1.5, alpha = .5) +
  CCI_ICONS +
  geom_line(data = CCI_MASTER_PLOT %>%
              filter(!(DISP == "EUC" & SC == "SC2") & !(DISP == "MDM" & SC == "SC2")),
            aes(x = YEAR, y = NUM, linetype = P, group = SC), color = "grey40", linewidth = .6) +
  geom_line(data = CCI_MASTER_PLOT %>%
              filter((DISP == "EUC" & SC == "SC2") | (DISP == "MDM" & SC == "SC2")),
            aes(x = YEAR, y = NUM, linetype = P, group = SC), color = "#33EDBC", linewidth = .6) +
  geom_line(data = CCI_MASTER_PLOT %>%
              filter(!(DISP == "EUC" & SC == "SC2") & !(DISP == "MDM" & SC == "SC2")),aes(x = YEAR, y = NUM, linetype = P, color = SC)) +
  scale_color_manual(values = c("#468892", "#74F3D3", "#FDC067", "#9F6619"),
                     name = "Habitat cluster (CCI)",
                     labels = c("SC1", "SC2", "SC3", "SC4")) +
  geom_point(aes(x = YEAR, y = NUM), size = 1.7, color = "grey40") +
  geom_point(aes(x = YEAR, y = NUM, color = SC)) +

  scale_linetype_manual(values = c("solid", "dotted"), name = NULL) +

  facet_wrap(~DISP) +

  theme(
    strip.text        = element_text(size = 6, color = "grey40", face = "bold"),
    legend.text       = element_text(size = 6, color = "grey40"),
    legend.title      = element_text(margin = margin(0,10,5,0), face = "bold", size = 8, color = "grey40"),
    legend.key.height = unit(4, "mm"),
    legend.key.width  = unit(4, "mm"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key        = element_rect(fill = "white", color = NA),
    legend.margin     = margin(0,0,0,0),
    plot.margin       = margin(0, 0, 0, 0),
    axis.text.y       = element_text(size = 6, color = "grey40"),
    axis.text.x       = element_text(size = 6, color = "grey40", angle = 45, vjust = 1.1, hjust = 1.05),
    axis.ticks        = element_line(color = "grey40"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_text(margin = margin(r = 6), face = "bold", size = 8, color = "grey40"),
    panel.grid.major  = element_line(linewidth = .4),
    panel.grid.minor  = element_line(linewidth = .2),
    aspect.ratio      = 1
  ) +

  guides(

    linetype = guide_legend(order = 2,
                            override.aes = list(color = "grey40"),
                            keywidth = unit(1.05, "lines")),
    color    = guide_legend(order = 1),
    fill     = guide_legend(order = 3)


  )

ggsave(P_CCI, filename= "./PLOTS/CCI.png", width= 174, height= 144, unit= "mm", dpi = 600)

### CC ####

RCI_MASTER_PLOT <- RCI_MASTER %>%
  st_drop_geometry() %>%
  filter(WB_TYPE != "CO") %>%
  dplyr::select(NUM, DISP, SC, CC) %>%
  as_tibble() %>%
  mutate(SC = paste0("SC", SC)) %>%
  mutate(across(c("DISP", "SC", "CC"), as.factor)) %>%
  mutate(DISP = factor(DISP, levels = c("EUC", "PD", "SDM", "MDM")))

RCI_MASTER_PLOT_SUMMARISED <- RCI_MASTER_PLOT %>%
  group_by(CC, DISP, SC) %>%
  summarise(NUM = mean(log(NUM+1))) %>%
  ungroup()

RCI_MASTER_PLOT_MEAN <- RCI_MASTER_PLOT %>%
  group_by(CC) %>%
  summarise(NUM_MEAN = mean(log(NUM+1))) %>%
  cbind(
    RCI_MASTER %>%
      st_drop_geometry() %>%
      filter(WB_TYPE != "CO") %>%
      dplyr::select(FID, YEAR, CC) %>%
      drop_na() %>%
      unique() %>%
      group_by(CC) %>%
      summarise(N = n()) %>%
      ungroup %>%
      dplyr::select(N)
  )

CC_LABELS <- c(
  "1" = "CC1",
  "2" = "CC2",
  "3" = "CC3",
  "4" = "CC4"
)

CC_LABELS_2 <- setNames(
  lapply(seq_len(nrow(RCI_MASTER_PLOT_MEAN)), function(i) {
    mn_txt <- as.character(round(RCI_MASTER_PLOT_MEAN$NUM_MEAN[i], 2))
    nn     <- RCI_MASTER_PLOT_MEAN$N[i]
    bquote(
      atop(
        bar(ln(RCI+1)) == bold(.(mn_txt)), .(paste0("n = ", nn))
      )
    )
  }),
  as.character(RCI_MASTER_PLOT_MEAN$CC)
)

RCI_MASTER_PLOT$CC <- factor(
  RCI_MASTER_PLOT$CC,
  levels = names(CC_LABELS_2),
  labels = CC_LABELS_2
)

RCI_MASTER_PLOT_MEAN$CC <- factor(
  RCI_MASTER_PLOT_MEAN$CC,
  levels = names(CC_LABELS_2),
  labels = CC_LABELS_2
)

CC_ICONS <- pmap(DISP_ICONS, function(ICON, X, Y, ...) ICON_LAYER(ICON, X = X, Y = Y, HEIGHT = .13))

P_CC_1 <- ggplot(RCI_MASTER_PLOT_SUMMARISED,
                 aes(x    = DISP,
                     y    = NUM,
                     fill = SC)) +

  geom_bar(stat     = "identity", alpha = .8,
           position = position_dodge2(width = 0.9, preserve = "single", padding = 0)) +
  CC_ICONS +
  scale_fill_manual(values =  c("#468892", "#74F3D3", "#FDC067", "#9F6619"), name = "Suitability cluster",
                    labels = c("SC1", "SC2", "SC3", "SC4")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_x_discrete(position = "top", name = "Connectivity cluster\n") +
  facet_wrap(~ CC, nrow = 1, scales = "fixed", labeller = as_labeller(CC_LABELS)) +

  coord_polar() +
  theme_minimal() +
  theme(
    axis.text.y          = element_blank(),
    axis.text.x          = element_text(size = 6, color = "grey40", margin = margin(t = 10)),
    axis.title.y         = element_blank(),
    axis.title.x         = element_text(size = 8, color = "grey40", face = "bold"),
    strip.text           = element_markdown(face = "plain", size = 8, color = "grey40"),
    legend.margin        = margin(0,0,0,0),
    legend.position      = "top",
    legend.direction     = "horizontal",
    legend.text          = element_text(size= 6, color = "grey40"),
    legend.title         = element_text(margin = margin(0,10,12,0), face = "bold", size= 8, color = "grey40"),
    legend.key.height    = unit(4,"mm"),
    legend.key.width     = unit(4,"mm"),
    panel.background     = element_rect(fill = "white", color = "white"),
    plot.background      = element_rect(fill = "white", color = "white"),
    plot.margin          = margin(0, 0, -10, 17),
    plot.caption         = element_markdown(hjust = .5, vjust = 10, size = 6)
  ) +

  guides(
    fill  = guide_legend(
      nrow           = 1,
      byrow          = TRUE,
      label.position = "bottom",
      label.hjust    = 0.5,
      title.position = "left"
    )
  )

LEGEND <- ggpubr::get_legend(P_CC_1)
LEGEND <- ggpubr::as_ggplot(LEGEND) + theme(plot.margin = margin(-35,50,-42,0))
P_CC_1 <- P_CC_1 + theme(legend.position = "none")

P_CC_2 <- ggplot(RCI_MASTER_PLOT) +
  geom_vline(
    data      = RCI_MASTER_PLOT_MEAN,
    aes(xintercept = NUM_MEAN),
    linetype  = 3, linewidth = 0.5, color = "grey40"
  ) +
  geom_density_ridges(
    aes(
      x     = log(NUM+1),
      group = interaction(SC, DISP),
      y     = DISP,
      fill  = SC
    ),
    alpha          = 0.6,
    scale          = 1.5,
    rel_min_height = 0.01,
    linewidth      = 0.1,
    color          = NA
  ) +
  scale_y_discrete(expand = expansion(add = c(0, 2))) +
  scale_x_continuous(name = expression(paste("ln(", bold("RCI"), "+1)"))) +
  facet_wrap(
    ~ CC,
    nrow     = 1,
    scales   = "fixed",
    labeller = labeller(CC = label_parsed)
  ) +
  scale_fill_manual(
    values = c("#468892", "#74F3D3", "#FDC067", "#9F6619"),
    guide  = "none"
  ) +
  theme(
    strip.text       = element_text(size = 6),
    strip.background = element_rect(fill = "white", color = "white"),
    axis.title.y     = element_blank(),
    axis.title.x     = element_text(size = 8, color = "grey40", face = "bold"),
    axis.text.x      = element_text(size = 6, color = "grey40", angle = 40, hjust = 1),
    axis.text.y      = element_text(size = 6, color = "grey40"),
    axis.ticks.y     = element_blank(),
    axis.ticks.x     = element_line(color = "grey40"),
    panel.grid.major = element_line(color = "white", linewidth = 0.3),
    panel.grid.minor = element_line(color = "white", linewidth = 0.15),
    plot.background  = element_rect(fill = "white", color = "white"),
    plot.margin      = margin(0, 0, 0, 0),
    aspect.ratio = 1
  )

P_CC <- ggpubr::ggarrange(P_CC_1, P_CC_2, LEGEND, ncol = 1, heights = c(.9, 1, .25)) +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        plot.margin     = margin(-3, 0, -8, 0))

ggsave(P_CC, filename= "./PLOTS/CC.png", width= 174, height= 128, unit= "mm", dpi = 600)

### ROBUSTNESS ####

ROBUSTNESS_PLOT <- ROBUSTNESS %>%
  drop_na(DIST_MEAN) %>%
  slice_sample(prop = 1) %>%
  mutate(DIST_MEAN = DIST_MEAN / NR_PATHS) %>%
  group_by(REMOVED, YEAR) %>%
  summarise(DIST_SD   = sd(DIST_MEAN),
            DIST_MEAN = mean(DIST_MEAN)) %>%
  ungroup() %>%
  mutate(INT_LOW = DIST_MEAN - DIST_SD,
         INT_UP  = DIST_MEAN + DIST_SD)

P_ROB <- ggplot(ROBUSTNESS_PLOT,
                aes(x     = REMOVED,
                    y     = log(DIST_MEAN + 1),
                    group = as.factor(YEAR),
                    color = as.factor(YEAR)))+
  geom_ribbon(aes(ymin = log(INT_LOW + 1), ymax = log(INT_UP + 1)),
              color = NA,
              fill = alpha("grey40", .5)) +
  geom_line(linewidth = .3) +
  geom_rug(
    color = "grey40",
    alpha       = .25,
    sides       = "l",
    linewidth   = .2,
    length      = unit(3, "mm"),
    position    = position_jitter(width = 0, height = 0)
  ) +

  scale_y_continuous(name = expression(paste("log(", bold("Path length"), ")", collapse = ""))) +
  scale_x_continuous(name = "Nodes removed", expand = expansion(mult = .1)) +
  scale_color_manual(values = alpha(c("#468892",
                                      "#74F3D3",
                                      "#751C6D",
                                      "#FF3BD5",
                                      "#FDC067",
                                      "#9F6619",
                                      "grey30"), 1), guide = "none") +
  scale_fill_manual(values =  alpha(c("#468892",
                                      "#74F3D3",
                                      "#751C6D",
                                      "#FF3BD5",
                                      "#FDC067",
                                      "#9F6619",
                                      "grey30"), 1), guide = "none") +
  theme(
    strip.text        = element_text(size = 6, color = "grey40", face = "bold"),
    plot.margin       = margin(0, 5, 0, 5),
    axis.text.y       = element_text(size = 6, color = "grey40"),
    axis.text.x       = element_text(size = 6, color = "grey40", angle = 45, vjust = 1.1, hjust = 1.05),
    axis.ticks        = element_line(color = "grey40"),
    axis.title.x      = element_text(margin = margin(t = 6), face = "bold", size = 8, color = "grey40"),
    axis.title.y      = element_text(margin = margin(r = 6), face = "bold", size = 8, color = "grey40"),
    panel.grid.major  = element_line(linewidth = .4),
    panel.grid.minor  = element_line(linewidth = .2),
    aspect.ratio      = 1,
    plot.title        = element_text(size = 8, color = "grey40", hjust = 0, vjust = 0)
  ) +
  labs(title = expression(paste(bold("A"))))

### DISSIMILARITY ####

P_DISS <- ggplot(DISSIMILARITY, aes(x = as.numeric(YEAR), y = DISSIMILARITY, fill = as.factor(YEAR))) +
  geom_boxplot(color= "grey40", alpha= .8, linewidth= .3, notch = F, width = 15, outliers = T, outlier.size = .5) +
  scale_x_continuous(breaks = c(1817, 1857, 1875, 1910, 1965, 1996, 2022)) +
  scale_y_continuous(name = "Dissimilarity") +
  scale_fill_manual(values = c("#468892",
                               "#74F3D3",
                               "#751C6D",
                               "#FF3BD5",
                               "#FDC067",
                               "#9F6619",
                               "grey40"), guide = "none") +
  theme(
    plot.margin       = margin(0, 5, 0, 5),
    axis.text.y       = element_text(size = 6, color = "grey40"),
    axis.text.x       = element_text(size = 6, color = "grey40", angle = 45, vjust = 1.1, hjust = 1.05),
    axis.ticks        = element_line(color = "grey40"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_text(margin = margin(r = 6), face = "bold", size = 8, color = "grey40"),
    panel.grid.major  = element_line(size = .4),
    panel.grid.minor  = element_line(size = .2),
    aspect.ratio      = 1,
    plot.title        = element_text(size = 8, color = "grey40", hjust = 0, vjust = 0)
  ) +
  labs(title = expression(paste(bold("B"))))

### MODULARITY ####

P_MOD <- ggplot(MODULARITY, aes(x = as.numeric(YEARS), y = MD, fill = as.factor(YEAR))) +
  geom_bar(stat = "identity", alpha = .8, color = "grey40", linewidth = .3) +
  scale_x_continuous(breaks = c(1817, 1857, 1875, 1910, 1965, 1996, 2022)) +
  scale_y_continuous(name = "Normalized CC modularity") +
  scale_fill_manual(values = c("#468892",
                               "#74F3D3",
                               "#751C6D",
                               "#FF3BD5",
                               "#FDC067",
                               "#9F6619",
                               "grey40"), name = "Year") +
  coord_cartesian(ylim = c(.7, 1)) +
  theme(
    plot.margin       = margin(0, 5, 0, 5),
    axis.text.y       = element_text(size = 6, color = "grey40"),
    axis.text.x       = element_text(size = 6, color = "grey40", angle = 45, vjust = 1.1, hjust = 1.05),
    axis.ticks        = element_line(color = "grey40"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_text(margin = margin(r = 6), face = "bold", size = 8, color = "grey40"),
    panel.grid.major  = element_line(linewidth = .4),
    panel.grid.minor  = element_line(linewidth = .2),
    aspect.ratio      = 1,
    plot.title        = element_text(size = 8, color = "grey40", hjust = 0, vjust = 0),
    legend.direction  = "horizontal",
    legend.position   = "bottom",
    legend.text       = element_text(size= 6, color = "grey40", margin = margin(0,6,0,3)),
    legend.title      = element_text(margin = margin(0,10,0,0), face = "bold", size= 8, color = "grey40"),
    legend.key.height = unit(4,"mm"),
    legend.key.width  = unit(4,"mm"),
    legend.box.margin = margin(8,0,13,30)
  ) +
  labs(title = expression(paste(bold("C")))) +
  guides(
    fill = guide_legend(
      nrow = 1, byrow = TRUE,
      override.aes = list(
        alpha    = 1,
        shape    = 22,
        size     = 5,
        fill     = c("#468892", "#74F3D3", "#751C6D", "#FF3BD5", "#FDC067", "#9F6619", "grey40"),
        linetype = 0
      )
    )
  )

LEGEND_YEARS <- ggpubr::get_legend(P_MOD)
LEGEND_YEARS <- ggpubr::as_ggplot(LEGEND_YEARS)
P_MOD        <- P_MOD + theme(legend.position = "none")

### ROB. + DISSIM. + MOD. ####

RESILIENCE <- LEGEND_YEARS / (P_ROB + P_DISS + P_MOD) +
  plot_layout(heights = c(.1, 1))

ggsave(RESILIENCE, filename= "./PLOTS/RESILIENCE.png", width= 174, height= 72, unit= "mm", dpi = 600, bg = "white")

### SC ####

set.seed(40)
NMDS <- metaMDS(SC_TAXA %>%
                  dplyr::select(ID_TAXA, H1, H2, H3, H4, H5) %>%
                  column_to_rownames("ID_TAXA")  %>%
                  unique(),
                k= 2, trymax= 10000, parallel= 5, distance= "euclidean")

stressplot(NMDS)

NMDS_TABLE <- NMDS$points %>%
  as_tibble() %>%
  cbind(ID = SC_TAXA %>%
          dplyr::select(ID_TAXA, H1, H2, H3, H4, H5) %>%
          column_to_rownames("ID_TAXA")  %>%
          unique() %>%
          rownames_to_column(var = "ID_TAXA") %>%
          mutate(ID = as.numeric(ID_TAXA)) %>%
          pull(ID)) %>%
  left_join(SC_TAXA %>%
              dplyr::select(ID_TAXA, SC, H1, H2, H3, H4, H5) %>%
              unique(), by = c("ID" = "ID_TAXA")
  )

NMDS_TABLE_LONG <- NMDS_TABLE %>%
  pivot_longer(cols = starts_with("H"), names_to = "HABITAT", values_to = "VALUE") %>%
  rbind(c(0,0,NA,NA,"Clusters",NA)) %>%
  mutate(across(c(MDS1, MDS2, VALUE), ~ as.numeric(.)))

COLOR_SCALE <- c("#468892", "#74F3D3","#751C6D",  "#FF3BD5", "#FDC067")

NMDS_TABLE_LONG <- NMDS_TABLE_LONG %>%
  mutate(SC = case_when(
    SC == "1" ~ "H1",
    SC == "2" ~ "H2",
    SC == "3" ~ "H3",
    SC == "4" ~ "H4",
    SC == "5" ~ "H5",
    SC == "Clusters" ~ "Clusters"),
    HABITAT = ifelse(HABITAT == "Clusters", "Clusters", HABITAT)
  ) %>%
  mutate(HABITAT = factor(HABITAT, levels = c("H1", "H2", "H3", "H4", "H5", "Clusters")))

P_INSET <- ggplot(NMDS_TABLE_LONG %>% filter(HABITAT == "H1"),
                  aes(x= MDS1, y= MDS2, shape = factor(SC), fill = factor(SC))) +
  geom_point(alpha = .8, color = "black", stroke = .2) +
  scale_fill_manual(values  = c("grey20", "grey40", "grey60", "grey70", "grey90"),
                    name= "Suitability cluster",
                    labels = c("SC1", "SC2", "SC3", "SC4", "SC5"))+
  scale_shape_manual(values  = c(25,22,21,23,24),
                     name= "Suitability cluster",
                     labels = c("SC1", "SC2", "SC3", "SC4", "SC5"))+
  theme(
    legend.position   = "top",
    legend.text       = element_text(size= 6, color = "grey40"),
    legend.title      = element_text(margin = margin(0,10,12,0), face = "bold", size= 8, color = "grey40"),
    legend.key.height = unit(4,"mm"),
    legend.key.width  = unit(4,"mm"),
    legend.box.margin = margin(-20,0,0,70),
    axis.title        = element_blank(),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    plot.margin       = margin(0, 0, 0, 0),
    plot.background   = element_rect(fill='transparent', color=NA),
    panel.grid.major  = element_line(linewidth = .4),
    panel.grid.minor  = element_line(linewidth = .2),
    strip.text        = element_text(size= 6, face= "bold")
  ) +
  coord_equal() +
  guides(
    shape = guide_legend(
      nrow           = 1,
      byrow          = TRUE,
      label.position = "bottom",
      label.hjust    = 0.5,
      title.position = "left"
    ),

    fill = guide_legend(
      nrow           = 1,
      byrow          = TRUE,
      label.position = "bottom",
      label.hjust    = 0.5,
      title.position = "left"
    )
  )

LEGEND_CL <- ggpubr::get_legend(P_INSET)
LEGEND_CL <- ggpubr::as_ggplot(LEGEND_CL)

P_INSET <- P_INSET + theme(legend.position = "none")

P_MAIN <- ggplot(NMDS_TABLE_LONG, aes(x= MDS1, y= MDS2, color= VALUE)) +
  geom_point(alpha= .6) +
  scale_color_gradientn(colors = COLOR_SCALE, name= "Habitat suitability",breaks = c(0, 2, 4, 6, 8, 10)) +
  facet_wrap(~HABITAT) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size= 6, color = "grey40"),
    legend.title      = element_text(margin = margin(r = 10, b = 12), size= 8, face = "bold", color = "grey40"),
    legend.key.height = unit(3,"mm"),
    legend.key.width  = unit(5,"mm"),
    legend.box.margin = margin(30,0,0,140),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    axis.title        = element_text(face = "bold", size = 8, color = "grey40"),
    plot.background   = element_rect(fill ="white", color = "white"),
    strip.text        = element_text(size = 8, face= "bold", color = "grey40"),
    panel.grid.major  = element_line(linewidth = .4),
    panel.grid.minor  = element_line(linewidth = .2),
    plot.margin       = margin(0, 0, 0, 0),
    plot.title        = element_text(size = 8, face = "bold", color = "grey40", hjust = .5)
  ) +
  coord_equal() +
  labs(title = expression(paste(bold("Habitat "))))

LEGEND_SC <- ggpubr::get_legend(P_MAIN)
LEGEND_SC <- ggpubr::as_ggplot(LEGEND_SC)

P_MAIN <- P_MAIN + theme(legend.position = "none")

LEGENDS <- ggpubr::ggarrange(LEGEND_SC, LEGEND_CL, ncol= 2, heights= c(10, 1)) + theme(plot.margin = margin(0,0,0,0))
LEGENDS <- LEGENDS + theme(plot.margin = margin(35,0,20,30))

P_SC <- cowplot::ggdraw() +
  cowplot::draw_plot(P_MAIN) +
  cowplot::draw_grob(ggplotGrob(P_INSET), x = 0.633, y = .03, width = 0.415, height = 0.415)

P_SC <- ggpubr::ggarrange(P_SC, LEGENDS, ncol= 1, heights= c(10, 1)) + theme(plot.margin = margin(0, 0, 15, 0)) +
  theme(plot.background  = element_rect(fill="white", color = "white"))

ggsave(P_SC, filename= "./PLOTS/SC.png", width= 174, height= 150, unit= "mm", dpi = 600)

### SC ERROR ####

SC_TAXA_PLOT <- SC_TAXA %>%
  dplyr::select(SC, ERROR_MAX, ERROR_CUM) %>%
  pivot_longer(cols = c("ERROR_MAX", "ERROR_CUM"), values_to = "VALUES", names_to = "VARIABLE") %>%
  mutate(GROUP_POS = case_when(
    VARIABLE == "ERROR_MAX"     ~ 1,
    VARIABLE == "ERROR_CUM" ~ 1.2
  ),

  SC = case_when(

    SC == 1 ~ "H1",
    SC == 2 ~ "H2",
    SC == 3 ~ "H3",
    SC == 4 ~ "H4",
    SC == 5 ~ "H5"

  )) %>%
  mutate(VARIABLE = factor(VARIABLE, levels = c("ERROR_MAX, ERROR_CUM")))

COLOR_SCALE <- c("#468892", "#74F3D3", "#FDC067", "#9F6619","#751C6D")

P_SC_ERROR <- ggplot(SC_TAXA_PLOT, aes(y= VALUES, group = GROUP_POS, x = GROUP_POS))+
  geom_boxplot(color= "grey40", alpha= .5, linewidth= .3, notch = F, width = .1, outliers = F, staplewidth = .25) +
  geom_jitter(aes(color = as.factor(SC)), alpha = .4, width = .04, size = .1) +
  scale_color_manual(values = COLOR_SCALE, name = "Suitability cluster", labels = c("SC1", "SC2", "SC3", "SC4", "SC5")) +
  scale_y_continuous(name= TeX("\\textbf{Deviation from } \\textit{$S_{sc,h}$}")) +
  scale_x_continuous(breaks = c(1, 1.2), labels = c("Maximum", "Cumulative"), limits = c(.9, 1.3)) +
  theme(
    legend.position    = "right",
    legend.box.just    = "top",
    legend.text        = element_text(size = 6, color = "grey40"),
    legend.title       = element_text(size = 8, color = "grey40", face = "bold"),
    legend.key.height  = unit(4, "mm"),
    legend.key.width   = unit(4, "mm"),
    legend.background  = element_rect(fill = "white", color = NA),
    legend.margin      = margin(t = 0, r = 0, b = 0, l = -5),
    legend.key         = element_rect(fill = "white", color = NA),
    axis.title.y       = element_text(face= "bold", size= 8, color = "grey40"),
    axis.text.y        = element_text(size= 6, color = "grey40"),
    axis.title.x       = element_blank(),
    axis.text.x        = element_text(size= 8, color = "grey40", face = "bold"),
    axis.ticks.x       = element_blank(),
    axis.ticks         = element_line(color = "grey40"),
    plot.margin        = margin(0,20,18,0),
    plot.title         = element_text(size = 8, color = "grey40", hjust = 0, vjust = 0),
    panel.grid.major.y = element_line(linewidth = .4),
    panel.grid.minor.y = element_line(linewidth = .2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    aspect.ratio = 1
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        alpha = 1,
        shape = 22,
        size  = 5,
        fill  = COLOR_SCALE,
        linetype = 0
      )
    )
  ) +
  labs(title = expression(paste(bold("A"))))

### WIDTH ####

WIDTHS_PLOT <-  WIDTHS %>%
  filter(IMP == 0, SECTION == "GENERAL") %>%
  mutate(LAB = case_when(

    WB_TYPE == "eua" ~ "Eupotamon A",
    WB_TYPE == "eub" ~ "Eupotamon B",
    WB_TYPE == "paa" ~ "Parapotamon A",
    WB_TYPE == "pab" ~ "Parapotamon B",
    WB_TYPE == "ple" ~ "Plesiopotamon",
    WB_TYPE == "tri" ~ "Tributary",
    WB_TYPE == "art" ~ "Artificial"

  ))

WIDTHS_PLOT$LAB <- factor(WIDTHS_PLOT$LAB, levels = c("Eupotamon A", "Eupotamon B", "Parapotamon A", "Parapotamon B", "Plesiopotamon", "Tributary", "Artificial"))

P_WIDTH <- ggplot(WIDTHS_PLOT, aes(x = YEAR, y = WIDTH, color = LAB)) +
  geom_line() +
  geom_point(aes(x = YEAR, y = WIDTH), color = "grey40", , size = 1.7, inherit.aes = FALSE)+
  geom_point() +
  scale_color_manual(
    values = c("#468892", "#74F3D3", "#751C6D", "#FF3BD5", "#FDC067", "#9F6619", "grey40"),
    name   = "Habitat"
  ) +

  scale_x_continuous(breaks = c(1817, 1857, 1875, 1910, 1965, 1996, 2022),
                     labels = as.character(c(1817, 1857, 1875, 1910, 1965, 1996, 2022))) +
  labs(x = "Year", y = expression(paste(bold("Average width"), " [m]")), color = "") +
  guides(
    color = guide_legend(
      override.aes = list(
        alpha = 1,
        shape = 22,
        size  = 5,
        fill  = c("#468892", "#74F3D3", "#751C6D", "#FF3BD5", "#FDC067", "#9F6619", "grey40"),
        linetype = 0
      )
    )
  ) +

  theme(
    legend.position   = "right",
    legend.text       = element_text(size = 6, color = "grey40"),
    legend.title      = element_text(size = 8, color = "grey40", face = "bold"),
    legend.key.height = unit(4, "mm"),
    legend.key.width  = unit(4, "mm"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key        = element_rect(fill = "white", color = NA),
    legend.margin     = margin(t = 0, r = 0, b = 0, l = -5),
    plot.margin       = margin(0,0,0,0),
    plot.title        = element_text(size = 8, color = "grey40", hjust = 0, vjust = 0),
    axis.text.y       = element_text(size = 6, color = "grey40"),
    axis.text.x       = element_text(size = 6, color = "grey40", angle = 45, vjust = 1.1, hjust = 1.05),
    axis.ticks        = element_line(color = "grey40"),
    axis.title.x      = element_text(face = "bold", size = 8, color = "grey40"),
    axis.title.y      = element_text(margin = margin(r = 6), face = "bold", size = 8, color = "grey40"),
    panel.grid.major  = element_line(linewidth = .4),
    panel.grid.minor  = element_line(linewidth = .2),
    aspect.ratio      = 1
  ) +
  labs(title = expression(paste(bold("B"))))

### SC ERROR + WIDTH ####

ERROR_WIDTH <- ggpubr::ggarrange(P_SC_ERROR, P_WIDTH, widths = c(1,.91)) +
  theme(plot.margin = margin(0,0,0,1), plot.background = element_rect(fill = "white", color = "white"))

ggsave(ERROR_WIDTH, filename= "./PLOTS/ERROR_WIDTHS.png", width= 174, height= 63, unit= "mm", dpi = 600)

### GROUP + DISP SC ####

SC_TAXA_PLOT_GROUP <- SC_TAXA %>%
  group_by(GROUP, SC) %>%
  summarise(N_TAXA = n()) %>%
  ungroup() %>%
  mutate(GROUP = case_when(

    GROUP == "PLANT" ~ "Plant",
    GROUP == "AMPH"  ~ "Amph.",
    GROUP == "FISH"  ~ "Fish",
    TRUE             ~ GROUP

  )) %>%
  mutate(VAR   = "GROUP",
         GROUP = factor(GROUP, levels = rev(c("Plant", "NFI", "FI", "Amph.", "Fish"))))

SC_TAXA_PLOT_DISP <- SC_TAXA %>%
  group_by(DISP, SC) %>%
  summarise(N_TAXA = n()) %>%
  mutate(VAR  = "DISP",
         DISP = factor(DISP, levels = c("PD", "EUC", "SDM", "MDM")))

P_GROUP <- ggplot(SC_TAXA_PLOT_GROUP, aes(x = as.factor(SC), y = N_TAXA, fill = GROUP))+
  geom_bar(stat = "identity", alpha = .75) +
  scale_fill_manual(values = c("#468892", "#74F3D3", "#751C6D",  "#FF3BD5", "#FDC067"), name = "Group") +
  scale_x_discrete(name = "SC") +
  scale_y_continuous(name = "Number of taxa") +
  theme(

    plot.title            = element_text(size = 8, color = "grey40", face = "bold"),

    axis.text             = element_text(size = 6, color = "grey40"),
    axis.title            = element_text(size = 8, color = "grey40", face = "bold"),
    axis.ticks.y          = element_line(color = "grey40"),
    axis.ticks.x          = element_blank(),

    legend.text           = element_text(size = 6, color = "grey40", margin = margin(0,5,0,3)),
    legend.title          = element_text(size = 8, color = "grey40", face = "bold", margin = margin(0,15,1,0)),
    legend.ticks          =  element_line(color = "grey40"),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.background     = element_rect(fill = "transparent", color = NA),
    legend.key.height     = unit(4, "mm"),
    legend.key.width      = unit(4, "mm"),
    legend.margin         = margin(0,0,0,0),
    legend.key            = element_rect(fill = "transparent", color = NA),
    legend.position       = "bottom",
    legend.direction      = "horizontal",

    panel.grid.major.y    = element_line(linewidth = .4),
    panel.grid.major.x    = element_blank(),
    panel.grid.minor      = element_line(linewidth = .2),
    plot.background       = element_rect(fill = "transparent", color = NA),
    plot.margin           = margin(0,0,0,0),
    aspect.ratio          = 1

  ) +
  labs(title = "A")

P_DISP <- ggplot(SC_TAXA_PLOT_DISP, aes(x = as.factor(SC), y = N_TAXA, fill = DISP))+
  geom_bar(stat = "identity", alpha = .75) +
  scale_fill_manual(values = c("#468892", "#74F3D3", "#751C6D",  "#FF3BD5"), name = "Dispersal") +
  scale_x_discrete(name = "SC") +
  scale_y_continuous() +
  theme(

    plot.title            = element_text(size = 8, color = "grey40", face = "bold"),

    axis.text             = element_text(size = 6, color = "grey40"),
    axis.title.x          = element_text(size = 8, color = "grey40", face = "bold"),
    axis.title.y          = element_blank(),
    axis.ticks.y          = element_line(color = "grey40"),
    axis.ticks.x          = element_blank(),

    legend.text           = element_text(size = 6, color = "grey40", margin = margin(0,5,0,3)),
    legend.title          = element_text(size = 8, color = "grey40", face = "bold", margin = margin(0,15,1,0)),
    legend.ticks          =  element_line(color = "grey40"),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.background     = element_rect(fill = "transparent", color = NA),
    legend.key.height     = unit(4, "mm"),
    legend.key.width      = unit(4, "mm"),
    legend.margin         = margin(0,0,0,0),
    legend.key            = element_rect(fill = "transparent", color = NA),
    legend.position       = "bottom",
    legend.direction      = "horizontal",

    panel.grid.major.y    = element_line(linewidth = .4),
    panel.grid.major.x    = element_blank(),
    panel.grid.minor      = element_line(linewidth = .2),
    plot.background       = element_rect(fill = "transparent", color = NA),
    plot.margin           = margin(0,0,0,5),
    aspect.ratio          = 1

  ) +
  labs(title = "B")

P_TAXA_SC <- ggpubr::ggarrange(P_GROUP, P_DISP) +
  theme(plot.margin = margin(0,-5,0,0))
ggsave(P_TAXA_SC, filename= "./PLOTS/TAXA_SC.png", width= 174, height= 95, unit= "mm", dpi = 600)

### K CLUSTER ####

K_WSS <- rbind(
  K_WSS_SC %>% mutate(VARIABLE = "SC"),
  K_WSS_CC %>% mutate(VARIABLE = "CC")
) %>%
  mutate(K = as.numeric(K)) %>%
  group_by(VARIABLE) %>%
  mutate(WSS = WSS/max(WSS)) %>%
  ungroup() %>%
  mutate(VARIABLE = factor(VARIABLE, levels = c("SC", "CC")))

P_K <- ggplot(K_WSS) +
  geom_point(aes(x = K, y = WSS), color = "grey40", alpha =.8) +
  geom_vline(data = tibble(K = c(5,4), VARIABLE = factor(c("SC", "CC"), levels = c("SC", "CC"))), aes(xintercept = K), color = "grey40", linetype = "dotted") +
  scale_x_continuous(breaks = seq(1,10,1)) +
  labs(x = "Number of clusters", y = expression(paste(bold("WSS "), "[%]"))) +
  facet_wrap(~VARIABLE) +

  theme(
    axis.text            = element_text(size = 6, color = "grey40"),
    axis.title.y         = element_text(size = 8, color = "grey40", , face = "bold", margin = margin(r = 10)),
    axis.title.x         = element_text(size = 8, color = "grey40", , face = "bold", margin = margin(t = 10)),
    strip.text           = element_markdown(face = "bold", size = 8, color = "grey40"),
    axis.ticks           = element_line(color = "grey40"),
    plot.background      = element_rect(fill = "white", color = "white"),
    plot.margin          = margin(0, 0, 0, 0),
    panel.grid.major.y   = element_line(linewidth = .4),
    panel.grid.minor.y   = element_line(linewidth = .2),
    panel.grid.major.x   = element_line(linewidth = .4),
    panel.grid.minor.x   = element_blank(),
    aspect.ratio = 1
  )

ggsave(P_K, filename= "./PLOTS/K.png", width= 174, height= 95, unit= "mm", dpi = 600)

### VALIDATION ####

VALIDATION <- VALIDATION %>%
  mutate(SC   = factor(SC, levels = c(1:4)),
         PRES = factor(PRES, levels = c("Present", "Absent")),
         DISP = factor(DISP, levels = c("EUC", "PD", "SDM", "MDM")))

SIGNIFICANCE <- SIGNIFICANCE %>%
  left_join(CLUSTER_VARIABLES, by = c("DISP", "SC")) %>%
  mutate(LABEL = ifelse(is.na(LABEL), " ", LABEL),
         SC    = factor(SC, levels = c(1:4)),
         DISP  = factor(DISP, levels = c("EUC", "PD", "SDM", "MDM")))

MEDIANS <- VALIDATION %>%
  st_drop_geometry() %>%
  group_by(SC, DISP, PRES) %>%
  summarise(MEDIAN_NUM = median(NUM, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = PRES, values_from = MEDIAN_NUM) %>%
  mutate(DISP = factor(DISP, levels = c("EUC", "PD", "SDM", "MDM")))

P_VALIDATION <- ggplot(VALIDATION, aes(y = NUM, x = PRES)) +
  geom_rect(
    data = distinct(VALIDATION, SC, DISP),
    aes(fill = SC),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE,
    alpha = 0.30
  ) +

  geom_hline(
    yintercept = seq(0, .9, .3),
    color = "white",
    size = 0.3
  ) +

  geom_boxplot(alpha = .6, size = .1, width = 0.5, outlier.size = .5, color = "grey40") +

  geom_segment(
    data = MEDIANS,
    aes(x = 1, xend = 2, y = Present, yend = Absent),
    inherit.aes = FALSE,
    color = "grey40",
    size = 0.3,
    linetype = "dashed"
  ) +

  stat_summary(fun = median, geom = "point", shape = 23, size = 1.5,
               fill = "#FDC067", color = "grey40") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3)), breaks = seq(0, 1, .25), position = "right") +
  scale_fill_manual(values = c("1" = "#468892", "2" = "#74F3D3",
                               "3" = "#751C6D", "4" = "#FF3BD5"),
                    guide = "none") +
  facet_grid(DISP ~ SC, switch="y",
             labeller = labeller(SC = c("1" = "SC1",
                                        "2" = "SC2",
                                        "3" = "SC3",
                                        "4" = "SC4"))
  ) +

  geom_text(data = SIGNIFICANCE, aes(x = 1.5, y = Inf, label = LEVEL),
            inherit.aes = FALSE, vjust = 1.2, size = 3, fontface = "bold") +
  geom_text(data = SIGNIFICANCE, aes(x = 0.5, y = Inf, label = paste0("n = ", N_Present)),
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.6, size = 2) +
  geom_text(data = SIGNIFICANCE, aes(x = 2.5, y = Inf, label = paste0("n = ", N_Absent)),
            inherit.aes = FALSE, hjust = 1.1, vjust = 1.6, size = 2) +
  geom_text(data = SIGNIFICANCE, aes(x = 1.5, y = Inf, label = LABEL),
            inherit.aes = FALSE, vjust = 3.25, size = 2) +

  theme(
    panel.grid         = element_blank(),
    axis.text.y        = element_text(size = 6, color = "grey40"),
    axis.text.x        = element_text(size = 8, color = "grey40", face = "bold"),
    axis.title.y.right = element_text(size = 8, color = "grey40", face = "bold", margin = margin(l = 10)),
    strip.text         = element_markdown(size = 8, color = "grey40", lineheight = 1.3, face = "bold"),
    axis.ticks.x       = element_blank(),
    axis.ticks.y       = element_line(color = "grey40"),
    plot.margin        = margin(0, 0, 0, 0)
  ) +
  labs(y = expression(paste(bold("RCI"), " (rescaled)")))

P_VALIDATION <- ggplotGrob(P_VALIDATION)
idx <- which(P_VALIDATION$layout$name %in% c("panel-4-3", "panel-4-4", "panel-3-4"))
for (i in idx) P_VALIDATION$grobs[[i]] <- nullGrob()

idx <- which(P_VALIDATION$layout$name %in% c("axis-b-3", "axis-b-4"))
P_VALIDATION$layout[idx, c("t", "b")] <- P_VALIDATION$layout[idx, c("t", "b")] - c(2, 4)

idx <- which(P_VALIDATION$layout$name %in% c("axis-r-3", "axis-r-4"))
P_VALIDATION$layout[idx, c("l", "r")] <- P_VALIDATION$layout[idx, c("l", "r")] + c(-2, -4)

agg_png("./PLOTS/VALIDATION.png", width= 174/25.4*600, height= 150/25.4*600, res = 600)
grid.draw(P_VALIDATION)
dev.off()

## CONCEPTIONAL ####

EQ_PT <- 7

MATH_FONT <- LaTeXpackage(
  name     = "unicode-math",
  preamble = sprintf(
    "\\usepackage{unicode-math}\\setmathfont{latinmodern-math.otf}[SizeFeatures = {{Size = %.2f-}, {Size = %.2f-%.2f, Style = MathScript}, {Size = -%.2f, Style = MathScriptScript}}]",
    .85 * EQ_PT, .6 * EQ_PT, .85 * EQ_PT, .6 * EQ_PT
  )
)

FONT <- "sans"

TEAL    <- "#468892"
MINT    <- "#74F3D3"
PURPLE  <- "#751C6D"
MAGENTA <- "#FF3BD5"
PEACH   <- "#FDC067"
BROWN   <- "#9F6619"
GREY    <- "#666666"
STRIP   <- "#D9D9D9"
PANEL   <- "#EBEBEB"
LINE    <- "#9E9E9E"
INK     <- "#4D4D4D"
TXT     <- "#333333"

MDM_ICON[, , 1:3] <- col2rgb(INK) / 255
SDM_ICON[, , 1:3] <- col2rgb(INK) / 255
PD_ICON[, , 1:3]  <- col2rgb(INK) / 255
EUC_ICON[, , 1:3] <- col2rgb(INK) / 255

FIG_W   <- 172
X_LEFT  <- 12
COL_W   <- 29.4
GAP     <- 2.75
ROW_GAP <- 7
HEAD    <- 5
PAD     <- 2
TOP     <- 1.4
KEY_DY  <- PAD + 1.8

COL_X0 <- X_LEFT + 0:4 * (COL_W + GAP)
COL_X1 <- COL_X0 + COL_W
COL_XM <- COL_X0 + COL_W / 2

Y_RES <- 2 + c(0, 24)
Y_CLU <- Y_RES[2] + ROW_GAP + c(0, 21)
Y_IDX <- Y_CLU[2] + ROW_GAP + c(0, 24)
Y_NET <- Y_IDX[2] + ROW_GAP + c(0, 28)
Y_DER <- Y_NET[2] + ROW_GAP + c(0, 24)
Y_DAT <- Y_DER[2] + ROW_GAP + c(0, 37)

FIG_H <- Y_DAT[2] + 2

X_MID   <- c(COL_X0[3], COL_XM[4] - GAP / 2)
X_RIGHT <- c(COL_XM[4] + GAP / 2, COL_X1[5])

CHIP_X0 <- COL_X0 + 1
CHIP_X1 <- COL_X1 - 1
CHIP_Y  <- c(Y_NET[1] + 1.5, Y_NET[2] - HEAD - 1.5)

BANDS <- tribble(
  ~LABEL,         ~Y0,      ~Y1,
  "Data",         Y_DAT[1], Y_DAT[2],
  "Connectivity", Y_CLU[1], Y_DER[2],
  "Resilience",   Y_RES[1], Y_RES[2]
)

BOXES <- tribble(
  ~ID,     ~STYLE, ~X0,        ~X1,        ~Y0,        ~Y1,        ~TITLE,
  "MAP",   "box",  COL_X0[1],  COL_X1[1],  Y_DAT[1],   Y_DAT[2],   "Reach mapping",
  "WID",   "box",  COL_X0[2],  COL_X1[2],  Y_DAT[1],   Y_DAT[2],   "Average width",
  "FHI",   "box",  COL_X0[3],  COL_X1[3],  Y_DAT[1],   Y_DAT[2],   "FHI",
  "DIS",   "box",  COL_X0[4],  COL_X1[4],  Y_DAT[1],   Y_DAT[2],   "Dispersal modes",
  "BAR",   "box",  COL_X0[5],  COL_X1[5],  Y_DAT[1],   Y_DAT[2],   "Transversal barriers",
  "ARE",   "box",  COL_X0[2],  COL_X1[2],  Y_DER[1],   Y_DER[2],   "Habitat area",
  "SUI",   "box",  COL_X0[3],  COL_X1[3],  Y_DER[1],   Y_DER[2],   "Habitat suitability",
  "NET",   "box",  COL_X0[1],  COL_X1[5],  Y_NET[1],   Y_NET[2],   "Habitat network and connectivity index parameterization (13 configurations per year: SC \u00d7 dispersal mode)",
  "NET_1", "chip", CHIP_X0[1], CHIP_X1[1], CHIP_Y[1],  CHIP_Y[2],  "Networks",
  "NET_2", "chip", CHIP_X0[2], CHIP_X1[2], CHIP_Y[1],  CHIP_Y[2],  "Node weight",
  "NET_3", "chip", CHIP_X0[3], CHIP_X1[3], CHIP_Y[1],  CHIP_Y[2],  "Weighted length",
  "NET_4", "chip", CHIP_X0[4], CHIP_X1[4], CHIP_Y[1],  CHIP_Y[2],  "Dispersal",
  "NET_5", "chip", CHIP_X0[5], CHIP_X1[5], CHIP_Y[1],  CHIP_Y[2],  "Barrier effect",
  "RCI",   "box",  COL_X0[1],  COL_X1[2],  Y_IDX[1],   Y_IDX[2],   "RCI",
  "VAL",   "box",  X_MID[1],   X_MID[2],   Y_IDX[1],   Y_IDX[2],   "Validation",
  "CCI",   "box",  X_RIGHT[1], X_RIGHT[2], Y_IDX[1],   Y_IDX[2],   "CCI",
  "CLU",   "box",  COL_X0[1],  COL_X1[2],  Y_CLU[1],   Y_CLU[2],   "Connectivity clusters (CC)",
  "MUL",   "box",  COL_X0[3],  COL_X1[5],  Y_CLU[1],   Y_CLU[2],   "Multilayer network",
  "MOD",   "box",  COL_X0[1],  COL_X1[2],  Y_RES[1],   Y_RES[2],   "CC modularity",
  "ROB",   "box",  X_MID[1],   X_MID[2],   Y_RES[1],   Y_RES[2],   "Robustness",
  "DSM",   "box",  X_RIGHT[1], X_RIGHT[2], Y_RES[1],   Y_RES[2],   "Dissimilarity"
)

TYPES <- tribble(
  ~TYPE,     ~SIZE, ~FACE,   ~COLOUR, ~H,   ~RENDER,
  "body",    6,     "plain", TXT,     2.85, "text",
  "code",    6,     "bold",  INK,     2.85, "text",
  "note",    5.2,   "plain", INK,     2.6,  "text",
  "fnote",   5.2,   "plain", INK,     2.6,  "text",
  "foot",    5.4,   "bold",  INK,     2.6,  "text",
  "bodyvar", 6,     "plain", TXT,     2.85, "plotmath",
  "notevar", 5.2,   "plain", INK,     2.6,  "plotmath",
  "math",    6,     "plain", TXT,     3.1,  "plotmath",
  "eq",      EQ_PT, "plain", TXT,     4,    "latex",
  "frac",    EQ_PT, "plain", TXT,     6.8,  "latex",
  "display", EQ_PT, "plain", TXT,     8,    "latex"
)

TEXT <- tribble(
  ~ID,     ~ROW, ~TYPE,     ~DX, ~LABEL,
  "MAP",   1,    "body",    0,   "Habitat network and types:",
  "MAP",   2,    "body",    2,   "Connector (no habitat)",
  "MAP",   3,    "bodyvar", 0,   "'- 7 years '*bold('y')*' (1817-2022)'",
  "MAP",   4,    "bodyvar", 0,   "'- *Habitat type '*bold('h')",
  "MAP",   5,    "fnote",   0,   "  *Hohensinner et al., 2011",
  "WID",   1,    "math",    0,   "bold(W[y*','*h])~'mean *width of every'",
  "WID",   2,    "bodyvar", 0,   "'habitat type '*bold('h')*' and year '*bold('y')",
  "WID",   3,    "fnote",   0,   "*Active channel",
  "FHI",   1,    "body",    0,   "*Floodplain habitat index:",
  "FHI",   2,    "bodyvar", 0,   "'Suitability of habitats '*bold('H1-H5')",
  "FHI",   3,    "body",    0,   "of aquatic taxa grouped",
  "FHI",   4,    "bodyvar", 0,   "'into suitability clusters '*bold('SC')",
  "FHI",   5,    "fnote",   0,   "*Waringer et al., 2005",
  "DIS",   1,    "code",    0,   "MDM",
  "DIS",   1,    "body",    6.4, "*Fish, 30 to 300 km",
  "DIS",   2,    "code",    0,   "SDM",
  "DIS",   2,    "body",    6.4, "**Fish, < 30 km",
  "DIS",   3,    "code",    0,   "PD",
  "DIS",   3,    "body",    6.4, "Passive dispersal",
  "DIS",   4,    "code",    0,   "EUC",
  "DIS",   4,    "body",    6.4, "Flying, overland",
  "DIS",   5,    "fnote",   0,   "*Medium dist. migrants",
  "DIS",   6,    "fnote",   0,   "**Short dist. migrants",
  "BAR",   1,    "bodyvar", 0,   "'Transversal barriers '*bold('m')~'with'",
  "BAR",   2,    "math",    0,   "'asymmetric passability '*bold(p[m])",
  "BAR",   3,    "body",    0,   "(upstream and downstream)",
  "ARE",   1,    "body",    0,   "Habitat area calculated as",
  "ARE",   2,    "body",    0,   "the product of the length",
  "ARE",   3,    "math",    0,   "bold(L[j])~'of reach'~bold('j')~'and'~bold(W[y*','*h])",
  "SUI",   1,    "math",    0,   "'Suitability '*bold(S[sc*','*h])*':'",
  "SUI",   2,    "body",    0,   "Translation of suitability",
  "SUI",   3,    "bodyvar", 0,   "'from'~bold('H1-H5')~'to'~bold('h')~'per'~bold('SC')",
  "NET_1", 1,    "math",    0,   "bold('Nodes:')~'reach segments'",
  "NET_1", 2,    "math",    0,   "'< 500 m and connectors'",
  "NET_1", 3,    "math",    0,   "bold('Links:')~'segment junctions,'",
  "NET_1", 4,    "math",    0,   "'confluences and barriers'",
  "NET_2", 1,    "frac",    0,   "$\\displaystyle w_j = \\ln(L_j\\,W_{y,h})\\,\\frac{S_{sc,h}}{10}$",
  "NET_2", 2,    "notevar", 0,   "'Weight of node '*bold('j')",
  "NET_2", 3,    "note",    0,   "Habitat quantity and quality",
  "NET_3", 1,    "display", 0,   "$\\displaystyle L_j^w = \\frac{L_j}{\\frac{S_{sc,h}}{10} + \\varepsilon}$",
  "NET_3", 2,    "notevar", 0,   "'Weighted length of node '*bold('j')",
  "NET_4", 1,    "eq",      0,   "$d_{ij} = \\sum L^w$",
  "NET_4", 2,    "eq",      0,   "$B_{ij} = e^{-\\alpha\\,d_{ij}}$",
  "NET_4", 3,    "notevar", 0,   "'Dist. '*bold(d[ij])*' from centre of '*bold('i')*' to '*bold('j')",
  "NET_4", 4,    "notevar", 0,   "'Example: '*bold(B[ij])*' expon. kernel'",
  "NET_5", 1,    "display", 0,   "$\\displaystyle c_{ij} = \\prod_{m=1}^{k} p_m$",
  "NET_5", 2,    "notevar", 0,   "bold('k')~'barriers ('*bold('m')*') on path'~bold('i')~'to'~bold('j')",
  "RCI",   1,    "body",    0,   "Reach connectivity index",
  "RCI",   2,    "body",    0,   "according to Baldan et al., 2022:",
  "RCI",   3,    "display", 0,   "$\\displaystyle RCI_i = \\sum_{j=1}^{n} B_{ij}\\;c_{ij}\\;w_j$",
  "VAL",   1,    "bodyvar", 0,   "'Comparing '*bold('RCI')*' values of'",
  "VAL",   2,    "body",    0,   "sites with presence against",
  "VAL",   3,    "body",    0,   "sites with absence of taxa",
  "VAL",   4,    "body",    0,   "(Wilcoxon test)",
  "CCI",   1,    "body",    0,   "Catchment connectivity index",
  "CCI",   2,    "body",    0,   "according to Baldan et al., 2022:",
  "CCI",   3,    "display", 0,   "$\\displaystyle CCI = \\sum_{i=1}^{n} \\sum_{j=1}^{n} B_{ij}\\;c_{ij}\\;w_i\\;w_j$",
  "CLU",   1,    "bodyvar", 0,   "bold('RCI')~'configurations grouped across'",
  "CLU",   2,    "bodyvar", 0,   "'years into four'~bold('CCs')*':'",
  "CLU",   3,    "bodyvar", 0,   "'6 '*bold('RCIs')*' (Kendall \u03c4 \u2264 0.7),'",
  "CLU",   4,    "body",    0,   "z and ln(x + 2) transformed",
  "MUL",   1,    "bodyvar", 0,   "'One layer per '*bold('RCI')*' configuration (13),'",
  "MUL",   2,    "body",    0,   "same topology, linked across layers",
  "MUL",   3,    "bodyvar", 0,   "'Node weights: '*bold('RCI')*' (scaled)'",
  "MUL",   4,    "math",    0,   "'Link weights: '*bold(L^w)*' (scaled)'",
  "MOD",   1,    "body",    0,   "Spatial modularity of CCs",
  "MOD",   2,    "body",    0,   "describing habitats with similar",
  "MOD",   3,    "body",    0,   "functionality per year",
  "MOD",   4,    "foot",    0,   "\u2191 modularity: spatial segregation",
  "ROB",   1,    "body",    0,   "Reachability adjusted mean",
  "ROB",   2,    "body",    0,   "path length following node",
  "ROB",   3,    "bodyvar", 0,   "'removal by increasing'~bold('RCI')",
  "ROB",   4,    "foot",    0,   "\u2191 path length: less robust",
  "DSM",   1,    "body",    0,   "Dissimilarities between network",
  "DSM",   2,    "body",    0,   "layers representing different",
  "DSM",   3,    "body",    0,   "functional groups of organisms",
  "DSM",   4,    "foot",    0,   "\u2193 dissimilarity: loss of",
  "DSM",   5,    "foot",    0,   "functional diversity"
)

ROWS <- TEXT %>%
  left_join(TYPES, by = "TYPE") %>%
  group_by(ID, ROW) %>%
  summarise(H_ROW = max(H), .groups = "drop") %>%
  group_by(ID) %>%
  mutate(
    Y_OFF = cumsum(H_ROW) - H_ROW / 2,
    USED  = sum(H_ROW)
  ) %>%
  ungroup()

TEXT_POS <- TEXT %>%
  left_join(TYPES, by = "TYPE") %>%
  left_join(BOXES, by = "ID") %>%
  left_join(ROWS, by = c("ID", "ROW")) %>%
  mutate(
    LEFT  = (STYLE == "box" | ID == "NET_1") & !ID %in% c("RCI", "CCI"),
    TOP_Y = if_else(STYLE == "chip", Y1 - TOP - 3.6, Y1 - HEAD - TOP),
    SPARE = if_else(STYLE == "chip", (TOP_Y - Y0 - TOP) - USED, 0),
    X     = if_else(LEFT, X0 + PAD + DX, (X0 + X1) / 2),
    Y     = TOP_Y - SPARE / 2 - Y_OFF - case_when(TYPE == "foot" ~ 1, TYPE == "fnote" ~ .5, TRUE ~ 0),
    HJUST = if_else(LEFT, 0, .5),
    LABEL = if_else(RENDER == "plotmath", paste0(LABEL, "*vphantom('Ig')"), LABEL)
  )

CON_KEY <- TEXT_POS %>%
  filter(ID == "MAP", ROW == 2) %>%
  transmute(X = X0 + PAD + .6, Y)

TITLE_NOTES <- tribble(
  ~ID,     ~NOTE,
  "NET_1", "(graphs)"
)

TITLES <- BOXES %>%
  left_join(TITLE_NOTES, by = "ID") %>%
  mutate(
    X     = (X0 + X1) / 2,
    Y     = if_else(STYLE == "chip", Y1 - TOP - 2.85 / 2, Y1 - HEAD / 2),
    SIZE  = if_else(STYLE == "chip", 6.2, 7),
    LABEL = if_else(
      is.na(NOTE),
      sprintf("bold('%s')*vphantom('Ig')", TITLE),
      sprintf("bold('%s')~'%s'*vphantom('Ig')", TITLE, NOTE)
    )
  )

SWATCH_W    <- 3
SWATCH_STEP <- 6
SWATCH_SPAN <- 3 * SWATCH_STEP + SWATCH_W

SWATCHES <- bind_rows(
  tibble(
    X    = COL_XM[3] - SWATCH_SPAN / 2 + 0:3 * SWATCH_STEP,
    Y    = Y_DER[1] + KEY_DY,
    FILL = c(TEAL, MINT, PEACH, BROWN),
    LAB  = paste0("SC", 1:4)
  ),
  tibble(
    X    = COL_X1[2] - PAD - SWATCH_SPAN + 0:3 * SWATCH_STEP,
    Y    = Y_CLU[1] + 7.5,
    FILL = c(TEAL, MINT, PURPLE, MAGENTA),
    LAB  = paste0("CC", 1:4)
  )
) %>%
  mutate(W = SWATCH_W)

PLANE_X0 <- mean(X_RIGHT) - 11.5

PLANES <- expand_grid(LAYER = 0:2, V = 1:4) %>%
  mutate(
    X     = PLANE_X0 + c(0, 18, 23, 5)[V],
    Y     = Y_CLU[1] + 2.4 + LAYER * 4.2 + c(0, 0, 2.8, 2.8)[V],
    GROUP = LAYER
  )

LAYER_NODES <- expand_grid(LAYER = 0:2, N = 1:4) %>%
  mutate(
    X      = PLANE_X0 + c(5, 10, 15, 18.5)[N],
    Y      = Y_CLU[1] + 2.4 + LAYER * 4.2 + c(1.5, 1, 1.9, 1.2)[N],
    COLOUR = c(TEAL, MINT, PEACH)[LAYER + 1]
  )

PATHS <- LAYER_NODES %>%
  transmute(X, Y, GROUP = paste0("L", LAYER), COLOUR = INK, LW = .3)

POINTS <- LAYER_NODES %>%
  transmute(X, Y, COLOUR, SIZE = 1, SHAPE = 16)

DASHED <- LAYER_NODES %>%
  filter(LAYER < 2) %>%
  transmute(X, Y, XEND = X, YEND = Y + 4.2)

ARCS <- LAYER_NODES %>%
  filter(LAYER == 0, N %in% c(1, 4)) %>%
  transmute(N, X, Y, XEND = X, YEND = Y + 8.4)

GLYPH_TEXT <- SWATCHES %>%
  filter(!is.na(LAB)) %>%
  transmute(X = X + W / 2, Y = Y - 1.25, LABEL = LAB, HJUST = .5)

X_RCI <- (COL_X0[1] + COL_X1[2]) / 2
Y_VAL <- (Y_IDX[1] + PAD + Y_IDX[2] - HEAD - TOP) / 2

ARROWS <- tribble(
  ~X,             ~Y,                ~XEND,          ~YEND,
  COL_XM[1],      Y_DAT[1],          COL_XM[1],      Y_NET[2],
  COL_X1[1] - 3,  Y_DAT[1],          COL_X0[2] + 4,  Y_DER[2],
  COL_XM[2],      Y_DAT[1],          COL_XM[2],      Y_DER[2],
  COL_XM[3],      Y_DAT[1],          COL_XM[3],      Y_DER[2],
  COL_XM[4],      Y_DAT[1],          COL_XM[4],      Y_NET[2],
  COL_XM[5],      Y_DAT[1],          COL_XM[5],      Y_NET[2],
  COL_XM[2],      Y_DER[1],          COL_XM[2],      Y_NET[2],
  COL_XM[3],      Y_DER[1],          COL_XM[3],      Y_NET[2],
  COL_X0[3] + 3,  Y_DER[1],          COL_X1[2] - 4,  Y_NET[2],
  CHIP_X1[3],     mean(CHIP_Y) - 2,  CHIP_X0[4],     mean(CHIP_Y) - 2,
  X_RCI,          Y_NET[1],          X_RCI,          Y_IDX[2],
  mean(X_RIGHT),  Y_NET[1],          mean(X_RIGHT),  Y_IDX[2],
  COL_X1[2],      Y_VAL,             X_MID[1],       Y_VAL,
  X_RCI,          Y_IDX[1],          X_RCI,          Y_CLU[2],
  COL_X1[2] - 6,  Y_IDX[1],          COL_X0[3] + 6,  Y_CLU[2],
  X_RCI,          Y_CLU[1],          X_RCI,          Y_RES[2],
  mean(X_MID),    Y_CLU[1],          mean(X_MID),    Y_RES[2],
  mean(X_RIGHT),  Y_CLU[1],          mean(X_RIGHT),  Y_RES[2]
)

INS_H   <- 10.4
INS_W   <- INS_H
INS_Y0  <- Y_DAT[1] + KEY_DY

MAIN_HW   <- 2.8
MAIN_Y    <- MAIN_HW + .8
ARM_HW    <- .95
ARM_U     <- c(.24, .24, .42, .64)
ARM_V     <- c(1, .72, .5, MAIN_Y / INS_H)
DAM_X     <- INS_W / 2
DAM_Y     <- INS_H / 2
DAM_HW    <- .4
NODE_SIZE <- 1
NODE_R    <- .63
LINK_LW   <- .28
CURV      <- .35

INS_PARTS <- tribble(
  ~BOX,  ~PART, ~X0,                     ~CY,    ~LABEL,
  "MAP", 1,     COL_X0[1] + PAD,         MAIN_Y, "Reaches",
  "MAP", 2,     COL_X1[1] - PAD - INS_W, MAIN_Y, "Network",
  "BAR", 1,     COL_XM[5] - INS_W / 2,   DAM_Y,  NA
) %>%
  mutate(
    X1 = X0 + INS_W,
    Y0 = INS_Y0,
    Y1 = INS_Y0 + INS_H
  )

to_panel <- function(DATA) {
  DATA %>%
    left_join(select(INS_PARTS, BOX, PART, OX = X0, OY = Y0), by = c("BOX", "PART")) %>%
    mutate(
      X = X + OX,
      Y = Y + OY
    ) %>%
    select(-OX, -OY)
}

ARM <- tibble(T = seq(0, 1, length.out = 200)) %>%
  mutate(
    X   = ((1 - T)^3 * ARM_U[1] + 3 * (1 - T)^2 * T * ARM_U[2] + 3 * (1 - T) * T^2 * ARM_U[3] + T^3 * ARM_U[4]) * INS_W,
    Y   = ((1 - T)^3 * ARM_V[1] + 3 * (1 - T)^2 * T * ARM_V[2] + 3 * (1 - T) * T^2 * ARM_V[3] + T^3 * ARM_V[4]) * INS_H,
    DX  = (3 * (1 - T)^2 * (ARM_U[2] - ARM_U[1]) + 6 * (1 - T) * T * (ARM_U[3] - ARM_U[2]) + 3 * T^2 * (ARM_U[4] - ARM_U[3])) * INS_W,
    DY  = (3 * (1 - T)^2 * (ARM_V[2] - ARM_V[1]) + 6 * (1 - T) * T * (ARM_V[3] - ARM_V[2]) + 3 * T^2 * (ARM_V[4] - ARM_V[3])) * INS_H,
    LEN = sqrt(DX^2 + DY^2),
    NX  = -DY / LEN,
    NY  = DX / LEN
  )

ARM_REACH <- ARM %>%
  filter(Y >= MAIN_Y + MAIN_HW) %>%
  mutate(S = cumsum(c(0, sqrt(diff(X)^2 + diff(Y)^2))))

CONFLUENCE <- ARM_REACH %>%
  slice_tail(n = 1)

JUNCTION <- tibble(X = CONFLUENCE$X, Y = MAIN_Y)

INS_MAIN <- INS_PARTS %>%
  transmute(
    X0,
    X1,
    Y0 = INS_Y0 + CY - MAIN_HW,
    Y1 = INS_Y0 + CY + MAIN_HW
  )

INS_ARM <- bind_rows(
  ARM %>% transmute(X = X + NX * ARM_HW, Y = Y + NY * ARM_HW),
  ARM %>% arrange(desc(T)) %>% transmute(X = X - NX * ARM_HW, Y = Y - NY * ARM_HW)
) %>%
  expand_grid(BOX = "MAP", PART = 1:2) %>%
  to_panel()

INS_DAM <- INS_PARTS %>%
  filter(BOX == "BAR") %>%
  transmute(
    X0 = X0 + DAM_X - DAM_HW,
    X1 = X0 + 2 * DAM_HW,
    Y0 = Y0 + DAM_Y - MAIN_HW - .6,
    Y1 = Y0 + 2 * (MAIN_HW + .6)
  )

INS_REACHES <- bind_rows(
  ARM_REACH %>% transmute(GROUP = "TEAL", X, Y, COLOUR = TEAL, LTY = "solid"),
  bind_rows(CONFLUENCE, JUNCTION) %>% transmute(GROUP = "CONNECTOR", X, Y, COLOUR = MAGENTA, LTY = "11"),
  tibble(GROUP = "MINT", X = c(0, JUNCTION$X), Y = MAIN_Y, COLOUR = MINT, LTY = "solid"),
  tibble(GROUP = "PEACH", X = c(JUNCTION$X, INS_W), Y = MAIN_Y, COLOUR = PEACH, LTY = "solid")
) %>%
  mutate(
    BOX  = "MAP",
    PART = 1
  ) %>%
  to_panel()

INS_ENDS <- bind_rows(CONFLUENCE, JUNCTION) %>%
  transmute(BOX = "MAP", PART = 1, X, Y) %>%
  to_panel()

INS_NODES <- bind_rows(
  ARM_REACH %>%
    slice_min(abs(S - max(S) / 2), n = 1) %>%
    transmute(BOX = "MAP", NODE = "TEAL", X, Y, R = NODE_R, COLOUR = TEAL),
  tribble(
    ~BOX,  ~NODE,           ~X,                       ~Y,                          ~R,     ~COLOUR,
    "MAP", "UPSTREAM_ARM",  ARM_U[1] * INS_W,         INS_H,                       0,      NA,
    "MAP", "UPSTREAM_MAIN", 0,                        MAIN_Y,                      0,      NA,
    "MAP", "DOWNSTREAM",    INS_W,                    MAIN_Y,                      0,      NA,
    "MAP", "MINT",          JUNCTION$X / 2,           MAIN_Y,                      NODE_R, MINT,
    "MAP", "CONNECTOR",     JUNCTION$X,               (CONFLUENCE$Y + MAIN_Y) / 2, NODE_R, MAGENTA,
    "MAP", "PEACH",         (JUNCTION$X + INS_W) / 2, MAIN_Y,                      NODE_R, PEACH,
    "BAR", "MINT",          DAM_X / 2,                DAM_Y,                       NODE_R, MINT,
    "BAR", "PEACH",         (DAM_X + INS_W) / 2,      DAM_Y,                       NODE_R, PEACH
  )
) %>%
  mutate(PART = if_else(BOX == "MAP", 2, 1)) %>%
  to_panel()

INS_LINKS <- tribble(
  ~BOX,  ~FROM,           ~TO,          ~CURVED,
  "MAP", "UPSTREAM_ARM",  "TEAL",       FALSE,
  "MAP", "UPSTREAM_MAIN", "MINT",       FALSE,
  "MAP", "TEAL",          "CONNECTOR",  FALSE,
  "MAP", "CONNECTOR",     "PEACH",      FALSE,
  "MAP", "MINT",          "PEACH",      FALSE,
  "MAP", "PEACH",         "DOWNSTREAM", FALSE,
  "BAR", "MINT",          "PEACH",      TRUE,
  "BAR", "PEACH",         "MINT",       TRUE
) %>%
  left_join(select(INS_NODES, BOX, FROM = NODE, X, Y, R_FROM = R), by = c("BOX", "FROM")) %>%
  left_join(select(INS_NODES, BOX, TO = NODE, XEND = X, YEND = Y, R_TO = R), by = c("BOX", "TO")) %>%
  mutate(
    THETA = atan2(YEND - Y, XEND - X),
    DELTA = if_else(CURVED, 2 * atan(CURV), 0),
    GAP   = if_else(R_TO > 0, R_TO + .2, 0),
    X     = X + R_FROM * cos(THETA + DELTA),
    Y     = Y + R_FROM * sin(THETA + DELTA),
    XEND  = XEND + GAP * cos(THETA + pi - DELTA),
    YEND  = YEND + GAP * sin(THETA + pi - DELTA)
  )

INS_ARROW <- tibble(
  X    = COL_X0[1] + PAD + INS_W + .5,
  XEND = COL_X1[1] - PAD - INS_W - .5,
  Y    = INS_Y0 + INS_H / 2
)

P_WIDTH_ICON <- P_WIDTH +
  theme_void() +
  theme(
    legend.position   = "none",
    plot.title        = element_blank(),
    plot.margin       = margin(0,0,0,0),
    panel.background  = element_rect(fill = "white", color = NA),
    aspect.ratio      = 1
  )

P_WIDTH_ICON$layers$geom_point...3$aes_params$size <- .4
P_WIDTH_ICON$layers$geom_point <- NULL
P_WIDTH_ICON$layers$geom_line$aes_params$linewidth <- .25

WID_LABELS <- tibble(
  X     = c(COL_XM[2], COL_XM[2] - INS_H / 2 - 1.25),
  Y     = c(INS_Y0 - 1.25, INS_Y0 + INS_H / 2),
  ANGLE = c(0, 90),
  LABEL = c("Time", "Width")
)

P_SC_ICON <- ggplot(NMDS_TABLE_LONG %>% filter(HABITAT == "H1"), aes(x= MDS1, y= MDS2, color= VALUE)) +
  geom_point(alpha= .6, size = .2) +
  scale_color_gradientn(colors = c(TEAL, MINT, PURPLE, MAGENTA, PEACH), limits = c(0, 10), name= "Habitat suitability",breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_void() +
  theme(
    legend.position   = "none",
    plot.title        = element_blank(),
    plot.margin       = margin(0,0,0,0),
    panel.background  = element_rect(fill = "white", color = NA),
    aspect.ratio      = 1
  )

FHI_LABELS <- tibble(
  X     = c(COL_XM[3], COL_XM[3] - INS_H / 2 - 1.25),
  Y     = c(INS_Y0 - 1.25, INS_Y0 + INS_H / 2),
  ANGLE = c(0, 90),
  LABEL = c("MDS1", "MDS2")
)

DIS_KEY  <- 5.6
DIS_LIFT <- 2
DIS_GAP <- (COL_W - 2 * PAD - 4 * DIS_KEY) / 3

DIS_ICONS <- tibble(
  CODE = c("MDM", "SDM", "PD", "EUC"),
  ICON = list(MDM_ICON, SDM_ICON, PD_ICON, EUC_ICON)
) %>%
  mutate(
    BOX_X0 = COL_X0[4] + PAD + (row_number() - 1) * (DIS_KEY + DIS_GAP),
    ASPECT = map_dbl(ICON, ~ nrow(.x) / ncol(.x)),
    W      = DIS_KEY * pmin(1, 1 / ASPECT),
    H      = W * ASPECT,
    X0     = BOX_X0 + (DIS_KEY - W) / 2,
    Y0     = INS_Y0 + DIS_LIFT + (DIS_KEY - H) / 2
  )

DIS_LAYERS <- pmap(DIS_ICONS, function(ICON, X0, Y0, W, H, ...) {
  annotation_raster(ICON, xmin = X0, xmax = X0 + W, ymin = Y0, ymax = Y0 + H, interpolate = TRUE)
})

DIS_LABELS <- DIS_ICONS %>%
  transmute(
    X     = BOX_X0 + DIS_KEY / 2,
    Y     = INS_Y0 - 1.25,
    ANGLE = 0,
    LABEL = CODE
  )

YEAR_COLOURS <- c(TEAL, MINT, PURPLE, MAGENTA, PEACH, BROWN, GREY)

ICON_THEME <- theme_void() +
  theme(
    legend.position  = "none",
    plot.margin      = margin(0, 0, 0, 0),
    panel.background = element_rect(fill = "white", colour = NA),
    aspect.ratio     = 1
  )

P_MOD_ICON <- ggplot(MODULARITY, aes(x = as.numeric(YEAR), y = MD, fill = as.factor(YEAR))) +
  geom_col(colour = INK, alpha = .8, linewidth = .15) +
  scale_fill_manual(values = YEAR_COLOURS) +
  coord_cartesian(ylim = c(.7, 1)) +
  ICON_THEME

P_ROB_ICON <- ggplot(ROBUSTNESS_PLOT, aes(x = REMOVED, group = as.factor(YEAR))) +
  geom_ribbon(aes(ymin = log(INT_LOW + 1), ymax = log(INT_UP + 1)), fill = alpha(GREY, .3)) +
  geom_line(aes(y = log(DIST_MEAN + 1), colour = as.factor(YEAR)), linewidth = .2) +
  scale_colour_manual(values = YEAR_COLOURS) +
  ICON_THEME

P_DISS_ICON <- ggplot(DISSIMILARITY, aes(x = as.numeric(YEAR), y = DISSIMILARITY, fill = as.factor(YEAR))) +
  geom_boxplot(colour = INK, alpha = .8, linewidth = .15, width = 15, outlier.size = .2, outlier.stroke = 0) +
  scale_fill_manual(values = YEAR_COLOURS) +
  ICON_THEME

RES_ICONS <- tibble(
  PLOT = list(P_MOD_ICON, P_ROB_ICON, P_DISS_ICON),
  X1   = c(COL_X1[2], X_MID[2], X_RIGHT[2]) - PAD,
  XLAB = c("Year", "Nodes removed", "Year"),
  YLAB = c("Modularity", "Path length", "Dissimilarity")
) %>%
  mutate(
    X0 = X1 - INS_W,
    Y0 = Y_RES[1] + KEY_DY,
    Y1 = Y0 + INS_H
  )

RES_LAYERS <- pmap(RES_ICONS, function(PLOT, X0, X1, Y0, Y1, ...) {
  annotation_custom(ggplotGrob(PLOT), xmin = X0, xmax = X1, ymin = Y0, ymax = Y1)
})

RES_LABELS <- bind_rows(
  RES_ICONS %>% transmute(X = (X0 + X1) / 2, Y = Y0 - 1.25, ANGLE = 0, LABEL = XLAB),
  RES_ICONS %>% transmute(X = X0 - 1.25, Y = (Y0 + Y1) / 2, ANGLE = 90, LABEL = YLAB)
)

VAL_X0 <- X_MID[2] - PAD - INS_W
VAL_Y0 <- Y_IDX[1] + KEY_DY

VAL_DUMMY <- tibble(
  PRES = factor(rep(c("Present", "Absent"), each = 80), levels = c("Present", "Absent")),
  NUM  = c(qbeta(ppoints(80), 3, 2.4), qbeta(ppoints(80), 2, 4))
)

P_VAL_ICON <- ggplot(VAL_DUMMY, aes(x = PRES, y = NUM)) +
  geom_boxplot(width = .5, fill = "white", colour = INK, linewidth = .15, outlier.size = .2, outlier.stroke = 0) +
  stat_summary(aes(group = 1), fun = median, geom = "line", colour = INK, linewidth = .15, linetype = "22") +
  stat_summary(fun = median, geom = "point", shape = 23, size = .9, fill = PEACH, colour = INK, stroke = .15) +
  scale_x_discrete(expand = c(0, .5)) +
  ICON_THEME

VAL_LABELS <- tibble(
  X     = c(VAL_X0 + INS_W / 4, VAL_X0 + INS_W * 3 / 4, VAL_X0 - 1.25),
  Y     = c(VAL_Y0 - 1.25, VAL_Y0 - 1.25, VAL_Y0 + INS_H / 2),
  ANGLE = c(0, 0, 90),
  LABEL = c("1", "0", "RCI")
)

FIG_WORKFLOW <- ggplot() +
  geom_rect(data = BANDS, aes(xmin = 2, xmax = 9, ymin = Y0, ymax = Y1), fill = STRIP) +
  geom_text(data = BANDS, aes(x = 5.5, y = (Y0 + Y1) / 2, label = LABEL), angle = 90, size = 7.5 / .pt, fontface = "bold", colour = INK, family = FONT) +
  geom_rect(data = filter(BOXES, STYLE == "box"), aes(xmin = X0, xmax = X1, ymin = Y0, ymax = Y1), fill = PANEL) +
  geom_rect(data = filter(BOXES, STYLE == "box"), aes(xmin = X0, xmax = X1, ymin = Y1 - HEAD, ymax = Y1), fill = STRIP) +
  geom_rect(data = filter(BOXES, STYLE == "chip"), aes(xmin = X0, xmax = X1, ymin = Y0, ymax = Y1), fill = "white") +
  geom_text(data = TITLES, aes(x = X, y = Y, label = LABEL, size = SIZE / .pt), parse = TRUE, colour = INK, family = FONT) +
  geom_rect(data = SWATCHES, aes(xmin = X, xmax = X + W, ymin = Y, ymax = Y + W, fill = FILL)) +
  geom_polygon(data = PLANES, aes(x = X, y = Y, group = GROUP), fill = "white", colour = LINE, linewidth = .3) +
  geom_segment(data = DASHED, aes(x = X, y = Y, xend = XEND, yend = YEND), colour = LINE, linewidth = .25, linetype = "22") +
  geom_curve(data = filter(ARCS, N == 1), aes(x = X, y = Y, xend = XEND, yend = YEND), curvature = -.8, colour = LINE, linewidth = .25, linetype = "22") +
  geom_curve(data = filter(ARCS, N == 4), aes(x = X, y = Y, xend = XEND, yend = YEND), curvature = .8, colour = LINE, linewidth = .25, linetype = "22") +
  geom_path(data = PATHS, aes(x = X, y = Y, group = GROUP, colour = COLOUR, linewidth = LW), lineend = "round") +
  geom_point(data = POINTS, aes(x = X, y = Y, colour = COLOUR, size = SIZE, shape = SHAPE)) +
  geom_text(data = GLYPH_TEXT, aes(x = X, y = Y, label = LABEL, hjust = HJUST), size = 4.6 / .pt, colour = INK, family = FONT) +
  geom_rect(data = INS_PARTS, aes(xmin = X0, xmax = X1, ymin = Y0, ymax = Y1), fill = "white") +
  geom_rect(data = INS_MAIN, aes(xmin = X0, xmax = X1, ymin = Y0, ymax = Y1), fill = "#D6D6D6") +
  geom_polygon(data = INS_ARM, aes(x = X, y = Y, group = PART), fill = "#D6D6D6") +
  geom_path(data = INS_REACHES, aes(x = X, y = Y, group = GROUP, colour = COLOUR, linetype = LTY), linewidth = .7, lineend = "butt") +
  geom_point(data = INS_ENDS, aes(x = X, y = Y), colour = INK, size = .7, stroke = 0) +
  geom_rect(data = INS_DAM, aes(xmin = X0, xmax = X1, ymin = Y0, ymax = Y1), fill = GREY) +
  geom_segment(
    data      = filter(INS_LINKS, !CURVED),
    aes(x = X, y = Y, xend = XEND, yend = YEND),
    colour    = INK,
    linewidth = LINK_LW,
    arrow     = arrow(length = unit(.6, "mm"), angle = 25, type = "closed")
  ) +
  geom_curve(
    data      = filter(INS_LINKS, CURVED),
    aes(x = X, y = Y, xend = XEND, yend = YEND),
    curvature = -CURV,
    colour    = INK,
    linewidth = LINK_LW,
    arrow     = arrow(length = unit(.6, "mm"), angle = 25, type = "closed")
  ) +
  geom_point(data = filter(INS_NODES, R > 0), aes(x = X, y = Y, colour = COLOUR), size = NODE_SIZE) +
  geom_segment(
    data      = INS_ARROW,
    aes(x = X, y = Y, xend = XEND, yend = Y),
    colour    = INK,
    linewidth = .35,
    arrow     = arrow(length = unit(.9, "mm"), type = "closed")
  ) +
  geom_text(data = filter(INS_PARTS, !is.na(LABEL)), aes(x = (X0 + X1) / 2, y = Y0 - 1.25, label = LABEL), size = 4.6 / .pt, colour = INK, family = FONT) +
  geom_point(data = CON_KEY, aes(x = X, y = Y), colour = MAGENTA, size = NODE_SIZE) +
  geom_text(data = filter(TEXT_POS, RENDER == "text"), aes(x = X, y = Y, label = LABEL, hjust = HJUST, size = SIZE / .pt, colour = COLOUR, fontface = FACE), family = FONT) +
  geom_text(data = filter(TEXT_POS, RENDER == "plotmath"), aes(x = X, y = Y, label = LABEL, hjust = HJUST, size = SIZE / .pt, colour = COLOUR), parse = TRUE, family = FONT) +
  geom_latex(data = filter(TEXT_POS, RENDER == "latex"), aes(x = X, y = Y, label = LABEL, hjust = HJUST, size = SIZE / .pt, colour = COLOUR), family = "serif", packages = MATH_FONT, engine = "xetex") +
  geom_text(data = bind_rows(WID_LABELS, FHI_LABELS, DIS_LABELS, VAL_LABELS, RES_LABELS), aes(x = X, y = Y, label = LABEL, angle = ANGLE), size = 4.6 / .pt, colour = INK, family = FONT) +
  geom_segment(
    data      = ARROWS,
    aes(x = X, y = Y, xend = XEND, yend = YEND),
    colour    = INK,
    linewidth = .4,
    arrow     = arrow(length = unit(1.4, "mm"), type = "closed")
  ) +
  annotation_custom(
    ggplotGrob(P_WIDTH_ICON),
    xmin = COL_XM[2] - INS_H / 2,
    xmax = COL_XM[2] + INS_H / 2,
    ymin = INS_Y0,
    ymax = INS_Y0 + INS_H
  ) +
  annotation_custom(
    ggplotGrob(P_SC_ICON),
    xmin = COL_XM[3] - INS_H / 2,
    xmax = COL_XM[3] + INS_H / 2,
    ymin = INS_Y0,
    ymax = INS_Y0 + INS_H
  ) +
  annotation_custom(
    ggplotGrob(P_VAL_ICON),
    xmin = VAL_X0,
    xmax = VAL_X0 + INS_W,
    ymin = VAL_Y0,
    ymax = VAL_Y0 + INS_H
  ) +
  DIS_LAYERS +
  RES_LAYERS +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_size_identity() +
  scale_linewidth_identity() +
  scale_shape_identity() +
  scale_linetype_identity() +
  coord_fixed(xlim = c(0, FIG_W), ylim = c(0, FIG_H), expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave("./PLOTS/CONCEPT.png", FIG_WORKFLOW, width = FIG_W, height = FIG_H, units = "mm", dpi = 300)

# END ####
