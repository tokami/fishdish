#' @name calc.swept.area
#' @title Calculate swept area
#' @author Klaas Sys
#' @param data DATRAS data set (see download.data)
#' @param plot Plot some summary graphs. Default: TRUE
#' @importFrom glmmTMB glmmTMB
#' @importFrom mgcv gam
#' @importFrom sp spDistsN1
#' @return Data with updated hh data set with calculated swept area
#' @export
calc.swept.area <- function(data, plot = TRUE){

    hh <- data$HH

    ## Check if all required variables included
    flag <- all(list.datras.variables.req()$HH %in% colnames(hh))
    if(!flag) stop("Not all variables required for the calculation of the swept area are included in the HH data set. Run list.datras.variables.req() to see the required variables.")


    # ------------------------------------------------------------------------------
    # Prepare HH data
    # ------------------------------------------------------------------------------
    # introduce NA's
    hh$DoorSpread[hh$DoorSpread<=0]     <- NA
    hh$WingSpread[hh$WingSpread<=0]     <- NA
    hh$Distance[hh$Distance < 0]       <- NA
    hh$GroundSpeed[hh$GroundSpeed < 0] <- NA
    hh$HaulDur[hh$HaulDur < 0]         <- NA
    hh$Depth[hh$Depth < 0]             <- NA
    hh$SweepLngt[hh$SweepLngt < 0]     <- NA
    hh$Warplngt[hh$Warplngt < 0]       <- NA

    hh$ShootLong[hh$ShootLong == -9]   <- NA
    hh$ShootLat[hh$ShootLat == -9]     <- NA
    hh$HaulLong[hh$HaulLong == -9]     <- NA
    hh$HaulLat[hh$HaulLat == -9]       <- NA

    # add missing depth information based on tow midpoints (if available, otherwise use shoot position)
    hh$meanLong <- (hh$ShootLong + hh$HaulLong)/2
    hh$meanLat  <- (hh$ShootLat + hh$HaulLat)/2
    hh$meanLong[is.na(hh$meanLong)] <- hh$ShootLong[is.na(hh$meanLong)]
    hh$meanLat[is.na(hh$meanLat)] <- hh$ShootLat[is.na(hh$meanLat)]


    # for improvement => could make use of EMODNet or NOAA bathymetric data to get depth data
    # REMARK: be careful with tidal effects (not included in EMODNet / NOAA)
    mod.depth <- mgcv::gam(log(Depth) ~ s(meanLong,meanLat, k = 50), data = hh)
    hh$Depth[is.na(hh$Depth)] <- exp(predict(mod.depth, newdata = hh[is.na(hh$Depth),]))

    # add an ID variable
    hh$ID <- paste0(hh$Survey,"_",hh$Country,"_",hh$Quarter,"_",hh$Gear,"_",hh$Ship,"_",
                    hh$HaulNo,"_",hh$Year,"_",hh$Month,"_",hh$Day)

    # Portugese HH data, seems also included in hh data
    data("ibts.pt")
    PT_data <- ibts.pt
    head(PT_data)
    PT_data$ID <- paste0(PT_data$Survey,"_",PT_data$Country,"_",PT_data$Quarter,"_",PT_data$Gear,"_",PT_data$Ship,"_",
                         PT_data$HaulNo,"_",PT_data$Year,"_",PT_data$Month,"_",PT_data$Day)

    table(is.na(PT_data$SweptAreaDSKM2))
    table(is.na(PT_data$SweptAreaWSKM2))

    # ------------------------------------------------------------------------------
    # Load FlexFiles and OSPAR data
    # ------------------------------------------------------------------------------

    # load FlexData
    data("flex")
    flex_data <- flex


    table(flex_data$HaulVal)
    flex_data <- subset(flex_data, HaulVal == "V")

    # load OSPARData
    data("ospar")
    ospar_data <- ospar

    # format different types
    ospar_data$DepthStratum <- minus9toNA(as.numeric(as.character(ospar_data$DepthStratum)))
    flex_data$DepthStratum  <- minus9toNA(as.numeric(as.character(flex_data$DepthStratum)))

    # create a similar ID variable
    flex_data$ID  <- paste0(flex_data$Survey,"_",flex_data$Country,"_",flex_data$Quarter,"_",flex_data$Gear,"_",flex_data$Ship,"_",
                            flex_data$HaulNo,"_",flex_data$Year,"_",flex_data$Month,"_",flex_data$Day)

    ospar_data$ID <- paste0(ospar_data$Survey,"_",ospar_data$Country,"_",ospar_data$Quarter,"_",ospar_data$Gear,"_",ospar_data$Ship,"_",
                            ospar_data$HaulNo,"_",ospar_data$Year,"_",ospar_data$Month,"_",ospar_data$Day)


    # reduce the number of columns
    flex_data  <- flex_data[,c("Survey","Ship","Country","Year","Quarter","Year","Depth","ShootLat","ShootLong",
                               "SweepLngt","Warplngt","DoorSpread","WingSpread","Distance","GroundSpeed","HaulDur",
                               "SweptAreaDSKM2","SweptAreaWSKM2","Gear","ID")]
    ospar_data <- ospar_data[,c("Survey","Ship","Country","Year","Quarter","Year","Depth","ShootLat","ShootLong",
                                "SweepLngt","Warplngt","DoorSpread","WingSpread","Distance","GroundSpeed","HaulDur",
                                "Wing/Door(Ratio)","SweptArea_wing_km_sqrd","Gear","ID")]

    # change names
    ospar_data$SweptAreaWSKM2 <- ospar_data$SweptArea_wing_km_sqrd
    ospar_data$SweptAreaDSKM2 <- ospar_data$SweptArea_wing_km_sqrd / ospar_data$`Wing/Door(Ratio)`

    ospar_data$SweptArea_wing_km_sqrd <- NULL
    ospar_data$`Wing/Door(Ratio)` <- NULL


    ospar_data <- ospar_data[,c("Survey","Ship","Country","Year","Quarter","Year","Depth","ShootLat","ShootLong",
                                "SweepLngt","Warplngt","DoorSpread","WingSpread","Distance","GroundSpeed","HaulDur",
                                "SweptAreaDSKM2","SweptAreaWSKM2","Gear","ID")]

    ospar_data$SweptAreaDSKM2 <- NA  # not sure if the conversion on line 99 is correct!


    # remove some observations
    flex_data <- subset(flex_data, SweptAreaWSKM2 > 0)
    flex_data <- subset(flex_data, SweptAreaDSKM2 > 0)
    flex_data <- subset(flex_data, DoorSpread > 0)
    flex_data <- subset(flex_data, WingSpread > 0)

    ospar_data <- subset(ospar_data, SweptAreaWSKM2 > 0)
    ospar_data <- subset(ospar_data, SweptAreaDSKM2 > 0)
    ospar_data <- subset(ospar_data, DoorSpread > 0)
    ospar_data <- subset(ospar_data, WingSpread > 0)

    # put data together
    all_data  <- rbind(flex_data, ospar_data)

    # mean swept area by ID (in case both OSPAR and FlexFile is available)
    swept_area_WS <- aggregate(SweptAreaWSKM2 ~ ID, FUN = "mean", data = all_data)
    swept_area_DS <- aggregate(SweptAreaDSKM2 ~ ID, FUN = "mean", data = all_data)

    # remove duplicates (use FlexFile in case it is available)
    all_data <- all_data[!duplicated(all_data$ID),]

    # add survey data
    hh_data  <- hh[!(hh$ID %in% all_data$ID),]     # make sure to not reuse observations already included in FlexFile / OSPAR data
    hh_data  <- hh_data[!duplicated(hh_data$ID),]
    hh_data  <- hh_data[hh_data$HaulVal == "V",]

    #-------------------------------------------------------------------------------
    # modelling Door and WingSpread
    #-------------------------------------------------------------------------------

    # performance metric: Root Mean Squared Error
    RMSE <- function (pred, obs, na.rm = TRUE) {
        return(sqrt(mean((pred - obs)^2, na.rm = na.rm)))
    }


    # fit models by GEAR type

    GEARS <- unique(c(unique(all_data$Gear), unique(hh_data$Gear)))

    for(GEAR in GEARS){
        ##        print(GEAR)

        # create empty objects to sture outputs
        rmse1 <- NA
        rmse2 <- NA
        rmse3 <- NA
        rmse4 <- NA
        rmse5 <- NA
        rmse6 <- NA

        wing_rmse1 <- NA
        wing_rmse2 <- NA
        wing_rmse3 <- NA
        wing_rmse4 <- NA
        wing_rmse5 <- NA
        wing_rmse6 <- NA

        door.norm1 <- NULL
        door.lnorm1 <- NULL
        door_warp.norm1 <- NULL
        door_warp.lnorm1 <- NULL
        door_depth.norm1 <- NULL
        door_depth.lnorm1 <- NULL
        wing.norm1 <- NULL
        wing.lnorm1 <- NULL
        wing_warp.norm1 <- NULL
        wing_warp.lnorm1 <- NULL
        wing_depth.norm1 <- NULL
        wing_depth.lnorm1 <- NULL


        # select and merge data
        data_door <- all_data[all_data$Gear == GEAR,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship")]
        data_wing <- all_data[all_data$Gear == GEAR,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship")]

        hh_door   <- hh_data[!is.na(hh_data$DoorSpread) & hh_data$Gear == GEAR,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship")]
        hh_wing   <- hh_data[!is.na(hh_data$WingSpread) & hh_data$Gear == GEAR,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship")]

        pt_door   <- PT_data[!is.na(PT_data$DoorSpread) & PT_data$Gear == GEAR,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship")]
        pt_wing   <- PT_data[!is.na(PT_data$WingSpread) & PT_data$Gear == GEAR,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship")]

        if(nrow(data_door) > 0 & nrow(hh_door) > 0) {
            data_door <- rbind(data_door, hh_door)
        }
        if(nrow(data_door) == 0 & nrow(hh_door) > 0){
            data_door <- hh_door
        }

        if(nrow(data_wing) > 0 & nrow(hh_wing) > 0) {
            data_wing <- rbind(data_wing, hh_wing)
        }
        if(nrow(data_wing) == 0 & nrow(hh_wing) > 0) {
            data_wing <- hh_wing
        }
        # the Portugese data is already included in the hh_data
        # if(nrow(pt_door) > 0) data_door <- rbind(data_door,pt_door)
        # if(nrow(pt_wing) > 0) data_wing <- rbind(data_wing,pt_wing)

        # add log transformed variable
        data_door$logDoorSpread <- log(data_door$DoorSpread)
        data_wing$logWingSpread <- log(data_wing$WingSpread)

        # door -------------------------------------------------------------------------

        model_data <- data_door[is.finite(data_door$logDoorSpread),]

        if(nrow(model_data) > 0){

            # cormat <- cor(model_data[,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed")])
            # corrplot(cormat, method = "number", type = "upper")

            # sweep and warplength higly correlated => include only one

            # sweeplength + depth
            if(sum(!is.na(model_data$SweepLngt))>0){
                if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                    door.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + SweepLngt + (1|Survey) + (1|Ship), model_data, family = gaussian())
                    rmse1 <- RMSE(predict(door.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                    door.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + SweepLngt + (1|Survey), model_data, family = gaussian())
                    rmse1 <- RMSE(predict(door.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                } else  {
                    door.norm1    <- lm(DoorSpread ~ Depth + SweepLngt, model_data)
                    rmse1   <- RMSE(predict(door.norm1, newdata = model_data), model_data$DoorSpread)
                }

                if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                    door.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + SweepLngt + (1|Survey) + (1|Ship), model_data, family = gaussian())
                    rmse2 <- RMSE(exp(predict(door.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                    door.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + SweepLngt + (1|Survey), model_data, family = gaussian())
                    rmse2 <- RMSE(exp(predict(door.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else  {
                    door.lnorm1    <- lm(logDoorSpread ~ Depth + SweepLngt, model_data)
                    rmse2    <- RMSE(exp(predict(door.lnorm1, newdata = model_data)), model_data$DoorSpread)
                }

            }


            if(sum(!is.na(model_data$Warplngt))>0){
                # warplength + depth
                if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                    door_warp.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + Warplngt + (1|Survey) + (1|Ship), model_data, family = gaussian())
                    rmse3 <- RMSE(predict(door_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                    door_warp.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + Warplngt + (1|Survey), model_data, family = gaussian())
                    rmse3 <- RMSE(predict(door_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                } else  {
                    door_warp.norm1    <- lm(DoorSpread ~ Depth + Warplngt, model_data)
                    rmse3   <- RMSE(predict(door_warp.norm1, newdata = model_data), model_data$DoorSpread)
                }

                if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                    door_warp.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + Warplngt + (1|Survey) + (1|Ship), model_data, family = gaussian())
                    rmse4 <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                    door_warp.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + Warplngt + (1|Survey), model_data, family = gaussian())
                    rmse4 <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else  {
                    door_warp.lnorm1    <- lm(logDoorSpread ~ Depth + Warplngt, model_data)
                    rmse4    <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data)), model_data$DoorSpread)
                }
            }

            # depth
            if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                door_depth.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + (1|Survey) + (1|Ship), model_data, family = gaussian())
                rmse5 <- RMSE((predict(door_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
            } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                door_depth.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + (1|Survey), model_data, family = gaussian())
                rmse5 <- RMSE((predict(door_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
            } else  {
                door_depth.norm1    <- lm(DoorSpread ~ Depth, model_data)
                rmse5    <- RMSE((predict(door_depth.norm1, newdata = model_data)), model_data$DoorSpread)
            }

            if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                door_depth.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + (1|Survey) + (1|Ship), model_data, family = gaussian())
                rmse6 <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
            } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                door_depth.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + (1|Survey), model_data, family = gaussian())
                rmse6 <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
            } else  {
                door_depth.lnorm1    <- lm(logDoorSpread ~ Depth, model_data)
                rmse6    <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data)), model_data$DoorSpread)
            }

            models_doorspread       <- list(door.norm1,
                                            door.lnorm1,
                                            door_warp.norm1,
                                            door_warp.lnorm1)
            names(models_doorspread) <- c("door.norm1",
                                          "door.lnorm1",
                                          "door_warp.norm1",
                                          "door_warp.lnorm1")

            depth_models_doorspread <- list(door_depth.norm1,
                                            door_depth.lnorm1)
            names(depth_models_doorspread) <- c("door_depth_lmm.norm1",
                                                "door_depth_lmm.lnorm1")

            # add model predictions door and wingspread according best performing model (RMSE)

            doorspread_rmse            <- c(rmse1,rmse2,rmse3,rmse4)
            names(doorspread_rmse)     <- names(models_doorspread)
            doorspread_d_rmse          <- c(rmse5,rmse6)
            names(doorspread_d_rmse)   <- names(depth_models_doorspread)

            # depth, sweeplength and warplength
            all_data_door <- hh[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR,]
            if(nrow(all_data_door) > 0 & any(!is.na(doorspread_rmse))){
                mod <- models_doorspread[[which.min(doorspread_rmse)]]
                hh$DoorSpread[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR] <-
                    predict(mod, newdata = all_data_door, allow.new.levels = T)
            }
            # depth and warplength
            all_data_door_warp <- hh[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR,]
            if(nrow(all_data_door_warp) > 0 & any(!is.na(doorspread_rmse[3:4]))){
                mod <- models_doorspread[3:4][[which.min(doorspread_rmse[3:4])]]
                hh$DoorSpread[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR] <-
                    predict(mod, newdata = all_data_door_warp, allow.new.levels = T)
            }
            # depth and sweeplength
            all_data_door_sweep <- hh[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR,]
            if(nrow(all_data_door_sweep) > 0 & any(!is.na(doorspread_rmse[1:2]))){
                mod <- models_doorspread[1:2][[which.min(doorspread_rmse[1:2])]]
                hh$DoorSpread[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR] <-
                    predict(mod, newdata = all_data_door_sweep, allow.new.levels = T)
            }
            # depth
            all_data_door_depth <- hh[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR,]
            if(nrow(all_data_door_depth) > 0 & any(!is.na(doorspread_d_rmse))){
                mod <- depth_models_doorspread[[which.min(doorspread_d_rmse)]]
                hh$DoorSpread[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR] <-
                    predict(mod, newdata = all_data_door_depth, allow.new.levels = T)
            }

        }



        # wing -------------------------------------------------------------------------

        # similar procedure for wing spread

        model_data <- data_wing[is.finite(data_wing$logWingSpread),]

        if(nrow(model_data) > 0){
            # cormat <- cor(model_data[,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed")])
            # corrplot(cormat, method = "number", type = "upper")

            # sweep and warplength higly correlated => include only one

            # sweeplength + depth
            if(sum(!is.na(model_data$SweepLngt))>0){
                if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                    wing.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + SweepLngt + (1|Survey) + (1|Ship), model_data, family = gaussian())
                    wing_rmse1 <- RMSE(predict(wing.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                    wing.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + SweepLngt + (1|Survey), model_data, family = gaussian())
                    wing_rmse1 <- RMSE(predict(wing.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                } else  {
                    wing.norm1    <- lm(WingSpread ~ Depth + SweepLngt, model_data)
                    wing_rmse1   <- RMSE(predict(wing.norm1, newdata = model_data), model_data$WingSpread)
                }

                if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                    wing.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + SweepLngt + (1|Survey) + (1|Ship), model_data, family = gaussian())
                    wing_rmse2 <- RMSE(exp(predict(wing.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                    wing.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + SweepLngt + (1|Survey), model_data, family = gaussian())
                    wing_rmse2 <- RMSE(exp(predict(wing.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else  {
                    wing.lnorm1    <- lm(logWingSpread ~ Depth + SweepLngt, model_data)
                    wing_rmse2    <- RMSE(exp(predict(wing.lnorm1, newdata = model_data)), model_data$WingSpread)
                }
            }


            # warplength + depth
            if(sum(!is.na(model_data$Warplngt))>0){
                if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                    wing_warp.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + Warplngt + (1|Survey) + (1|Ship), model_data, family = gaussian())
                    wing_rmse3 <- RMSE(predict(wing_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                    wing_warp.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + Warplngt + (1|Survey), model_data, family = gaussian())
                    wing_rmse3 <- RMSE(predict(wing_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                } else  {
                    wing_warp.norm1    <- lm(WingSpread ~ Depth + Warplngt, model_data)
                    wing_rmse3   <- RMSE(predict(wing_warp.norm1, newdata = model_data), model_data$WingSpread)
                }

                if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                    wing_warp.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + Warplngt + (1|Survey) + (1|Ship), model_data, family = gaussian())
                    wing_rmse4 <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                    wing_warp.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + Warplngt + (1|Survey), model_data, family = gaussian())
                    wing_rmse4 <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else  {
                    wing_warp.lnorm1    <- lm(logWingSpread ~ Depth + Warplngt, model_data)
                    wing_rmse4    <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data)), model_data$WingSpread)
                }
            }


            # depth
            if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                wing_depth.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + (1|Survey) + (1|Ship), model_data, family = gaussian())
                wing_rmse5 <- RMSE((predict(wing_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
            } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                wing_depth.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + (1|Survey), model_data, family = gaussian())
                wing_rmse5 <- RMSE((predict(wing_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
            } else  {
                wing_depth.norm1    <- lm(WingSpread ~ Depth, model_data)
                wing_rmse5    <- RMSE((predict(wing_depth.norm1, newdata = model_data)), model_data$WingSpread)
            }

            if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))> 2){
                wing_depth.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + (1|Survey) + (1|Ship), model_data, family = gaussian())
                wing_rmse6 <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
            } else if(length(unique(model_data$Survey))> 2 & length(unique(model_data$Ship))<= 2){
                wing_depth.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + (1|Survey), model_data, family = gaussian())
                wing_rmse6 <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
            } else  {
                wing_depth.lnorm1    <- lm(logWingSpread ~ Depth, model_data)
                wing_rmse6    <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data)), model_data$WingSpread)
            }

            models_wingspread       <- list(wing.norm1,
                                            wing.lnorm1,
                                            wing_warp.norm1,
                                            wing_warp.lnorm1)
            names(models_wingspread) <- c("wing.norm1",
                                          "wing.lnorm1",
                                          "wing_warp.norm1",
                                          "wing_warp.lnorm1")

            depth_models_wingspread <- list(wing_depth.norm1,
                                            wing_depth.lnorm1)
            names(depth_models_wingspread) <- c("wing_depth_lmm.norm1",
                                                "wing_depth_lmm.lnorm1")

            wingspread_rmse            <- c(wing_rmse1,wing_rmse2,wing_rmse3,wing_rmse4)
            names(wingspread_rmse)     <- names(models_wingspread)
            wingspread_d_rmse          <- c(wing_rmse5,wing_rmse6)
            names(wingspread_d_rmse)   <- names(depth_models_wingspread)

            # depth, sweeplength and warplength
            all_data_wing <- hh[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR,]
            if(nrow(all_data_wing) > 0 & any(!is.na(wingspread_rmse))){
                mod <- models_wingspread[[which.min(wingspread_rmse)]]
                hh$WingSpread[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR] <-
                    predict(mod, newdata = all_data_wing, allow.new.levels = T)
            }
            # depth and warplength
            all_data_wing_warp <- hh[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR,]
            if(nrow(all_data_wing_warp) > 0 & any(!is.na(wingspread_rmse[3:4]))){
                mod <- models_wingspread[3:4][[which.min(wingspread_rmse[3:4])]]
                hh$WingSpread[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR] <-
                    predict(mod, newdata = all_data_wing_warp, allow.new.levels = T)
            }
            # depth and sweeplength
            all_data_wing_sweep <- hh[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR,]
            if(nrow(all_data_wing_sweep) > 0 & any(!is.na(wingspread_rmse[1:2]))){
                mod <- models_wingspread[1:2][[which.min(wingspread_rmse[1:2])]]
                hh$WingSpread[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR] <-
                    predict(mod, newdata = all_data_wing_sweep, allow.new.levels = T)
            }
            # depth
            all_data_wing_depth <- hh[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR,]
            if(nrow(all_data_wing_depth) > 0 & any(!is.na(wingspread_d_rmse))){
                mod <- depth_models_wingspread[[which.min(wingspread_d_rmse)]]
                hh$WingSpread[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Gear == GEAR] <-
                    predict(mod, newdata = all_data_wing_depth, allow.new.levels = T)
            }

        }

        table(is.na(hh$DoorSpread))
        table(is.na(hh$WingSpread))
    }

    # ------------------------------------------------------------------------------
    # for the beam trawl surveys, add the beam width
    # ------------------------------------------------------------------------------

    # for the beam trawl surveys, take the width of the gear
    hh$BeamWidth <- NA
    unique(hh$Gear[hh$Survey == "BTS"])
    unique(hh$Gear[hh$Survey == "SNS"])
    unique(hh$Gear[hh$Survey == "DYFS"])

    hh$BeamWidth[hh$Gear == "BT8"]    <- 8
    hh$BeamWidth[hh$Gear == "BT4A"]   <- 4
    hh$BeamWidth[hh$Gear == "BT7"]    <- 7
    hh$BeamWidth[hh$Gear == "BT4AI"]  <- 4
    hh$BeamWidth[hh$Gear == "BT4S"]   <- 4
    hh$BeamWidth[hh$Gear == "BT4P"]   <- 4
    hh$BeamWidth[hh$Gear == "BT6"]    <- 6
    hh$BeamWidth[hh$Gear == "BT3"]    <- 3

    table(hh$Gear,is.na(hh$DoorSpread))[names(table(hh$Gear[is.na(hh$DoorSpread)])),]

    # ------------------------------------------------------------------------------
    # estimate trawled distance (where missing)
    # ------------------------------------------------------------------------------

    # if positions shoot / haul positions are the same and haulduration is 0 => remove
    idx <- which((hh$ShootLat == hh$HaulLat) &
                 (hh$ShootLong == hh$HaulLong) &
                 (hh$HaulDur == 0))
    if(length(idx) > 0) hh <- hh[-idx,]

    hh$Distance[hh$Distance==0]       <- NA
    hh$GroundSpeed[hh$GroundSpeed==0] <- NA
    hh$HaulDur[hh$HaulDur==0]         <- NA

    ## 1) based on haul duration and groundspeed
    if(any(is.na(hh$Distance))){
        knots_to_m_h <- 1852
        prop.table(table(is.na(hh$Distance)))

        hh$Distance[is.na(hh$Distance)] <- knots_to_m_h * hh$GroundSpeed[is.na(hh$Distance)] * hh$HaulDur[is.na(hh$Distance)]/60
        hh$Distance[hh$Distance==0]    <- NA
        prop.table(table(is.na(hh$Distance)))
    }



    ## 2) based on shoot and haul positions

    ## check shoot and haul positions
    if(any(is.na(hh$Distance))){
        idx <- is.na(hh$Distance) & !is.na(hh$ShootLong) & !is.na(hh$ShootLat) & !is.na(hh$HaulLong) & !is.na(hh$HaulLat)
        hh$Distance[idx] <- 1000 * apply(cbind(hh$ShootLong[idx],hh$ShootLat[idx],hh$HaulLong[idx],hh$HaulLat[idx]),1,
                                         function(x)sp::spDistsN1(matrix(x[1:2], ncol = 2),matrix(x[3:4], ncol = 2), longlat = T))
        hist(hh$Distance[hh$Distance>10000], xlim = c(10000, max(hh$Distance, na.rm = T)))
        hh$Distance[hh$Distance>10000] <- NA
        prop.table(table(is.na(hh$Distance)))

        hh$Distance[hh$Distance==0]    <- NA
    }

    ## 3) based on haul duration and mean vessel speed
    if(any(is.na(hh$Distance))){
        table(is.na(hh$Distance))
        table(is.na(hh$Distance) & is.na(hh$HaulDur))
        table(is.na(hh$Distance) & is.na(hh$GroundSpeed))

        mean_speed         <- aggregate(GroundSpeed ~ Ship, data = hh, FUN = "mean", na.rm =T)
        hh$mean_ship_speed <- mean_speed$GroundSpeed[match(hh$Ship,mean_speed$Ship)]

        hh$Distance[is.na(hh$Distance) & is.na(hh$GroundSpeed)] <- knots_to_m_h * hh$mean_ship_speed[is.na(hh$Distance) & is.na(hh$GroundSpeed)] *
            hh$HaulDur[is.na(hh$Distance) & is.na(hh$GroundSpeed)]/60
        hh$mean_ship_speed <- NULL
        hh$Distance[hh$Distance==0]    <- NA

        prop.table(table(is.na(hh$Distance)))
    }

    ## 4) based on haul duration and mean "survey x country" speed
    if(any(is.na(hh$Distance))){
        mean_speed <- aggregate(GroundSpeed ~ Survey + Country, data = hh, FUN = "mean", na.rm =T)
        hh$id         <- paste0(hh$Survey,"_",hh$Country)
        mean_speed$id <- paste0(mean_speed$Survey,"_",mean_speed$Country)
        hh$mean_survey_speed <- mean_speed$GroundSpeed[match(hh$id,mean_speed$id)]

        hh$Distance[is.na(hh$Distance) & is.na(hh$GroundSpeed)] <- knots_to_m_h * hh$mean_survey_speed[is.na(hh$Distance) & is.na(hh$GroundSpeed)] *
            hh$HaulDur[is.na(hh$Distance) & is.na(hh$GroundSpeed)]/60
        hh$mean_survey_speed <- NULL
        hh$Distance[hh$Distance==0]    <- NA

        prop.table(table(is.na(hh$Distance)))
    }

    ## 5) based on the mean survey speed
    if(any(is.na(hh$Distance))){
        mean_speed <- aggregate(GroundSpeed ~ Survey , data = hh, FUN = "mean", na.rm =T)
        hh$mean_survey_speed <- mean_speed$GroundSpeed[match(hh$Survey,mean_speed$Survey)]

        hh$Distance[is.na(hh$Distance)] <- knots_to_m_h * hh$mean_survey_speed[is.na(hh$Distance)] * hh$HaulDur[is.na(hh$Distance)]/60
        hh$mean_survey_speed <- NULL
        hh$Distance[hh$Distance==0]    <- NA

        prop.table(table(is.na(hh$Distance)))
    }

    ## 6) based on mean speed
    if(any(is.na(hh$Distance))){
        mean_speed <- mean(hh$GroundSpeed, na.rm =T)
        hh$Distance[is.na(hh$Distance)] <- knots_to_m_h * mean_speed * hh$HaulDur[is.na(hh$Distance)]/60
        hh$Distance[hh$Distance==0]    <- NA
        prop.table(table(is.na(hh$Distance)))
    }

    ## 7) based on mean distance by Ship and Year
    if(any(is.na(hh$Distance))){
        mean_distance <- aggregate(Distance ~ Ship + Year, mean, na.rm = T, data = hh[hh$Ship %in% unique(hh$Ship[is.na(hh$Distance)]),])
        hh$id         <- paste0(hh$Ship,"_",hh$Year)
        mean_distance$id <- paste0(mean_distance$Ship,"_",mean_distance$Year)
        hh$mean_distance <- mean_distance$Distance[match(hh$id,mean_distance$id)]

        hh$Distance[is.na(hh$Distance)] <- hh$mean_distance[is.na(hh$Distance)]
        hh$mean_distance <- NULL
        hh$Distance[hh$Distance==0]    <- NA
        table(is.na(hh$Distance))
        table(hh$Distance == 0)
    }

    ## 8) based on mean distance by Ship
    if(any(is.na(hh$Distance))){
        mean_distance <- aggregate(Distance ~ Ship, mean, na.rm = T,
                                   data = hh[hh$Ship %in% unique(hh$Ship[is.na(hh$Distance)]),])
        hh$id         <- paste0(hh$Ship)
        mean_distance$id <- paste0(mean_distance$Ship)
        hh$mean_distance <- mean_distance$Distance[match(hh$id,mean_distance$id)]

        hh$Distance[is.na(hh$Distance)] <- hh$mean_distance[is.na(hh$Distance)]
        hh$mean_distance <- NULL
        hh$Distance[hh$Distance==0]    <- NA
        table(is.na(hh$Distance))
        table(hh$Distance == 0)
    }

    ## 9) based on mean distance by Survey and Year
    if(any(is.na(hh$Distance))){
        mean_distance <- aggregate(Distance ~ Survey + Year, mean, na.rm = T, data = hh[hh$Survey %in% unique(hh$Survey[is.na(hh$Distance)]),])
        hh$id         <- paste0(hh$Survey,"_",hh$Year)
        mean_distance$id <- paste0(mean_distance$Survey,"_",mean_distance$Year)
        hh$mean_distance <- mean_distance$Distance[match(hh$id,mean_distance$id)]

        hh$Distance[is.na(hh$Distance)] <- hh$mean_distance[is.na(hh$Distance)]
        hh$mean_distance <- NULL
        hh$Distance[hh$Distance==0]    <- NA
        table(is.na(hh$Distance))
        table(hh$Distance == 0)
    }

    ## 10) based on mean distance by Survey
    if(any(is.na(hh$Distance))){
        mean_distance <- aggregate(Distance ~ Survey, mean, na.rm = T, data = hh[hh$Survey %in% unique(hh$Survey[is.na(hh$Distance)]),])
        hh$id         <- paste0(hh$Survey)
        mean_distance$id <- paste0(mean_distance$Survey)
        hh$mean_distance <- mean_distance$Distance[match(hh$id,mean_distance$id)]

        hh$Distance[is.na(hh$Distance)] <- hh$mean_distance[is.na(hh$Distance)]
        hh$mean_distance <- NULL
        hh$Distance[hh$Distance==0]    <- NA
        table(is.na(hh$Distance))
        table(hh$Distance == 0)
    }

    ## ------------------------------------------------------------------------------
    ## calculate swept area (= gearwidth x distance)
    ## ------------------------------------------------------------------------------


    hh$SweptAreaDSKM2 <- NA ## based on doorspread
    hh$SweptAreaWSKM2 <- NA ## based on wingspread
    hh$SweptAreaBWKM2 <- NA ## based on beam size

    ## where available, add swept area available in flexfile / OSPAR data
    hh$SweptAreaWSKM2 <- swept_area_WS$SweptAreaWSKM2[match(hh$ID,swept_area_WS$ID)]
    hh$SweptAreaDSKM2 <- swept_area_DS$SweptAreaDSKM2[match(hh$ID,swept_area_DS$ID)]

    table(hh$Year, is.na(hh$SweptAreaDSKM2))
    table(hh$Year, is.na(hh$SweptAreaDSKM2))

    hh$SweptAreaWSKM2[is.na(hh$SweptAreaWSKM2)] <- hh$WingSpread[is.na(hh$SweptAreaWSKM2)] * hh$Distance[is.na(hh$SweptAreaWSKM2)]/1000000 ## in km²
    hh$SweptAreaDSKM2[is.na(hh$SweptAreaDSKM2)] <- hh$DoorSpread[is.na(hh$SweptAreaDSKM2)] * hh$Distance[is.na(hh$SweptAreaDSKM2)]/1000000
    hh$SweptAreaBWKM2                           <- hh$BeamWidth * hh$Distance/1000000


    ## some gear levels were missing in the data, so need an altnerative prediction of door/wingspread
    ## fill missing data by Survey
    tmp_ms <- table(hh$Survey,(is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))
    if("TRUE" == colnames(tmp_ms)){
        missing_surveys <- tmp_ms[,"TRUE"]
        SURVEYS         <- names(missing_surveys[missing_surveys>0])

        for(SURVEY in SURVEYS){

            ## print(SURVEY)
            rmse1 <- NA
            rmse2 <- NA
            rmse3 <- NA
            rmse4 <- NA
            rmse5 <- NA
            rmse6 <- NA

            wing_rmse1 <- NA
            wing_rmse2 <- NA
            wing_rmse3 <- NA
            wing_rmse4 <- NA
            wing_rmse5 <- NA
            wing_rmse6 <- NA

            door.norm1 <- NULL
            door.lnorm1 <- NULL
            door_warp.norm1 <- NULL
            door_warp.lnorm1 <- NULL
            door_depth.norm1 <- NULL
            door_depth.lnorm1 <- NULL
            wing.norm1 <- NULL
            wing.lnorm1 <- NULL
            wing_warp.norm1 <- NULL
            wing_warp.lnorm1 <- NULL
            wing_depth.norm1 <- NULL
            wing_depth.lnorm1 <- NULL


            data_door <- all_data[all_data$Survey == SURVEY,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship","Gear")]
            data_wing <- all_data[all_data$Survey == SURVEY,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship","Gear")]

            hh_door   <- hh_data[!is.na(hh_data$DoorSpread) & hh_data$Survey == SURVEY,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship","Gear")]
            hh_wing   <- hh_data[!is.na(hh_data$WingSpread) & hh_data$Survey == SURVEY,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship","Gear")]

            if(nrow(data_door) > 0 & nrow(hh_door) > 0) {
                data_door <- rbind(data_door, hh_door)
            }
            if(nrow(data_door) == 0 & nrow(hh_door) > 0){
                data_door <- hh_door
            }

            if(nrow(data_wing) > 0 & nrow(hh_wing) > 0) {
                data_wing <- rbind(data_wing, hh_wing)
            }
            if(nrow(data_wing) == 0 & nrow(hh_wing) > 0) {
                data_wing <- hh_wing
            }

            ## logtransform
            data_door$logDoorSpread <- log(data_door$DoorSpread)
            data_wing$logWingSpread <- log(data_wing$WingSpread)

            ## door -------------------------------------------------------------------------

            ## model_data <- data_door[complete.cases(data_door),]
            model_data <- data_door[is.finite(data_door$logDoorSpread),]

            if(nrow(model_data) > 0){

                ## cormat <- cor(model_data[,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed")])
                ## corrplot(cormat, method = "number", type = "upper")

                ## sweep and warplength higly correlated => include only one

                ## sweeplength
                if(sum(!is.na(model_data$SweepLngt))>0){
                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        door.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + SweepLngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        rmse1 <- RMSE(predict(door.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        door.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + SweepLngt + (1|Gear), model_data, family = gaussian())
                        rmse1 <- RMSE(predict(door.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                    } else  {
                        door.norm1    <- lm(DoorSpread ~ Depth + SweepLngt, model_data)
                        rmse1   <- RMSE(predict(door.norm1, newdata = model_data), model_data$DoorSpread)
                    }

                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        door.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + SweepLngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        rmse2 <- RMSE(exp(predict(door.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        door.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + SweepLngt + (1|Gear), model_data, family = gaussian())
                        rmse2 <- RMSE(exp(predict(door.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                    } else  {
                        door.lnorm1    <- lm(logDoorSpread ~ Depth + SweepLngt, model_data)
                        rmse2    <- RMSE(exp(predict(door.lnorm1, newdata = model_data)), model_data$DoorSpread)
                    }

                }


                if(sum(!is.na(model_data$Warplngt))>0){
                    ## warplength
                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        door_warp.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + Warplngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        rmse3 <- RMSE(predict(door_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        door_warp.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + Warplngt + (1|Gear), model_data, family = gaussian())
                        rmse3 <- RMSE(predict(door_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                    } else  {
                        door_warp.norm1    <- lm(DoorSpread ~ Depth + Warplngt, model_data)
                        rmse3   <- RMSE(predict(door_warp.norm1, newdata = model_data), model_data$DoorSpread)
                    }

                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        door_warp.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + Warplngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        rmse4 <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        door_warp.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + Warplngt + (1|Gear), model_data, family = gaussian())
                        rmse4 <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                    } else  {
                        door_warp.lnorm1    <- lm(logDoorSpread ~ Depth + Warplngt, model_data)
                        rmse4    <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data)), model_data$DoorSpread)
                    }
                }



                ## depth
                if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                    door_depth.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + (1|Gear) + (1|Ship), model_data, family = gaussian())
                    rmse5 <- RMSE((predict(door_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                    door_depth.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + (1|Gear), model_data, family = gaussian())
                    rmse5 <- RMSE((predict(door_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else  {
                    door_depth.norm1    <- lm(DoorSpread ~ Depth, model_data)
                    rmse5    <- RMSE((predict(door_depth.norm1, newdata = model_data)), model_data$DoorSpread)
                }

                if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                    door_depth.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + (1|Gear) + (1|Ship), model_data, family = gaussian())
                    rmse6 <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                    door_depth.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + (1|Gear), model_data, family = gaussian())
                    rmse6 <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else  {
                    door_depth.lnorm1    <- lm(logDoorSpread ~ Depth, model_data)
                    rmse6    <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data)), model_data$DoorSpread)
                }

                models_doorspread       <- list(door.norm1,
                                                door.lnorm1,
                                                door_warp.norm1,
                                                door_warp.lnorm1)
                names(models_doorspread) <- c("door.norm1",
                                              "door.lnorm1",
                                              "door_warp.norm1",
                                              "door_warp.lnorm1")

                depth_models_doorspread <- list(door_depth.norm1,
                                                door_depth.lnorm1)
                names(depth_models_doorspread) <- c("door_depth_lmm.norm1",
                                                    "door_depth_lmm.lnorm1")

                ## add model predictions door and wingspread according best performing model (RMSE)

                doorspread_rmse            <- c(rmse1,rmse2,rmse3,rmse4)
                names(doorspread_rmse)     <- names(models_doorspread)
                doorspread_d_rmse          <- c(rmse5,rmse6)
                names(doorspread_d_rmse)   <- names(depth_models_doorspread)

                ## depth, sweeplength and warplength
                all_data_door <- hh[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_door) > 0 & any(!is.na(doorspread_rmse))){
                    mod <- models_doorspread[[which.min(doorspread_rmse)]]
                    hh$DoorSpread[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_door, allow.new.levels = T)
                }
                ## depth and warplength
                all_data_door_warp <- hh[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_door_warp) > 0 & any(!is.na(doorspread_rmse[3:4]))){
                    mod <- models_doorspread[3:4][[which.min(doorspread_rmse[3:4])]]
                    hh$DoorSpread[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_door_warp, allow.new.levels = T)
                }
                all_data_door_sweep <- hh[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_door_sweep) > 0 & any(!is.na(doorspread_rmse[1:2]))){
                    mod <- models_doorspread[1:2][[which.min(doorspread_rmse[1:2])]]
                    hh$DoorSpread[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_door_sweep, allow.new.levels = T)
                }
                all_data_door_depth <- hh[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_door_depth) > 0 & any(!is.na(doorspread_d_rmse))){
                    mod <- depth_models_doorspread[[which.min(doorspread_d_rmse)]]
                    hh$DoorSpread[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_door_depth, allow.new.levels = T)
                }

            }



            ## wing -------------------------------------------------------------------------

            ## model_data <- data_wing[complete.cases(data_wing),]
            model_data <- data_wing[is.finite(data_wing$logWingSpread),]

            if(nrow(model_data) > 0){
                ## cormat <- cor(model_data[,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed")])
                ## corrplot(cormat, method = "number", type = "upper")

                ## sweep and warplength higly correlated => include only one

                ## sweeplength
                if(sum(!is.na(model_data$SweepLngt))>0){
                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        wing.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + SweepLngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        wing_rmse1 <- RMSE(predict(wing.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        wing.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + SweepLngt + (1|Gear), model_data, family = gaussian())
                        wing_rmse1 <- RMSE(predict(wing.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                    } else  {
                        wing.norm1    <- lm(WingSpread ~ Depth + SweepLngt, model_data)
                        wing_rmse1   <- RMSE(predict(wing.norm1, newdata = model_data), model_data$WingSpread)
                    }

                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        wing.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + SweepLngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        wing_rmse2 <- RMSE(exp(predict(wing.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        wing.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + SweepLngt + (1|Gear), model_data, family = gaussian())
                        wing_rmse2 <- RMSE(exp(predict(wing.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                    } else  {
                        wing.lnorm1    <- lm(logWingSpread ~ Depth + SweepLngt, model_data)
                        wing_rmse2    <- RMSE(exp(predict(wing.lnorm1, newdata = model_data)), model_data$WingSpread)
                    }
                }


                ## warplength
                if(sum(!is.na(model_data$Warplngt))>0){
                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        wing_warp.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + Warplngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        wing_rmse3 <- RMSE(predict(wing_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        wing_warp.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + Warplngt + (1|Gear), model_data, family = gaussian())
                        wing_rmse3 <- RMSE(predict(wing_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                    } else  {
                        wing_warp.norm1    <- lm(WingSpread ~ Depth + Warplngt, model_data)
                        wing_rmse3   <- RMSE(predict(wing_warp.norm1, newdata = model_data), model_data$WingSpread)
                    }

                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        wing_warp.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + Warplngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        wing_rmse4 <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        wing_warp.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + Warplngt + (1|Gear), model_data, family = gaussian())
                        wing_rmse4 <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                    } else  {
                        wing_warp.lnorm1    <- lm(logWingSpread ~ Depth + Warplngt, model_data)
                        wing_rmse4    <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data)), model_data$WingSpread)
                    }
                }


                ## depth
                if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                    wing_depth.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + (1|Gear) + (1|Ship), model_data, family = gaussian())
                    wing_rmse5 <- RMSE((predict(wing_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                    wing_depth.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + (1|Gear), model_data, family = gaussian())
                    wing_rmse5 <- RMSE((predict(wing_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else  {
                    wing_depth.norm1    <- lm(WingSpread ~ Depth, model_data)
                    wing_rmse5    <- RMSE((predict(wing_depth.norm1, newdata = model_data)), model_data$WingSpread)
                }

                if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                    wing_depth.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + (1|Gear) + (1|Ship), model_data, family = gaussian())
                    wing_rmse6 <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                    wing_depth.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + (1|Gear), model_data, family = gaussian())
                    wing_rmse6 <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else  {
                    wing_depth.lnorm1    <- lm(logWingSpread ~ Depth, model_data)
                    wing_rmse6    <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data)), model_data$WingSpread)
                }

                models_wingspread       <- list(wing.norm1,
                                                wing.lnorm1,
                                                wing_warp.norm1,
                                                wing_warp.lnorm1)
                names(models_wingspread) <- c("wing.norm1",
                                              "wing.lnorm1",
                                              "wing_warp.norm1",
                                              "wing_warp.lnorm1")

                depth_models_wingspread <- list(wing_depth.norm1,
                                                wing_depth.lnorm1)
                names(depth_models_wingspread) <- c("wing_depth_lmm.norm1",
                                                    "wing_depth_lmm.lnorm1")

                wingspread_rmse            <- c(wing_rmse1,wing_rmse2,wing_rmse3,wing_rmse4)
                names(wingspread_rmse)     <- names(models_wingspread)
                wingspread_d_rmse          <- c(wing_rmse5,wing_rmse6)
                names(wingspread_d_rmse)   <- names(depth_models_wingspread)

                ## depth, sweeplength and warplength
                all_data_wing <- hh[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_wing) > 0 & any(!is.na(wingspread_rmse))){
                    mod <- models_wingspread[[which.min(wingspread_rmse)]]
                    hh$WingSpread[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_wing, allow.new.levels = T)
                }
                ## depth and warplength
                all_data_wing_warp <- hh[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_wing_warp) > 0 & any(!is.na(wingspread_rmse[3:4]))){
                    mod <- models_wingspread[3:4][[which.min(wingspread_rmse[3:4])]]
                    hh$WingSpread[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_wing_warp, allow.new.levels = T)
                }
                all_data_wing_sweep <- hh[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_wing_sweep) > 0 & any(!is.na(wingspread_rmse[1:2]))){
                    mod <- models_wingspread[1:2][[which.min(wingspread_rmse[1:2])]]
                    hh$WingSpread[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_wing_sweep, allow.new.levels = T)
                }
                all_data_wing_depth <- hh[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_wing_depth) > 0 & any(!is.na(wingspread_d_rmse))){
                    mod <- depth_models_wingspread[[which.min(wingspread_d_rmse)]]
                    hh$WingSpread[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_wing_depth, allow.new.levels = T)
                }

            }

            table(is.na(hh$DoorSpread))
            table(is.na(hh$WingSpread))
        }

        ## calculate the swept areas
        hh$SweptAreaWSKM2[is.na(hh$SweptAreaWSKM2)] <- hh$WingSpread[is.na(hh$SweptAreaWSKM2)] * hh$Distance[is.na(hh$SweptAreaWSKM2)]/1000000
        hh$SweptAreaDSKM2[is.na(hh$SweptAreaDSKM2)] <- hh$DoorSpread[is.na(hh$SweptAreaDSKM2)] * hh$Distance[is.na(hh$SweptAreaDSKM2)]/1000000

        ## check for missing observations
        table(hh$Year,(is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))
        table(hh$Survey,(is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))

        ## add the Portugese data
        hh$ID <- paste0(hh$Survey,"_",hh$Country,"_",hh$Quarter,"_",hh$Gear,"_",hh$Ship,"_",
                        hh$HaulNo,"_",hh$Year,"_",hh$Month,"_",hh$Day)

        hh_PT <- subset(hh, Survey == "PT-IBTS")
        hh    <- subset(hh, Survey != "PT-IBTS")

        hh_PT$SweptAreaDSKM2  <- NULL
        hh_PT$SweptAreaWSKM2  <- NULL

        hh_PT <- merge(hh_PT, PT_data[,c("ID","SweptAreaDSKM2","SweptAreaWSKM2")], by = "ID", all.x = TRUE)
        hh_PT <- hh_PT[,colnames(hh)]

        hh <- rbind(hh,hh_PT)

        PT_data$DoorSpread[is.na(PT_data$DoorSpread)] <- PT_data$SweptAreaDSKM2[is.na(PT_data$DoorSpread)] / PT_data$Distance[is.na(PT_data$DoorSpread)] * 1000000
        PT_data$WingSpread[is.na(PT_data$WingSpread)] <- PT_data$SweptAreaWSKM2[is.na(PT_data$WingSpread)] / PT_data$Distance[is.na(PT_data$WingSpread)] * 1000000

        ## check for missing observations
        table(hh$Year,(is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))
        table(hh$Survey,(is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))

    }

    ## some gear levels were missing in the data, so need an altnerative prediction of door/wingspread
    ## fill missing data by Survey
    tmp_ms <- table(hh$Survey,(is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))
    if("TRUE" == colnames(tmp_ms)){
        missing_surveys <- tmp_ms[,"TRUE"]
        SURVEYS         <- names(missing_surveys[missing_surveys>0])

        for(SURVEY in SURVEYS){

            ## print(SURVEY)
            rmse1 <- NA
            rmse2 <- NA
            rmse3 <- NA
            rmse4 <- NA
            rmse5 <- NA
            rmse6 <- NA

            wing_rmse1 <- NA
            wing_rmse2 <- NA
            wing_rmse3 <- NA
            wing_rmse4 <- NA
            wing_rmse5 <- NA
            wing_rmse6 <- NA

            door.norm1 <- NULL
            door.lnorm1 <- NULL
            door_warp.norm1 <- NULL
            door_warp.lnorm1 <- NULL
            door_depth.norm1 <- NULL
            door_depth.lnorm1 <- NULL
            wing.norm1 <- NULL
            wing.lnorm1 <- NULL
            wing_warp.norm1 <- NULL
            wing_warp.lnorm1 <- NULL
            wing_depth.norm1 <- NULL
            wing_depth.lnorm1 <- NULL


            data_door <- PT_data[PT_data$Survey == SURVEY,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship","Gear")]
            data_wing <- PT_data[PT_data$Survey == SURVEY,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed","Survey","Ship","Gear")]

            ## logtransform
            data_door$logDoorSpread <- log(data_door$DoorSpread)
            data_wing$logWingSpread <- log(data_wing$WingSpread)

            ## door -------------------------------------------------------------------------

            ## model_data <- data_door[complete.cases(data_door),]
            model_data <- data_door[is.finite(data_door$logDoorSpread),]

            if(nrow(model_data) > 0){

                ## cormat <- cor(model_data[,c("DoorSpread","Depth","SweepLngt","Warplngt","GroundSpeed")])
                ## corrplot(cormat, method = "number", type = "upper")

                ## sweep and warplength higly correlated => include only one

                ## sweeplength
                if(sum(!is.na(model_data$SweepLngt))>0){
                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        door.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + SweepLngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        rmse1 <- RMSE(predict(door.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        door.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + SweepLngt + (1|Gear), model_data, family = gaussian())
                        rmse1 <- RMSE(predict(door.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                    } else  {
                        door.norm1    <- lm(DoorSpread ~ Depth + SweepLngt, model_data)
                        rmse1   <- RMSE(predict(door.norm1, newdata = model_data), model_data$DoorSpread)
                    }

                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        door.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + SweepLngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        rmse2 <- RMSE(exp(predict(door.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        door.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + SweepLngt + (1|Gear), model_data, family = gaussian())
                        rmse2 <- RMSE(exp(predict(door.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                    } else  {
                        door.lnorm1    <- lm(logDoorSpread ~ Depth + SweepLngt, model_data)
                        rmse2    <- RMSE(exp(predict(door.lnorm1, newdata = model_data)), model_data$DoorSpread)
                    }

                }


                if(sum(!is.na(model_data$Warplngt))>0){
                    ## warplength
                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        door_warp.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + Warplngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        rmse3 <- RMSE(predict(door_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        door_warp.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + Warplngt + (1|Gear), model_data, family = gaussian())
                        rmse3 <- RMSE(predict(door_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$DoorSpread)
                    } else  {
                        door_warp.norm1    <- lm(DoorSpread ~ Depth + Warplngt, model_data)
                        rmse3   <- RMSE(predict(door_warp.norm1, newdata = model_data), model_data$DoorSpread)
                    }

                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        door_warp.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + Warplngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        rmse4 <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        door_warp.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + Warplngt + (1|Gear), model_data, family = gaussian())
                        rmse4 <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                    } else  {
                        door_warp.lnorm1    <- lm(logDoorSpread ~ Depth + Warplngt, model_data)
                        rmse4    <- RMSE(exp(predict(door_warp.lnorm1, newdata = model_data)), model_data$DoorSpread)
                    }
                }



                ## depth
                if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                    door_depth.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + (1|Gear) + (1|Ship), model_data, family = gaussian())
                    rmse5 <- RMSE((predict(door_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                    door_depth.norm1   <- glmmTMB::glmmTMB(DoorSpread ~ Depth + (1|Gear), model_data, family = gaussian())
                    rmse5 <- RMSE((predict(door_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else  {
                    door_depth.norm1    <- lm(DoorSpread ~ Depth, model_data)
                    rmse5    <- RMSE((predict(door_depth.norm1, newdata = model_data)), model_data$DoorSpread)
                }

                if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                    door_depth.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + (1|Gear) + (1|Ship), model_data, family = gaussian())
                    rmse6 <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                    door_depth.lnorm1   <- glmmTMB::glmmTMB(logDoorSpread ~ Depth + (1|Gear), model_data, family = gaussian())
                    rmse6 <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$DoorSpread)
                } else  {
                    door_depth.lnorm1    <- lm(logDoorSpread ~ Depth, model_data)
                    rmse6    <- RMSE(exp(predict(door_depth.lnorm1, newdata = model_data)), model_data$DoorSpread)
                }

                models_doorspread       <- list(door.norm1,
                                                door.lnorm1,
                                                door_warp.norm1,
                                                door_warp.lnorm1)
                names(models_doorspread) <- c("door.norm1",
                                              "door.lnorm1",
                                              "door_warp.norm1",
                                              "door_warp.lnorm1")

                depth_models_doorspread <- list(door_depth.norm1,
                                                door_depth.lnorm1)
                names(depth_models_doorspread) <- c("door_depth_lmm.norm1",
                                                    "door_depth_lmm.lnorm1")

                ## add model predictions door and wingspread according best performing model (RMSE)

                doorspread_rmse            <- c(rmse1,rmse2,rmse3,rmse4)
                names(doorspread_rmse)     <- names(models_doorspread)
                doorspread_d_rmse          <- c(rmse5,rmse6)
                names(doorspread_d_rmse)   <- names(depth_models_doorspread)

                ## depth, sweeplength and warplength
                all_data_door <- hh[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_door) > 0 & any(!is.na(doorspread_rmse))){
                    mod <- models_doorspread[[which.min(doorspread_rmse)]]
                    hh$DoorSpread[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_door, allow.new.levels = T)
                }
                ## depth and warplength
                all_data_door_warp <- hh[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_door_warp) > 0 & any(!is.na(doorspread_rmse[3:4]))){
                    mod <- models_doorspread[3:4][[which.min(doorspread_rmse[3:4])]]
                    hh$DoorSpread[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_door_warp, allow.new.levels = T)
                }
                all_data_door_sweep <- hh[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_door_sweep) > 0 & any(!is.na(doorspread_rmse[1:2]))){
                    mod <- models_doorspread[1:2][[which.min(doorspread_rmse[1:2])]]
                    hh$DoorSpread[is.na(hh$DoorSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_door_sweep, allow.new.levels = T)
                }
                all_data_door_depth <- hh[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_door_depth) > 0 & any(!is.na(doorspread_d_rmse))){
                    mod <- depth_models_doorspread[[which.min(doorspread_d_rmse)]]
                    hh$DoorSpread[is.na(hh$DoorSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_door_depth, allow.new.levels = T)
                }

            }



            ## wing -------------------------------------------------------------------------

            ## model_data <- data_wing[complete.cases(data_wing),]
            model_data <- data_wing[is.finite(data_wing$logWingSpread),]

            if(nrow(model_data) > 0){
                ## cormat <- cor(model_data[,c("WingSpread","Depth","SweepLngt","Warplngt","GroundSpeed")])
                ## corrplot(cormat, method = "number", type = "upper")

                ## sweep and warplength higly correlated => include only one

                ## sweeplength
                if(sum(!is.na(model_data$SweepLngt))>0){
                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        wing.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + SweepLngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        wing_rmse1 <- RMSE(predict(wing.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        wing.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + SweepLngt + (1|Gear), model_data, family = gaussian())
                        wing_rmse1 <- RMSE(predict(wing.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                    } else  {
                        wing.norm1    <- lm(WingSpread ~ Depth + SweepLngt, model_data)
                        wing_rmse1   <- RMSE(predict(wing.norm1, newdata = model_data), model_data$WingSpread)
                    }

                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        wing.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + SweepLngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        wing_rmse2 <- RMSE(exp(predict(wing.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        wing.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + SweepLngt + (1|Gear), model_data, family = gaussian())
                        wing_rmse2 <- RMSE(exp(predict(wing.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                    } else  {
                        wing.lnorm1    <- lm(logWingSpread ~ Depth + SweepLngt, model_data)
                        wing_rmse2    <- RMSE(exp(predict(wing.lnorm1, newdata = model_data)), model_data$WingSpread)
                    }
                }


                ## warplength
                if(sum(!is.na(model_data$Warplngt))>0){
                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        wing_warp.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + Warplngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        wing_rmse3 <- RMSE(predict(wing_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        wing_warp.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + Warplngt + (1|Gear), model_data, family = gaussian())
                        wing_rmse3 <- RMSE(predict(wing_warp.norm1, newdata = model_data, allow.new.levels = T), model_data$WingSpread)
                    } else  {
                        wing_warp.norm1    <- lm(WingSpread ~ Depth + Warplngt, model_data)
                        wing_rmse3   <- RMSE(predict(wing_warp.norm1, newdata = model_data), model_data$WingSpread)
                    }

                    if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                        wing_warp.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + Warplngt + (1|Gear) + (1|Ship), model_data, family = gaussian())
                        wing_rmse4 <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                    } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                        wing_warp.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + Warplngt + (1|Gear), model_data, family = gaussian())
                        wing_rmse4 <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                    } else  {
                        wing_warp.lnorm1    <- lm(logWingSpread ~ Depth + Warplngt, model_data)
                        wing_rmse4    <- RMSE(exp(predict(wing_warp.lnorm1, newdata = model_data)), model_data$WingSpread)
                    }
                }


                ## depth
                if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                    wing_depth.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + (1|Gear) + (1|Ship), model_data, family = gaussian())
                    wing_rmse5 <- RMSE((predict(wing_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                    wing_depth.norm1   <- glmmTMB::glmmTMB(WingSpread ~ Depth + (1|Gear), model_data, family = gaussian())
                    wing_rmse5 <- RMSE((predict(wing_depth.norm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else  {
                    wing_depth.norm1    <- lm(WingSpread ~ Depth, model_data)
                    wing_rmse5    <- RMSE((predict(wing_depth.norm1, newdata = model_data)), model_data$WingSpread)
                }

                if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))> 2){
                    wing_depth.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + (1|Gear) + (1|Ship), model_data, family = gaussian())
                    wing_rmse6 <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else if(length(unique(model_data$Gear))> 2 & length(unique(model_data$Ship))<= 2){
                    wing_depth.lnorm1   <- glmmTMB::glmmTMB(logWingSpread ~ Depth + (1|Gear), model_data, family = gaussian())
                    wing_rmse6 <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data, allow.new.levels = T)), model_data$WingSpread)
                } else  {
                    wing_depth.lnorm1    <- lm(logWingSpread ~ Depth, model_data)
                    wing_rmse6    <- RMSE(exp(predict(wing_depth.lnorm1, newdata = model_data)), model_data$WingSpread)
                }

                models_wingspread       <- list(wing.norm1,
                                                wing.lnorm1,
                                                wing_warp.norm1,
                                                wing_warp.lnorm1)
                names(models_wingspread) <- c("wing.norm1",
                                              "wing.lnorm1",
                                              "wing_warp.norm1",
                                              "wing_warp.lnorm1")

                depth_models_wingspread <- list(wing_depth.norm1,
                                                wing_depth.lnorm1)
                names(depth_models_wingspread) <- c("wing_depth_lmm.norm1",
                                                    "wing_depth_lmm.lnorm1")

                wingspread_rmse            <- c(wing_rmse1,wing_rmse2,wing_rmse3,wing_rmse4)
                names(wingspread_rmse)     <- names(models_wingspread)
                wingspread_d_rmse          <- c(wing_rmse5,wing_rmse6)
                names(wingspread_d_rmse)   <- names(depth_models_wingspread)

                ## depth, sweeplength and warplength
                all_data_wing <- hh[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_wing) > 0 & any(!is.na(wingspread_rmse))){
                    mod <- models_wingspread[[which.min(wingspread_rmse)]]
                    hh$WingSpread[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_wing, allow.new.levels = T)
                }
                ## depth and warplength
                all_data_wing_warp <- hh[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_wing_warp) > 0 & any(!is.na(wingspread_rmse[3:4]))){
                    mod <- models_wingspread[3:4][[which.min(wingspread_rmse[3:4])]]
                    hh$WingSpread[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & !is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_wing_warp, allow.new.levels = T)
                }
                all_data_wing_sweep <- hh[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_wing_sweep) > 0 & any(!is.na(wingspread_rmse[1:2]))){
                    mod <- models_wingspread[1:2][[which.min(wingspread_rmse[1:2])]]
                    hh$WingSpread[is.na(hh$WingSpread) & !is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_wing_sweep, allow.new.levels = T)
                }
                all_data_wing_depth <- hh[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY,]
                if(nrow(all_data_wing_depth) > 0 & any(!is.na(wingspread_d_rmse))){
                    mod <- depth_models_wingspread[[which.min(wingspread_d_rmse)]]
                    hh$WingSpread[is.na(hh$WingSpread) & is.na(hh$SweepLngt)  & is.na(hh$Warplngt) & !is.na(hh$Depth) & hh$Survey == SURVEY] <-
                        predict(mod, newdata = all_data_wing_depth, allow.new.levels = T)
                }

            }

            table(is.na(hh$DoorSpread[hh$Survey == SURVEY]))
            table(is.na(hh$WingSpread[hh$Survey == SURVEY]))
        }

    }


    ## calculate the swept areas
    hh$SweptAreaWSKM2[is.na(hh$SweptAreaWSKM2)] <- hh$WingSpread[is.na(hh$SweptAreaWSKM2)] * hh$Distance[is.na(hh$SweptAreaWSKM2)]/1000000
    hh$SweptAreaDSKM2[is.na(hh$SweptAreaDSKM2)] <- hh$DoorSpread[is.na(hh$SweptAreaDSKM2)] * hh$Distance[is.na(hh$SweptAreaDSKM2)]/1000000

    table((is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))



    ## check for missing observations
    table(hh$Year,(is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))
    table(hh$Survey,(is.na(hh$SweptAreaWSKM2) | is.na(hh$SweptAreaDSKM2)) & is.na(hh$SweptAreaBWKM2))

    ## remove some columns
    hh$ID <- NULL
    hh$Depth_gam <- hh$Depth
    hh$Depth       <- NULL
    hh$id          <- NULL

    if(plot){
        ## some simple plots
        par(mfrow = c(1,2))
        boxplot(SweptAreaWSKM2 ~ Survey, hh)
        boxplot(SweptAreaWSKM2 ~ Year, hh)

    }

    data$hh <- hh

    return(data)
}
