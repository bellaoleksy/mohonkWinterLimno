#Script 05-TrendsInWinterWaterTemperature.R####
#Explore the winter water temperature and associated metrics for Mohonk Lake
#Created 15Jul2022, by David Richardson (DCR)

#Run previous code to get in data####
source('analysis/00_main.R')


# Set theme ---------------------------------------------------------------


theme_MS <- function () {
  theme_base(base_size=10) %+replace%
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, linewidth=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.y = element_text(size=10, angle=270),
      strip.text.x = element_text(size=10),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}

theme_set(theme_MS())

      
      
#Final model - Model 7 includes a more linear version####
      model7<-'
        
        MeanUnderIce_HypoTemp_degC_scale~IceInDayofYear_fed_scale
        MeanUnderIce_EpiTemp_degC_scale~IceInDayofYear_fed_scale
        MeanDelta1_11mWaterDensity_kgperm3_scale ~ MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
        MeanHeatContent_MegaJoules_scale~MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale+ MeanDelta1_11mWaterDensity_kgperm3_scale
        IceOutDayofYear_fed_scale~MeanDelta1_11mWaterDensity_kgperm3_scale+MeanUnderIce_EpiTemp_degC_scale+MeanUnderIce_HypoTemp_degC_scale+MeanHeatContent_MegaJoules_scale
        LengthSpringMixedPeriod_days_scale ~~  IceOutDayofYear_fed_scale
       
        LengthFallMixedPeriod_days_scale~~IceInDayofYear_fed_scale
        MeanUnderIce_EpiTemp_degC_scale~~MeanUnderIce_HypoTemp_degC_scale
        LengthFallMixedPeriod_days_scale~~0*LengthSpringMixedPeriod_days_scale
        LengthFallMixedPeriod_days_scale~~0*MeanHeatContent_MegaJoules_scale
         
         
        '    
      #IceOutDayofYear_fed_scale~MeanDelta1_11mWaterDensity_kgperm3_scale+MeanUnderIce_EpiTemp_degC_scale+MeanUnderIce_HypoTemp_degC_scale+MeanHeatContent_MegaJoules_scale
      # LengthSpringMixedPeriod_days_scale ~  IceOutDayofYear_fed_scale
      #StartOfStratification_HydroDay_scale~LengthSpringMixedPeriod_days_scale
      #StartOfStratification_HydroDay_scale~IceOutDayofYear_fed_scale
      #StartOfStratification_HydroDay_scale~~LengthSpringMixedPeriod_days_scale
      #IceInDayofYear_fed_scale~LengthFallMixedPeriod_days_scale
      # LengthSpringMixedPeriod_days_scale ~  IceOutDayofYear_fed_scale
      #FinalHeatContent_MegaJoules_scale~MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
      #LengthFallMixedPeriod_days_scale~~EndOfStratification_HydroDay_scale
      
      #**Fit the new temporal model####
      #From lavaan package
      fit<-sem(model7,data=AnnualUnderIceSummary_SEM,meanstructure=TRUE)
      varTable(fit)
      
      #***view the results####
      summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
      
      parameterEstimates(fit)
      
      #Visualize the SEM
      semPlot::semPaths(fit,'std',layout='spring',edge.label.cex = 1.3,label.cex=1.1,intercepts=FALSE,curve=TRUE)          
      
      #Look at all the fall inter-comparisons####
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthFallMixedPeriod_days_scale,y=EndOfStratification_HydroDay_scale))+geom_point()
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthFallMixedPeriod_days_scale,y=IceInDayofYear_fed_scale))+geom_point()
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthFallMixedPeriod_days,y=IceInDayofYear_fed))+geom_point()
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=EndOfStratification_HydroDay_scale,y=IceInDayofYear_fed_scale))+geom_point()
      
      #Fall mixing and ice####
      ggplot(data=AnnualUnderIceSummary_SEM,aes(y=LengthFallMixedPeriod_days,x=IceInDayofYear_fed))+geom_point()
      
      #Under ice dynamics####
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceInDayofYear_fed,y=MeanUnderIce_HypoTemp_degC))+geom_point()
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceInDayofYear_fed,y=MeanUnderIce_EpiTemp_degC))+geom_point()
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceInDayofYear_fed,y=MeanDelta1_11mWaterDensity_kgperm3))+geom_point()
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=MeanUnderIce_EpiTemp_degC,y=MeanDelta1_11mWaterDensity_kgperm3))+geom_point()
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=MeanUnderIce_HypoTemp_degC,y=MeanDelta1_11mWaterDensity_kgperm3))+geom_point()
      cor.test(AnnualUnderIceSummary_SEM$MeanUnderIce_EpiTemp_degC,AnnualUnderIceSummary_SEM$MeanDelta1_11mWaterDensity_kgperm3)
      
      #Under water affecting ice off####
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=MeanUnderIce_HypoTemp_degC,y=IceOutDayofYear_fed))+geom_point()
      cor.test(AnnualUnderIceSummary_SEM$MeanUnderIce_HypoTemp_degC,AnnualUnderIceSummary_SEM$IceOutDayofYear_fed)
      
      #Ice off and spring mixing####
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceOutDayofYear_fed,y=LengthSpringMixedPeriod_days))+geom_point()
      cor.test(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,AnnualUnderIceSummary_SEM$LengthSpringMixedPeriod_days)

      
      
      
#MS FIGURE - SEM: Create customized ggplot of network from lavaan object####
      #https://www.ethan-young.com/code/sem-diagrams-for-lavaan-models/
      #*Extract all the parameters####
      lavaan_parameters <- parameterestimates(fit)
      
      #Supplemental table: Create an output file for optimal SEM model####
      ms_suppTable_sem<-lavaan_parameters%>%
        #filter(!op=="~1")%>% #remove all the intercepts
        dplyr::select(-ci.lower,-ci.upper)%>%
        rename(estimate=est,
               estimate_se=se,
               z_statistic=z,
               )%>%
        filter(!is.na(z_statistic))%>% #remove the na's for z stat
        filter(!(op=="~~"&z_statistic>3.3))%>% #remove the extraneous covariances
        mutate(type=case_when(op=="~"~"Regression",
                              op=="~~"~"Covariance",
                              op=="~1"~"Intercept"))%>%
        filter(!(lhs=="IceInDayofYear_fed_scale"&op=="~1"))%>%
        filter(!(lhs=="LengthSpringMixedPeriod_days_scale"&op=="~1"))%>%
        filter(!(lhs=="LengthFallMixedPeriod_days_scale"&op=="~1"))%>%
        mutate(reorder_index=c(4,
                               2,
                               7,
                               8,
                               10,
                               11,
                               12,
                               14,
                               15,
                               16,
                               17,
                               # 19,
                               1,
                               6,
                               5,
                               3,
                               9,
                               13,
                               18))%>% #create an index to arrange the rows left to right
        arrange(reorder_index)
      
      #export supplemental table####
      write_csv(file="output/MohkWinterLimno_SupplementalTable_SEM.csv",x=ms_suppTable_sem)
      
      
      #*Create graphical locations for each of the nodes####
      nodes <- lavaan_parameters %>% 
        dplyr::select(lhs) %>% 
        rename(name = lhs) %>% 
        distinct(name) %>% 
        mutate(
          x = case_when(name == "LengthFallMixedPeriod_days_scale"         ~ 0.05,
                        name == "IceInDayofYear_fed_scale"                 ~ 0.25,
                        name == "MeanUnderIce_EpiTemp_degC_scale"          ~ 0.4,
                        name == "MeanUnderIce_HypoTemp_degC_scale"         ~ 0.4,
                        name == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ 0.6,
                        name == "MeanHeatContent_MegaJoules_scale"         ~ 0.6,
                        name == "IceOutDayofYear_fed_scale"                ~ 0.75,
                        name == "LengthSpringMixedPeriod_days_scale"       ~ 0.95
                        ),
          y = case_when(name == "LengthFallMixedPeriod_days_scale"         ~ 0.4,
                        name == "IceInDayofYear_fed_scale"                 ~ 0.4,
                        name == "MeanUnderIce_EpiTemp_degC_scale"          ~ 0.75,
                        name == "MeanUnderIce_HypoTemp_degC_scale"         ~ 0.05,
                        name == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ 0.75,
                        name == "MeanHeatContent_MegaJoules_scale"         ~ 0.05,
                        name == "IceOutDayofYear_fed_scale"                ~ 0.4,
                        name == "LengthSpringMixedPeriod_days_scale"       ~ 0.4
                        ),
          xend = x,
          yend = y
        )
      #*Create graphical locations for each of the edges####
      edges <- lavaan_parameters %>%
        filter(op %in% c("~","~~"))
      #Combine edges and nodes
      combined <- nodes %>% 
        bind_rows(
          left_join(edges,nodes %>% select(name,xend,yend),by=c("lhs"="name")) %>%
            left_join(nodes %>% select(name,x,y),by = c("rhs"="name"))
        )
      #Create edge labels
      combined_edge_labels <- combined %>% 
        mutate(
          est = round(est,2),
          p.code     = ifelse(pvalue<.05,"p < .05","p > .05"),
          shape      = "observed",

          node.labels=case_when(name=="MeanUnderIce_HypoTemp_degC_scale" ~ as.character(expression(Deep*degree*C)),
                                name=="MeanUnderIce_EpiTemp_degC_scale" ~ as.character(expression(Shal*degree*C)),
                                name=="MeanDelta1_11mWaterDensity_kgperm3_scale" ~ as.character(expression(paste("Dens ",Delta))),
                                name=="LengthSpringMixedPeriod_days_scale" ~ as.character(expression(paste("Spr Mix"))),
                                name=="LengthOfIceCover_days_scale" ~ as.character(expression(paste("Ice Dur"))),
                                name=="IceOutDayofYear_fed_scale" ~ as.character(expression(paste("Ice-Off"))),
                                name=="IceInDayofYear_fed_scale" ~ as.character(expression(paste("Ice-On"))),
                                name=="LengthFallMixedPeriod_days_scale" ~ as.character(expression(paste("Fll Mix"))),
                                name=="MeanHeatContent_MegaJoules_scale" ~ as.character(expression(paste("Heat")))
                                
                                
          ),
          arrow.ends=case_when(op=="~"~"last",
                               op=="~~"~"both"
          ),
          edge_color=case_when(est<0&pvalue<0.05~"red",
                               est>0&pvalue<0.05~"green",
                               TRUE~"darkgrey"),
          
          #create a variable that is a factor for the weight of the edges
          edge.factor=factor(abs(est)),
          x_arrow_start=x,
          x_arrow_end=xend,
          y_arrow_start=y,
          y_arrow_end=yend,
          x_arrow_start=case_when(lhs == "IceInDayofYear_fed_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ x_arrow_start+0.055,
                                  lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ x_arrow_start,
                                  lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ NA_real_, #turns off this arrow
                                  lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "IceInDayofYear_fed_scale" ~ x_arrow_start,
                                  lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "IceInDayofYear_fed_scale" ~ x_arrow_start,
                                  lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ x_arrow_start,
                                  lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ x_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ x_arrow_start,
                                  lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ x_arrow_start,
                                  lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ x_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ x_arrow_start,
                                  lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ x_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ x_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanHeatContent_MegaJoules_scale" ~ x_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "LengthSpringMixedPeriod_days_scale" ~ x_arrow_start-0.05,
                                  lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ NA_real_, #turns off this arrow
                                  TRUE~x_arrow_start
                                  ),
          x_arrow_end=case_when(lhs == "IceInDayofYear_fed_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ x_arrow_end-0.055,
                                lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ x_arrow_end,
                                lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ NA_real_, #turns off this arrow
                                lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "IceInDayofYear_fed_scale" ~ x_arrow_end-0.05,
                                lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "IceInDayofYear_fed_scale" ~ x_arrow_end-0.05,
                                lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ x_arrow_end-0.05,
                                lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ x_arrow_end-0.05,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ x_arrow_end-0.05,
                                lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ x_arrow_end-0.05,
                                lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ x_arrow_end-0.05,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ x_arrow_end-0.05,
                                lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ x_arrow_end,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ x_arrow_end-0.05,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanHeatContent_MegaJoules_scale" ~ x_arrow_end-0.05,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "LengthSpringMixedPeriod_days_scale" ~ x_arrow_end+0.05,
                                lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ NA_real_, #turns off this arrow
                                  TRUE~x_arrow_end
                                  ),
          y_arrow_start=case_when(lhs == "IceInDayofYear_fed_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ y_arrow_start,
                                  lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y_arrow_start-0.13,
                                  lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ NA_real_, #turns off this arrow
                                  lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "IceInDayofYear_fed_scale" ~ y_arrow_start+0.1,
                                  lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "IceInDayofYear_fed_scale" ~ y_arrow_start-0.1,
                                  lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y_arrow_start,
                                  lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y_arrow_start,
                                  lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ y_arrow_start,
                                  lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ y_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ y_arrow_start,
                                  lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ y_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ y_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanHeatContent_MegaJoules_scale" ~ y_arrow_start,
                                  lhs == "IceOutDayofYear_fed_scale" & rhs == "LengthSpringMixedPeriod_days_scale" ~ y_arrow_start,
                                  lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ NA_real_, #turns off this arrow
                                  TRUE~y_arrow_start
                                  
                                  ),
          y_arrow_end=case_when(lhs == "IceInDayofYear_fed_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ y_arrow_end,
                                lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y_arrow_end+0.13,
                                lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ NA_real_, #turns off this arrow
                                lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "IceInDayofYear_fed_scale" ~ y_arrow_end,
                                lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "IceInDayofYear_fed_scale" ~ y_arrow_end,
                                lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y_arrow_end,
                                lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y_arrow_end+0.08,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y_arrow_end+0.02,
                                lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ y_arrow_end-0.08,
                                lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ y_arrow_end,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ y_arrow_end-0.02,
                                lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ y_arrow_end+0.13,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ y_arrow_end+0.1,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanHeatContent_MegaJoules_scale" ~ y_arrow_end-0.1,
                                lhs == "IceOutDayofYear_fed_scale" & rhs == "LengthSpringMixedPeriod_days_scale" ~ y_arrow_end,
                                lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "LengthFallMixedPeriod_days_scale" ~ NA_real_, #turns off this arrow
                                  TRUE~y_arrow_end
                                  
          ),
          midpoint.x = (x_arrow_start + x_arrow_end)/2,
          midpoint.y = (y_arrow_start + y_arrow_end)/2,
          midpoint.x = case_when(lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.x + 0.02,
                                 lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ midpoint.x + 0.02,
                                 lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanHeatContent_MegaJoules_scale" ~ midpoint.x + 0.02,
                                 lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ midpoint.x,
                                 lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ midpoint.x + 0.02,
                                 lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ midpoint.x - 0.06,
                                 lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.x - 0.06,
                                 lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.x - 0.025,
                                 lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ midpoint.x - 0.025,
                                 TRUE~midpoint.x),
          midpoint.y = case_when(lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.y,
                                 lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ midpoint.y - 0.04,
                                 lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanHeatContent_MegaJoules_scale" ~ midpoint.y + 0.04,
                                 lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ midpoint.y - 0.065,
                                 lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ midpoint.y,
                                 lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ midpoint.y - 0.06,
                                 lhs == "IceOutDayofYear_fed_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.y + 0.06,
                                 lhs == "MeanHeatContent_MegaJoules_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.y + 0.07,
                                 lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ midpoint.y - 0.07,
                                 TRUE~midpoint.y)
        )
      
      #Plot locations of the labels####
      text_labels<-tibble(x_text=c(0.15,0.5,0.85),y_text=rep(0.92,3),text=c("Fall Mixed","Under Ice","Spring Mixed"))
      #Rectangle locations
      rect_size<-tibble(xmin=c(0),xmax=c(1),ymin=c(0.88),ymax=c(1))
      rect_ice<-tibble(xmin=c(0.25),xmax=c(0.75),ymin=c(0.98),ymax=c(1)) #represents the ice
      rect_fall <- tibble(x = seq(from = 0, to = 0.25, by = 0.0001),
                    y = rep(0.94, 2501))
      rect_spring <- tibble(x = seq(from = 0.75, to = 1, by = 0.0001),
                          y = rep(0.94, 2501))
      
      #Plot using ggplot and some of ggnetwork calls####
      (gg.networkplot<-ggplot() +
         geom_edges(data=combined_edge_labels%>%filter(op=="~"),aes(x = x_arrow_start, y = y_arrow_start, xend = x_arrow_end, yend = y_arrow_end,color=edge_color,size=edge.factor),
                    arrow = arrow(length = unit(6, "pt"), type = "open",ends = "last"),lineend="round",linejoin="mitre") + #edges for the regressions
         geom_edges(data=combined_edge_labels%>%filter(op=="~~"),aes(x = x_arrow_start, y = y_arrow_start, xend = x_arrow_end, yend = y_arrow_end,color=edge_color,size=edge.factor),
                    arrow = arrow(length = unit(6, "pt"), type = "open",ends = "both"),lineend="round",linejoin="mitre",curvature=0) + #edges for the covariances
         geom_nodes(data=combined_edge_labels,aes(x = x, y = y,shape="observed"), color = "black",fill="white",size = 22,shape=22) +
         geom_nodetext(data=combined_edge_labels,aes(x = x, y = y,label = node.labels),parse=TRUE,fontface = "bold",size=4) + 
         geom_label(data=combined_edge_labels%>%filter(op%in%c("~","~~")&lhs!=rhs),aes(x = midpoint.x, y = midpoint.y, label = as.character(format(est,nsmall=2))), color="black",label.size = NA,hjust = .5,vjust=.5,size=3.7,label.padding=unit(0.1,"lines")) +
         geom_rect(data=rect_size, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),  fill=rgb(100, 149, 237,max=255), color="black",size=0.5) +
         geom_rect(data=rect_ice, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),  fill="white", color="black",size=0.2) +
         geom_tile(data=rect_fall,aes(x=x,y=y,fill=x),height=0.12)+ #Rectangles in the fall to do a gradient from red to blue
         scale_fill_gradient(low = 'red', high = rgb(100, 149, 237,max=255)) +
         new_scale_fill() +
         geom_tile(data=rect_spring,aes(x=x,y=y,fill=x),height=0.12)+ #Rectangles in the fall to do a gradient from red to blue
         scale_fill_gradient(low = rgb(100, 149, 237,max=255), high = 'red') +
         geom_text(data=text_labels,aes(x=x_text,y=y_text,label=text),color="white")+ #labels at the top
         scale_y_continuous(expand = c(.07,0.07),limits=c(0,1)) +
         scale_x_continuous(expand = c(.04,0.04),limits=c(0,1)) +
         scale_shape_manual(values = c(15,19),guide=F) +
         # scale_color_manual(values=c(rgb(75,174,76,maxColorValue = 255),rgb(203,84,80,maxColorValue = 255)))+
         scale_color_manual(values=c("grey",rgb(100, 149, 237,max=255),"red"))+
         scale_size_discrete(range=c(0.15,1.5))+
         #theme(legend.position="none") #comment out
         theme_blank()+
         theme(legend.position = "none",plot.margin = unit(c(0.1,0.1,0.1,0.1),"pt"),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               axis.title.y=element_blank(),
         )
      )
      



###Section for partial residual plots#################################      
      #ice in vs. Length of fall mix (y vs. x) - this is a covariance  DONE!!
      #Hypo temp vs. Ice in (y vs. x) - regression residual DONE!!
      #Delta density vs. hypo temp (y vs. x) - regression residual
      #Length of spring mix vs. ice off (y vs. x) - this is a covariance DONE!!
      #Partial residual plots for variables of interest####
      #*https://en.wikipedia.org/wiki/Partial_residual_plot
      #*Calculate the resiudals for the variable using the equation
      #*Calculate the partial residuals by doing the model residuals + Beta1*x and plot vs. x for a variable of interest
      #*use Beta*x for the CCPR - component and compenent plus residual showing where the fitted line would lie
      #***Plot the partial residuals for the top variable for Length of mixing####
      #****Create labels and breaks in the right spots to back transform####

      #MS partial figure: Ice in with length of fall mix####
      #For covariance
      labels_lengthfallmixed<-c(50,75,100)
      breaks_lengthfallmixed<-(labels_lengthfallmixed-mean(AnnualUnderIceSummary_SEM$LengthFallMixedPeriod_days,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$LengthFallMixedPeriod_days,na.rm=TRUE)
      
      labels_IceInDayofYear_fed<-c(76,93,107,124)
      breaks_IceInDayofYear_fed<-(labels_IceInDayofYear_fed-mean(AnnualUnderIceSummary_SEM$IceInDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceInDayofYear_fed,na.rm=TRUE)
      limits_IceInDayofYear_fed_scale<-(c(70,124)-mean(AnnualUnderIceSummary_SEM$IceInDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceInDayofYear_fed,na.rm=TRUE)
      
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthFallMixedPeriod_days,y=IceInDayofYear_fed))+
        geom_point()
      
      (gg.covariance.iceInvsFallMixed<-ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthFallMixedPeriod_days_scale,y=IceInDayofYear_fed_scale))+
                                      geom_point(shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
                                      #geom_line(aes(y=LengthFallMixedPeriod_days_scale*0.836))+
                                      ylab("Ice-on date")+
                                      xlab("Fall mixed period (d)")+
                                      theme_bw()+
                                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))+
                                      #Can modify to actual day here... but is better to get more evenly spaced numbers and then figure out the z-score of those numbers for the breaks
                                      scale_x_continuous(breaks=breaks_lengthfallmixed,labels=labels_lengthfallmixed)+
                                      scale_y_continuous(breaks=breaks_IceInDayofYear_fed,labels=c("15-Dec","01-Jan","15-Jan","01-Feb"),limits=limits_IceInDayofYear_fed_scale) #,limits=c(70,139)
      )                             
      
      
      #MS partial figure: Ice out with length of spring mix####
      #For covariance
      labels_lengthspringmixed<-c(10,20,30,40,50)
      breaks_lengthspringmixed<-(labels_lengthspringmixed-mean(AnnualUnderIceSummary_SEM$LengthSpringMixedPeriod_days,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$LengthSpringMixedPeriod_days,na.rm=TRUE)
      
      labels_IceOutDayofYear_fed<-c(166,183,197)
      breaks_IceOutDayofYear_fed<-(labels_IceOutDayofYear_fed-mean(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE)
      limits_IceOutDayofYear_fed<-(c(160,200)-mean(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE)
      
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceOutDayofYear_fed,y=LengthSpringMixedPeriod_days))+
        geom_point()
      
      (gg.covariance.iceOutvsSpringMixed<-ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceOutDayofYear_fed_scale,y=LengthSpringMixedPeriod_days_scale))+
          geom_point(shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
          #geom_line(aes(y=LengthFallMixedPeriod_days_scale*0.836))+
          xlab("Ice-off date")+
          ylab("Spring mixed period (d)")+
          theme_bw()+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))+
          #Can modify to actual day here... but is better to get more evenly spaced numbers and then figure out the z-score of those numbers for the breaks
          scale_y_continuous(breaks=breaks_lengthspringmixed,labels=labels_lengthspringmixed)+
          scale_x_continuous(breaks=breaks_IceOutDayofYear_fed,labels=c("15-Mar","01-Apr","15-Apr"),limits=limits_IceOutDayofYear_fed) #,limits=c(70,139)
      )                       
      
      
      #MS partial figure: Hypo temp vs. Ice in (y vs. x) - regression residual####
      #For regression
      
      labels_IceInDayofYear_fed2<-c(76,107)
      breaks_IceInDayofYear_fed2<-(labels_IceInDayofYear_fed2-mean(AnnualUnderIceSummary_SEM$IceInDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceInDayofYear_fed,na.rm=TRUE)
      limits_IceInDayofYear_fed_scale2<-(c(70,124)-mean(AnnualUnderIceSummary_SEM$IceInDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceInDayofYear_fed,na.rm=TRUE)
      
      labels_MeanUnderIce_HypoTemp_degC<-c(2.5,3.0,3.5,4.0,4.5)
      breaks_MeanUnderIce_HypoTemp_degC<-(labels_MeanUnderIce_EpiTemp_degC-mean(AnnualUnderIceSummary_SEM$MeanUnderIce_EpiTemp_degC,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$MeanUnderIce_EpiTemp_degC,na.rm=TRUE)
      labels2_MeanUnderIce_HypoTemp_degC<-c("2.5","3.0","3.5","4.0","4.5")
      
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceInDayofYear_fed_scale,y=MeanUnderIce_HypoTemp_degC_scale))+
        geom_point()
      
      (gg.partialResid.HypoTempvsIceIn<-AnnualUnderIceSummary_SEM%>%mutate(MeanUnderIce_HypoTemp_degC_scale_Resids=MeanUnderIce_HypoTemp_degC_scale-(-0.426*IceInDayofYear_fed_scale+0.103))%>%
          ggplot(.,aes(y=(MeanUnderIce_HypoTemp_degC_scale_Resids-0.426*IceInDayofYear_fed_scale),x=IceInDayofYear_fed_scale))+
          geom_point(shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
          geom_line(aes(y=(IceInDayofYear_fed_scale*-0.426)+0.103))+
          xlab("Ice-on date")+
          ylab(bquote(Under~ice~deep~(degree*C)))+
          theme_bw()+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))+
          #Can modify to actual day here... but is better to get more evenly spaced numbers and then figure out the z-score of those numbers for the breaks
          scale_y_continuous(breaks=breaks_MeanUnderIce_HypoTemp_degC,labels=labels2_MeanUnderIce_HypoTemp_degC)+
          scale_x_continuous(breaks=breaks_IceInDayofYear_fed2,labels=c("15-Dec","15-Jan"),limits=limits_IceInDayofYear_fed_scale2)
      )
      
      #MS partial figure: Delta density vs. hypo temp (y vs. x) - regression residual####
      #For regression
      
      labels_MeanDelta1_11mWaterDensity_kgperm3<-c(-0.02,-0.01,0,0.01)
      breaks_MeanDelta1_11mWaterDensity_kgperm3<-(labels_MeanDelta1_11mWaterDensity_kgperm3-mean(AnnualUnderIceSummary_SEM$MeanDelta1_11mWaterDensity_kgperm3,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$MeanDelta1_11mWaterDensity_kgperm3,na.rm=TRUE)
      labels2_MeanDelta1_11mWaterDensity_kgperm3<-c("-0.02","-0.01","0.00","0.01")
      
      labels_MeanUnderIce_HypoTemp_degC<-c(2.5,3.0,3.5,4.0,4.5)
      breaks_MeanUnderIce_HypoTemp_degC<-(labels_MeanUnderIce_EpiTemp_degC-mean(AnnualUnderIceSummary_SEM$MeanUnderIce_EpiTemp_degC,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$MeanUnderIce_EpiTemp_degC,na.rm=TRUE)
      labels2_MeanUnderIce_HypoTemp_degC<-c("2.5","3.0","3.5","4.0","4.5")
      
      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=MeanUnderIce_HypoTemp_degC,y=MeanDelta1_11mWaterDensity_kgperm3_scale))+
        geom_point()
      
      (gg.partialResid.DeltaDensityvsHypoTemp<-AnnualUnderIceSummary_SEM%>%mutate(MeanDelta1_11mWaterDensity_kgperm3_scale_Resids=MeanDelta1_11mWaterDensity_kgperm3_scale-(-0.874*MeanUnderIce_HypoTemp_degC_scale+1.267*MeanUnderIce_EpiTemp_degC_scale+0.005))%>%
          ggplot(.,aes(y=(MeanDelta1_11mWaterDensity_kgperm3_scale_Resids-0.874*MeanUnderIce_HypoTemp_degC_scale),x=MeanUnderIce_HypoTemp_degC_scale))+
          geom_point(shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
          geom_line(aes(y=(MeanUnderIce_HypoTemp_degC_scale*-0.874)+0.005))+
          xlab(bquote(Under~ice~deep~(degree*C)))+
          ylab(bquote(Density~Delta~(kg~m^-3)))+
          theme_bw()+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))+
          #Can modify to actual day here... but is better to get more evenly spaced numbers and then figure out the z-score of those numbers for the breaks
          scale_x_continuous(breaks=breaks_MeanUnderIce_HypoTemp_degC,labels=labels2_MeanUnderIce_HypoTemp_degC,limits=c(-2.0,1.5))+
          scale_y_continuous(breaks=breaks_MeanDelta1_11mWaterDensity_kgperm3,labels=labels2_MeanDelta1_11mWaterDensity_kgperm3,limits=c(-2.0,1.5))
      )
      
      
      #xlab(bquote(Under~ice~epi.~temp.~(degree*C)))+
        #ylab(bquote(Density~Delta~(kg~m^-3)))+
        
      # labels_lengthspringmixed<-c(10,20,30,40,50)
      # breaks_lengthspringmixed<-(labels_lengthspringmixed-mean(AnnualUnderIceSummary_SEM$LengthSpringMixedPeriod_days,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$LengthSpringMixedPeriod_days,na.rm=TRUE)
      # 
      # labels_IceOutDayofYear_fed<-c(166,183,197)
      # breaks_IceOutDayofYear_fed<-(labels_IceOutDayofYear_fed-mean(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE)
      # limits_IceOutDayofYear_fed<-(c(160,200)-mean(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE)
      # 
      # (gg.partialResid.XXXX<-AnnualUnderIceSummary_SEM%>%mutate(LengthSpringMixedPeriod_days_scale_Resids=LengthSpringMixedPeriod_days_scale-(-0.426*IceOutDayofYear_fed_scale-0.103))%>%
      #     ggplot(.,aes(y=(LengthSpringMixedPeriod_days_scale_Resids-0.426*IceOutDayofYear_fed_scale),x=IceOutDayofYear_fed_scale))+
      #     geom_point()+
      #     geom_line(aes(y=(IceOutDayofYear_fed_scale*-0.426)-0.103))+
      #     xlab("Ice-out date")+
      #     ylab("Spr. mixed period (d)")+
      #     theme_bw()+
      #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))+
      #     #Can modify to actual day here... but is better to get more evenly spaced numbers and then figure out the z-score of those numbers for the breaks
      #     scale_y_continuous(breaks=breaks_lengthspringmixed,labels=labels_lengthspringmixed)+
      #     scale_x_continuous(breaks=breaks_IceOutDayofYear_fed,labels=c("15-Mar","01-Apr","15-Apr"),limits=limits_IceOutDayofYear_fed)
      # )
      # 
      # 
      #Alternative arrangement
      #****If we go with this one, modify arrow locations, increase size of the nodes****####
      (Fig5_update<-plot_grid(gg.networkplot,
                              plot_grid(gg.covariance.iceInvsFallMixed,gg.partialResid.HypoTempvsIceIn,gg.partialResid.DeltaDensityvsHypoTemp,gg.covariance.iceOutvsSpringMixed,
                                        ncol=4,nrow=1,labels=c("b","c","d","e"),label_size=11,label_x=c(0.25,0.28,0.28,0.24),hjust=0,label_y=0.98)
                              ,ncol=1,nrow=2,labels=c("a",""),label_size=11,label_x=c(0.05),hjust=0,label_y=0.98,rel_heights = c(0.9,0.5)))
      
      ggsave("figures/MS/Fig5.SEMplot5panelsPartialResids_update.jpg", plot=Fig5_update, width=7.4, height=5.6,units="in", dpi=300)      
      
