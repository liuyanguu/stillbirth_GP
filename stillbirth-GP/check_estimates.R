
###################################################################
###################################################################

# the information of the points in the plot
rawdat <- data.frame(y_i = exp(jags.data$y.i), 
                     getc.i = jags.data$getc.i,
                     year = SBR_input$year, 
                     Source = factor(SBR_input$n_context5, 
                                     labels = c("High National Subroutine", 
                                                "Low National Subroutine", 
                                                "National Survey", 
                                                "Sub-national", 
                                                "Other") ))

# save the plots in a pdf
pdf_name <- paste0("fig/graphsCT_ar1&basic.pdf")
pdf(pdf_name, width = 8, height = 5)

### you should input a dataset_list inlcuding all your datasets and get different CIs
dataset_list <- list(mod.ar1MVN)
estimates_name <- c('mu.ct', 'adyhat.ct')
GetCIs(dataset_list, estimates_name, lower = 0.025, upper = 0.975) %>% lapply(CheckEsts)

# pdf off
dev.off()
