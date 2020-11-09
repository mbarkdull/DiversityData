# Load packages:
library(plyr)
library(tidyverse)
library(scales)
library(ggpubr)
theme_set(theme_pubr())

# Read completion data files:
IPEDS2017 <- read_csv("./IPEDSData/CSV_1192020-936.csv")
IPEDS2016 <- read_csv("./IPEDSData/CSV_1192020-687.csv")
IPEDS2019 <- read_csv("./IPEDSData/CSV_1192020-519.csv")
IPEDS2018 <- read_csv("./IPEDSData/CSV_1192020-366.csv")
IPEDS2015 <- read_csv("./IPEDSData/CSV_1192020-194.csv")

# Process the completion data:
IPEDS2015 <- select(IPEDS2015, -c("unitid", "year", "C2015_A.First or Second Major", "C2015_A.CIP Code -  2010 Classification", "C2015_A.Award Level code", "CipTitle", "IDX_C"))
IPEDS2015$ProportionBlack <- IPEDS2015$"C2015_A_RV.Black or African American total" / IPEDS2015$"C2015_A_RV.Grand total"
IPEDS2015Long <- pivot_longer(IPEDS2015, cols = "C2015_A_RV.Grand total":"ProportionBlack")

IPEDS2016 <- select(IPEDS2016, -c("unitid", "year", "C2016_A.First or Second Major", "C2016_A.CIP Code -  2010 Classification", "C2016_A.Award Level code", "CipTitle", "IDX_C"))
IPEDS2016$ProportionBlack <- IPEDS2016$"C2016_A_RV.Black or African American total" / IPEDS2016$"C2016_A_RV.Grand total"
IPEDS2016Long <- pivot_longer(IPEDS2016, cols = "C2016_A_RV.Grand total":"ProportionBlack")

IPEDS2017 <- select(IPEDS2017, -c("unitid", "year", "C2017_A.First or Second Major", "C2017_A.CIP Code -  2010 Classification", "C2017_A.Award Level code", "CipTitle", "IDX_C"))
IPEDS2017$ProportionBlack <- IPEDS2017$"C2017_A_RV.Black or African American total" / IPEDS2017$"C2017_A_RV.Grand total"
IPEDS2017Long <- pivot_longer(IPEDS2017, cols = "C2017_A_RV.Grand total":"ProportionBlack")

IPEDS2018 <- select(IPEDS2018, -c("unitid", "year", "C2018_A.First or Second Major", "C2018_A.CIP Code -  2010 Classification", "C2018_A.Award Level code", "CipTitle", "IDX_C"))
IPEDS2018$ProportionBlack <- IPEDS2018$"C2018_A_RV.Black or African American total" / IPEDS2018$"C2018_A_RV.Grand total"
IPEDS2018Long <- pivot_longer(IPEDS2018, cols = "C2018_A_RV.Grand total":"ProportionBlack")

IPEDS2019 <- select(IPEDS2019, -c("unitid", "year", "C2019_A.First or Second Major", "C2019_A.CIP Code -  2010 Classification", "C2019_A.Award Level code", "CipTitle", "IDX_C"))
IPEDS2019$ProportionBlack <- IPEDS2019$"C2019_A.Black or African American total" / IPEDS2019$"C2019_A.Grand total"
IPEDS2019Long <- pivot_longer(IPEDS2019, cols = "C2019_A.Grand total":"ProportionBlack")

# Plot completion data:
BlackProportionPlotting <- function(data, plotname, title) {
  plotname <- ggplot(data = data) + 
    geom_col(mapping = aes(x = reorder(`institution name`, -ProportionBlack), y = ProportionBlack)) + 
    theme(axis.text.x = element_text(angle = 90), 
          axis.text = element_text(size = 2), 
          axis.title = element_text(size = 2), 
          axis.line = element_line(size = 0.1), 
          axis.ticks = element_line(size = 0.05), 
          plot.title = element_text(size = 5)) + 
    annotate("segment", 
             x = "Cornell University", 
             xend = "Cornell University", 
             y = 1, 
             yend = 0, 
             size = 0.1,  
             arrow = arrow()) + 
    labs(title = title, 
         x = "Institution", 
         y = "Proportion of PhD Awardees who are Black") +
    coord_flip()
  plot(plotname)
}

BPC2015 <- BlackProportionPlotting(IPEDS2015, "BPC2015", "Proportion of \nBlack PhD Recipients (2015)")
BPC2016 <- BlackProportionPlotting(IPEDS2016, "BPC2016", "Proportion of \nBlack PhD Recipients (2016)")
BPC2017 <- BlackProportionPlotting(IPEDS2017, "BPC2017", "Proportion of \nBlack PhD Recipients (2017)")
BPC2018 <- BlackProportionPlotting(IPEDS2018, "BPC2018", "Proportion of \nBlack PhD Recipients (2018)")
BPC2019 <- BlackProportionPlotting(IPEDS2019, "BPC2019", "Proportion of \nBlack PhD Recipients (2019)")

BPCCombined <- ggarrange(BPC2015, BPC2016, BPC2017, BPC2018, BPC2019, ncol = 2, nrow = 3)
plot(BPCCombined)
ggsave(filename = "BPCCombined.pdf", plot = last_plot(), device = "pdf")
