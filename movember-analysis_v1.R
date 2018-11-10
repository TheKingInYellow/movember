# filename  : movember_analysis_v1.r
# author    : kody crowell (@hummushero)
# date      : 30 oct 2018

# load libs
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ggrepel)
library(ggmap)
library(ggthemes)
library(ggalt)
library(grid)
library(maps)
library(rworldmap)
library(viridis)
library(RColorBrewer)

'%!in%' <- function(x,y)!('%in%'(x,y));

# excellent source: https://ourworldindata.org/cancer
# excellent source 2: https://ourworldindata.org/mental-health

## theme for mir
mir.red <- "#cf0808" #"#f00d0d"
mir.white <- "#f9f9f9"
mir.gray <- "#4b4b4b"
mir.lgray <- "#eeeeee"
red.ramp <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d')

theme_mir <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Georgia", color = mir.gray),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle=45, hjust=1),
      axis.text.y = element_text(size=rel(1)),
      axis.title.y = element_text(size=rel(1), vjust=2),
      axis.title.x = element_text(size=rel(1), vjust=-0.5),
      panel.grid.major = element_line(color = mir.lgray, size = 0.2),
      panel.grid.minor = element_line(color = mir.lgray, size = 0.2),
      plot.background = element_rect(fill = mir.white, color = NA), 
      panel.background = element_rect(fill = mir.white, color = NA), 
      legend.background = element_rect(fill = mir.white, color = NA),
      panel.border = element_blank(),
      ...
    )
}

# load data
df <- read.csv("IHME-GBD_2016_DATA-97bb3c0d-1.csv", header=T)
df.ts.age <- read.csv("IHME-GBD_2016_DATA-0c9e7a4e-1.csv", header=T)
world <- map_data("world")

# match location names in each datasets
regions <- unique(world$region)
locations <- unique(df$location_name)
regions[which(regions %!in% locations)]
locations[which(locations %!in% regions)]

world$region <- recode(as.character(world$region), 
                       "USA" = "United States",
                       "Russia" = "Russian Federation",
                       "Antigua" = "Antigua and Barbuda",
                       "Barbuda" = "Antigua and Barbuda",
                       "Gambia" = "The Gambia",
                       "Micronesia" = "Federated States of Micronesia",
                       "Trinidad" = "Trinidad and Tobago",
                       "Tobago" = "Trinidad and Tobago",
                       "UK" = "United Kingdom",
                       "Bahamas" = "The Bahamas",
                       "Ivory Coast" = "Cote d'Ivoire",
                       "Republic of Congo" = "Congo",
                       "Virgin Islands" = "Virgin Islands, U.S.",
                       "Grenadines" = "Saint Vincent and the Grenadines",
                       "Saint Vincent" = "Saint Vincent and the Grenadines")

world <- world %>%
  select(-subregion) %>%
  distinct(.)

table(df$measure_id, df$measure_name)
table(df$sex_id, df$sex_name)
table(df$cause_id, df$cause_name)
table(df$metric_id, df$metric_name)
# note: drug use is use of cannabis, opioids, or amphetamines, or use of injecting drugs.
# rate per 100k popn: this estimate shows in a single country-year-age-sex, the deaths due to cause X divided by the population.

# restructure data for plotting
cancer <- df %>%
  filter(cause_name %in% c("Testicular cancer", "Prostate cancer"),
         metric_name == "Rate",
         sex_name == "Male") %>%
  select(-c(age_id, age_name, metric_id, metric_name, year, sex_name,
            measure_id, location_id, sex_id, cause_id)) %>%
  rename("location" = "location_name",
         "measure" = "measure_name",
         "cause" = "cause_name")

cancer.ts <- df.ts.age %>%
  filter(measure == "Prevalence", sex == "Male", metric == "Rate",
         age %!in% c("Under 5", "5-14 years"),
         cause %in% c("Prostate cancer", "Testicular cancer",
                      "Colon and rectum cancer", 
                      "Thyroid cancer", "Liver cancer", "Larynx cancer")) %>%
  # cause %!in% c("Drug use disorders", "Depressive disorders",
  #               "Malignant skin melanoma", "Non-melanoma skin cancer",
  #                "Tracheal, bronchus, and lung cancer",
  #                "Pancreatic cancer", "Larynx cancer", 
  #               "Gallbladder and biliary tract cancer")) %>%
  select(-measure, -sex, -metric, -location)

################################################################################
# plot testicular and prostate cancer rates over time for men
p <- ggplot(data=cancer.ts %>% filter(age == "15-49 years"), 
            aes(x=year, y=val, color=cause)) +
  geom_line(size=1) +
  geom_ribbon(aes(x=year, ymax=upper, ymin=lower, group=cause), 
              fill=mir.red, inherit.aes=F, alpha=0.1) +
  geom_text(data=cancer.ts %>% filter(year==last(year) & age=="15-49 years"), 
            aes(label=cause, x=year+0.5, y=val, family="Georgia"), hjust=-0.1)+
            # vjust="inward", hjust="inward") +
  scale_x_continuous(limits=c(min(cancer.ts$year), max(cancer.ts$year)+4)) +
  coord_fixed(0.5) +
  labs(x="Year", y="Rate per 100k population",
       title="Increasing prevalence of cancer among men",
       subtitle="Global prevalence rates of various cancers among men aged 15-49, 1990-2016",
       caption="Author: Kody Crowell (@hummushero); Source: IHME (2016)") +
  scale_color_manual(values=rev(brewer.pal(9,"Reds"))) +
  guides(color=F) +
  theme_mir() +
  theme(strip.text.x = element_text(size=rel(1)),
        strip.text.y = element_text(size=rel(1)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0))

p # save as 1330 x 550

# national prev/death for test/prostate
# prevalence coloured for each nation
world.c <- world %>%
  inner_join(cancer %>%
               filter(cause=="Testicular cancer" & measure=="Deaths"),
             by=c("region"="location"))

# highest test cancer rates
world.c %>% 
filter(val > .8) %>% 
  select(-long, -lat, -group, -order) %>% 
  distinct()

labels <- structure(list(region = c("Chile: 102.6", 
                                    "Germany: 102.0", "Luxembourg: 92.9", 
                                    "Netherlands: 87.7", "Norway: 101.9",
                                  "Serbia: 89.7", "Slovakia: 103.9", 
                                  "Slovenia: 92.1", "United States: 81.8",
                                  "Canada: 57.1", "South Africa: 4.8",
                                  "Saudi Arabia: 7.6", "India: 2.8",
                                  "Japan: 37.1", "Australia: 64.4",
                                  "Turkmenistan: 38.4", "Brunei: 57.9"), 
                         lon = c(-72.5430, 10.4515, 6.1296, 5.2913,
                                 8.4689, 21.0059, 19.6990, 14.9955, -95.7129,
                                 -116.3468, 22.9375, 45.0792, 78.9629,
                                 138.2529, 133.7751, 59.5563, 114.7277),
                         lat = c(-35.6751, 51.1657, 49.8153, 52.1326,
                                 60.4720, 44.0165, 48.6690, 46.1512, 33.0902,
                                 53.1304, -30.5595, 23.8859, 20.5937,
                                 36.2048, -25.2744, 38.9697, 4.5353)),
                    class = "data.frame", .Names = c("region","lon", "lat"), 
                    row.names = c(NA, -17L))

labels2 <- structure(list(region = c("Chile: 1.6", "Argentina: 1.47",
                                     "Bulgaria: 1.1", "Hungary: 1.0",
                                     "Kiribati: 2.28", "Mexico: 1.21",
                                     "Macedonia: 1.0", "Serbia: 1.36",
                                     "Uruguay: 0.99", "Fiji: 0.87", 
                                     "Pakistan: 0.95", "Russia: 0.57",
                                     "CAR: 0.69", "United States: 0.37"), 
                         lon = c(-72.5430, -63.6167, 25.4858, 19.5033,
                                 -168.7340, -102.5528, 21.7453, 21.0059,
                                 -55.7658, 178.0650, 69.3451, 105.3188, 
                                 20.9394, -95.7129),
                         lat = c(-35.6751, -38.4161, 42.7339, 47.1625,
                                 -3.3704, 23.6345, 41.6086, 44.0165,
                                 -32.5228, -17.7134, 30.3753, 61.5240,
                                 6.6111, 33.0902)),
                    class = "data.frame", .Names = c("region","lon", "lat"), 
                    row.names = c(NA, -14L))

pp <- ggplot(data=world.c) +
  geom_map(map=world.c, aes(map_id=region, fill=val)) +
  expand_limits(x=world.c$long, y=world.c$lat) + 
  scale_fill_gradient(low = mir.lgray, high = mir.red,
                      name = "Deaths per 100k",
                      guide = guide_colorbar(
                        direction = "horizontal", barheight = unit(2, units = "mm"),
                        barwidth = unit(80, units = "mm"), ticks=F,
                        title.position = 'top', title.hjust = 0.5, label.hjust = 0.5,
                        label.position = "bottom", nrow = 1, byrow = T, reverse = T
                      )) +
  labs(x="", y="",
       title="Testicular Cancer Deaths in the World",
       subtitle="National death rates of testicular cancer in men aged 15-49, 2016",
       caption="Author: Kody Crowell; Source: IHME GBD (2016)") +
  geom_label_repel(data = labels2, aes(x=lon, y=lat, label=region, family="Georgia"), size=3) +
  theme_mir() +
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.justification = c(0.5, 1),
        legend.background = element_blank(),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0)) +
  coord_fixed()

pp # save as 900x550

cancer.ts <- df.ts.age %>%
  filter(measure == "Prevalence", sex == "Male", metric == "Rate",
         age %!in% c("Under 5", "5-14 years"),
         cause %!in% c("Drug use disorders", "Depressive disorders")) %>%
         # cause %in% c("Prostate cancer", "Testicular cancer",
          #             "Colon and rectum cancer")) %>%
  # cause %!in% c("Drug use disorders", "Depressive disorders",
  #               "Malignant skin melanoma", "Non-melanoma skin cancer",
  #                "Tracheal, bronchus, and lung cancer",
  #                "Pancreatic cancer", "Larynx cancer", 
  #               "Gallbladder and biliary tract cancer")) %>%
  select(-measure, -sex, -metric, -location)
# bar plot for age groups in cancer
q <- ggplot(data=cancer.ts %>% 
              filter(age != "All Ages" & year==2016), 
       aes(x=cause, y=val, fill=age)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Cancer Type", y="Rate per 100k population",
       title="Prevalence rate of cancers among men by age",
       subtitle="Global prevalence rates of various cancers through different age groups, 2016",
       caption="Author: Kody Crowell (@hummushero); Source: IHME (2016)") +
  scale_fill_manual(values=rev(brewer.pal(9, "Reds")), name="Age group") +
  theme_mir() +
  theme(strip.text.x = element_text(size=rel(1)),
        strip.text.y = element_text(size=rel(1)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0))

q # save as 900x700

world.c2 <- world %>%
  inner_join(cancer %>%
               filter(measure=="Prevalence"),
             by=c("region"="location")) %>%
  group_by(cause) %>%
  mutate_at(c("val", "upper", "lower"), funs(scale(., scale=F) %>% as.vector)) %>%
  mutate(val = ifelse(cause != "Prostate cancer", -val, val))

qq <- ggplot(data=world.c2) +
  geom_map(map=world.c2, aes(map_id=region, fill=val)) +
  expand_limits(x=world.c2$long, y=world.c2$lat) + 
  scale_fill_gradient2(low="blue", mid=mir.lgray, high=mir.red,
                      name = "Prevalence score",
                      guide = guide_colorbar(
                        direction = "horizontal", barheight = unit(2, units = "mm"),
                        barwidth = unit(80, units = "mm"), ticks=F,
                        title.position = 'top', title.hjust = 0.5, label.hjust = 0.5,
                        label.position = "bottom", nrow = 1, byrow = T, reverse = T
                      )) +
  labs(x="", y="",
       title="Comparison of Men's Cancer around the World",
       subtitle="Scaled prevalence rates of testicular and prostate cancer in men aged 15-49, 2016",
       caption="Author: Kody Crowell; Source: IHME GBD (2016)") +
  # geom_label_repel(data = labels2, aes(x=lon, y=lat, label=region, family="Georgia"), size=3) +
  theme_mir() +
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.justification = c(0.5, 1),
        legend.background = element_blank(),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0)) +
  coord_fixed()

qq


# bar chart of types among men, women
# how death has changed over time among cancers
# dot chart men, women wrt mental illness
# prevalence in males vs prevalence in females by country coloured

# point range of estimates -- cleveland dot plot
ppp <- ggplot(cancer, aes(x=reorder(region, val), y=val, 
                         ymin=lower, ymax=upper))#,
# color=as.factor(viotype),
# label=round(vio_est, 2)))
ppp <- ppp + geom_hline(yintercept=seq(0,60,by=10), color="grey90")
#pp <- ppp +  geom_hline(yintercept=regional_means$mean_vio_est, colour=c(rep("#ED6925FF", 4), rep("#000004FF", 4)))
ppp <- ppp +  geom_pointrange(size=0.45)#, aes(color=as.factor(viotype))) 
# ppp <- ppp +  facet_wrap(~continent, ncol=1, scales="free_y") 
ppp <- ppp +  coord_flip(ylim=c(0, 60)) 
ppp <- ppp + geom_pointrange(size=0.45, data=cancer.ts, color="#ED6925FF") 
ppp <- ppp +  #scale_color_manual(values=inferno(3), name="Violence Type")+# ,type="qual", palette="Dark2", name="Violence Type", 
  # labels=c("General Violence", "Sexual Violence")) +
  labs(y="", x="", 
       title="",
       subtitle="",
       caption="")
ppp <- ppp + theme_minimal() +
  theme(axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = c(0.85, 1.2),
        legend.background = element_blank(),
        legend.direction="vertical",
        text = element_text(family = "Times", size=11),
        plot.title = element_text(size = 24, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 10, margin = margin(t = 10), color = "grey60", hjust = 0))

ppp
