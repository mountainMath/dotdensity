## ---- message=FALSE, warning=FALSE---------------------------------------
#devtools::install_github("mountainmath/cancensus")
library(cancensus)
library(dotdensity)
library(tidyverse)
library(sf)
# options(cancensus.api_key='your_api_key')
regions=list(CMA="59933")
vectors_2011=c("v_CA11N_265","v_CA11N_268","v_CA11N_304","v_CA11N_334","v_CA11N_373","v_CA11N_376","v_CA11N_379","v_CA11N_382")
vectors=c("v_CA16_3636","v_CA16_3639","v_CA16_3669","v_CA16_3699","v_CA16_3741","v_CA16_3810","v_CA16_3750","v_CA16_3756","v_CA16_3783")

## ------------------------------------------------------------------------
categories=c("Americas","Europe","Africa + Oceania","Philippines","China","India","Other Asian Countries")
colors=c("#7a0177", "#3333cc", "#ff00ff", "#00ffff", "#ff1a1c", "#4dff4a", "#ffff33")

prep_data <- function(geo){
  data <- geo %>% replace(is.na(.), 0)
  data <- rename(data,
    total=v_CA16_3636,
    Americas=v_CA16_3639,
    Europe=v_CA16_3669,
    Philippines=v_CA16_3783,
    China=v_CA16_3750,
    India=v_CA16_3756)
  data %>% mutate(`Other Asian Countries` = v_CA16_3741-Philippines-China-India,
                     `Africa + Oceania`=v_CA16_3699 + v_CA16_3810)
}
prep_data_2011 <- function(geo){
  data <- geo %>% replace(is.na(.), 0)
  data <- rename(data,
    total=v_CA11N_265,
    Americas=v_CA11N_268,
    Europe=v_CA11N_304,
    Africa=v_CA11N_334,
    Philippines=v_CA11N_376,
    China=v_CA11N_379,
    India=v_CA11N_382)
  data %>% mutate(`Other Asian Countries` = v_CA11N_373-Philippines-China-India)
}

## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
data_csd=get_census(dataset = 'CA16', regions=regions,vectors=vectors,geo_format='sf',labels='short',level='CSD') %>% prep_data
data_ct=get_census(dataset = 'CA16', regions=regions,vectors=vectors,geo_format='sf',labels='short',level='CT') %>% prep_data
data_da=get_census(dataset = 'CA16', regions=regions,vectors=vectors,geo_format='sf',labels='short',level='DA') %>% prep_data
data_db=get_census(dataset = 'CA16', regions=regions,geo_format='sf',labels='short',level='DB')

## ------------------------------------------------------------------------
# ct level data does not always align with CSD, so use care when doing this
data_ct <- proportional_re_aggregate(data=data_ct,parent_data=data_csd,geo_match=setNames("GeoUID","CSD_UID"),categories=categories,base="Population")
data_da <- proportional_re_aggregate(data=data_da,parent_data=data_ct,geo_match=setNames("GeoUID","CT_UID"),categories=categories,base="Population")
data_db <- proportional_re_aggregate(data=data_db,parent_data=data_da,geo_match=setNames("GeoUID","DA_UID"),categories=categories,base="Population")

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(ggplot2)
bg_color="#111111"
base_color="#333333"
text_color="#eeeeee"
theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_rect(fill = bg_color, colour = NA),
                       plot.background = element_rect(fill=bg_color, size=1,linetype="solid",color=text_color),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_text(size=70,hjust = 0.5, color=text_color),
                       plot.subtitle = element_text(size=50,hjust = 0.5, color=text_color),
                       plot.caption = element_text(size=25, color=text_color),
                       legend.title = element_text(size=35, color=text_color),
                       legend.text = element_text(size=35, color=text_color),
                       legend.background = element_rect(fill=bg_color, size=1,linetype="solid",color=bg_color),
                       legend.key = element_rect(fill = bg_color,color = bg_color),
                       legend.key.width = unit(3, 'lines'),
                       legend.position = "bottom"),
                 coord_sf(xlim=c(-123.29,-122.6), ylim=c(49.02,49.35))  # zoom in a bit
)



## ------------------------------------------------------------------------
scale=5 # 1 dot = 5 immigrants

basemap <-   ggplot(data_csd) +
  geom_sf(fill = base_color, size=0.1, color = 'grey') +
  guides(colour = guide_legend(nrow=1,override.aes = list(size=15))) +
  theme_opts +
  scale_colour_manual(values = colors) +
  labs(color = "", title="Immigrants 2011 - 2016",
       caption="Source: StatCan Census 2016 via cancensus & CensusMapper.ca",
       subtitle = paste0("1 dot = ",scale," people"))

## ---- fig.height=14, fig.width=14, message=FALSE, warning=FALSE----------
dots <- compute_dots(geo_data = data_csd, categories = categories, scale=scale) %>% st_as_sf
basemap +
  geom_sf(data=dots,aes(color=Category),alpha=0.75,size=0.25,show.legend = "point") +
  theme_opts

ggsave('../images/recent_immigrants_CSD.png',width=26,height=26)

## ---- fig.height=14, fig.width=14, message=FALSE, warning=FALSE----------
dots <- compute_dots(geo_data = data_ct, categories = categories, scale=scale) %>% st_as_sf
basemap +
  geom_sf(data=dots,aes(color=Category),alpha=0.75,size=0.25,show.legend = "point") +
  theme_opts

ggsave('../images/recent_immigrants_CT.png',width=26,height=26)

## ---- fig.height=14, fig.width=14, message=FALSE, warning=FALSE----------
dots <- compute_dots(geo_data = data_da, categories = categories, scale=scale) %>% st_as_sf
basemap +
  geom_sf(data=dots,aes(color=Category),alpha=0.75,size=0.25,show.legend = "point") +
  theme_opts

ggsave('../images/recent_immigrants_DA.png',width=26,height=26)

## ---- fig.height=14, fig.width=14, message=FALSE, warning=FALSE----------
dots <- compute_dots(geo_data = data_db, categories = categories, scale=scale) %>% st_as_sf
basemap +
  geom_sf(data=dots,aes(color=Category),alpha=0.75,size=0.25,show.legend = "point") +
  theme_opts

ggsave('../images/recent_immigrants_DB.png',width=26,height=26)

