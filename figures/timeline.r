

## working from: https://stats.andrewheiss.com/misc/gantt.html
library(tidyverse)
library(lubridate)
library(scales)

tasks <- tribble(
  ~Start,       ~End,         ~Project,            ~Task,
  "2018-04-01", "2019-06-01", "1) 2D UCS",         "1) 2D UCS",
  "2018-04-01", "2019-02-01", "1) 2D UCS",         "1) application",
  "2018-12-01", "2019-06-01", "1) 2D UCS",         "1) paper (The R journal)",
  "2019-11-01", "2019-12-01", "1) 2D UCS",         "1) paper edits",
  "2019-01-01", "2019-03-01", "Milestones",        "Candidature confirmation",
  
  "2019-03-01", "2020-04-01", "2) 2D UCS vs alts", "2) 2D UCS vs alternatives",
  "2019-03-01", "2020-02-01", "2) 2D UCS vs alts", "2) app development",
  "2020-01-01", "2020-04-01", "2) 2D UCS vs alts", "2) study&paper (VAST 2020)", # submission: Saturday, March 21, 2020
  "2020-02-01", "2020-03-01", "Milestones",        "Mid candidature review",
  
  "2020-01-01", "2020-10-01", "3) 3D UCS",         "3) 3D UCS",
  "2020-01-01", "2020-04-01", "3) 3D UCS",         "3) theory",
  "2020-03-01", "2020-09-01", "3) 3D UCS",         "3) application",
  "2020-08-01", "2020-10-01", "3) 3D UCS",         "3) paper (VAST 2021?)", # submission: Thursday Sep. 10,
  
  "2020-08-01", "2021-05-01", "4) 3D UCS vs alts", "4) 3D UCS vs alternatives",
  "2020-08-01", "2020-11-01", "4) 3D UCS vs alts", "4) app development",
  "2020-10-01", "2021-04-01", "4) 3D UCS vs alts", "4) study & paper",
  "2021-03-01", "2021-05-01", "4) 3D UCS vs alts", "4) paper ", # submission: Thursday Sep. 10,
  
  "2021-02-01", "2021-03-01", "Milestones",        "pre-submission seminar",
  "2021-01-01", "2021-08-01", "Milestones",        "thesis composition"
)
### Conferences:
# IEEE VIS - VAST 2020: 25-30 October 2020 Salt Lake City, Utah, USA
# submission: Saturday, March 21, 2020 (+ 10 days for full) VAST - Empirical Study
# http://ieeevis.org/year/2020/info/call-participation/vast-paper-types
#
# CHI 2021: May 8-13, 2021 Yokohama, Japan
# submission: Thursday Sep. 10, 2020 (+ 1wk for full)
# https://chi2021.acm.org/


# Convert data to long for ggplot
tasks.long <- tasks %>%
  mutate(Start = ymd(Start),
         End = ymd(End)) %>%
  gather(date.type, task.date, -c(Project, Task)) %>%
  #arrange(date.type, task.date) %>%
  mutate(Task = factor(Task, levels=rev(unique(Task)), ordered=T))
# Custom theme for making a clean Gantt chart
theme_gantt <- function(base_size=11) {
  ret <- theme_bw(base_size) %+replace%
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size = 0.5, colour = "grey80"),
          axis.ticks=element_blank(),
          legend.position="bottom",
          axis.title=element_text(size=rel(0.8)),
          strip.text=element_text(size=rel(1)),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y=unit(1.5, "lines"),
          legend.key = element_blank())

  ret
}

# Calculate where to put the dotted lines that show up every three entries
x.breaks <- seq(length(tasks$Task) + 0.5 - 3, 0, by=-3)

# Build plot
timeline <- ggplot(tasks.long, aes(x=Task, y=task.date, colour=Project)) +
  geom_line(size=6) +
  geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") +
  guides(colour=guide_legend(title=NULL)) +
  labs(x=NULL, y=NULL) + coord_flip() +
  scale_y_date(date_breaks="2 months", labels=date_format("%b ‘%y")) +
  theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_color_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12))
timeline
#ggsave("./figures/phd_timeline.PNG", timeline, width = 9, height = 5)
#ggsave("./_slides/slide_figures/phd_timeline.PNG", timeline, width = 9, height = 5)

# ## Program requirements
# <!-- http://www.monash.edu/pubs/2018handbooks/aos/information-technology-phd-program/ -->
#   
#   - WES Academic record
# - FIT5144: 2019 S1+2, **In progress**, extended to pre-submission seminar with unit cordinator for the ussual 2 oppertunities to complete.
# - Hours: 147>120 hours __Tracked__, followng required (12 hr total)
# - _Needed:_ CYR 2 (A & B) -- 2x 3hr 
# - _Needed:_ Faculty of IT Workshop 1 and 3 on Ethical Research and Publishing -- 2x 3hr
# - FIT5113: 2018 S2, **Exemption**
#   <!-- (submitted:08/2018, recorded 4/2019) -->
#   - FIT6021: 2018 S2, **Completed** with distinction
# - myDevelopment - IT: Monash Doctoral Program - Compulsory Module
# - Monash graduate research student induction: **Completed** 
#   <!-- last assesed: 20/02/2018 -->
#   - Research Integrity - Choose the Option most relevant: **Completed** 
#   <!-- Last Accessed:	20/02/2018 (2 of 4 required & completed) -->
#   - Faculty Induction: **Completed** 
#   <!-- marked completed, online class "08/05/2019 @ 09:45 for 3hr 15min" -->
#   <!-- previously: **Content unavailable** (01/04/2019: "Currently being updated and will be visible in this section soon") -->

