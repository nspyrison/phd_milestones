## Following:
if(F)
  browseURL("https://stats.andrewheiss.com/misc/gantt.html")

## Requirements -----
require("tidyverse")
require("lubridate")
require("scales")

## Local func, custom theme making a gantt chart
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


## Input details ----
timeline_tib <- tribble(
  ~Start,       ~End,         ~Project,      ~Task,
  ## Example of project 1:
  # "2018-04-01", "2019-06-01", "1) 2D UCS",  "1) 2D UCS",
  # "2018-04-01", "2019-02-01", "1) 2D UCS",  "1) application",
  # "2018-12-01", "2019-06-01", "1) 2D UCS",  "1) paper (The R journal)",
  # "2019-11-01", "2019-12-01", "1) 2D UCS",  "1) paper edits",
  # "2019-01-01", "2019-03-01", "Milestones", "Candidature confirmation",
  "2021-09-01", "2021-09-15", "Milestones",  "Presubmission document & presentation",
  "2021-09-05", "2021-09-18", "RO 3) Cheem", "3) Submit WHY-21 workshop paper",
  "2021-09-18", "2021-10-27", "Thesis",      "T) Primary resturcture and writing",
  "2021-10-25", "2021-10-31", "Thesis",      "T) Draft to supervisors",
  "2021-10-28", "2021-11-15", "Thesis",      "T) Edits and polish",
  "2021-11-10", "2021-11-20", "Thesis",      "T) Supervisor review",
  "2021-11-16", "2021-12-14", "Thesis",      "T) Final edits",
  "2021-12-13", "2021-12-15", "Milestones",  "Thesis submission"
)

## Convert tibble to long format
timeline_long <- timeline_tib %>%
  mutate(Start = ymd(Start),
         End = ymd(End)) %>%
  gather(date.type, task.date, -c(Project, Task)) %>%
  #arrange(date.type, task.date) %>%
  mutate(Task = factor(Task, levels = rev(unique(Task)), ordered = T))
## Initalize breaks for dotted lines
.x_breaks <- seq(length(timeline_tib$Task) + 0.5 - 3, 0, by = -3)

## Build plot -----
ggp_timeline <- ggplot(timeline_long, aes(x=Task, y=task.date, colour=Project)) +
  geom_line(size=6) +
  geom_vline(xintercept=.x_breaks, colour="grey80", linetype="dotted") +
  guides(colour=guide_legend(title=NULL)) +
  labs(x=NULL, y=NULL) + coord_flip() +
  scale_y_date(date_breaks="1 month", labels=date_format("%b ‘%Y")) +
  theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_color_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12))
ggp_timeline

## SAVE -----
if(F)
  ggsave("./figures/timeline_post_presubmission.png",
         ggp_timeline, device = "png", width = 9, height = 5, unit = "in")
