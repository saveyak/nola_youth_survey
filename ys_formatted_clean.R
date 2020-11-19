#NEW ORLEANS YOUTH SURVEY
#Coded by Sharon Lurye
#Purpose: visualize data for the policy report, "Voices of New Orleans Youth: What Do the City's Young People Think About Their Schools and Communities?" by Lindsay Bell Weixler, Douglas N. Harris, and Alica Gerry of the Education Research Alliance for New Orleans. Published June 8, 2020. 
#Read the report here: https://educationresearchalliancenola.org/publications/voices-of-new-orleans-youth-what-do-the-citys-young-people-think-about-their-schools-and-communities

setwd("C:/Users/slurye/Documents/data/youth_survey")

library(readxl)
library(tidyverse)
library(ggplot2)
library(forcats) 
library(extrafont)
library(scales)
library(reshape2)
library(ggrepel)

data = "youth_survey_tables.xlsx"

#Tidying the data on race#### 

climate = read_excel(data, sheet=4)
beliefs = read_excel(data, sheet=7)
neighborhood1 = read_excel(data, sheet=12)
neighborhood2 = read_excel(data, sheet=13)
college = read_excel(data, sheet=8)

tidy = function(df) {
  df %>% gather(key="race", value="pct_responses", 2:4) %>%
    filter(race != "Hispanic")
}

#To make the graphs simpler, we did not include Hispanic students. However, on most measures Hispanic students had similar responses as Black students. 

climate_tidy = tidy(climate)
beliefs_tidy = tidy(beliefs)
hood1_tidy = tidy(neighborhood1)
hood2_tidy = tidy(neighborhood2)
college_tidy = tidy(college)

#Multiply everything by 100

climate_tidy$pct_responses = climate_tidy$pct_responses * 100
beliefs_tidy$pct_responses = beliefs_tidy$pct_responses *100
hood1_tidy$pct_responses = hood1_tidy$pct_responses *100
hood2_tidy$pct_responses = hood2_tidy$pct_responses *100
college_tidy$pct_responses = college_tidy$pct_responses *100

#Reorder everything

climate_tidy <- climate_tidy %>% 
  mutate(Domain=fct_reorder(Domain, desc(Domain_Order)))
beliefs_tidy = beliefs_tidy %>% 
  mutate(Domain=fct_reorder(Domain, desc(Domain_Order)))
hood1_tidy = hood1_tidy%>% 
  mutate(Domain=fct_reorder(Domain, desc(Domain_Order))) 
hood2_tidy = hood2_tidy%>% 
  mutate(Domain=fct_reorder(Domain, desc(Domain_Order)))
college_tidy = college_tidy%>% 
  mutate(Domain=fct_reorder(Domain, desc(Domain_Order)))

#Themes#### 

horizontal_theme = theme_bw(base_family = "Georgia") +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x = element_text(hjust=0.5, size = 10, margin=margin(t=10, b=10)),
    axis.text.y = element_text(size=10, margin=margin(r=10), color="black", hjust=0),
    axis.text.x = element_text(size=10, margin=margin(t=10), color="black"),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position= "bottom",
    legend.margin=margin(-10, 10, 0, 0),
    legend.text = element_text(size = 10, margin = margin(r = 10)),
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(size=1), 
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(30, 10, 10, 10, "pt")) 

vertical_theme = horizontal_theme +
  theme(panel.grid.major.y = element_blank() ,
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size=1), 
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(10, 30, 10, 10, "pt"))

#Functions to create dumbbell plots and bar graphs####


dumbbell = function(df) {
  ggplot(df, aes(pct_responses, Domain)) +
    geom_line(aes(group=Domain)) +
    geom_point(aes(color=race), size=5) +
    vertical_theme + 
    scale_color_manual(labels = c("Black Students", "White Students"), 
                       values=c("#073b47", "#e59918")) +
    scale_x_continuous(expand = c(0, 0), 
                       limits=c(0,100), 
                       breaks = seq(0, 100, by=20),
                       labels = function(x) paste0(x,"%")) +
    labs(x = "% of Affirmative Responses") +
    scale_y_discrete(labels = wrap_format(40)) +
    geom_text_repel(aes(label=paste0(round(pct_responses),"%")), size=3.52778, family="Georgia", vjust=-1.5, segment.color=NA)
}

dumbbell2 = function(df) {
  ggplot(df, aes(pct_responses, Domain)) +
    geom_line(aes(group=Domain)) +
    geom_point(aes(color=race), size=5) +
    vertical_theme + 
    scale_color_manual(labels = c("Black Students", "White Students"), 
                       values=c("#073b47", "#e59918")) +
    scale_x_continuous(expand = c(0, 0), 
                       limits=c(0,100), 
                       breaks = seq(0, 100, by=20),
                       labels = function(x) paste0(x,"%")) +
    labs(x = "% of Students") +
    scale_y_discrete(labels = wrap_format(40)) +
    geom_text_repel(aes(label=paste0(round(pct_responses),"%")), size=3.52778, family="Georgia", vjust=-1.5, segment.color=NA)
}


barchart = function(df) {
  ggplot(df, aes(x = reorder(domain, desc(domain)), y = pct_responses)) +
    geom_bar(position = "dodge", stat="identity", width=0.5, fill="#3bbae0") +
    vertical_theme +
    labs(y = "% of Affirmative Responses") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,1),
                       breaks = seq(0,1,0.2),
                       labels = function(x) paste0(x*100, "%")) +
    coord_flip() +
    scale_x_discrete(labels = wrap_format(50)) +
    geom_text(aes(label= paste0(round(pct_responses*100),"%")), position = position_dodge(width = 0.5), size=3.52778, hjust=-0.25, family="Georgia")
}

barchart2 = function(df) {
  ggplot(df, aes(x = reorder(domain, desc(domain)), y = pct_responses)) +
    geom_bar(position = "dodge", stat="identity", width=0.5, fill="#3bbae0") +
    vertical_theme +
    labs(y = "% of Students") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,1),
                       breaks = seq(0,1,0.2),
                       labels = function(x) paste0(x*100, "%")) +
    coord_flip() +
    scale_x_discrete(labels = wrap_format(50)) +
    geom_text(aes(label= paste0(round(pct_responses*100),"%")), position = position_dodge(width = 0.5), size=3.52778, hjust=-0.25, family="Georgia")
}

#fig_1#### 

#Figure 1: New Orleans students rated their teachers lower than a national comparison group on every dimension of teacher quality. 

#Multiply everything by 100 to get percents instead of decimals and then round

fig1 = read_excel(data, sheet = 1)

fig1[2:8] = fig1[2:8] *100
fig1[2:8] = round(fig1[2:8])

fig1 = fig1 %>% filter(score_type %in% c("New Orleans Domain Score","National Domain Score")) %>% gather(key = "domain", value = "score", 2:8)

fig1$domain = factor(fig1$domain, levels = unique(fig1$domain))

ggplot(fig1, aes(fill=score_type, y=score, x=reorder(domain, desc(domain)))) +
  geom_bar(position="dodge", stat="identity", width=0.75) +
  coord_flip() +
  vertical_theme +
  ylab('% of Affirmative Responses') +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0, 100), 
                     breaks = seq(0, 100, by=20),
                     label = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = wrap_format(50)) +
  theme(axis.text.y = element_text(size=9, margin=margin(r=10), color="black")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(labels = c("National Comparison Group", "New Orleans Surveyed Students"), values=c("#d65c2a", "#3bbae0")) +
  geom_text(aes(label= paste0(score,"%"), vjust=ifelse(score_type == "New Orleans Domain Score", 0.2, 0.4)), position = position_dodge(width = 0.75), size=3.52778, family="Georgia", hjust=-0.25)

#ggsave("formatted_figs/ys_fig1_formatted.pdf", w=6.5, h=4.5, unit="in", device=cairo_pdf)

#fig_2####

#Figure 2: Black students rate their teachers lower on every measure of quality, especially in Classroom Management.

fig2 = read_excel(data, sheet=2)

fig2 = fig2 %>% gather(key="race", value="pct_agreement", 3:5) %>% 
  filter(race != "Hispanic") %>% select('Domain', 'race', 'pct_agreement')

fig2$Domain = factor(fig2$Domain, levels=unique(fig2$Domain))
fig2$Domain = fct_rev(fig2$Domain)

fig2$pct_agreement = fig2$pct_agreement * 100

ggplot(fig2, aes(pct_agreement, Domain)) +
  geom_line(aes(group=Domain)) +
  geom_point(aes(color=race), size=5) +
  vertical_theme + 
  scale_color_manual(labels = c("Black Students", "White Students"), 
                     values=c("#073b47","#e59918")) +
  scale_x_continuous(expand = c(0, 0), 
                     limits=c(0,100), 
                     breaks = seq(0, 100, by=20),
                     labels = function(x) paste0(x,"%")) +
  labs(x = "% of Affirmative Responses") +
  scale_y_discrete(labels = wrap_format(45)) +
  geom_text_repel(aes(label= paste0(round(pct_agreement),"%")), size=3.52778, nudge_y=0.4, nudge_x=2, family="Georgia", segment.color = NA)

#ggsave("formatted_figs/ys_fig2_formatted.pdf", w=6.5, h=5, unit="in", device=cairo_pdf)

#fig_3####

#Figure 3: Slightly more than half of students give positive responses on our school climate measures.

fig3 = read_excel(data, sheet = 3)

fig3 = fig3[-1, ]
colnames(fig3) = c("domain", "pct_responses")

#Axis label: % of Affirmative Responses

fig3$domain = factor(fig3$domain, levels = fig3$domain)

barchart(fig3)

#ggsave("formatted_figs/ys_fig3_formatted.pdf", w=6.5, h=5, unit="in", device=cairo_pdf)

#fig_4####
#Figure 4: Black students in New Orleans perceive poorer school climates.

dumbbell(climate_tidy)
#ggsave("formatted_figs/ys_fig4_formatted.pdf", w=6.5, h=5, unit="in", device=cairo_pdf)

#fig_5####
#Figure 5: Students frequently report that they value education and believe that their effort pays off.

fig5 = read_excel(data, sheet = 5)

#Axis label: % of Affirmative Responses

colnames(fig5) = c("domain", "pct_responses")
fig5 = fig5[-1, ]

fig5$domain = factor(fig5$domain, levels=fig5$domain)

barchart(fig5)

#ggsave("formatted_figs/ys_fig5_formatted.pdf", w=6.5, h=3.6, unit="in", device=cairo_pdf)

#fig_6####
#Figure 6: A large majority of students believe that they will get at least a college degree.

fig6 = read_excel(data, sheet=6)
#Axis label = % of Students

fig6 = fig6[-1, ]

colnames(fig6) = c("domain", "Undergraduate Degree", "Graduate Degree", "total")

fig6 = fig6 %>% select(domain, total)

fig6$total = as.numeric(fig6$total)

fig6$domain = factor(fig6$domain, levels = fig6$domain)

ggplot(fig6, aes(x=reorder(domain,desc(domain)), y=total)) +
  geom_col(width=0.5, fill="#3bbae0") +
  coord_flip() +
  vertical_theme +
  labs(y = "% of Students", x = "") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0,1,0.2),
                     labels = function(x) paste0(x*100, "%")) +
  scale_x_discrete(labels = wrap_format(40)) +
  geom_text(aes(label = paste0(round(stat(y)*100),"%"), group=domain),
            stat = 'summary', fun.y = sum, hjust = -0.25, size = 3.52778, family = "Georgia")

#ggsave("formatted_figs/ys_fig6_formatted.pdf", w=6.5, h=3.4, unit="in", device=cairo_pdf)


#fig_7#### 

#Figure 7: Black students report lower levels of self-control and academic behaviors, but they do not believe less in the value of education or growth mindset.

dumbbell(beliefs_tidy)

#ggsave("formatted_figs/ys_fig7_formatted.pdf", w=6.5, h=3.6, unit="in", device=cairo_pdf)

#fig_8####

#Figure 8: Black students are less likely to believe that they will attend college than their white peers.

dumbbell2(college_tidy)

#ggsave("formatted_figs/ys_fig8_formatted.pdf", w=6.5, h=3, unit="in", device=cairo_pdf)

#fig_9####

#Figure 9: Most students travel 30 minutes or less to school in the morning.

fig9 = read_excel(data, sheet=9)
#Axis label: % of Students

fig9 = fig9[-1,-3]

colnames(fig9) = c("time", "pct_students")

fig9$pct_students = as.numeric(fig9$pct_students)
fig9$mode = "All forms of transport"

ggplot(fig9, aes(x=mode, y=pct_students, fill=fct_rev(time))) +
  geom_bar(position = "fill", stat="identity", width=0.5) +
  vertical_theme +
  labs(y = "% of Students (All Modes of Transport)", x = "") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0,1, by=.2),
                     labels = percent_format()) +
  scale_fill_manual(values=c("#20535F", "#6c9ca9", "#9fcddb", "#b9e6f4")) +
  geom_text(aes(label=paste0(round(pct_students*100), "%")), position=position_fill(vjust=0.5), size = 3.52788, family="Georgia") +
  coord_flip() +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#ggsave("formatted_figs/ys_fig9_formatted.pdf", w=6.5, h=3.8, unit="in", device=cairo_pdf)

#fig_10####
#Figure 10: Students from New Orleans East, the West Bank, and Uptown experience longer travel times to school in the morning.
#Alica Gerry created this figure. 

#fig_11####

#Figure 11: Most students report having social support and feeling safe in their neighborhoods. 
#% of Affirmative Responses

fig11 = read_excel(data, sheet=10)

fig11 = fig11[-1,]
colnames(fig11) = c("domain", "pct_responses")
fig11$domain = factor(fig11$domain, levels = fig11$domain)

barchart(fig11)

#ggsave("formatted_figs/ys_fig11_formatted.pdf", w=6.5, h=2.4, unit="in", device=cairo_pdf)

#fig_12####

#Figure 12: Though most high school students reported no experiences of discrimination, including being hassled by police, fewer than half reported feeling safer in the presence of police. 
#% of Students

fig12 = read_excel(data, sheet=11)
fig12 = fig12[-1,]
colnames(fig12) = c("domain", "pct_responses")

fig12$domain = factor(fig12$domain, levels = fig12$domain)

barchart2(fig12)

#ggsave("formatted_figs/ys_fig12_formatted.pdf", w=6.5, h=3.8, unit="in", device=cairo_pdf)

#fig_13####

#Figure 13: White students feel safer in their neighborhoods than their non-white peers.

dumbbell(hood1_tidy)

#ggsave("formatted_figs/ys_fig13_formatted.pdf", w=6.5, h=2.8, unit="in", device=cairo_pdf)

#fig_14####

#Figure 14: White students are less likely to report experiencing discrimination, and more likely to report feeling safer in the presence of police, than their black peers.

dumbbell2(hood2_tidy)

#ggsave("formatted_figs/ys_fig14_formatted.pdf", w=6.5, h=3.8, unit="in", device=cairo_pdf)

