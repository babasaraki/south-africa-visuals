# Introduction to the Dataset

#Working with the south african dataset to run all the analyses. 
#Create a new project. Make sure you put it somewhere you'll be able to find it again later!

#Import the dataset "dataset.csv" from data folder in your project 

## Load the packages that could be used
library(tidyverse) ## because tidyverse
library(skimr) ## looking at the data
library(ggpubr) ## for qq plots
library(rstatix) ## for shapiro_test that is pipe-friendly
library(cowplot) ## for publication ready graphics  

# Importing data
library(readr)
dataset <- read_csv("data/dataset.csv")
View(dataset)

## Know the type of data in the datafile
str(dataset)
summary(dataset)
sapply(dataset, class)

## Data cleaning. 
library(dplyr)
dat <- dataset %>% 
        rename(Year = Year, Country = Country, Topic = Topic, Authors = Authors, 
               No_authors = No_Authors, Title = Title, Institution = Institution, 
               Journal = Journal, African_Journal = African_Journal, Impact = Impact, 
               Mean_Citations = Means_Citations, Model = Model, 
               Genetic = Genetic, Research_Types = Research_Types, 
               Adv_Methods = Adv_Methods, Medicinal_Plants_Usage = Medicinal_Plants_Usage, p_name = P_Name, 
               p_use = P_Use, p_success = P_Success, 
               n_affiliations = N_affiliations, International_collab = InternationalCollab) 

## ifelse(dat$african_journal=="Yes", 1,0) 

#Transfer to csv file
write.csv(dat, "data/dat_rename.csv")
dat_rename <- read_csv("data/dat_rename.csv")
View(dat_rename)
spec(dat_rename) # Retrieve the full column specification


##(1) Total publication per year for clinical and basic research. 

pap_counts <- dat_rename %>% 
        group_by(Year, Research_Types) %>% count() 
dat_c <- pap_counts %>% 
        rename(Papers_Published = n)
view(dat_c)


##(ALL) Total publication per year for ALL institutions. 
pupln_counts <- dat_rename %>% 
        group_by(Year, Institution) %>% count() 
publn_c <- pupln_counts %>% 
   rename(Papers_Published = n)
       write.csv(publn_c, "data/publn_c.csv")
       
##(Articles) Total publication per year for ALL Journals 
jonl_counts <- dat_rename %>% 
        group_by(Year, Journal) %>% count() 
jonl_c <- jonl_counts %>% 
   rename(Papers_Published = n)
      write.csv(jonl_c, "data/jonl_c.csv")
       
#ggplot(data = dat_c) +
 #  geom_line(mapping = aes(x = Year, y = Papers_Published, 
  #                         linetype = Research_Types, color = Research_Types)) + theme_bw()
   #   ggsave(filename = "plots/publication_year.png", width = 12, height = 8, dpi = 300)
      
# Plot line graph by group
plot1 <- ggplot(dat_c, aes(x = Year, 
                           y = Papers_Published, 
                                linetype = Research_Types, group = Research_Types)) + 
  geom_line()+ geom_point() + theme_bw()

# Eliminates background, grid lines and chart boarder
     plot1 + theme(axis.line = element_line(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank())
# Save plot     
     ggsave(filename = "plots/publication_year.png")
     
# Plot line graph by colour
     plot1 <- ggplot(dat_c, aes(x = Year, 
                                y = Papers_Published, 
                                linetype = Research_Types, colour = Research_Types)) + 
       geom_line() + geom_point() + theme_bw()
     
# Eliminates background, grid lines and chart boarder
   plot2 <-  plot1 + theme(axis.line = element_line(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank())
# Save plot     
     ggsave(filename = "plots/publication_year_1.png")
     save_plot(filename ="plots/publication_year_II.png", plot2)


##(2) Mean citations per annum for clinical (green) and basic research (black). 
      view(dat_rename)
mean_counts <- dat_rename %>% 
     group_by(Year, Mean_Citations, Research_Types) %>% count() 
mean_c <- mean_counts %>% 
     rename(Papers_Published = n)
write.csv(mean_c, "data/mean_c.csv")
       view(mean_c)
       
# Plot line graph by colour
plot3 <- ggplot(mean_c, aes(x = Year, 
                                  y = Mean_Citations, 
                                  linetype = Research_Types, colour = Research_Types)) + 
         geom_line()+ geom_point() + theme_bw()
       
# Eliminates background, grid lines and chart boarder
       Plot4 <- plot3 + theme(axis.line = element_line(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/mean_citations_year.png", Plot4)
       

##(3) Journal impact factor (IF) parallels paper citations over the period studied
      view(dat_rename)
data_cat <- dat_rename %>%
   pull(Impact) %>%
           cut(breaks=c(-Inf, 1, 4, 8, Inf), 
                  labels=c("0-1 IF", "1-4 IF", "4-8 IF", "8+ IF"))

dat_cat <- mutate(dat_rename, Impact_Cat = data_cat)
view(dat_cat)


# Add a new column IF range


view(data_cat)
summarise(dat_rename)

data_c <- dat_rename %>% 
        group_by(Mean_Citations,  Research_Types) %>% count() 
mean_c <- mean_counts %>% 
        rename(Papers_Published = n)
write.csv(mean_c, "data/mean_c.csv")

view(mean_c)


## (5) Journals publishing basic research


## (6) Model systems used in basic and clinical research.
model_counts <- dat_rename %>% 
        group_by(Research_Types, Model) %>% count() 
model_c <- model_counts %>% 
        rename(Number_of_Studies = n)
write.csv(model_c, "data/model_c.csv")
view(model_c)


# make a barplot for models

plot6 <- ggplot(model_c, aes(x = Number_of_Studies, 
                             y = Model, colour = Research_Types)) + 
  geom_bar(stat='identity', position = "dodge") + theme_bw()

# Eliminates background, grid lines and chart boarder
Plot7 <- plot6 + theme(axis.line = element_line(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/model_studies.png", Plot7)


## (7) Techniques used in basic neuroscience
Tech_counts <- dat_rename %>% 
  group_by(Adv_Methods) %>% count() 
tech_c <- Tech_counts %>% 
  rename(Number_of_Studies = n)
write.csv(tech_c, "data/Tech_c.csv")
view(Tech_c)


# make a barplot for models
plot7 <- ggplot(tech_c, aes(x = Number_of_Studies, y = Adv_Methods)) +
                  geom_bar(stat = 'identity') 


# Eliminates background, grid lines and chart boarder
Plot8 <- plot7 + theme(axis.line = element_line(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/tech_studies.png", Plot8)


## (8) Articles studying medicinal plants (all papers)
# Remove the missing values in a column
dat_rename_2 <- dat_rename[!is.na(dat_rename$Medicinal_Plants_Usage),]

Plants_counts <- dat_rename_2 %>% 
  group_by(Medicinal_Plants_Usage) %>% count() 
plants_c <- Plants_counts %>% 
  rename(Article_Studying_Medicinal_Plants = n)
write.csv(plants_c, "data/Plants_c.csv")
view(Plants_c)


# make a barplot for medicinal plants
plot8 <- ggplot(plants_c, aes(x = Article_Studying_Medicinal_Plants, 
                              y = Medicinal_Plants_Usage)) +
  geom_bar(stat = 'identity', position = "dodge") 


# Eliminates background, grid lines and chart boarder
Plot9 <- plot8 + theme(axis.line = element_line(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/plants_medic.png", Plot9)


## (8-1) Articles studying medicinal plants (basic and clinical)
Plants_counts <- dat_rename_2 %>% 
  group_by(Medicinal_Plants_Usage, Research_Types) %>% count() 
plants_c_II <- Plants_counts %>% 
  rename(Article_Studying_Medicinal_Plants = n)
write.csv(plants_c_II, "data/Plants_c.csv")

plants_c_II %>% select(Research_Types, Article_Studying_Medicinal_Plants, 
                       -Medicinal_Plants_Usage)

# Make a data frame from vectors 
Research_Types <- c('Basic', 'Clinical')
Article_Studying_Medicinal_Plants <- c('18', '2')
plants_c_III <- data.frame(Research_Types, Article_Studying_Medicinal_Plants)
write.csv(plants_c_III, "data/Plants_c_II.csv")


# make a barplot for medicinal plants (basic and clinical)
plot9 <- ggplot(plants_c_III, aes(x = Article_Studying_Medicinal_Plants, 
                              y = Research_Types)) + 
  geom_bar(position="dodge", stat="identity")

# Eliminates background, grid lines and chart boarder
Plot10 <- plot9 + theme(axis.line = element_line(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/plants_medic_II.png", Plot10)
ggsave(filename = "plots/plants_medic_III.png")


## (9) Research questions of studying medicinal plants
# Remove the missing values in a column
dat_rename_3 <- dat_rename[!is.na(dat_rename$p_name),]

# Group the number of articles
Medic_counts <- dat_rename_3 %>% 
  group_by(p_use) %>% count() 
article_c <- Medic_counts %>% 
  rename(Number_of_Articles = n)
write.csv(article_c, "data/Article_c.csv")
view(Article_c)

# Rename the p_use column
article_c_I <- article_c %>% 
  rename(Plants_Use = p_use)
write.csv(article_c_I, "data/Article_c_I.csv")


# make a barplot for articles and plants
plot10 <- ggplot(article_c_I, aes(x = Number_of_Articles, 
                                  y = Plants_Use)) + 
  geom_bar(width = NULL, na.rm = FALSE, orientation = NA, 
           position="dodge", stat="identity")

# Eliminates background, grid lines and chart boarder
Plot11 <- plot10 + theme(axis.line = element_line(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/plants_articles.png", Plot11)
ggsave(filename = "plots/plants_articles_I.png")

## (10) Studies conducted to evaluate the effectiveness of medicinal plants
# Remove the missing values 
dat_rename_4 <- dat_rename[!is.na(dat_rename$p_success),]

# Group the number of articles
Success_counts <- dat_rename_4 %>% 
  group_by(p_success) %>% count() 
success_c <- Success_counts %>% 
  rename(Number_of_Articles = n)
write.csv(success_c, "data/Success_c.csv")
view(success_c)

# Rename the p_success column
success_c_I <- success_c %>% 
  rename(Plants_Success = p_success)
write.csv(success_c_I, "data/Success_c_I.csv")

# make a barplot for plants success
plot11 <- ggplot(success_c_I, aes(x = Number_of_Articles, 
                                  y = Plants_Success)) + 
  geom_bar(width = NULL, na.rm = FALSE, orientation = NA, 
           position="dodge", stat="identity")

# Eliminates background, grid lines and chart boarder
Plot12 <- plot11 + theme(axis.line = element_line(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(),
                         panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/plants_success.png", Plot12)
ggsave(filename = "plots/plants_success_I.png")


## (11) Total publications by institution
# Remove the missing values 
dat_rename_5 <- dat_rename[!is.na(dat_rename$Institution),]

# Group the number of articles
Inst_counts <- dat_rename_5 %>% 
  group_by(Institution) %>% count() 
inst_c <- Inst_counts %>% 
  rename(Number_of_Articles = n)
write.csv(inst_c, "data/inst_c.csv")
view(inst_c)

# Read the inst_c csv file separately 
inst_cc <- read_csv("data/inst_c.csv")

# make a barplot for plants success
plot12 <- ggplot(inst_cc, aes(x = Number_of_Articles, 
                                  y = Institution)) + 
  geom_bar(width = NULL, na.rm = FALSE, orientation = NA, 
           position="dodge", stat="identity")

# Eliminates background, grid lines and chart boarder
Plot13 <- plot12 + theme(axis.line = element_line(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(),
                         panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/total_inst.png", Plot13)
ggsave(filename = "plots/total_inst_I.png")


## (11-1) Total citation by institution

# Group the institution and mean citations 
Mean_ca <- dat_rename %>% 
  group_by(Institution, Mean_Citations) %>% count() 
mean_ca <- Mean_ca %>% 
  rename(Mean_C = n)
write.csv(mean_ca, "data/mean_ca.csv")
view(mean_ca)


# Read the mean_ca csv file separately 
mean_cc <- read_csv("data/mean_ca.csv")
spec(mean_ca)

# make a barplot for plants success
plot13 <- ggplot(mean_cc, aes(x = Mean_Citations, 
                              y = Institution)) + 
  geom_bar(width = NULL, na.rm = TRUE, orientation = NA, 
           position="dodge", stat="identity")

# Eliminates background, grid lines and chart boarder
Plot14 <- plot13 + theme(axis.line = element_line(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(),
                         panel.background = element_blank())
# Save plot     
save_plot(filename ="plots/mean_ca.png", Plot14)
ggsave(filename = "plots/mean_ca_I.png")


# (12) Total number of papers by institution by single vs multiple 
dat_rename_6 <- dat_rename[!is.na(dat_rename$Institution),]

# Group the institution and category of authors
Authrs_counts <- dat_rename_6 %>% 
  group_by(Institution, Category_of_Authors) %>% count() 
authrs_c <- Authrs_counts %>% 
  rename(Number_of_Articles = n)
write.csv(authrs_c, "data/authrs_c.csv")
view(authrs_c)


## .....Imported to Python for visualization from here....

# (13) Total number of papers by institution by historically adv. 
dat_rename_7 <- dat_rename[!is.na(dat_rename$Institution),]
dat_rename_8 <- dat_rename_7[!is.na(dat_rename_7$Historically_Adv_Disav),]


# Group the institution and category of authors
Hist_counts <- dat_rename_8 %>% 
  group_by(Institution, Historically_Adv_Disav) %>% count() 
hist_c <- Hist_counts %>% 
  rename(Number_of_Articles = n)
write.csv(hist_c, "data/hist_c.csv")
view(hist_c)

## ....Imported to Python for visualization from here....

# (14) Collaborations structure between historically adv vs historically disadv
collab <- read_csv("data/dat_rename.csv")
spec(collab)
dat_rename_9 <- collab[!is.na(collab$Institution),]
view(collab)
dat_rename_10 <- dat_rename_9[!is.na(dat_rename_9$Historically_Adv_Disav),]
dat_rename_11 <- dat_rename_10[!is.na(dat_rename_10$International_collab),]

# Group the institution, historically adv and collaboration
Collab_counts <- dat_rename_11 %>% 
  group_by(Institution, Historically_Adv_Disav, International_collab) %>% count() 
collab_c <- Collab_counts %>% 
  rename(Number_of_Articles = n)
write.csv(collab_c, "data/collab_c.csv")
view(collab_c)


## (15) Total citation by institution for citations

# Group the institution and mean citations 

dat_rename_7 <- dat_rename[!is.na(dat_rename$Institution),]
dat_rename_8 <- dat_rename_7[!is.na(dat_rename_7$Historically_Adv_Disav),]


# Group the institution and category of authors
Hist_counts <- dat_rename_8 %>% 
  group_by(Institution, Mean_Citations, Historically_Adv_Disav) %>% count() 
hist_cc <- Hist_counts %>% 
  rename(Number_of_Articles = n)

# Read the mean_ca csv file separately 
write.csv(hist_cc, "data/hist_cc.csv")
view(hist_cc)


# Group the authors by means citations 
Authr_counts <- dat_rename_8 %>% 
        group_by(Category_of_Authors, Mean_Citations, Historically_Adv_Disav) %>% count() 
authr_cc <- Authr_counts %>% 
        rename(Number_of_Articles = n)

# Read the mean_ca csv file separately 
write.csv(authr_cc, "data/authr_cc.csv")
view(authr_cc)

# Group by Publications, Year and Citations 
Publ_counts <- dat_rename_8 %>% 
        group_by(Institution, Mean_Citations, Year) %>% count() 
publ_cc <- Publ_counts %>% 
        rename(Number_of_Articles = n)

# Read the publn_ca csv file separately 
write.csv(publ_cc, "data/publ_cc.csv")
view(hist_cc)


