#https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic
#Packages
#?j???w??packages #visulization #visulization #visulization #data manipulation # imputation #classification algorithm
packages = c("ggplot2", "ggthemes", "scales", "dplyr","mice", "randomForest")
for (i in packages){install.packages(i)}
#?@?????Jpackages
sapply(packages, FUN = library, character.only = TRUE)
#?T?{
search()

train = read.csv(file.choose(), stringsAsFactors = F)
test = read.csv(file.choose(), stringsAsFactors = F)
full =  bind_rows(train, test) # bind training & test data
str(full) #check data

#Grab title from passenger names
full$Title = gsub("(.*, )|(\\..*)", "", full$Name) 
# '.'表全???; '???'表刪???; '.*'表全?????????; '.*'???鍵字擺後表之?????????全?????????，???'.*,'???','?????????全?????????, 
# ???規表示???中\???特殊???能???特殊???能，???以\\???能表示\???跳脫符???; '\\..*'???'\\.'後面????????????

#Show title counts by sex
table(full$Sex, full$Title)

#Title with very low cell counts to be combinded to "rare" level
rare_title = c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")

#Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == "Mlle"] = "Miss"
full$Title[full$Title == "Ms"] = "Miss"
full$Title[full$Title == "Mme"] = "Mrs"
full$Title[full$Title %in% rare_title]  = 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finaaly, grab surname from passenger name
full$Surname = sapply(full$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
head(full$Surname)
cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))


###### Do families sink or swim together?
# Creat a fanily size variable including the passenger themselves
full$Fsize = full$SibSp + full$Parch + 1

# Create a family variable
full$Family = paste(full$Surname, full$Fsize, sep = "_")

# Use ggplot2 to visulize the relationship between family size & survival 
ggplot(full[1:891,], aes(x = Fsize, fill = factor (Survived))) +
  geom_bar(stat = "count", position = "dodge") +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = "Family Size") +
  theme_few()

# ?o?{?j?P?̥ͦs?vcount?i?��T?s?A1, >1 & <5, >4
# Discretize family size
full$FsizeD[full$Fsize == 1] = "singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] = "small"
full$FsizeD[full$Fsize > 4] = "large"

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main = "Family Size by Surevival", shade = T)

###### Treat a few more variables
# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. Ex:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A-F:
full$Deck = factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
## There??s more that likely could be done here including looking into cabins with multiple rooms listed (e.g., row 28: ??C23 C25 C27??), but given the sparseness of the column we??ll stop here.
## ?Ӧh?ʥ??ȡA????

###### Missingness ?]?˥??ƤӤp?A???A?X?ϥΧR???ʥ??ȡA?ҥH?u?n?ɯʥ??ȡC
#PassengerId == 62, 83???ʥ??ȡA?R???C
full[c(62, 830), 'Embarked']
cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))

# Get rid of our missing passenger IDs
embark_fare = full %>%
  filter(PassengerId != 62  & PassengerId != 830)

#Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot()+
  geom_hline(aes(yintercept = 80),
             colour ="red", linetype = "dashed", lwd = 2)+
  scale_y_continuous(labels = dollar_format())+
  theme_few()
# ?ѤW?ϩҥ?Q and S ???ȥI???????S???W?L80???ҥH?X?z?{?? PassengerId == 62, 830???ȤH who paid $80 ?k???bC
# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] = "C"

# PassengerId == 1044 ?? Fare (?O??) ==0?A???X???ݩ?????class (????) ?? Embarked (?n???a?I)?A?��ոɨ??ʥ???
# Show row 1044 
full[1044,]
# PassengerId == 1044 ?ݩ? class == 3 & Embarked == S

#Let??s visualize Fares among all others sharing their class and embarkment (n = 494).
ggplot(full[full$Pclass == "3" & full$Embarked == "S", ], 
       aes(x = Fare))+
  geom_density(fill = "#99d6ff", alpha = 0.4)+
  geom_vline(aes(xintercept = median (Fare, na.rm = T)),
             colour = "red", linetype = "dashed", lwd = 1)+
  scale_x_continuous(labels = dollar_format()) +
  theme_few()
# class == 3 & Embarked == S ?????p?????b median ?B?A?ҥH?N median ?��? Fare ?? NA 
full$Fare[1044] = median(full[full$Pclass == "3" & full$Embarked == "S", ]$Fare, na.rm = T) #na.rm: NA????

# ?ɨ? missing Age ?ȡA?Q?Ψ??L?ܼƫؼҹw?? Age
# Show number of missing Age value
sum(is.na(full$Age))

# ?z?פW?Q??CART (rpart)?ɨ? Missing Age data, but ?@?̷Q?ոլ? mice
# Make variable factors in to factors
factor_vars = c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] = lapply(full[factor_vars], function(x) as.factor(x))

#Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod = mice(full[, !names(full) %in%  c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = "rf")

# Save the complete output
mice_output = complete(mice_mod)

# ???????l???ƻP?ɻ????? by mice model ???��??t?? 
# Plot age distributions.
par(mfrow = c(1, 2))
hist(full$Age, freq = F, main ="Age: Original Data", col ="darkgreen", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main ="Age: MICE Output", col ="lightgreen", ylim = c(0, 0.04))

# Replace Age variable from mice model
full$Age = mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))

###### Feature Engineering: Round 2
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) +
  geom_histogram()+
  # I iclude Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex)+
  theme_few()

# Create the colum child, and indicate whether child or adult
full$Child[full$Age < 18] = "Child"
full$Child[full$Age >= 18] = "Adult"
# Show counts
table(full$Child, full$Survived)

# ?ݶ??????ͦs?v
# Adding Mother variable
full$Mother = "Not Mother"
full$Mother[full$Sex == "female" & full$Parch > 0 & full$Age > 18 & full$Title != "Miss"] = "Mother"
# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variable
full$Child = factor(full$Child)
full$Mother = factor(full$Mother)
#?ˬd???L?ʭ?
md.pattern(full)

###### Prediction (by using randomForest)
train = full[1:891,]
test = full[892:1039,]

# Building the model
# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model = randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother, data = train)
rf_model
# Show model error
plot(rf_model, ylim = c(0,0.36))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fil = 1:3 )
# ?i?o?{?w?????`???s?????ǽT?װ??ܦh

# ?}?l?D?ܼ?
# Get importance
importance = importance(rf_model)
importance
varImportance = data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"],2))
varImportance

# Creat a rank variable based on importance
rankImportance = varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
rankImportance

# Use ggplot2 to visualize the relative importance of variable
# reorder(?Q?n?ƧǪ??????A?Q?n?Ω??M?w?ƧǪ??????A?ϥΪ?????)

ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
      geom_bar(stat = "identity") +
      geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") +
      labs(x = "Variables") +
  coord_flip()+
  theme_few()

#### Prediction
# predict using the test set
prediction = predict(rf_model, test)
prediction

# Save the solution to a dataframe with two colums: PassengerId and Survived (prediction)
solution = data.frame(PassengerID = test$PassengerId, Survived = prediction)
solution

# Write the solution to life
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
getwd()
