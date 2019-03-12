# import data frame
data_01 <- read.csv("data_01.csv", header = TRUE, sep = ",", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "")

# graphing figures from HERE


############################ Figure 1 ############################

mygraphdata = matrix(0,2009-1962+1,2)
Y = dim(mygraphdata)[1]
mygraphdata[,2] = 1962:2009
exper0 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(0), deduc_2 = c(0),
                    deduc_3 = c(0), deduc_4 = c(0), drace_1 = 0, drace_2 = 0)
exper1 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(1), deduc_2 = c(0),
                    deduc_3 = c(0), deduc_4 = c(0), drace_1 = 0, drace_2 = 0)
exper2 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(0), deduc_2 = c(1),
                    deduc_3 = c(0), deduc_4 = c(0), drace_1 = 0, drace_2 = 0)
exper3 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(0), deduc_2 = c(0),
                    deduc_3 = c(1), deduc_4 = c(0), drace_1 = 0, drace_2 = 0)
exper4 = data.frame(exp = c(5,15,25,35,45), deduc_1 = c(0), deduc_2 = c(0),
                    deduc_3 = c(0), deduc_4 = c(1), drace_1 = 0, drace_2 = 0)

edu.exp = matrix(0, 10, 5)
toweight = array(0, dim = c(10, 5, Y))
allww = matrix(0,5,5)

for (i in 1:Y){
  for (j in 1:2){
    data_02 <- filter(data_01, year == (1963+i) & sex == j)
    regression1 = lm(lrwage ~ deduc_1+deduc_2+deduc_3+deduc_4+
                   poly(exp,4,raw=TRUE)+(deduc_1+deduc_2+deduc_3+deduc_4):poly(exp,4,raw = TRUE)+
                   drace_1+drace_2+(deduc_1+deduc_2+deduc_3+deduc_4):exp:(drace_1+drace_2), data = data_02)
    camlw0 = predict(regression1, exper0)
    camlw1 = predict(regression1, exper1)
    camlw2 = predict(regression1, exper2)
    camlw3 = predict(regression1, exper3)
    camlw4 = predict(regression1, exper4)
    keepme = matrix(c(camlw0,camlw1,camlw2,camlw3,camlw4),5,5, byrow = TRUE)
    if (j==1){
      edu.exp[1:5,] = keepme
    } else {
      edu.exp[6:10,] = keepme
      toweight[,,i] = edu.exp
    }
  }
}
aa = matrix(0,Y,10)
for (i in 1:Y){
  aa[i,] = rowMeans(toweight[,,i])
}

# weighting for different exp levels

wt1 = dim(filter(data_00, exp<=9))[1]
wt2 = dim(filter(data_00, 10<exp & exp<=20))[1]
wt3 = dim(filter(data_00, 20<exp & exp<=30))[1]
wt4 = dim(filter(data_00, 30<exp & exp<=40))[1]
wt5 = dim(filter(data_00, 40<exp & exp<=50))[1]
to = wt1 + wt2 + wt3 + wt4 + wt5 
ww = matrix(c(wt1/to,wt2/to,wt3/to,wt4/to,wt5/to,wt1/to,wt2/to,wt3/to,wt4/to,wt5/to),Y,10, byrow = TRUE)

# weighting predicted values
bb = aa*ww
cc = matrix(0,Y,2)
for (i in 1:Y){
  cc[i,1] = (bb[i,1]+bb[i,2]+bb[i,6]+bb[i,7])/((bb[i,1]+bb[i,2]+bb[i,6]+bb[i,7]+bb[i,3]+bb[i,4]+bb[i,5]+bb[i,8]+bb[i,9]+bb[i,10]))
  cc[i,2] = (bb[i,3]+bb[i,4]+bb[i,5]+bb[i,8]+bb[i,9]+bb[i,10])/((bb[i,1]+bb[i,2]+bb[i,6]+bb[i,7]+bb[i,3]+bb[i,4]+bb[i,5]+bb[i,8]+bb[i,9]+bb[i,10]))
}
dd = cc[,2]/cc[,1]

# Plot
#years = seq(1964, 1964+Y-1, 1)
years = seq(1964, 1983)

forplot = data.frame(log.wage.gap = dd[1:20], year = years)
ggplot(forplot, aes(x = year, y = log.wage.gap)) +
  geom_line(colour = 'red') +
  geom_smooth() +
  ggtitle("Adjusted college / high-school log weekly wages ratio")


############################ Figure 2 ############################

data_03 <- data_01 %>%      
  mutate(edlvl = ifelse(deduc_1==1,1,ifelse(deduc_2==1,3,ifelse(deduc_3==1,4,ifelse(deduc_4==1,5,2)))))

# labour supply based on group
lsup <- data_03 %>%
  group_by(year, sex, exp, edlvl) %>%
  summarise(labsup = mean(wkswork))

avgwage <- data_03 %>%
  group_by(year, sex, exp, edlvl) %>%
  summarise(mrwage = mean(rwage))

relw <- avgwage %>%
  group_by(year) %>%
  summarise(mrwage_year = mean(mrwage))

avgwage <- merge(x = avgwage, y = relw, by = c("year"))

avgwage <- avgwage %>%
  mutate(relativewage = mrwage/mrwage_year)

lsupavgwage <- merge(x = lsup, y = avgwage, by = c("year","sex","exp","edlvl"))

lsupavgwage <- lsupavgwage %>%
  mutate(sindex = lsupavgwage$labsup * lsupavgwage$relativewage)

supindex <- lsupavgwage %>%
  group_by(year, dum1 = edlvl == 0 | edlvl == 1, dum2 = edlvl == 2 | edlvl == 3 | edlvl == 4) %>%
  summarise(index = mean(sindex))

highschool = filter(supindex, dum1 == TRUE)
college = filter(supindex, dum2 == TRUE)
table = merge(highschool, college, by = c("year"))

forplot = table %>%
  mutate(relsupply = index.x/index.y)

ggplot(forplot, aes(x = year, y = relsupply)) +
  geom_point(colour = 'red') +
  geom_line() +
  ggtitle('College/high-school log relative supply')


############################ Figure 3 ############################
# Unfortunately, this didn't work... #############################

#HSD_m <- data_00 %>%
#  filter(sex == 1) %>%                                         
# filter(deduc_1 == 1)
#SD_f <- data_00 %>%
# filter(sex == 2) %>%                                         
# filter(deduc_1 == 1)

#COL_m <- data_00 %>%
#  filter(sex == 1) %>%                                         
#  filter(deduc_2 == 1)
#COL_f <- data_00 %>%
#  filter(sex == 2) %>%                                         
#  filter(deduc_2 == 1)

#COL4_m <- data_00 %>%
#  filter(sex == 1) %>%                                         
#  filter(deduc_3 == 1)
#COL4_f <- data_00 %>%
#  filter(sex == 2) %>%                                         
#  filter(deduc_3 == 1)

#GTC_m <- data_00 %>%
#  filter(sex == 1) %>%                                         
#  filter(deduc_4 == 1)
#GTC_f <- data_00 %>%
#  filter(sex == 2) %>%                                         
#  filter(deduc_4 == 1)

#ggplot(COL_f, aes(x = year, y = lrwage)) +
#  geom_point(colour = 'red') +
#  geom_smooth() +
# ggtitle("Adjusted college / high-school log weekly wages ratio")


#edu.expm = matrix(0, 5, 5)
#toweightm = array(0, dim = c(5, 5, Y))
#allww = matrix(0,5,5)

#for (i in 1:Y){
#  #for (j in 1:2){
#  dat1 <- filter(data_00, year == (1963+i) & sex == 1)
#  fig1.lm = lm(lrwage ~ deduc_1+deduc_2+deduc_3+deduc_4+
#                 poly(exp,4,raw=TRUE)+(deduc_1+deduc_2+deduc_3+deduc_4):poly(exp,4,raw = TRUE)+
#                 drace_1+drace_2+(deduc_1+deduc_2+deduc_3+deduc_4):exp:(drace_1+drace_2), data = dat1)
#  camlw0 = predict(fig1.lm, exper0)
#  camlw1 = predict(fig1.lm, exper1)
# camlw2 = predict(fig1.lm, exper2)
#  camlw3 = predict(fig1.lm, exper3)
# camlw4 = predict(fig1.lm, exper4)
# keepme = matrix(c(camlw0,camlw1,camlw2,camlw3,camlw4),5,5, byrow = TRUE)
# #if (j==1){
# edu.expm[1:5,] = keepme
#  # } else {
#  #  edu.exp[6:10,] = keepme
#  toweightm[,,i] = edu.expm
#  # }
#  #}
#}
#aa = matrix(0,Y,10)
#for (i in 1:Y){
#  aa[i,] = rowMeans(toweightm[,,i])
#}
#cc = matrix(0,Y,2)
#for (i in 1:Y){
#  cc[i,1] = 1/4*(aa[i,1]+aa[i,2]+aa[i,6]+aa[i,7])                       # high school variables
#  cc[i,2] = 1/6*(aa[i,3]+aa[i,4]+aa[i,5]+aa[i,8]+aa[i,9]+aa[i,10])      # college variables
#}
#dd = cc[,1]/cc[,2]
