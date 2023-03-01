# https://myfiles.lshtm.ac.uk/filr/user/net-folder/folder/29286
# https://www.thelancet.com/cms/10.1016/S1473-3099(20)30984-1/attachment/fa03be20-c489-421a-a960-1354e11229a4/mmc1.pdf
# https://github.com/nicholasdavies/covid-tiers
#
# https://europepmc.org/article/ppr/ppr535751
# https://www.sciencedirect.com/science/article/pii/S1755436522000469#sec0085
# https://www.nature.com/articles/s41598-022-22747-3
# https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-022-02543-6
# https://www.sciencedirect.com/science/article/pii/S1755436522000494
# 
#create time intervals for different types of restrictions
T1 <- interval(ymd("2020-03-02"), ymd("2020-03-22"))
L1 <- interval(ymd("2020-03-23"), ymd("2020-05-31"))
T2 <- interval(ymd("2020-06-01"), ymd("2020-07-04"))
F1 <- interval(ymd("2020-07-05"), ymd("2020-09-13"))
T3 <- interval(ymd("2020-09-14"), ymd("2020-11-04"))
L2 <- interval(ymd("2020-11-05"), ymd("2020-12-01"))
T4 <- interval(ymd("2020-12-02"), ymd("2021-01-05"))
L3 <- interval(ymd("2021-01-06"), ymd("2021-03-07"))
T5 <- interval(ymd("2021-03-08"), ymd("2021-07-18"))
F2 <- interval(ymd("2021-07-19"), ymd("2021-12-07"))
T6 <- interval(ymd("2021-12-08"), ymd("2022-02-21"))

#assign value to each type of restriction
lockdowns$status <- ifelse(ymd(lockdowns$date) %within% T1, 1, 
                     ifelse(ymd(lockdowns$date) %within% L1, 2, 
                         ifelse(ymd(lockdowns$date) %within% T2, 1, 
                              ifelse(ymd(lockdowns$date) %within% T3, 1, 
                                  ifelse(ymd(lockdowns$date) %within% L2, 2, 
                                       ifelse(ymd(lockdowns$date) %within% T4, 1, 
                                            ifelse(ymd(lockdowns$date) %within% L3, 2, 
                                                 ifelse(ymd(lockdowns$date) %within% T5, 1,
                                                        ifelse(ymd(lockdowns$date) %within% T6, 1, 0)))))))))

#create factor
lockdown_fac <- factor(lockdowns$status, levels = c(0, 1, 2, 3),
                       labels = c("No restrictions", "Some restrictions",
                                  "Lockdown", "Pre-Pandemic"))
lockdowns$status <- lockdown_fac
