#libraries
library(tidyverse)
library(logitnorm)

#columns of data frame needed to begin the updating
#1. child_id: Unique identifier for each child, also an identifier for the simulated child - I used 1111
#2. areacode: The full milestone code e.g. V_1.1 for vision milestone, month 1, sequence 1
#3. I split areacode into several variables:
#      Area: V
#      code: 1.1
#      month: 1
#      sequence: 1
#4: status: status of the milestone- 1 = achieved, 0 = not achieved


#the first step of the BSU is to get the columns z, N, a, b set up
#make sure your data is ordered by child_id, area, month, sequence
#get z
data_z<- data %>%
         group_by(child_id, area)%>%
         mutate(z = cumsum(status))
    
#get N
data_N_step_1<- data_z %>%
                ungroup()%>%
                mutate(N = rep(1, length.out = nrow(data_z)))
         
data_N<- data_N_step_1 %>%
         group_by(child_id, area)%>%
         mutate(N=cumsum(N))

#get a 
data_a_step_1 <- data_N %>%
          group_by(child_id, area)%>%
          mutate(a = ifelse(status == 1, 1, 0))
#make sure first a for each functional domain is 1
data_a_step_2 <- data_a_step_1 %>%
  group_by(child_id, area)%>%
  mutate(a = ifelse(code == min(code), 1, a))

data_a<- data_a_step_2 %>% 
         group_by(child_id, area)%>%
         mutate(a = cumsum(a))
  
#get b
data_b_step_1 <- data_a %>%
  group_by(child_id, area)%>%
  mutate(b = ifelse(status == 0, 1, 0))
#make sure first a for each functional domain is 1
data_b_step_2 <- data_b_step_1 %>%
  group_by(child_id, area)%>%
  mutate(b = ifelse(code == min(code), 1, b))

data_b<- data_b_step_2 %>% 
  group_by(child_id, area)%>%
  mutate(b = cumsum(b))

#next we calculate the posterior mean for each row - each milestone
data_with_postmean<- data_b %>%
                     mutate(posterior_mean = (z+a)/(N+a+b))

#calculate the upper and low HDI limits 
#I used the funxtion in Kruschke's source code 
#visit https://sites.google.com/site/doingbayesiandataanalysis/software-installation to obtain the code
source("BernBeta.R")
source("DBDA2E-utilities.R")
HDI_output<- apply(data_with_postmean[,c("a", "b", "N", "z")], 1, function(x) HDIofICDF(ICDFname=qbeta,shape1=x[1]+x[4], 
                                                                                    shape2 = x[2]+x[3]-x[4], credMass = 0.95, tol=1e-8))

HDI_output<- as.data.frame(HDI_output)

HDI_output<-t(HDI_output)
colnames(HDI_output)<- c("low_HDI", "High_HDI")

HDI_output<-as.data.frame(HDI_output)



data_with_postmean$low_HDI<- HDI_output$low_HDI
data_with_postmean$high_HDI<- HDI_output$High_HDI
#extract data from "gold standard kid and cbind as its own column
ref_child<- filter(data_with_postmean, child_id == 1111)
ref_child_select_columns<- ref_child %>%
                           ungroup() %>%
                           dplyr::select(area, N, ref_post = posterior_mean) 

data_with_ref<- left_join(data_with_postmean, ref_child_select_columns)

#Caluclate areas and rescale
#function for areas
area<- function(x, curve_one, curve_two){
  a<-c()
  for(i in 1:length(x)){
    
    a[i]<-(curve_two[i]- curve_one[i])*(x[i+1] - x[i])
    result<-sum(a, na.rm=T)
  } 
  result
}

#calculate absolute areas 
data_with_area<-data_with_ref %>%
  group_by(child_id, area)%>%
  mutate(area_curves = area(N, posterior_mean, ref_post))


#rescale areas
rescaled_area<-  data_with_area %>%
  group_by(child_id, area) %>%
  mutate(n_codes = n_distinct(areacode))%>%
  mutate(rescaled_area = area_curves/n_codes)



#spread into a data friame of just areas
data_with_area_wide<- dplyr::select(rescaled_area, child_id, area, rescaled_area)%>%
                      unique()%>%
                      spread(area, rescaled_area, fill=NA)        

#remove child 1111
data_with_area_wide<- filter(data_with_area_wide, child_id != 1111)

#transform to logit scale
#replace 0's with the smallest areas
sort(data_with_area_wide$V)
data_with_area_wide$A[data_with_area_wide$A == 0] <- 0.0009663029
data_with_area_wide$H[data_with_area_wide$H == 0] <- 0.0004040404 
data_with_area_wide$M[data_with_area_wide$M == 0] <- 0.0003105108
data_with_area_wide$S[data_with_area_wide$S == 0] <- 0.0005737235
data_with_area_wide$T[data_with_area_wide$T == 0] <- 0.0003105108
data_with_area_wide$V[data_with_area_wide$V == 0] <- 0.0007824726



logit_data<- data_with_area_wide %>%
  mutate(logit_A = logit(A),
         logit_H = logit(H),
         logit_M = logit(M),
         logit_S = logit(S),
         logit_T = logit(T),
         logit_V = logit(V))


