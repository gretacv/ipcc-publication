library(pdftools)
library(plyr)
library(dplyr)
library(stringi)
library(ggplot2)
# chapter 2 downloaded from: https://report.ipcc.ch/ar6wg2/pdf/IPCC_AR6_WGII_FinalDraft_Chapter02.pdf Then only the refs have been put in a pdf
d = pdf_text("ipcc_ch2_refs.pdf")%>% 
  strsplit(split = "\n") %>% 
  unlist()
length(d)
pp = d %>% 
  str_extract("[:digit:][:digit:][:digit:][:digit:]:") 
class(pp)
pp_clean = pp[which(pp!="NA")]

length(pp_clean)
head(pp_clean)
which(nchar(pp_clean)!=5)
pp_num= as.numeric(substr(pp_clean,1,nchar(pp_clean)-1))
pp_num_clean = pp_num[which(pp_num>1800)]
hist(pp_num_clean)
summary(pp_num_clean)
#the year of publication has a colon afterwards, we have to look for the pattern four numbers + colon
df = data.frame(year=pp_num_clean) %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  mutate(ipcc_time = ifelse(year<2014,"Fifth","Sixth"))  
  
ggplot(df)+
  geom_col(aes(x=year, y = count), fill = "#17005E")+
  geom_text(aes(label=ifelse(count>100,count,NA), x=year, y = count+5))+
  facet_wrap(~ipcc_time, scales = "free_x")+
  scale_x_continuous("Year of publication")+
  scale_y_continuous("Number of papers")+
  theme_bw()+
  ggtitle("Year of publication of the articles referenced\n in Chapter 2 of the 6th IPCC report")+
  theme(strip.background =element_rect(fill="#17005E"),
        strip.text = element_text(colour = '#FCAA6A'))
