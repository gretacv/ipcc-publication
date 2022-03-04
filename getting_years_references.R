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

# Getting the years within the text of chapter 2 to see when in the text each year is mentioned.
d_text = pdf_text("IPCC_AR6_WGII_FinalDraft_Chapter02.pdf")%>% 
  strsplit(split = "\n") %>% 
  unlist() %>% 
  str_extract("[:digit:][:digit:][:digit:][:digit:]")
df_text = data.frame(years=d_text[which(d_text!="NA"&d_text%in%df$year)]) %>% 
  mutate(position_in_text = row_number())
head(df_text)
ggplot(df_text)+
  #geom_tile(aes(y=years, x=position_in_text), fill = "#17005E")+
  geom_point(aes(y=years, x=position_in_text), colour = "#17005E", size = 1)+
  theme_bw()+
  coord_fixed(20)+
  ggtitle("Year of publication of the articles referenced in Chapter 2 of the 6th IPCC report\n and their position in the text of the chapter")
            