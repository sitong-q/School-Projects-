

#1)first section is read all email_file_name


library(stringr)
#read file name -- complete 
###First read the file name from each directory and then read the content of email from each file name 
setwd('/Users/ssta/Desktop/SpamAssassinTrain')
email_file_name <- unlist(lapply(c('/Users/ssta/Desktop/SpamAssassinTrain/easy_ham',
                                   '/Users/ssta/Desktop/SpamAssassinTrain/easy_ham_2',
                                   '/Users/ssta/Desktop/SpamAssassinTrain/hard_ham',
                                   '/Users/ssta/Desktop/SpamAssassinTrain/spam',
                                   '/Users/ssta/Desktop/SpamAssassinTrain/spam_2'),list.files,full.names = T)) #Give a name of directory from OH 10.19

#email_data_whole <- lapply(unlist_email_file_name,readLines)
View(email_file_name)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#2)Check if it was actually a email : result file[4865] is not email, delete it from the email_file_name
# get the idea from professor's office hour 

#email_file_name_fix
###Seven types of start #1) From,        regular expression : ^From .*[0-9]{4}$
                        #2) Return-Path, regular expression : ^Return-Path:.*$
                        #3) Forwarded,   regular expression : ^Forwarded:.*[0-9]{4}$
                        #4) Replied,     regular expression : ^Replied:.*[0-9]{4}$
                        #5) Received,    regular expression : ^Received:.*$ -- have two types, .com and ])
                        #6) Delivered,   regular expression : ^Delivered:.*$
                        #7) X-Status,    regular expression : ^X-Status:.*$

first <- sapply(email_file_name, readLines, 1)
table(grepl('^From .* [0-9]{4}$|^Return-Path:.*$|^Forwarded:.*[0-9]{4}$|^Replied:.*[0-9]{4}$|^Received:.*$|^Delivered-To.*org$|X-Status:.*$',first))
w = grepl('^From .* [0-9]{4}$|^Return-Path:.*$|^Forwarded:.*[0-9]{4}$|^Replied:.*[0-9]{4}$|^Received:.*$|^Delivered-To.*org$|X-Status:.*$',first)
head(first[!w])

#/Users/ssta/Desktop/SpamAssassinTrain/spam/0000.7b1b73cf36cf9dbc3d64e3f2ee2b91f1 -- not a email at tall 

no_email <- grepl('/Users/ssta/Desktop/SpamAssassinTrain/spam/0000.7b1b73cf36cf9dbc3d64e3f2ee2b91f1',email_file_name)
no_email_index <- match(TRUE, no_email) #4865
email_file_name_fix <- email_file_name[-4865]

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#3)read_email,get_header,get_body2,get_attachment2
#get the idea from professor's office hour but do the detailed code by myself 

#read email
read_email <- function(file){                
  email = readLines(file) 
}     

#get_header
get_header <- function(filename){
  email = readLines(filename)
  Headerend <- match(TRUE, grepl('^\\s*?$', email))#locate the first line start with empty space
  
  if(length(Headerend )>0){
    header <- email[1:(Headerend-1)]                  #from the first line to the empty space is the header
    con = textConnection(header)          
    header_value <- read.dcf(con,all = TRUE)          #have it in a data-frame, name-value form, get this code from professor office hour
  }
  else{header_value = NA}
  return(header_value)
}

#get_body2
#combined the situation happened in the attachment, with header code. 
#if email has attachment, body = email - header - attachment 
#if email does not have attachment, body = email -header 
get_body2 <- function(filename){
  email = readLines(filename)
  Headerend <- which(grepl('^\\s*?$', email) == TRUE)[1]#locate the first line start with empty space
  header <- email[1:(Headerend-1)]                  #from the first line to the empty space is the header
  header_value <- read.dcf(textConnection(header),all = TRUE)  #have it in a data-frame, name-value form
  
  if(length(header_value$`Content-Type`) > 0){
    split_content_type <- strsplit(header_value$`Content-Type`, ';\n|;', useBytes = T)
    boundary_exsit <- grep('boundary',split_content_type)
    
    if(length(boundary_exsit) > 0){
      
      x <- gsub("^.*boundary=\\\\|^.*boundary=","", split_content_type)           #get rid of specical character with just boundary left, get the idea from piazza, but write code myself
      boundaryline <- regmatches(x, regexec('"(.*?)\\\\', x))[[1]][2]             #xtract between two special character
      
      if(is.na(boundaryline) == TRUE){
        appleline = sub('\\").*', "", x)              #apple string here, because the boundary type is different than the normal, thus, create a if function to solve sepcial type 
        index_apple_line = grep(appleline, email)
        attachment_1_start = index_apple_line[2]      #check position for nomral boundary, since first appeared in header, want the second one.
        last_attahment_end = index_apple_line[length(index_apple_line)]
        body = email[-c((1:(Headerend-1)),(attachment_1_start:last_attahment_end))]
        
      }else{
        index_boundary_line <- agrep(boundaryline, email)
        attachment_1_start <- index_boundary_line[2]       #check position for nomral boundary, since first appeared in header, want the second one.
        last_attahment_end <- index_boundary_line[length(index_boundary_line)]
        body = email[-c((1:(Headerend-1)),(attachment_1_start:last_attahment_end))]
      }
      if(is.na(email[(attachment_1_start+1):(last_attahment_end-1)]) == TRUE){
        body = email[-c((1:(Headerend-1)),(attachment_1_start+1))]
      }
      else{body = email[-c((1:(Headerend-1)),(attachment_1_start:last_attahment_end))]}
    }
    
    else if(length(boundary_exsit) == 0){body = email[-(1:(Headerend-1))]}
    
  }
  
  else if(length(header_value$`Content-Type`) == 0){
    
    index_boundary_line_multi = agrep("--=Multipart Boundary",email) #strange boundary type agrep so that it allows some modification
    
    if(length(index_boundary_line_multi) == 0){
      
      body = email[-(1:(Headerend-1))]
      
    }
    
    
  }else if(length(index_boundary_line_multi) > 0){
    
    attachment_1_start = index_boundary_line_multi[1]
    last_attahment_end = index_boundary_line_multi[length(index_boundary_line_multi)]
    body = email[-c((1:(Headerend-1)),(attachment_1_start:last_attahment_end))]
    
  }
  
  return(body)
  
}



#get_attachment2
#first find if content-type exsit
  #if it exsit then detect if boundary addresed in it
      #if boundary addresed in it, extract the particular string after boundary 
        #work out the position 
        #extract attachment between the second to the last(since first it in the content type)
      #if boundary not addresed in it, return NA

#second else if ontent-type exsit not exsit 
  # if it exsit multipart boundary in the email, 
      #work out the position 
      #extract attachment between the first to the last(since no content type, first appearance is the start of attachment)
  # if it exsit multipart boundary in the email = false, return (NA), 
      
# added cases for not having the ending boundary line in each subcases

get_attachment2 <- function(filename){
  email = readLines(filename)
  Headerend <- which(grepl('^\\s*?$', email) == TRUE)[1]#locate the first line start with empty space
  header <- email[1:(Headerend-1)]                  #from the first line to the empty space is the header
  header_value <- read.dcf(textConnection(header),all = TRUE)  #have it in a data-frame, name-value form
  
  if(length(header_value$`Content-Type`) > 0){
    split_content_type <- strsplit(header_value$`Content-Type`, ';\n|;', useBytes = T)
    boundary_exsit <- grep('boundary',split_content_type)
    
    if(length(boundary_exsit) > 0){
      
      x <- gsub("^.*boundary=\\\\|^.*boundary=","", split_content_type)           #get rid of specical character with just boundary left
      boundaryline <- regmatches(x, regexec('"(.*?)\\\\', x))[[1]][2]             #xtract between two special character-look for online help on stacksflow
      
      if(is.na(boundaryline) == TRUE){
        appleline = sub('\\").*', "", x)                                    #some apple email does not follow the boundary extracting method 
        index_apple_line = grep(appleline, email)
        attachment_1_start = index_apple_line[2]      #check position for nomral boundary, since first appeared in header, want the second one.
        last_attahment_end = index_apple_line[length(index_apple_line)]
        attachment = email[(attachment_1_start+1):(last_attahment_end-1)]
        
      }else{
        index_boundary_line <- agrep(boundaryline, email) #agrep here because there are some slighty changes in some boundaries, 'with or without --'
        attachment_1_start <- index_boundary_line[2]       #check position for nomral boundary, since first appeared in header, want the second one.
        last_attahment_end <- index_boundary_line[length(index_boundary_line)]
        attachment = email[(attachment_1_start+1):(last_attahment_end-1)]
      }
      if(is.na(email[(attachment_1_start+1):(last_attahment_end-1)]) == TRUE){
        attachment = email[(attachment_1_start+1):length(email)]
      }
      else{attachment = email[(attachment_1_start+1):(last_attahment_end-1)]}
    }
    
    else if(length(boundary_exsit) == 0){attachment = NA}
    
  }
  
  else if(length(header_value$`Content-Type`) == 0){
    
    index_boundary_line_multi = agrep("--=Multipart Boundary",email)#strange boundary type
    
    if(length(index_boundary_line_multi) == 0){
      attachment = NA
    
    }else if(length(index_boundary_line_multi) > 0){
      
      attachment_1_start = index_boundary_line_multi[1]
      last_attahment_end = index_boundary_line_multi[length(index_boundary_line_multi)]
      attachment = email[(attachment_1_start+1):(last_attahment_end-1)]
    }
    attachment = NA
  }
  
  return(attachment)
  
}



#read_all_email
#have data.fram here since it is easy for later computation. 
read_all_email = function(filenames) { 
  email = sapply(filenames, read_email) 
  HEADER <- sapply(filenames, get_header)
  BODY <- sapply(filenames, get_body2)
  ATTACHMENT <- sapply(filenames, get_attachment2)
  list(email_name = filenames,
       Header = data.frame(HEADER),    #Header = data.frame(HEADER)
       Body = BODY,
       Attachment = ATTACHMENT)
}

EMAIL <- lapply(email_file_name_fix, read_all_email)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#4)Variables
# write function in this way since it can just give the result and easy for debug. 
# major technique used here, if grep can't find the pattern, then it will return integer(0), and the length of integer(0) is 0, thus it can be used to tell if length is 0, then there is no such pattern in the emails.

#IsSpam-1
IsSpam = sapply(1:length(EMAIL),function(i){
  spam_indicator <- grep('spam',EMAIL[[i]][["email_name"]]) #check if spam is in the email_name, if it is then print True, if it not, print False
  if(length(spam_indicator)>0)
    print(TRUE)
  else{
    print(FALSE)
  }
})

#IsRe-2
IsRe = sapply(1:length(EMAIL),function(i){
  Re_indicator <- grep('Re',EMAIL[[i]][["Header"]][[1]][['Subject']]) #check if spam is in the email_name, if it is then print True, if it not, print False
  if(length(Re_indicator)>0)
    print(TRUE)
  else{
    print(FALSE)
  }
})

#NumLinesInBody -3
NumLinesInBody = sapply(1:length(EMAIL),function(i){
  num_lines_in_body <- length(EMAIL[[i]][["Body"]])
  print(num_lines_in_body)
  return(num_lines_in_body)
})

#BodyCharacterCount-4
BodyCharacterCount = sapply(1:length(EMAIL),function(i){
  body_character_count <- sum(as.numeric(nchar(EMAIL[[i]][["Body"]])))
  if(length(body_character_count) > 0)
    print(body_character_count)
})

#SubjectExclamationCount-5
library(stringr)
SubjectExclamationCount = sapply(1:length(EMAIL),function(i){
  subject_exclamation_count <- str_count(EMAIL[[i]][["Header"]][[1]][['Subject']], pattern = '!') #check if ! is in the subject
  if(length(subject_exclamation_count) > 0)
    print(subject_exclamation_count)
  else{
    print(NA)
  }
})

#SubjectQuestCount-6
SubjectQuestCount = sapply(1:length(EMAIL),function(i){
  subject_quest_count <- str_count(EMAIL[[i]][["Header"]][[1]][['Subject']], pattern = '\\?') #check if ? is in the subject
  if(length(subject_quest_count) > 0)
    print(subject_quest_count)
  else{
    print(NA)
  }
})

#replyUnderline-7
ReplyUnderLine <- sapply(1:length(EMAIL),function(i){
  reply_under_line_to <- str_count(EMAIL[[i]][["Header"]][[1]][['Reply-To']], pattern = '_[:graph:]')#_[:graph:] 
  if(length(reply_under_line_to) > 0)
    print(TRUE)
  else{
    print(FALSE)
  }
})

#Priority--8
Priority = sapply(1:length(EMAIL),function(i){
  priority <- agrep('X-Prioirity',names(EMAIL[[i]][["Header"]][[1]])) #check if X-Priority is in the header name, if it is, print Ture, if not print False 
  if(length(priority)>0)
    print(TRUE)
  else{
    print(FALSE)}
})

#IsInReplyTo-9
IsInReplyTo = sapply(1:length(EMAIL),function(i){
  is_in_reply_to <- agrep('In-Reply-To',names(EMAIL[[i]][["Header"]][[1]])) #check if In- is in the header name, if it is, print Ture, if not print False 
  if(length(is_in_reply_to)>0)
    print(TRUE)
  else{
    print(FALSE)}
})

#numDollarSigns-10
NumDollarSigns =  sapply(1:length(EMAIL),function(i){
  num_dollar_signs <- sum(as.numeric(str_count( EMAIL[[i]][["Body"]],"\\$\\d+(?:.(\\d+))?")))        #dollar sign followed by one or more digits with optional deccimal part 
  print(num_dollar_signs)
}) 

#subejctSpamWords-11
SubjectSpamWords = sapply(1:length(EMAIL),function(i){
  subject_spam_words <- grep('viagra|pounds|free|weight|guarantee|millions|dollars|credit|risk|prescription|generic|drug|money back|credit card',EMAIL[[i]][["Header"]][[1]][['Subject']])
  if(length(subject_spam_words)>0)
    print(TRUE)
  else{
    print(FALSE)}
})

#isDear-12
IsDear = sapply(1:length(EMAIL),function(i){
  is_dear <- grep('Dear', EMAIL[[i]][["Body"]] )
  if(length(is_dear) > 0)
    print(TRUE)
  else{
    print(FALSE)}
})

#isYelling-13!
isYelling = sapply(1:length(EMAIL),function(i){
  is_yelling <- grep("[[:lower:]]", EMAIL[[i]][["Header"]][[1]][["Subject"]])  #want to know if subject are all capital letter, is there is one lower case letter, then it is not yelling 
  if(length(is_yelling) == 0)
    print(TRUE)
  else{print(FALSE)}
})

#percentSubjectBlanks-14
PercentSubjectBlanks = sapply(1:length(EMAIL),function(i){
  subject_blanks <- str_count(EMAIL[[i]][["Header"]][[1]][["Subject"]],' ')
  subject_character <- str_count(EMAIL[[i]][["Header"]][[1]][["Subject"]],'')
  subject_blanks <- subject_blanks/subject_character
  percent_subject_blanks <- paste(round(100*subject_blanks,2),'%')
  if(percent_subject_blanks > 0)
    print(percent_subject_blanks)
  else{
    print(NA)}
})

#isOriginalMessage-15
IsOriginalMessage = sapply(1:length(EMAIL),function(i){
  originial_message <- agrep('original message',EMAIL[[i]][["Body"]])
  if (length(originial_message) >0)
    print(TRUE)
  else{
    print(FALSE)}
})

#hourSent-16
hourSent <- sapply(1:length(EMAIL),function(i){
  cut <- str_extract(EMAIL[[i]][["Header"]][[1]][["Date"]],'(\\d{2}):(\\d{2}):(\\d{2})')  #first form, extract 00:00:00
  hour_sent <- strsplit(cut, ':')[[1]][[1]] #take hour out of string 
  cut_2 <- str_extract(EMAIL[[i]][["Header"]][[1]][["Date"]],'(\\d{1}):(\\d{2}):(\\d{2})') #second form, extract 0:00:00
  hour_sent_fix <- strsplit(cut_2, ':')[[1]][[1]]
  if(is.na(hour_sent) == 'TRUE' )           #take hour out of string 
    #hour_sent_fix <- strsplit(cut_2, ':')[[1]][[1]]
    hour_sent <- paste0('0',hour_sent_fix)  #paste 0 to have everything in a consistent forms like make 3 beccome 03 for later convient 
  print(hour_sent)
})

#multipartText-17
#Having integer(0) here because, persumably, not all header have content-type. thus to convert integer(0) to something doable, add length function, if it is 0, then print false. Howver, the question is it may include if header have the content-type section, but don't have boundary, So, further clean it by checking if boundary_indicator is 0 
MultipartText <- sapply(1:length(EMAIL),function(i){
  boundary_indicator <- str_count(EMAIL[[i]][["Header"]][[1]][["Content-Type"]], "boundary") 
  if(length(boundary_indicator) == 0) 
    print(FALSE)
  else if(boundary_indicator == 0){
    print(FALSE)}
  else{print(TRUE)}
})

#isPGPsigned-18
IsPGPsigned <- sapply(1:length(EMAIL),function(i){
  is_PGP_signed_indicator <- str_count(EMAIL[[i]][["Header"]][[1]][["Content-Type"]], "signed") 
  if(length(is_PGP_signed_indicator) == 0) 
    print(FALSE)
  else if(is_PGP_signed_indicator == 0){
    print(FALSE)}
  else{print(TRUE)}
})

#containsImages-19
containsImages <- sapply(1:length(EMAIL),function(i){
  contains_images <- grep("HTML",EMAIL[[i]][["Body"]]) 
  if(length(contains_images) == 0) 
    print(FALSE)
  else if(contains_images == 0){
    print(FALSE)}
  else{print(TRUE)}
})


#get_all_features 

get_all_features =  data.frame(
  filename = email_file_name_fix,
  IsSpam = IsSpam,                                 # combine the above extracted information into one dataframe.
  IsRe = IsRe,                               
  NumLinesInBody = NumLinesInBody,
  isYelling = isYelling,
  SubjectExclamationCount = SubjectExclamationCount,
  SubjectQuestCount = SubjectQuestCount,
  ReplyUnderLine = ReplyUnderLine,
  Priority = Priority,
  IsInReplyTo = IsInReplyTo,
  NumDollarSigns  = NumDollarSigns, 
  SubjectSpamWords  = SubjectSpamWords,
  IsDear = IsDear,
  PercentSubjectBlanks = PercentSubjectBlanks,
  IsOriginalMessage = IsOriginalMessage,
  hourSent = hourSent,
  MultipartText = MultipartText,
  IsPGPsigned = IsPGPsigned
)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(ggthemes)
#refer to the basic ggplot grammar online 
#search color theme online 
reg <- ggplot(get_all_features, 
            aes(x = IsSpam, 
                fill = isYelling )) + 
  geom_bar(position = "stack")


yelg <- ggplot(get_all_features, 
              aes(x = IsSpam, 
                fill = isYelling )) + 
  geom_bar(position = "stack")

exg <- ggplot(get_all_features, 
        aes(x = IsSpam, y = SubjectExclamationCount)) +  geom_boxplot() +ylim(0,10)

priorityp <- ggplot(get_all_features, 
              aes(x = IsSpam, 
                  fill =  Priority )) + 
  geom_bar(position = "stack")

origp <- ggplot(get_all_features, 
                    aes(x = IsSpam, 
                        fill =  IsOriginalMessage )) + 
  geom_bar(position = "stack")

IsInReplyTop <- ggplot(get_all_features, 
              aes(x = IsSpam, 
                  fill =  IsInReplyTo )) + 
  geom_bar(position = "stack")

pgp <- ggplot(get_all_features, 
            aes(x = IsSpam, y = IsPGPsigned)) +   geom_bar(position = "stack")

MultipartTextp <- ggplot(get_all_features, 
                         aes(x = IsSpam, fill = MultipartText)) +   geom_bar(position = "stack")

dearp <- ggplot(get_all_features, 
              aes(x = IsSpam, 
                  fill =  IsDear))+geom_bar(position = "stack")
dollarsigng <- ggplot(get_all_features, 
                 aes(x = NumDollarSigns, fill =  IsSpam))+geom_bar(position = "stack")+  ggtitle('Number Dollar Sign Count')+
  theme_wsj()+ scale_colour_wsj("colors6") 
SubjectExclamationCountp <- ggplot(get_all_features, 
                      aes(x = SubjectExclamationCount, fill =  IsSpam))+geom_bar(position = "stack") +  ggtitle('Subject Exclamation Count')+
                      theme_wsj()+ scale_colour_wsj("colors6") 
                     
SubjectQuestCountp <- ggplot(get_all_features, 
                      aes(x = SubjectQuestCount, fill =  IsSpam))+geom_bar(position = "stack") +  ggtitle('Subject Quest Count')+theme_wsj()+ scale_colour_wsj("colors6")

table(get_all_features$hourSent=='0NA')
hournoNA <- subset(get_all_features,get_all_features$hourSent!='0NA')

HourCountp <- ggplot(hournoNA, aes(x = hourSent , fill =  IsSpam))+geom_bar(position = "stack") +  ggtitle('Hour Sent Count')+
  theme_wsj()+ scale_colour_wsj("colors6") 

NumLinesInBodyp1 <- ggplot(get_all_features, aes(x = NumLinesInBody , fill =  IsSpam))+geom_bar(position = "stack") +  ggtitle('Number Lines In Body Count 1-150 ')+
  theme_wsj()+ scale_colour_wsj("colors6") + xlim(-1,150) # two part since the data range is large

NumLinesInBodyp2 <- ggplot(get_all_features, aes(x = NumLinesInBody , fill =  IsSpam))+geom_bar(position = "stack") +  ggtitle('Number Lines In Body Count 1-150 ')+
  theme_wsj()+ scale_colour_wsj("colors6") + xlim(150,300)

PercentSubjectBlanksp <-  ggplot(get_all_features, 
                                aes(x = PercentSubjectBlanks, fill =  IsSpam))+geom_bar(position = "stack") +  ggtitle('Subject Quest Count')+theme_wsj()+ scale_colour_wsj("colors6")

Spam <- subset(get_all_features, get_all_features$IsSpam == 'TRUE')
Ham <- subset(get_all_features, get_all_features$IsSpam == 'FALSE')
table(Spam$hourSent)
table(Ham$hourSent)
sort(table(Spam$PercentSubjectBlanks))
sort(table(Ham$PercentSubjectBlanks))



