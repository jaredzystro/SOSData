library(officer)
doc <- read_docx("/Users/jaredzystro/Dropbox/Jared/OSA/Active Projects/StateOfOrganicSeed2018/InteractiveReport/SOSInteractiveSurvey/NORA19 Convenience Codebook for Numerical Data.docx")
content <- docx_summary(doc)
head(content)

question <- content %>% filter(col_span == 5)
question <- question$text 

label <- content %>% filter(cell_id == 3 & row_id == 3)
label <- label$text

label <- label[2:length(label)]

key <- data_frame(question = question, label = label)

write.csv(key, "question_labels.csv")