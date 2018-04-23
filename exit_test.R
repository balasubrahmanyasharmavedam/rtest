#Question 1

prob = c(sort(rexp(50,25),decreasing = FALSE),sort(rexp(50,25),decreasing = TRUE))
vect = sample(1:100,1000,replace = TRUE,prob = prob)
hist(vect)

#Question 2
facebook = c(100,200)
instagram = c(100,400)
snapchat = c(50,100)
social = data.frame(facebook,instagram,snapchat)
rownames(social) = c('adult','teenagers')

row_total = rowSums(social)
col_tot = colSums(social)
expected_adult = (row_total['adult'] * col_tot) / sum(social)
expected_teenagers = (row_total['teenagers'] * col_tot) / sum(social)
expected_social = t(data.frame(expected_adult,expected_teenagers))
social
expected_social

#Question 3
teams = c('SRH','RCB','MI','CSK','RR','KKR','KXIP','DD')
ipl = sample(teams,60,prob = c(0.3,0.1,0.1,0.2,0.05,0.075,0.025,0.15),replace = TRUE)
ipl[[5]] = NA

ipl_table = table(ipl)
mode_ipl = names(ipl_table[ipl_table == max(ipl_table)])
mode_ipl
sum(is.na(ipl))
ipl[is.na(ipl)] = mode_ipl[[1]]
sum(is.na(ipl))

#Question 4
marks = sample(30:80,43,replace = TRUE)
attendace = sample(40:60,43,replace = TRUE)
interns = data.frame(marks,attendace)

interns['type'] = 'nothing'
interns[interns$attendace > 50 & interns$marks >70,'type'] = 'certificate + stipend'
interns[interns$attendace < 50 & interns$marks >70,'type'] = 'certificate'
interns

#Question 5
library(quantmod)
getSymbols("^NSEI")
NSEI$daily_ret = dailyReturn(NSEI)
NSEI = data.frame(NSEI)
NSEI = na.omit(NSEI)
colnames(NSEI)[1:6] = c('open','high','low','close','volume','adjusted')


count_10pct = function(data)
{
  sum = 0
  count = 0
  for(i in 1:nrow(data))
  {
    if(sum == 0)
    {
      if(data$daily_ret[i]<0)
      {
        start_value = data$close[i]
        sum = data$daily_ret[i]
      }
    }else
    {
      sum = (data$close[i]-start_value)/start_value
      if(sum <= -0.1)
      {
        count = count+1
        sum = 0
      }else if(sum > 0)
      {
        sum = 0
      }
    }
  }
  return(count)
}
count_10pct(data = NSEI)

#Question 6
getSymbols('^GSPC')
GSPC$daily_ret = dailyReturn(GSPC)
GSPC = data.frame(GSPC)
GSPC = na.omit(GSPC)
colnames(GSPC)[1:5] = c('open','high','low','close','volume','adjusted')
indecies = list(NSEI,GSPC)
names(indecies) = c('Nifty','SP500')

lapply(indecies,FUN = count_10pct)
sapply(indecies,FUN = count_10pct)

#Question 8
details_1 = c(100,120)
details_2 = c(150,80)
confusion_matrix = data.frame(details_1,details_2)
colnames(confusion_matrix) = c('pred_0','pred_1')
rownames(confusion_matrix) = c('actual_0','actual_1')
confusion_matrix = as.matrix(confusion_matrix)


accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
recall = confusion_matrix[1,1] / sum(confusion_matrix[1,])
precision = confusion_matrix[1,1] / sum(confusion_matrix[,1])


