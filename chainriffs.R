# Set up ------------------------------------------------------------------
# set working directory
setwd("R Projects/MIDI")

# initiate libraries
library(tidyverse)
library(tuneR)
library(tidytext)
library(markovchain)

# find midi tracks
midi <- list.files(path = paste0(getwd(), '/forbidden knowledge'),
                   pattern="*.mid")

# check which songs you're loading
cat(midi, sep = '\n')

# import midi tracks and convert to single dataframe
songs <- midi %>%
  paste0(paste0(getwd(), '/forbidden knowledge/'), .) %>%
  map_df(
    ~readMidi(.) %>%
      as_tibble() %>%
      getMidiNotes()
  )

rm(midi)

# markov setup ------------------------------------------------------------
# append note duration to pitch and set as note 'id'
temp <- songs %>%
  unite(id, note, length, sep = 'x')

# generate transition matrix
chain <- markovchainFit(temp$id)


# create empty vector to store the RIFFS
riffs <- NULL
#number of notes per riff
n <- 16
# set seed for consistent results
set.seed(666)

# generate 1000 new RIFFS
for(i in 1:10){
  riffs <- c(riffs, 
             c(paste(markovchainSequence(n=n, markovchain=chain$estimate), collapse=' '))) 
}

# Check out the first few
head(riffs)

#translate back to table format
riffs <- tibble(notes = riffs) %>%
  rowid_to_column('riff') %>%
  separate(notes, 
           c(paste0('note_', LETTERS[1:n])),
           sep = ' ') %>%
  gather(id, info, -riff) %>%
  separate(info, 
           c('note', 'length'),
           sep = 'x') %>%
  mutate(notename = notenames(as.integer(note) + 3)) %>%
  arrange(riff, id)

# i don't know how to convert 'riffs' back to MIDI...
# this is so dumb im sorry
