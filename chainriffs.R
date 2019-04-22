# Set up ------------------------------------------------------------------
# set working directory
setwd("~/Documents/Projects/MIDI")

# initiate libraries
library(tidyverse)
library(tuneR)
library(tidytext)
library(markovchain)

# read in reference pitch table
pitch.tbl <- read_csv("midi pitch table.csv")

# find midi tracks
midi <- list.files(path = paste0(getwd(), '/forbidden knowledge'),
                   pattern="*.mid")

# check which songs you're loading
cat(midi, sep = '\n')

# import midi tracks and convert to single dataframe
songs <- tibble(filename = midi) %>% 
  mutate(file_contents = map(filename,
                             ~readMidi(paste0(getwd(), '/forbidden knowledge/', .)) %>%
                               as_tibble() %>%
                               getMidiNotes()
  )) %>%
  unnest()

#remove filename obj from workspace
rm(midi)

# TIMING: markov chain --------------------------------------
# remove duplicate rows by time (indicating chords) to get table of note lengths in songs
duration <- songs %>%
  distinct(filename, time, length) %>%
  ungroup()

# generate transition matrix (note that there is no pitch info here)
time.chain <- markovchainFit(duration$length)

#number of notes per riff
n <- 16
# create empty vector to store the RIFFS
timings <- NULL
# set seed for consistent results
set.seed(666)

# generate new RIFFS
for(i in 1:10){
  timings<- c(timings, 
              c(paste(
                markovchainSequence(n = n, markovchain = time.chain$estimate), 
                collapse=' '))) 
}

# Check out the first few
head(timings)

#translate back to table format
timings <- tibble(time = timings) %>%
  rowid_to_column('riff') %>%
  separate(time, 
           c(paste0('note_', LETTERS[1:n])),
           sep = ' ') %>%
  gather(id, length, -riff) %>%
  arrange(riff, id)

# PITCH: markov chain --------------------------------------
# concatenate notes together if occuring at same time (aka, chords!)
pitch <- songs %>%
  group_by(filename, time) %>%
  summarise(pitch = paste0(note, collapse = '_')) %>%
  ungroup()

# generate transition matrix (note that there is no duration info here)
note.chain <- markovchainFit(pitch$pitch)


# create empty vector to store the RIFFS
notes <- NULL
# set seed for consistent results
set.seed(666)

# generate new notes
for(i in 1:10){
  notes <- c(notes, 
             c(paste(
               markovchainSequence(n = n, markovchain = note.chain$estimate), 
               collapse=' '))) 
}

# Check out the first few
head(notes)


# CREATE RIFF TABLE -------------------------------------------------------
#translate back to table format
riffs <- tibble(notes = notes) %>%
  rowid_to_column('riff') %>%
  separate(notes, 
           c(paste0('note_', LETTERS[1:n])),
           sep = ' ') %>%
  gather(id, note, -riff) %>%
  arrange(riff, id) %>%
  left_join(timings) %>% 
  separate(note, 
           c(paste0('', 1:3)), 
           sep = "_", ) %>%
  gather(pitch.id, midi.note, -riff, -id, -length) %>%
  mutate(midi.note = as.numeric(midi.note)) %>%
  left_join(pitch.tbl %>% 
              select(midi.note,
                     name,
                     frequency)) %>%
  filter(!is.na(frequency)) %>%
  gather(info, value, -riff:-pitch.id) %>%
  unite(dummy, info, pitch.id) %>%
  spread(dummy, value, fill = '')

# JUNK --------------------------------------------------------------------


# i don't know how to convert 'riffs' back to MIDI...


#set tempo event 51: microseconds per quarter note

# this is so dumb im sorry
