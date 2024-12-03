library(tidyverse)


# Data import -------------------------------------------------------------

proj_data <- read.csv("../Data/Survey Data.csv")
# head(proj_data)
# str(proj_data)



# All variables are characters; coercing them to factor -------------------

for (i in c(1:ncol(proj_data))) {
  proj_data[, i] <- as.factor(proj_data[, i])
}


# Data Re-coding ----------------------------------------------------------

  # Re-coding the responses to the HADS question

proj_data <- proj_data %>% 
  mutate(
    I_feel_tense_or_wound_up_A = case_when(
      I.feel.tense.or..wound.up...A. == 'Most of the time' ~ 3,
      I.feel.tense.or..wound.up...A. == 'A lot of the time' ~ 2,
      I.feel.tense.or..wound.up...A. == 'From time to time, occasionally' ~ 1,
      I.feel.tense.or..wound.up...A. == 'Not at all' ~ 0
    ),
    
    I_feel_as_if_I_am_slowed_down_D = case_when(
      I.feel.as.if.I.am.slowed.down..D. == 'Nearly all the time' ~ 3,
      I.feel.as.if.I.am.slowed.down..D. == 'Very often' ~ 2,
      I.feel.as.if.I.am.slowed.down..D. == 'Sometimes' ~ 1,
      I.feel.as.if.I.am.slowed.down..D. == 'Not at all' ~ 0
    ),
    
    I_still_enjoy_the_things_I_used_to_enjoy_D = case_when(
      I.still.enjoy.the.things.I.used.to.enjoy..D. == 'Definitely as much' ~ 0,
      I.still.enjoy.the.things.I.used.to.enjoy..D. == 'Not quite so much' ~ 1,
      I.still.enjoy.the.things.I.used.to.enjoy..D. == 'Only a little' ~ 2,
      I.still.enjoy.the.things.I.used.to.enjoy..D. == 'Hardly at all' ~ 3
    ),
    
    I_get_a_sort_of_frightened_feeling_like_butterflies_in_the_stomach_A = case_when(
      I.get.a.sort.of.frightened.feeling.like..butterflies..in.the.stomach..A. == 'Not at all' ~ 0,
      I.get.a.sort.of.frightened.feeling.like..butterflies..in.the.stomach..A. == 'Occasionally' ~ 1,
      I.get.a.sort.of.frightened.feeling.like..butterflies..in.the.stomach..A. == 'Quite Often' ~ 2,
      I.get.a.sort.of.frightened.feeling.like..butterflies..in.the.stomach..A. == 'Very Often' ~ 3
    ),
    
    I_get_a_sort_of_frightened_feeling_as_if_something_awful_is_about_to_happen_A = case_when(
      I.get.a.sort.of.frightened.feeling.as.if.something.awful.is.about.to.happen..A. == 
        'Very definitely and quite badly' ~ 3,
      I.get.a.sort.of.frightened.feeling.as.if.something.awful.is.about.to.happen..A. == 
        'Yes, but not too badly' ~ 2,
      I.get.a.sort.of.frightened.feeling.as.if.something.awful.is.about.to.happen..A. == 
        "A little, but it doesn't worry me" ~ 1,
      I.get.a.sort.of.frightened.feeling.as.if.something.awful.is.about.to.happen..A. == 
        'Not at all' ~ 0,
    ),
    
    I_have_lost_interest_in_my_appearance_D = case_when(
      I.have.lost.interest.in.my.appearance..D. == 'Definitely' ~ 3,
      I.have.lost.interest.in.my.appearance..D. == "I don't take as much care as I should" ~ 2,
      I.have.lost.interest.in.my.appearance..D. == 'I may not take quite as much care' ~ 1,
      I.have.lost.interest.in.my.appearance..D. == 'I take just as much care as ever' ~ 0
    ),
    
    I_can_laugh_and_see_the_funny_side_of_things_D = case_when(
      I.can.laugh.and.see.the.funny.side.of.things..D. == 'As much as I always could' ~ 0,
      I.can.laugh.and.see.the.funny.side.of.things..D. == 'Not quite so much now' ~ 1,
      I.can.laugh.and.see.the.funny.side.of.things..D. == 'Definitely not so much now' ~ 2,
      I.can.laugh.and.see.the.funny.side.of.things..D. == 'Not at all' ~ 3
    ),
    
    I_feel_restless_as_I_have_to_be_on_the_move_A = case_when(
      I.feel.restless.as.I.have.to.be.on.the.move..A. == 'Very much indeed' ~ 3,
      I.feel.restless.as.I.have.to.be.on.the.move..A. == 'Quite a lot' ~ 2,
      I.feel.restless.as.I.have.to.be.on.the.move..A. == 'Not very much' ~ 1,
      I.feel.restless.as.I.have.to.be.on.the.move..A. == 'Not at all' ~ 0
    ),
    
    Worrying_thoughts_go_through_my_mind_A = case_when(
      Worrying.thoughts.go.through.my.mind...A. == 'A great deal of the time' ~ 3,
      Worrying.thoughts.go.through.my.mind...A. == 'A lot of the time' ~ 2,
      Worrying.thoughts.go.through.my.mind...A. == 'From time to time, but not too often' ~ 1,
      Worrying.thoughts.go.through.my.mind...A. == 'Only occasionally' ~ 0
    ),
    
    I_look_forward_with_enjoyment_to_things_D = case_when(
      I.look.forward.with.enjoyment.to.things..D. == 'As much as I ever did' ~ 0,
      I.look.forward.with.enjoyment.to.things..D. == 'Rather less than I used to' ~ 1,
      I.look.forward.with.enjoyment.to.things..D. == 'Definitely less than I used to' ~ 2,
      I.look.forward.with.enjoyment.to.things..D. == 'Hardly at all' ~ 3
    ),
    
    I_feel_cheerful_D = case_when(
      I.feel.cheerful..D. == 'Most of the time' ~ 0,
      I.feel.cheerful..D. == 'Sometimes' ~ 1,
      I.feel.cheerful..D. == 'Not often' ~ 2,
      I.feel.cheerful..D. == 'Not at all' ~ 3
    ),
    
    I_get_sudden_feelings_of_panic_A = case_when(
      I.get.sudden.feelings.of.panic..A. == 'Very often indeed' ~ 3,
      I.get.sudden.feelings.of.panic..A. == 'Quite often' ~ 2,
      I.get.sudden.feelings.of.panic..A. == 'Not very often' ~ 1,
      I.get.sudden.feelings.of.panic..A. == 'Not at all' ~ 0
    ),
    
    I_can_sit_at_ease_and_feel_relaxed_A = case_when(
      I.can.sit.at.ease.and.feel.relaxed..A. == 'Definitely' ~ 0,
      I.can.sit.at.ease.and.feel.relaxed..A. == 'Usually' ~ 1,
      I.can.sit.at.ease.and.feel.relaxed..A. == 'Not Often' ~ 2,
      I.can.sit.at.ease.and.feel.relaxed..A. == 'Not at all' ~ 3
    ),
    
    I_can_enjoy_a_good_book_or_radio_or_TV_program_D = case_when(
      I.can.enjoy.a.good.book.or.radio.or.TV.program..D. == 'Very seldom' ~ 3,
      I.can.enjoy.a.good.book.or.radio.or.TV.program..D. == 'Not often' ~ 2,
      I.can.enjoy.a.good.book.or.radio.or.TV.program..D. == 'Sometimes' ~ 1,
      I.can.enjoy.a.good.book.or.radio.or.TV.program..D. == 'Often' ~ 0
    ),
    
    Anxiety_score = I_feel_tense_or_wound_up_A +
      I_get_a_sort_of_frightened_feeling_like_butterflies_in_the_stomach_A +
      I_get_a_sort_of_frightened_feeling_as_if_something_awful_is_about_to_happen_A +
      I_feel_restless_as_I_have_to_be_on_the_move_A + 
      Worrying_thoughts_go_through_my_mind_A +
      I_get_sudden_feelings_of_panic_A +
      I_can_sit_at_ease_and_feel_relaxed_A,
    
    Depression_score = I_feel_as_if_I_am_slowed_down_D +
      I_still_enjoy_the_things_I_used_to_enjoy_D +
      I_have_lost_interest_in_my_appearance_D +
      I_can_laugh_and_see_the_funny_side_of_things_D +
      I_look_forward_with_enjoyment_to_things_D +
      I_feel_cheerful_D +
      I_can_enjoy_a_good_book_or_radio_or_TV_program_D,
    
    Anxiety_class = case_when(
      Anxiety_score < 8 ~ 'Normal',
      Anxiety_score < 11 ~ 'Borderline Abnormal',
      Anxiety_score >= 11 ~ 'Abnormal'
    ),
    
    Depression_class = case_when(
      Depression_score < 8 ~ 'Normal',
      Depression_score < 11 ~ 'Borderline Abnormal',
      Depression_score >= 11 ~ 'Abnormal'
    ),
    
    Anxiety_2_classes = ifelse(Anxiety_class == 'Normal', 'Normal', 'Abnormal'),
    
    Depression_2_classes = ifelse(Depression_class == 'Normal', 'Normal', 'Abnormal')
  )


View(proj_data)

