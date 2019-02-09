use std::collections::HashMap;

use pitch::LetterOctave;

use super::scale::Scale;

pub trait SinglePartConstraint {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { true }
    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool;
}

pub trait TwoPartConstraint {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { true }
    fn is_valid(&self, prefix: &Vec<LetterOctave>, other: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale,  target_length: usize) -> bool;
}

pub struct NoteIsTonic(pub i32);
impl SinglePartConstraint for NoteIsTonic {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        if self.0 == 0 {
            prefix.len() == 0
        } else {
            prefix.len() + 1 == target_length
        }
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
        if let Some(degree) = scale.degree_from_note(&choice.letter()) {
            degree == 0
        } else { false }
    }
}

pub struct ClimaxNearTheMiddle;
impl SinglePartConstraint for ClimaxNearTheMiddle {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        let intro_end = target_length / 3;
        let middle_end = target_length - intro_end;
        prefix.len() > middle_end
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
        let intro_end = target_length / 3;
        let middle_end = target_length - intro_end;
        let central_max = prefix[intro_end-1..middle_end+1].iter().max().unwrap();
        let climax_count:usize = prefix.iter().map(|n| if n >= central_max { 1 } else { 0 }).sum();
        let climax_count = climax_count + if choice >= central_max { 1 } else { 0 };
        climax_count == 1
    }
}

pub struct ConstrainRange(pub usize);
impl SinglePartConstraint for ConstrainRange {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        prefix.len() > 1
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
      let min = prefix.iter().min().unwrap().min(choice);
      let max = prefix.iter().max().unwrap().max(choice);
      let interval = super::calculate_interval_number(min, max);
      interval <= self.0
    }
}

pub struct ConstrainIntervals(pub Vec<super::Interval>);
impl SinglePartConstraint for ConstrainIntervals {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        prefix.len() > 1
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
        let interval = super::calculate_interval(&prefix[prefix.len()-1], choice);
        self.0.contains(&interval)
    }
}

pub struct ConstrainRepeats(pub usize);
impl SinglePartConstraint for ConstrainRepeats {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        prefix.len() > 1
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
          let mut counts = HashMap::with_capacity(prefix.len()+1);
        for note in prefix {
            *counts.entry(note).or_insert(0) += 1;
        }
        *counts.entry(choice).or_insert(0) += 1;
        counts.values().max().unwrap() < &self.0
    }
}

pub struct ConstrainContinuousSlopes(pub usize);
impl SinglePartConstraint for ConstrainContinuousSlopes {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        prefix.len() > self.0 - 1
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
        let slope = prefix[prefix.len() - 1].cmp(choice);
        !(2..self.0).all(|i| prefix[prefix.len() - i].cmp(&prefix[prefix.len() - (i-1)]) == slope)
    }
}

pub struct FinalApprochedByAStep;
impl SinglePartConstraint for FinalApprochedByAStep {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        prefix.len() + 1 == target_length
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
        let a = scale.degree_from_note(&prefix[prefix.len() - 1].letter()).unwrap();
        let b = scale.degree_from_note(&choice.letter()).unwrap();
        // This assumes there are no large intervals in the scale. Probably a better approach
        // somewhere
        let step = (a as i32 - b as i32).abs();
        super::calculate_interval_semitones(&prefix[prefix.len() - 1], choice) < 5 && step != 1 && step != scale.len() as i32 - 1
    }
}

pub struct NoConsecutiveSkips;
impl SinglePartConstraint for NoConsecutiveSkips {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        prefix.len() > 1
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
        let a = scale.degree_from_note(&prefix[prefix.len() - 2].letter()).unwrap();
        let b = scale.degree_from_note(&prefix[prefix.len() - 1].letter()).unwrap();
        let step = (a as i32 - b as i32).abs();
        let first_step = super::calculate_interval_semitones(&prefix[prefix.len() - 2], &prefix[prefix.len() - 1]) > 5 || (step != 1 && step != scale.len() as i32 - 1);

        let a = scale.degree_from_note(&prefix[prefix.len() - 1].letter()).unwrap();
        let b = scale.degree_from_note(&choice.letter()).unwrap();
        let step = (a as i32 - b as i32).abs();
        let second_step = super::calculate_interval_semitones(&prefix[prefix.len() - 1], choice) > 5 || (step != 1 && step != scale.len() as i32 - 1);

        !(first_step && second_step)
    }
}


pub struct LargeLeapsFollowedByContraryMotion(pub usize);
impl SinglePartConstraint for LargeLeapsFollowedByContraryMotion {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        prefix.len() > 1
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
        let front_leap = super::calculate_interval_number(&prefix[prefix.len()-2], &prefix[prefix.len()-1]);
        let back_leap = super::calculate_interval_number(&prefix[prefix.len()-1], choice);
        if front_leap > self.0 || back_leap > self.0 {
            if &prefix[prefix.len()-2] > &prefix[prefix.len()-1] {
                if &prefix[prefix.len()-1] >= choice {
                    return false;
                }
            } else {
                if &prefix[prefix.len()-1] <= choice {
                    return false;
                }
            }
        }
        true
    }
}

pub struct AscendingSixthFollowedByFall;
impl SinglePartConstraint for AscendingSixthFollowedByFall {
    fn is_active(&self, prefix: &Vec<LetterOctave>, target_length: usize) -> bool { 
        prefix.len() > 1
    }

    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Scale, target_length: usize) -> bool {
        let interval = super::calculate_interval(&prefix[prefix.len()-2], &prefix[prefix.len()-1]);
        match interval {
            super::Interval::MinorSixth => {
               if &prefix[prefix.len()-1] <= choice {
                   return false
               }
            },
            _ => (),
        };
        true
    }
}
