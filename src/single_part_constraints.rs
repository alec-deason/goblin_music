use std::collections::HashMap;
use std::fmt::Debug;

use music_tools::scale::{Scale, IntervalPattern};
use music_tools::pitch::{PitchClassOctave, PitchClassSpace};
use music_tools::interval::{Quality, ChromaticInterval, Interval};

use super::{PC, Note};

pub trait SinglePartConstraint: Debug {
    fn is_active(&self, _prefix: &Vec<Note>, _target_length: usize) -> bool {
        true
    }
    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        scale: &Scale<PC>,
        target_length: usize,
    ) -> bool;
}

#[derive(Debug)]
pub struct NoteIsTonic(pub i32);
impl SinglePartConstraint for NoteIsTonic {
    fn is_active(&self, prefix: &Vec<Note>, target_length: usize) -> bool {
        if self.0 == 0 {
            prefix.len() == 0
        } else {
            prefix.len() + 1 == target_length
        }
    }

    fn is_valid(
        &self,
        _prefix: &Vec<Note>,
        choice: &Note,
        scale: &Scale<PC>,
        _target_length: usize,
    ) -> bool {
        if let Some(degree) = scale.degree_from_note(&choice.0) {
            degree == 0
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub struct ClimaxNearTheMiddle;
impl SinglePartConstraint for ClimaxNearTheMiddle {
    fn is_active(&self, prefix: &Vec<Note>, target_length: usize) -> bool {
        let intro_end = target_length / 3;
        let middle_end = target_length - intro_end;
        prefix.len() >= middle_end
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
        target_length: usize,
    ) -> bool {
        let intro_end = target_length / 3;
        let middle_end = target_length - intro_end;
        let idx = if prefix.len() == middle_end {
            middle_end
        } else {
            middle_end + 1
        };
        let mut central_max = prefix[intro_end..idx].iter().max().unwrap();
        if prefix.len() == middle_end {
            central_max = central_max.max(choice);
        }
        let climax_count: usize = prefix
            .iter()
            .map(|n| if n >= central_max { 1 } else { 0 })
            .sum();
        let climax_count = climax_count + if choice >= central_max { 1 } else { 0 };
        climax_count == 1
    }
}

#[derive(Debug)]
pub struct ConstrainRange(pub usize);
impl SinglePartConstraint for ConstrainRange {
    fn is_active(&self, prefix: &Vec<Note>, _target_length: usize) -> bool {
        prefix.len() > 0
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
        _target_length: usize,
    ) -> bool {
        let min = prefix.iter().min().unwrap().min(choice);
        let max = prefix.iter().max().unwrap().max(choice);
        let interval = ChromaticInterval::new(min, max).0;
        interval <= self.0
    }
}

#[derive(Debug)]
pub struct ConstrainIntervals(pub Vec<ChromaticInterval>);
impl SinglePartConstraint for ConstrainIntervals {
    fn is_active(&self, prefix: &Vec<Note>, _target_length: usize) -> bool {
        prefix.len() > 0
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
        _target_length: usize,
    ) -> bool {
        let interval = ChromaticInterval::new(&prefix[prefix.len() - 1], choice);
        self.0.contains(&interval)
    }
}

#[derive(Debug)]
pub struct ConstrainRepeats(pub usize);
impl SinglePartConstraint for ConstrainRepeats {
    fn is_active(&self, prefix: &Vec<Note>, _target_length: usize) -> bool {
        prefix.len() > 0
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
        _target_length: usize,
    ) -> bool {
        let mut counts = HashMap::with_capacity(prefix.len() + 1);
        for note in prefix {
            *counts.entry(note).or_insert(0) += 1;
        }
        *counts.entry(choice).or_insert(0) += 1;
        counts.values().max().unwrap() < &self.0
    }
}

#[derive(Debug)]
pub struct ConstrainContinuousSlopes(pub usize);
impl SinglePartConstraint for ConstrainContinuousSlopes {
    fn is_active(&self, prefix: &Vec<Note>, _target_length: usize) -> bool {
        prefix.len() > self.0 - 1
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
        _target_length: usize,
    ) -> bool {
        let slope = prefix[prefix.len() - 1].cmp(choice);
        !(2..self.0).all(|i| prefix[prefix.len() - i].cmp(&prefix[prefix.len() - (i - 1)]) == slope)
    }
}

#[derive(Debug)]
pub struct FinalApprochedByAStep;
impl SinglePartConstraint for FinalApprochedByAStep {
    fn is_active(&self, prefix: &Vec<Note>, target_length: usize) -> bool {
        prefix.len() + 1 == target_length
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        scale: &Scale<PC>,
        _target_length: usize,
    ) -> bool {
        scale.is_step(&prefix[prefix.len() - 1], choice)
    }
}

#[derive(Debug)]
pub struct NoConsecutiveSkips;
impl SinglePartConstraint for NoConsecutiveSkips {
    fn is_active(&self, prefix: &Vec<Note>, _target_length: usize) -> bool {
        prefix.len() > 1
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        scale: &Scale<PC>,
        _target_length: usize,
    ) -> bool {
        let a = &prefix[prefix.len() - 2];
        let b = &prefix[prefix.len() - 1];
        let c = choice;

        if a == b || b == c {
            true
        } else {
            scale.is_step(a, b) || scale.is_step(b, c)
        }
    }
}

#[derive(Debug)]
pub struct LargeLeapsSurroundedByContraryMotion(pub usize);
impl SinglePartConstraint for LargeLeapsSurroundedByContraryMotion {
    fn is_active(&self, prefix: &Vec<Note>, _target_length: usize) -> bool {
        prefix.len() > 1
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
        _target_length: usize,
    ) -> bool {
        let leap =
            ChromaticInterval::new(&prefix[prefix.len() - 2], &prefix[prefix.len() - 1]).0;
        let direction = &prefix[prefix.len() - 2] > &prefix[prefix.len() -   1];
        if leap > self.0 {
            if prefix.len() > 2 {
                //Preceded by contrary motion
                if direction == (&prefix[prefix.len() - 3] > &prefix[prefix.len() - 2]) {
                    return false;
                }
            }
            if direction == (&prefix[prefix.len() - 1] > choice) {
                return false;
            }
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pitch::Letter;

    fn run_scale_free_test(
        constraint: &Box<dyn SinglePartConstraint>,
        degrees: &[i32],
        target_length: usize,
        expectation: bool,
    ) {
        for tonic in &[PC::from_str("E").unwrap(), PC::from_str("C").unwrap(), PC::from_str("A").unwrap()] {
            for pattern in &[IntervalPattern::Major, IntervalPattern::Minor] {
                let scale = Scale::new(*tonic, *pattern);
                let mut prefix: Vec<Note> = degrees
                    .iter()
                    .map(|d| {
                        let n = scale.note_from_degree(*d);
                        n
                    })
                    .collect();
                let choice = prefix.pop().unwrap();
                assert!(constraint.is_active(&prefix, target_length));
                assert_eq!(
                    constraint.is_valid(&prefix, &choice, &scale, target_length),
                    expectation,
                    "{:?}",
                    scale
                );
            }
        }
    }

    #[test]
    fn test_note_is_tonic_first_note() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(NoteIsTonic(0));
        assert!(constraint.is_active(&vec![], 10));
        assert!(!constraint.is_active(&vec![PitchClassOctave(PC::from_str("E").unwrap(), 1)], 10));
        run_scale_free_test(&constraint, &[0], 10, true);
        run_scale_free_test(&constraint, &[1], 10, false);
    }

    #[test]
    fn test_note_is_tonic_last_note() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(NoteIsTonic(-1));
        assert!(constraint.is_active(&vec![PitchClassOctave(PC::from_str("E").unwrap(), 1)], 2));
        assert!(!constraint.is_active(&vec![], 2));
        run_scale_free_test(&constraint, &[0, 0, 0], 3, true);
        run_scale_free_test(&constraint, &[0, 0, 1], 3, false);
    }

    #[test]
    fn test_climax_near_the_middle() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(ClimaxNearTheMiddle);
        assert!(!constraint.is_active(&vec![], 10));
        run_scale_free_test(&constraint, &[0, 1, 0], 3, true);
        run_scale_free_test(&constraint, &[0, -1, 0], 3, false);
        run_scale_free_test(&constraint, &[0, 1, 2, 3, 4, 5, 4, 3, 2], 10, true);
        run_scale_free_test(&constraint, &[0, 1, 2, 3, 4, 5, 5, 3, 2], 10, false);
    }

    #[test]
    fn test_constrain_range() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(ConstrainRange(10));
        assert!(!constraint.is_active(&vec![], 10));
        run_scale_free_test(&constraint, &[0, 0], 9, true);
        run_scale_free_test(&constraint, &[0, 20], 9, false);
        run_scale_free_test(&constraint, &[0, 8], 9, true);
        run_scale_free_test(&constraint, &[0, -20], 9, false);
        run_scale_free_test(&constraint, &[0, -8], 9, true);
    }

    #[test]
    fn test_constrain_intervals() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(ConstrainIntervals(vec![
            ChromaticInterval(8, Quality::Perfect),
            ChromaticInterval(3, Quality::Major),
        ]));
        assert!(!constraint.is_active(&vec![], 10));
        //TODO
    }

    #[test]
    fn test_constrain_repeats() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(ConstrainRepeats(3));
        assert!(!constraint.is_active(&vec![], 10));
        run_scale_free_test(&constraint, &[0, 0], 9, true);
        run_scale_free_test(&constraint, &[0, 1, 0, 0], 9, false);
        run_scale_free_test(&constraint, &[0, 0, 0, 0], 9, false);
    }

    #[test]
    fn test_constrain_slopes() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(ConstrainContinuousSlopes(3));
        assert!(!constraint.is_active(&vec![], 10));
        run_scale_free_test(&constraint, &[0, 1, 2, 1], 9, true);
        run_scale_free_test(&constraint, &[0, 1, 2, 3], 9, false);
        run_scale_free_test(&constraint, &[0, 1, 2, 3], 9, false);
        run_scale_free_test(&constraint, &[0, 1, 2, 1, 0, -1, -2], 9, false);
    }

    #[test]
    fn test_final_approach_by_a_step() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(FinalApprochedByAStep);
        run_scale_free_test(&constraint, &[0, 1, 2, 1], 4, true);
        run_scale_free_test(&constraint, &[0, 1, 3, 1], 4, false);
        run_scale_free_test(&constraint, &[0, 1, 0, -1], 4, true);
        run_scale_free_test(&constraint, &[0, 1, -2, 1], 4, false);
    }

    #[test]
    fn test_consecutive_skips() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(NoConsecutiveSkips);
        run_scale_free_test(&constraint, &[1, 2, 1], 4, true);
        run_scale_free_test(&constraint, &[1, 4, 3], 4, true);
        run_scale_free_test(&constraint, &[1, 4, 1], 4, false);
        run_scale_free_test(&constraint, &[1, 4, 6], 4, false);
    }

    #[test]
    fn test_large_leaps() {
        let constraint: Box<dyn SinglePartConstraint> = Box::new(LargeLeapsSurroundedByContraryMotion(4));
        run_scale_free_test(&constraint, &[1, 7, 1], 4, true);
        run_scale_free_test(&constraint, &[1, 7, 8], 4, false);
        run_scale_free_test(&constraint, &[1, 2, 3], 4, true);

        run_scale_free_test(&constraint, &[0, 1, 2, 3], 4, true);
        run_scale_free_test(&constraint, &[0, -1, 7, 3], 4, true);
        run_scale_free_test(&constraint, &[0, 1, 7, 3], 4, false);
    }
}
