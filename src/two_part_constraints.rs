use rand::Rng;

use music_tools::pitch::{PitchClassSpace, PitchConverter, PitchSpace, PitchClassOctave};
use music_tools::scale::Scale;
use music_tools::interval::{Quality, ChromaticInterval, Interval};
use music_tools::pitch::chromatic::ChromaticPitchClassSpace as PC;
type Note = PitchClassOctave<PC>;

pub trait TwoPartConstraint {
    fn is_active(&self, _prefix: &Vec<Note>, _other: &Vec<Note>) -> bool {
        true
    }
    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        scale: &Scale<PC>,
    ) -> bool;
}

pub struct LimitNoteInterval(pub i32, pub Vec<usize>, pub Vec<usize>);
impl TwoPartConstraint for LimitNoteInterval {
    fn is_active(&self, prefix: &Vec<Note>, other: &Vec<Note>) -> bool {
        if self.0 == 0 {
            prefix.len() == 0
        } else {
            prefix.len() + 1 == other.len()
        }
    }

    fn is_valid(
        &self,
        _prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        let idx = if self.0 == 0 { 0 } else { other.len() - 1 };
        let interval = ChromaticInterval::new(&other[idx], choice).0;
        if &other[idx] <= choice {
            // counterpoint above
            if !self.1.contains(&interval) {
                return false;
            }
        } else {
            // counterpoint below
            if !self.2.contains(&interval) {
                return false;
            }
        }
        true
    }
}

pub struct PenultimateSimultaneity;
impl TwoPartConstraint for PenultimateSimultaneity {
    fn is_active(&self, prefix: &Vec<Note>, other: &Vec<Note>) -> bool {
        prefix.len() + 2 == other.len()
    }

    fn is_valid(
        &self,
        _prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        let interval = ChromaticInterval::new(&other[other.len() - 2], choice).0;
        [3, 6].contains(&interval)
    }
}

pub struct LimitInterval(pub Vec<ChromaticInterval>);
impl TwoPartConstraint for LimitInterval {
    fn is_active(&self, prefix: &Vec<Note>, other: &Vec<Note>) -> bool {
        prefix.len() > 0 && prefix.len() + 1 < other.len()
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        let interval = ChromaticInterval::new(&other[prefix.len()], choice);
        self.0.contains(&interval)
    }
}

pub struct NoCrossing;
impl TwoPartConstraint for NoCrossing {
    fn is_active(&self, prefix: &Vec<Note>, other: &Vec<Note>) -> bool {
        prefix.len() > 1 && prefix.len() + 1 < other.len()
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        let initial = prefix[1] > other[1];
        (choice > &other[prefix.len()]) == initial
    }
}

pub struct NoParallelPerfects;
impl TwoPartConstraint for NoParallelPerfects {
    fn is_active(&self, prefix: &Vec<Note>, _other: &Vec<Note>) -> bool {
        prefix.len() > 0
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        let initial_p = ChromaticInterval::new(&prefix[prefix.len() - 1], &other[prefix.len() - 1]).1 == Quality::Perfect;
        let initial_n =
            ChromaticInterval::new(&prefix[prefix.len() - 1], &other[prefix.len() - 1]).0;
        if initial_p {
            let next_n = ChromaticInterval::new(choice, &other[prefix.len()]).0;
            next_n != initial_n
        } else {
            true
        }
    }
}

pub struct NoAntiParallelPerfects;
impl TwoPartConstraint for NoAntiParallelPerfects {
    fn is_active(&self, prefix: &Vec<Note>, _other: &Vec<Note>) -> bool {
        prefix.len() > 0
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        let initial_p = ChromaticInterval::new(&prefix[prefix.len() - 1], &other[prefix.len() - 1]).1 == Quality::Perfect;
        if initial_p {
            let motion_a = &prefix[prefix.len() - 1] > choice;
            let motion_b = other[prefix.len() - 1] > other[prefix.len()];
            let initial_n = ChromaticInterval::new(
                &prefix[prefix.len() - 1],
                &other[prefix.len() - 1],
            ).0 % 8;
            let next_n = ChromaticInterval::new(choice, &other[prefix.len()]).0 % 8;
            motion_a != motion_b && next_n != initial_n
        } else {
            true
        }
    }
}

pub struct NoHiddenPerfects;
impl TwoPartConstraint for NoHiddenPerfects {
    fn is_active(&self, prefix: &Vec<Note>, _other: &Vec<Note>) -> bool {
        prefix.len() > 0
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        if ChromaticInterval::new(choice, &other[prefix.len()]).1 == Quality::Perfect {
            let motion_a = &prefix[prefix.len() - 1] > choice;
            let motion_b = other[prefix.len() - 1] > other[prefix.len()];
            motion_a != motion_b
        } else {
            true
        }
    }
}

pub struct LimitIntervalChains(pub usize, pub usize);
impl TwoPartConstraint for LimitIntervalChains {
    fn is_active(&self, prefix: &Vec<Note>, _other: &Vec<Note>) -> bool {
        prefix.len() > self.0 - 1
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        let interval = ChromaticInterval::new(choice, &other[prefix.len()]).0;
        if self.1 == interval {
            for i in 0..self.0 - 1 {
                let int = ChromaticInterval::new(
                    &prefix[prefix.len() - (i + 1)],
                    &other[prefix.len() - 1],
                ).0;
                if int != interval {
                    return true;
                }
            }
            false
        } else {
            true
        }
    }
}

pub struct PreferContraryMotion;
impl TwoPartConstraint for PreferContraryMotion {
    fn is_active(&self, prefix: &Vec<Note>, _other: &Vec<Note>) -> bool {
        prefix.len() > 1
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        _scale: &Scale<PC>,
    ) -> bool {
        // TODO: more granularity. Have opinions about oblique motion
        let a_motion = &prefix[prefix.len() - 2] > choice;
        let b_motion = &other[prefix.len() - 1] > &other[prefix.len()];
        if a_motion != b_motion {
            rand::thread_rng().gen::<f64>() > 0.2
        } else {
            rand::thread_rng().gen::<f64>() > 0.8
        }
    }
}

pub struct NoSimultaniousLeaps;
impl TwoPartConstraint for NoSimultaniousLeaps {
    fn is_active(&self, prefix: &Vec<Note>, _other: &Vec<Note>) -> bool {
        prefix.len() > 0
    }

    fn is_valid(
        &self,
        prefix: &Vec<Note>,
        other: &Vec<Note>,
        choice: &Note,
        scale: &Scale<PC>,
    ) -> bool {
        let is_skip = !scale.is_step(&prefix[prefix.len() - 1], &choice) && &prefix[prefix.len() - 1] != choice;
        if is_skip {
            let is_skip = !scale.is_step(&other[prefix.len()], &other[prefix.len() - 1]) && &other[prefix.len() - 1] != &other[prefix.len() - 1];
            !is_skip
        } else {
            true
        }
    }
}
