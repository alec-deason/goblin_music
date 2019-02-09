use pitch::Letter;

#[derive(Copy, Clone)]
pub enum IntervalPattern {
    Major,
    Minor,
}

impl IntervalPattern {
    fn len(&self) -> usize {
        match self {
            Major => 7,
            Minor => 7,
        }
    }

    fn interval_pattern(&self) -> Vec<usize> {
        match self {
            IntervalPattern::Major => vec![2, 2, 1, 2, 2, 2, 1],
            IntervalPattern::Minor => vec![2, 1, 2, 2, 1, 2, 2],
        }
    }

    fn semitones_from_tonic(&self, degree: i32) -> i32 {
        // Musical convention is that the first note of the scale
        // is degree 1 but, especially when you want to support
        // negative degrees, that is very hard to think about
        // so I am using zero indexed degrees, convention be
        // damned.
        let direction = if degree < 0 { -1 } else { 1 };
        let mut pattern = self.interval_pattern().clone();
        if direction == -1 {
            pattern.reverse();
        };

        let abs_delta:usize = pattern.iter().cycle().take(degree.abs() as usize).sum();
        abs_delta as i32 * direction
    }
}

pub struct Scale {
    tonic: Letter,
    interval_pattern: IntervalPattern,
}

impl Scale {
    pub fn new(tonic: Letter, interval_pattern: IntervalPattern) -> Scale {
        Scale {
            tonic,
            interval_pattern,
        }
    }

    pub fn len(&self) -> usize {
        self.interval_pattern.len()
    }

    pub fn note_from_degree(&self, degree: i32) -> (Letter, i32) {
        let semitones_delta = self.interval_pattern.semitones_from_tonic(degree);
        let letter = self.tonic + semitones_delta;
        // This assumes equal temperament but I think the pitch library is already
        // assuming that
        let octave_delta = semitones_delta / 12;
        (letter, octave_delta)
    }

    pub fn letters(&self) -> Vec<Letter> {
        (0..self.len()).map(|d| self.note_from_degree(d as i32).0).collect()
    }

    pub fn degree_from_note(&self, note: &Letter) -> Option<usize> {
        // Ignores octave...
        self.letters().iter().position(|l| l == note)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_note_from_degree_positive_degrees() {
        let scale = Scale::new(Letter::C, IntervalPattern::Major);
        for (degree, expected) in &[
            (0, (Letter::C, 0)),
            (1, (Letter::D, 0)),
            (2, (Letter::E, 0)),
            (3, (Letter::F, 0)),
            (4, (Letter::G, 0)),
            (5, (Letter::A, 0)),
            (6, (Letter::B, 0)),
            (7, (Letter::C, 1)),
            (8, (Letter::D, 1)),
            (9, (Letter::E, 1)),
        ] {
            assert_eq!(scale.note_from_degree(*degree), *expected);
        }
    }

    #[test]
    fn test_note_from_degree_negative_degrees() {
        let scale = Scale::new(Letter::A, IntervalPattern::Minor);
        for (degree, expected) in &[
            (0, (Letter::A, 0)),
            (-1, (Letter::G, 0)),
            (-2, (Letter::F, 0)),
            (-3, (Letter::E, 0)),
            (-4, (Letter::D, 0)),
            (-5, (Letter::C, 0)),
            (-6, (Letter::B, 0)),
            (-7, (Letter::A, -1)),
            (-8, (Letter::G, -1)),
            (-9, (Letter::F, -1)),
        ] {
            assert_eq!(scale.note_from_degree(*degree), *expected);
        }
    }

    #[test]
    fn test_letters() {
        let scale = Scale::new(Letter::A, IntervalPattern::Minor);
        assert_eq!(scale.letters(), vec![Letter::A, Letter::B, Letter::C, Letter::D, Letter::E, Letter::F, Letter::G]);
        let scale = Scale::new(Letter::D, IntervalPattern::Major);
        assert_eq!(scale.letters(), vec![Letter::D, Letter::E, Letter::Fsh, Letter::G, Letter::A, Letter::B, Letter::Csh])
    }

    #[test]
    fn test_degree_from_note() {
        let scale = Scale::new(Letter::G, IntervalPattern::Major);
        assert_eq!(scale.degree_from_note(&Letter::G), Some(0));
        assert_eq!(scale.degree_from_note(&Letter::A), Some(1));
        assert_eq!(scale.degree_from_note(&Letter::B), Some(2));
        assert_eq!(scale.degree_from_note(&Letter::Fsh), Some(6));
        assert_eq!(scale.degree_from_note(&Letter::Csh), None);
    }
}
