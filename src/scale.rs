use pitch::Letter;

enum IntervalPattern {
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

struct Scale {
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

    pub fn degree(&self, degree: i32) -> (Letter, i32) {
        let semitones_delta = self.interval_pattern.semitones_from_tonic(degree);
        let letter = self.tonic + semitones_delta;
        // This assumes equal temperament but I think the pitch library is already
        // assuming that
        let octave_delta = semitones_delta / 12;
        (letter, octave_delta)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scale_positive_degrees() {
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
            assert_eq!(scale.degree(*degree), *expected);
        }
    }

    #[test]
    fn test_scale_negative_degrees() {
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
            assert_eq!(scale.degree(*degree), *expected);
        }
    }
}
