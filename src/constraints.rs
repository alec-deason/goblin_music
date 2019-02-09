use pitch::LetterOctave;

enum ConstraintResult {
    GloballyValid,
    LocallyValid,
    Invalid,
}

trait SinglePartConstraint {
    fn is_valid(&self, prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Vec<LetterOctave>, target_length: usize) -> ConstraintResult;
}

trait TwoPartConstraint {
    fn is_valid(&self, prefix: &Vec<LetterOctave>, other: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Vec<LetterOctave>,  target_length: usize) -> ConstraintResult;
}
