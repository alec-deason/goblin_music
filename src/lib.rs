extern crate pitch_calc as pitch;
extern crate portaudio;
extern crate rand;
extern crate sample;
extern crate synth;
extern crate music_tools;

mod single_part_constraints;
mod two_part_constraints;

use crate::single_part_constraints as cf;
use crate::two_part_constraints as cp;

use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::thread;

use rand::prelude::*;
use rand::Rng;

use synth::dynamic;

use music_tools::pitch::{PitchClassSpace, PitchConverter, PitchSpace, PitchClassOctave};
use music_tools::pitch::chromatic::ChromaticPitchClassSpace as PC;
use music_tools::pitch::midi::MIDIPitchSpace as PS;
use music_tools::scale::{Scale, IntervalPattern};
use music_tools::interval::{ChromaticInterval, Quality};

type Note = PitchClassOctave<PC>;


#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum InstrumentName {
    TootHorn,
    BassHorn,
    TapStick,
}

type CantusFirmusRules = Vec<Box<dyn cf::SinglePartConstraint>>;
fn minimal_cantus_firmus_rules() -> CantusFirmusRules {
    vec![
        Box::new(cf::NoteIsTonic(0)),  // first note
        Box::new(cf::NoteIsTonic(-1)), // last note
        Box::new(cf::ConstrainRange(10)),
        Box::new(cf::ClimaxNearTheMiddle),
        Box::new(cf::ConstrainIntervals(vec![
            ChromaticInterval(4, Quality::Perfect),
            ChromaticInterval(5, Quality::Perfect),
            ChromaticInterval(8, Quality::Perfect),
            ChromaticInterval(2, Quality::Major),
            ChromaticInterval(2, Quality::Minor),
            ChromaticInterval(3, Quality::Major),
            ChromaticInterval(4, Quality::Minor),
            ChromaticInterval(6, Quality::Minor),
        ])),
        Box::new(cf::FinalApprochedByAStep),
        Box::new(cf::ConstrainRepeats(4)),
        Box::new(cf::ConstrainContinuousSlopes(5)),
        Box::new(cf::NoConsecutiveSkips),
        Box::new(cf::LargeLeapsSurroundedByContraryMotion(3)),
        // TODO: Avoid tritones
        // TODO: An outlining of a seventh is avoided within a single line moving in the same direction
    ]
}

fn cantus_firmus_maker(scale: &Scale<PC>, len: usize) -> Option<Vec<(Note, f64, f64)>> {
    let rules = minimal_cantus_firmus_rules();

    fn rec_next_note(
        prefix: &Vec<Note>,
        scale: &Scale<PC>,
        target_len: usize,
        rules: &CantusFirmusRules,
        current_degree: i32,
    ) -> Option<Vec<Note>> {
        let mut rng = rand::thread_rng();
        let max_jump = (scale.len() as f32 * 1.0) as i32;
        let mut weighted: Vec<(i32, i32)> = (-max_jump..max_jump)
            .map(|j| (j, rng.gen_range(0, ((max_jump + 1) - j.abs()) * 10000)))
            .collect();
        weighted.sort_unstable_by(|(_, wa), (_, wb)| wb.partial_cmp(wa).unwrap());
        let mut jumps: Vec<i32> = weighted.iter().map(|(v, _)| *v).collect();
        while jumps.len() > 0 {
            let new_degree = current_degree + jumps.pop().unwrap();
            let choice = scale.note_from_degree(new_degree);

            if rules
                .iter()
                .filter(|r| r.is_active(prefix, target_len))
                .all(|r| r.is_valid(prefix, &choice, scale, target_len))
            {
                let mut new_prefix = prefix.clone();
                new_prefix.push(choice);
                if new_prefix.len() < target_len {
                    let result = rec_next_note(&new_prefix, scale, target_len, rules, new_degree);
                    if result.is_some() {
                        return result;
                    }
                } else {
                    return Some(new_prefix);
                }
            }
        }
        None
    }

    if let Some(mut melody) = rec_next_note(&Vec::new(), &scale, len, &rules, scale.len() as i32 *4) {
        Some(melody.drain(..).map(|p| (p, 4.0, 1.0)).collect())
    } else {
        None
    }
}

fn first_species_counterpoint(
    melody: &Vec<(Note, f64, f64)>,
    scale: &Scale<PC>,
) -> Option<Vec<(Note, f64, f64)>> {
    let true_melody: Vec<Note> = melody.iter().map(|(p, _, _)| *p).collect();

    type Rules = Vec<Box<dyn cp::TwoPartConstraint>>;
    let rules: Rules = vec![
        Box::new(cp::LimitNoteInterval(0, vec![1, 5, 8], vec![1, 8])),
        Box::new(cp::LimitNoteInterval(-1, vec![1, 8], vec![1, 8])),
        Box::new(cp::PenultimateSimultaneity),
        Box::new(cp::LimitInterval(vec![
            ChromaticInterval(3, Quality::Major),
            ChromaticInterval(3, Quality::Minor),
            ChromaticInterval(5, Quality::Diminished),
            ChromaticInterval(5, Quality::Perfect),
            ChromaticInterval(6, Quality::Major),
            ChromaticInterval(6, Quality::Minor),
            ChromaticInterval(8, Quality::Perfect),
            ChromaticInterval(10, Quality::Minor),
            ChromaticInterval(10, Quality::Minor),
        ])),
        Box::new(cp::NoCrossing),
        Box::new(cp::NoParallelPerfects),
        Box::new(cp::NoAntiParallelPerfects),
        Box::new(cp::NoHiddenPerfects),
        Box::new(cp::LimitIntervalChains(3, 3)),
        Box::new(cp::LimitIntervalChains(3, 6)),
        Box::new(cp::PreferContraryMotion),
        Box::new(cp::NoSimultaniousLeaps),
        // TODO: Thirds and sixths predominate in exercises, with fifths and octaves thoughtfully deployed for variety.
        // TODO: Avoid a cross relation against the leading tone in minor.
        // TODO: It is conducive to independence when the climax of the counterpoint does not coincide with that of the cantus
    ];

    let len = melody.len();
    fn rec_next_note(
        prefix: &Vec<Note>,
        melody: &Vec<Note>,
        scale: &Scale<PC>,
        target_len: usize,
        rules: &Rules,
        current_degree: i32,
    ) -> Option<Vec<Note>> {
        let is_final = prefix.len() + 1 == target_len;

        let mut rng = rand::thread_rng();
        let max_jump = (scale.len() as f32 * 1.0) as i32;
        let mut weighted: Vec<(i32, i32)> = (-max_jump..max_jump)
            .map(|j| (j, rng.gen_range(0, ((max_jump + 1) - j.abs()) * 10000)))
            .collect();
        weighted.sort_unstable_by(|(_, wa), (_, wb)| wb.partial_cmp(wa).unwrap());
        let mut jumps: Vec<i32> = weighted.iter().map(|(v, _)| *v).collect();
        while jumps.len() > 0 {
            let new_degree = current_degree + jumps.pop().unwrap();
            let choice = scale.note_from_degree(new_degree);

            if rules
                .iter()
                .filter(|r| r.is_active(prefix, melody))
                .all(|r| r.is_valid(prefix, &melody, &choice, scale))
            {
                let mut new_prefix = prefix.clone();
                new_prefix.push(choice);
                if !is_final {
                    let result = rec_next_note(&new_prefix, melody, scale, target_len, rules, new_degree);
                    if result.is_some() {
                        return result;
                    }
                } else {
                    return Some(new_prefix.to_vec());
                }
            }
        }
        None
    }

    let initial_degree = scale.degree_from_note(&true_melody[0].0).unwrap();
    let initial_degree = initial_degree as i32 + true_melody[0].1 * scale.len() as i32;
    if let Some(mut melody) = rec_next_note(&Vec::new(), &true_melody, scale, len, &rules, initial_degree) {
        Some(melody.drain(..).map(|p| (p, 4.0, 1.0)).collect())
    } else {
        None
    }
}

pub struct Composer {
}

impl Composer {
    pub fn new() -> Composer {
        Composer {
        }
    }

    fn launch_worker(self, channel: SyncSender<(f64, InstrumentName, Note, f32, f64)>) {
        thread::spawn(move || {
            let mut rng = rand::thread_rng();
            let beat_duration = 60.0 / 180.0;

            let s = Scale::new(
                *[PC::from_str("A").unwrap(), PC::from_str("B").unwrap(), PC::from_str("C").unwrap(), PC::from_str("D").unwrap(), PC::from_str("E").unwrap(), PC::from_str("F").unwrap(), PC::from_str("G").unwrap()].choose(&mut rng).unwrap(),
                *[IntervalPattern::Major, IntervalPattern::Minor].choose(&mut rng).unwrap()
            );
			let len = rand::thread_rng().gen_range(9, 15);
            let mut melody_part = match cantus_firmus_maker(&s, len) {
                Some(part) => part,
                None => {
                    eprintln!("bad scale: {:?}", s);
                    panic!();
                }
            };
            let mut counterpoint_part = first_species_counterpoint(&melody_part, &s);
            while counterpoint_part.is_none() {
                melody_part = cantus_firmus_maker(&s, len).unwrap();
                counterpoint_part = first_species_counterpoint(&melody_part, &s);
            }
            let counterpoint_part = counterpoint_part.unwrap();
            eprintln!("cantus firmus: {:?}", melody_part.iter().map(|n| n.0).collect::<Vec<Note>>());
            eprintln!("counterpoint : {:?}", counterpoint_part.iter().map(|n| n.0).collect::<Vec<Note>>());
            let mut parts = vec![
                (0.0, melody_part, InstrumentName::TootHorn),
                (0.0, counterpoint_part, InstrumentName::BassHorn),
            ];
            let mut notes: usize = parts.iter().map(|(_, p, _)| p.len()).sum();

            loop {
                if notes == 0 {
                    let next_start = parts[0].0 + 16.0;
					let len = rand::thread_rng().gen_range(9, 15);
                    let mut melody_part = cantus_firmus_maker(&s, len).unwrap();
                    let mut counterpoint_part = first_species_counterpoint(&melody_part, &s);
                    while counterpoint_part.is_none() {
                        melody_part = cantus_firmus_maker(&s, len).unwrap();
                        counterpoint_part = first_species_counterpoint(&melody_part, &s);
                    }
                    let counterpoint_part = counterpoint_part.unwrap();
                    eprintln!("cantus firmus: {:?}", melody_part.iter().map(|n| n.0).collect::<Vec<Note>>());
                    eprintln!("counterpoint : {:?}", counterpoint_part.iter().map(|n| n.0).collect::<Vec<Note>>());
                    parts = vec![
                        (next_start, melody_part, InstrumentName::TootHorn),
                        (next_start, counterpoint_part, InstrumentName::BassHorn),
                    ];
                    notes = parts.iter().map(|(_, p, _)| p.len()).sum();
                }
                let mut min_time = parts[0].0;
                let mut next = 0;
                for i in 1..parts.len() {
                    let part_next = parts[i].0;
                    if part_next < min_time {
                        next = i;
                        min_time = part_next;
                    }
                }
                let (pitch, duration, velocity) = parts[next].1[0];
                let instrument = parts[next].2;
                parts[next].0 += duration;
                parts[next].1.rotate_left(1);
                channel
                    .send((
                        min_time * beat_duration,
                        instrument,
                        pitch,
                        velocity as f32,
                        duration * 0.75 * beat_duration,
                    ))
                    .unwrap();
                notes -= 1;
            }
        });
    }
}

enum NoteEvent {
    On(f64, InstrumentName, Note, f32),
    Off(f64, InstrumentName, Note),
}

impl NoteEvent {
    fn time(&self) -> f64 {
        match self {
            &NoteEvent::On(time, _, _, _) => time,
            &NoteEvent::Off(time, _, _) => time,
        }
    }
}

impl Eq for NoteEvent {}
impl PartialEq for NoteEvent {
    fn eq(&self, other: &NoteEvent) -> bool {
        match (self, other) {
            (&NoteEvent::On(_, _, _, _), &NoteEvent::Off(_, _, _)) => false,
            (&NoteEvent::Off(_, _, _), &NoteEvent::On(_, _, _, _)) => false,
            (
                &NoteEvent::Off(a_time, a_instrument, a_pitch),
                &NoteEvent::Off(b_time, b_instrument, b_pitch),
            ) => a_time == b_time && a_instrument == b_instrument && a_pitch == b_pitch,
            (
                &NoteEvent::On(a_time, a_instrument, a_pitch, a_velocity),
                &NoteEvent::On(b_time, b_instrument, b_pitch, b_velocity),
            ) => {
                a_time == b_time
                    && a_instrument == b_instrument
                    && a_pitch == b_pitch
                    && a_velocity == b_velocity
            }
        }
    }
}

impl Ord for NoteEvent {
    fn cmp(&self, other: &NoteEvent) -> Ordering {
        ((self.time() * -1_000_000.0) as i64).cmp(&((other.time() * -1_000_000.0) as i64))
    }
}

impl PartialOrd for NoteEvent {
    fn partial_cmp(&self, other: &NoteEvent) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn toot_horn() -> dynamic::Synth {
    dynamic::Synth::dynamic_legato()
        .oscillator(dynamic::oscillator::new())
        .loop_points(0.0, 1.0)
        .fade(10.0, 300.0)
        .num_voices(1)
        //     .spread(0.2)
        //   .detune(0.2)
        .volume(0.25)
}

fn tap_stick() -> dynamic::Synth {
    let oscillator = dynamic::Oscillator::new(
        dynamic::oscillator::Waveform::Square,
        //dynamic::oscillator::Amplitude::Envelope(amp_env),
        dynamic::oscillator::Amplitude::Constant(0.7),
        dynamic::oscillator::Frequency::Hz(PS::to_frequency(&PS::to_pitch(&PitchClassOctave(PC::from_str("C").unwrap(), 1))) as f64),
        dynamic::oscillator::FreqWarp::None,
    );
    let oscillator2 = dynamic::Oscillator::new(
        dynamic::oscillator::Waveform::Noise,
        dynamic::oscillator::Amplitude::Constant(0.7),
        dynamic::oscillator::Frequency::Hz(PS::to_frequency(&PS::to_pitch(&PitchClassOctave(PC::from_str("C").unwrap(), 1))) as f64),
        dynamic::oscillator::FreqWarp::None,
    );

    dynamic::Synth::dynamic_retrigger()
        .oscillator(oscillator)
        .oscillator(oscillator2)
        .duration(2000.0)
        .loop_points(0.0, 1.0)
        .fade(10.0, 10.0)
        .volume(0.1)
}

pub struct Performer {
    synths: HashMap<InstrumentName, dynamic::Synth>,
    current_time: f64,
    pending_events: BinaryHeap<NoteEvent>,
    note_source: Receiver<(f64, InstrumentName, Note, f32, f64)>,
}

impl Performer {
    pub fn new(composer: Composer) -> Performer {
        let mut synths = HashMap::new();
        synths.insert(InstrumentName::TootHorn, toot_horn());
        synths.insert(InstrumentName::BassHorn, toot_horn());
        synths.insert(InstrumentName::TapStick, tap_stick());

        let (sender, receiver) = sync_channel(100);
        composer.launch_worker(sender);

        let mut p = Performer {
            synths,
            current_time: 0.0,
            pending_events: BinaryHeap::new(),
            note_source: receiver,
        };
        p.fill_notes();
        p
    }

    pub fn is_complete(&self) -> bool {
        false
    }

    fn fill_notes(&mut self) {
        for _ in 0..10 {
            let (onset, instrument, pitch, velocity, duration) = self.note_source.recv().unwrap();
            self.pending_events
                .push(NoteEvent::On(onset, instrument, pitch.clone(), velocity));
            self.pending_events
                .push(NoteEvent::Off(onset + duration, instrument, pitch));
        }
    }

    pub fn fill_slice(&mut self, buffer: &mut [[f32; 2]], hz: f64) {
        let duration = buffer.len() as f64 / hz;
        let end_time = self.current_time + duration;

        let mut current_sample = 0;
        while current_sample < buffer.len() {
            let next_event_time = self
                .pending_events
                .peek()
                .map_or(end_time + 1.0, |e| e.time());
            if next_event_time <= self.current_time {
                // Trigger event
                let event = self.pending_events.pop().unwrap();
                let (onset, instrument, pitch, velocity, duration) =
                    self.note_source.recv().unwrap();
                self.pending_events
                    .push(NoteEvent::On(onset, instrument, pitch.clone(), velocity));
                self.pending_events
                    .push(NoteEvent::Off(onset + duration, instrument, pitch));
                match event {
                    NoteEvent::On(_, instrument, pitch, velocity) => {
                        self.synths
                            .get_mut(&instrument)
                            .unwrap()
                            .note_on(PS::to_frequency(&PS::to_pitch(&pitch)), velocity);
                    }
                    NoteEvent::Off(_, instrument, pitch) => {
                        self.synths.get_mut(&instrument).unwrap().note_off(PS::to_frequency(&PS::to_pitch(&pitch)));
                    }
                };
            } else {
                // move forward
                let target = next_event_time.min(end_time);
                let gap = target - self.current_time;
                let samples = (gap * hz).ceil() as usize;
                let samples = samples.min(buffer.len() - current_sample);
                for synth in self.synths.values_mut() {
                    synth.fill_slice(&mut buffer[current_sample..current_sample + samples], hz);
                }
                current_sample += samples;
                self.current_time += gap;
            }
        }
    }
}

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        #[ignore]
        fn cantus_firmus_examples() {
      	    let scale = Scale::new(PC::from_str("C").unwrap(), IntervalPattern::Major);
            let cantus = [0, 1, -2, 0, 1, 3, 2, 1, 0];
            let rules = minimal_cantus_firmus_rules();


            let mut prefix = Vec::new();
            for degree in &cantus {
                let note = scale.note_from_degree(*degree);
				let choice = PitchClassOctave(note.0, note.1);
				for rule in rules.iter().filter(|r| r.is_active(&prefix, cantus.len())) {
					assert!(rule.is_valid(&prefix, &choice, &scale, cantus.len()), "{:?} {:?} {:?}", prefix, choice, rule);
				}
				prefix.push(choice);
			}
        }

        #[test]
        fn basic_cantus_firmus() {
            for tonic in &[PC::from_str("A").unwrap(), PC::from_str("B").unwrap(), PC::from_str("C").unwrap(), PC::from_str("D").unwrap(), PC::from_str("E").unwrap(), PC::from_str("F").unwrap(), PC::from_str("G").unwrap()] {
                for pattern in &[IntervalPattern::Major, IntervalPattern::Minor] {
      			  let scale = Scale::new(*tonic, *pattern);
      			  assert!(cantus_firmus_maker(&scale, 10).is_some(), "{:?}", scale);
                }
             }
        }
    }
