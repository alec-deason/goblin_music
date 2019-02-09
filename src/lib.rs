extern crate pitch_calc as pitch;
extern crate portaudio;
extern crate sample;
extern crate synth;
extern crate rand;

mod constraints;
mod scale;

use std::collections::{BinaryHeap, HashMap};
use std::cmp::Ordering;
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::thread;

use rand::prelude::*;
use rand::Rng;
use rand::distributions::{Normal, Distribution};

use pitch::{Letter, LetterOctave};
use synth::{Point, Synth, Oscillator, oscillator, Envelope};
use synth::dynamic;

#[derive(Debug)]
enum Interval {
    Unison,
    Octave,
    DiminishedFifth,
    PerfectFifth,
    PerfectFourth,
    MajorThird,
    MinorThird,
    MajorSecond,
    MinorSecond,
    MinorSixth,
    MajorSixth,
    MinorSeventh,
    MajorSeventh,
    Other,
}

fn calculate_interval_semitones(a: &LetterOctave, b: &LetterOctave) -> i32 {
    let min = a.min(b);
    let max = a.max(b);
    let d_octave = max.octave() - min.octave();
    let d_note = max.letter() as i32 - min.letter() as i32;
    d_octave * 12 + d_note
}

fn calculate_interval_number(a: &LetterOctave, b: &LetterOctave) -> i32 {
    let semitones = calculate_interval_semitones(a, b);
    semitones / 2 + semitones % 2
}


fn calculate_interval(a: &LetterOctave, b: &LetterOctave) -> Interval {
    let semitones = calculate_interval_semitones(a, b);
    match semitones {
        0 => Interval::Unison,
        1 => Interval::MinorSecond,
        2 => Interval::MajorSecond,
        3 => Interval::MinorThird,
        4 => Interval::MajorThird,
        5 => Interval::PerfectFourth,
        6 => Interval::DiminishedFifth,
        7 => Interval::PerfectFifth,
        8 => Interval::MinorSixth,
        9 => Interval::MajorSixth,
        10 => Interval::MinorSeventh,
        11 => Interval::MajorSeventh,
        12 => Interval::Octave,
        _ => Interval::Other,
    }
}


#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum InstrumentName {
    TootHorn,
    BassHorn,
    TapStick,
}

fn cantus_firmus_maker(scale: &Vec<LetterOctave>) -> Vec<(LetterOctave, f64, f64)> {
    let mut rng = rand::thread_rng();

    let len = 10;//rng.gen_range(8, 15);
    fn rec_next_note(prefix: &Vec<LetterOctave>, scale: &Vec<LetterOctave>, idx: usize, target_len: usize) -> Option<Vec<LetterOctave>> {

        let mut rng = rand::thread_rng();
        let mut weighted:Vec<(i32, i32)> = [-7i32, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7].iter().map(|j| (*j, rng.gen_range(0, (8-j.abs())*10000))).collect();
        weighted.sort_unstable_by(|(_, wa), (_, wb)| wb.partial_cmp(wa).unwrap());
        let mut jumps:Vec<i32> = weighted.iter().map(|(v, _)| *v).collect();
        while jumps.len() > 0 {
            let new_idx = (idx as i32 + jumps.pop().unwrap()).min(scale.len() as i32 -1).max(0) as usize;
            let choice = scale[new_idx];

            if cantus_firmus_rules(&prefix, &choice, target_len, &scale) {
                let mut new_prefix = prefix.clone();
                new_prefix.push(choice);
                if new_prefix.len() < target_len {
                    let result = rec_next_note(&new_prefix, scale, new_idx, target_len);
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



    if let Some(mut melody) = rec_next_note(&Vec::new(), &scale, 7, len) {
        eprintln!("{:?}\n{}", melody, len);
        melody.drain(..).map(|p| (p, 4.0, 1.0)).collect() 
    } else {
        panic!()
    }
}

fn first_species_counterpoint(melody: &Vec<(LetterOctave, f64, f64)>, scale: &Vec<LetterOctave>) -> Vec<(LetterOctave, f64, f64)> {
    let mut rng = rand::thread_rng();
    let true_melody:Vec<LetterOctave> = melody.iter().map(|(p, _, _)| *p).collect();

    let len = melody.len();
    fn rec_next_note(prefix: &Vec<LetterOctave>, melody: &Vec<LetterOctave>, scale: &Vec<LetterOctave>, idx: usize, target_len: usize) -> Option<Vec<LetterOctave>> {
        let is_final = prefix.len() + 1 == target_len;

        let mut rng = rand::thread_rng();
        let mut weighted:Vec<(i32, i32)> = [-7i32, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7].iter().map(|j| (*j, rng.gen_range(0, (8-j.abs())*10000))).collect();
        weighted.sort_unstable_by(|(_, wa), (_, wb)| wb.partial_cmp(wa).unwrap());
        let mut jumps:Vec<i32> = weighted.iter().map(|(v, _)| *v).collect();
        while jumps.len() > 0 {
            let new_idx = (idx as i32 + jumps.pop().unwrap()).min(scale.len() as i32 -1).max(0) as usize;
            let choice = scale[new_idx];

            if first_species_rules(&prefix, &choice, &scale, &melody, 1) {
                let mut new_prefix = prefix.clone();
                new_prefix.push(choice);
                if !is_final {
                    let result = rec_next_note(&new_prefix, melody, scale, new_idx, target_len);
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



    if let Some(mut melody) = rec_next_note(&Vec::new(), &true_melody, scale, rng.gen_range(0, scale.len()), len) {
        melody.drain(..).map(|p| (p, 4.0, 1.0)).collect() 
    } else {
        panic!()
    }
}

fn drums_maker() -> Vec<(LetterOctave, f64, f64)> {
    let drum_accents = vec![0.35, 0.35, 0.35, 1.00];
    let drum_syncopes = vec![0.80, 1.20, 0.80, 1.20];
    let mut notes = Vec::new();
    for i in 0..4 {
        notes.push((LetterOctave(Letter::A, 3), drum_syncopes[i] * 0.5, drum_accents[i]))
    }
    notes
}

fn first_species_rules(prefix: &Vec<LetterOctave>, choice: &LetterOctave, scale: &Vec<LetterOctave>, melody: &Vec<LetterOctave>, stride: usize) -> bool {
    // First interval must be unison, octive or fith
    if prefix.len() == 0 {
        let interval = calculate_interval_number(&melody[0], choice);
        if &melody[0] <= choice {
            if ![1, 5, 8].contains(&interval) {
                return false;
            }
        } else {
            if ![1, 8].contains(&interval) {
                return false;
            }
        }
    }
    
    // ending interval must be unison or octave
    if prefix.len() + 1 == melody.len() {
        let interval = calculate_interval_number(&melody[melody.len()-1], choice);
        if ![1, 8].contains(&interval) {
            return false;
        }
    }

    //The penultimate simultaneity must be an imperfect consonance
    if prefix.len() + 2 == melody.len() {
        let interval = calculate_interval_number(&melody[melody.len()-2], choice);
        if ![3, 6].contains(&interval) {
            return false;
        }
    }
    true
}

fn cantus_firmus_rules(prefix: &Vec<LetterOctave>, choice: &LetterOctave, target_length: usize, scale: &Vec<LetterOctave>) -> bool {
    let is_final = prefix.len() + 1 == target_length;

    // First choice is always valid
    if prefix.len() == 0 {
        return true;
    }
    // End where you started
    if prefix.len() + 1 == target_length {
        if &prefix[0] != choice {
            return false;
        }
    }

    let choice_on_scale = scale.iter().position(|a| a == choice).unwrap() as i32;

    // There should be a climax near the middle
    let intro_end = target_length / 3;
    let middle_end = target_length - intro_end;
    if prefix.len() > middle_end {
        let central_max = prefix[intro_end..middle_end].iter().max().unwrap();
        if central_max <= choice {
            return false;
        }
        let climax_count:usize = prefix.iter().map(|n| if n == central_max { 1 } else { 0 }).sum();
        if climax_count > 1 {
            return false;
        }
    }

    // Total range should be <= a tenth
    let min = prefix.iter().min().unwrap().min(choice);
    let max = prefix.iter().max().unwrap().max(choice);
    let interval = calculate_interval_number(min, max);
    if interval > 10 {
        return false;
    }

    let penultimate_on_scale = scale.iter().position(|a| a == &prefix[prefix.len()-1]).unwrap() as i32;
    // Final must be approached by a step
    if is_final {
        if (choice_on_scale as i32 - penultimate_on_scale as i32).abs() != 1 {
            return false;
        }
    }

    // Permitted melodic intervals are the perfect fourth, fifth, and octave, as well as the major and minor second, major and minor third, and ascending minor sixth.
    let interval = calculate_interval(&prefix[prefix.len()-1], choice);
    let is_permitted = match interval {
        Interval::PerfectFourth => true,
        Interval::PerfectFifth => true,
        Interval::Octave => true,
        Interval::MajorSecond => true,
        Interval::MinorSecond => true,
        Interval::MajorThird => true,
        Interval::MinorThird => true,
        Interval::MinorSixth => {
            &prefix[prefix.len()-1] < choice
        },
        _ => false,
    };
    if !is_permitted {
        return false
    }



    if prefix.len() > 1 {
        // The ascending minor sixth must be immediately followed by motion downwards.
        let interval = calculate_interval(&prefix[prefix.len()-2], &prefix[prefix.len()-1]);
        match interval {
            Interval::MinorSixth => {
               if &prefix[prefix.len()-1] <= choice {
                   return false
               }
            },
            _ => (),
        };

        // No consequitive skips
        let pen_penultimate_on_scale = scale.iter().position(|a| a == &prefix[prefix.len()-2]).unwrap() as i32;
        let first_gap = penultimate_on_scale - pen_penultimate_on_scale;
        let second_gap = choice_on_scale - penultimate_on_scale;
        if first_gap.abs() > 1 && second_gap.abs() > 1 {
            return false;
        }

        // leaps larger than a third should be followed and preceded by motion in the opposite direction
        let front_leap = calculate_interval_number(&prefix[prefix.len()-2], &prefix[prefix.len()-1]);
        let back_leap = calculate_interval_number(&prefix[prefix.len()-1], choice);
        if front_leap > 3 || back_leap > 3 {
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
    }


    // continuous assents and descents should be limited to 5 notes
    if prefix.len() > 5 {
        let initial_direction = prefix[0] > prefix[1];
        let mut current_direction = initial_direction;
        for n in 2..prefix.len() {
            current_direction = prefix[n-1] > prefix[n];
            if current_direction != initial_direction {
                break;
            }
        }
        if current_direction == initial_direction {
            return false;
        }
    }

	// No note should appear more than 3 times
    let mut counts = HashMap::with_capacity(prefix.len()+1);
	for note in prefix {
        *counts.entry(note).or_insert(0) += 1;
	}
    *counts.entry(choice).or_insert(0) += 1;
    if counts.values().max().unwrap() >= &3 {
        return false;
    }

    // TODO: Avoid tritones
    // TODO: An outlining of a seventh is avoided within a single line moving in the same direction

    true
}

pub struct Composer {
    last_drum_hit: f64,
    drum_accents: Vec<f32>,
    drum_syncopes: Vec<f64>,
    last_toot: f64,
}

impl Composer {
    pub fn new() -> Composer {
        Composer {
            last_drum_hit: 0.0,
            drum_accents:  vec![0.35, 0.35, 0.35, 1.00],
            drum_syncopes: vec![0.80, 1.20, 0.80, 1.20],
            last_toot: 0.0,
        }
    }

    fn launch_worker(mut self, channel: SyncSender<(f64, InstrumentName, LetterOctave, f32, f64)>) {
        thread::spawn(move || {
            let mut rng = rand::thread_rng();
            let beat_duration = 60.0 / 180.0;
            let mut pitches = vec![
                LetterOctave(Letter::E, 2),
                LetterOctave(Letter::Fsh, 2),
                LetterOctave(Letter::G, 2),
                LetterOctave(Letter::A, 2),
                LetterOctave(Letter::B, 2),
                LetterOctave(Letter::C, 3),
                LetterOctave(Letter::D, 3),
            ];
            pitches.extend(pitches.clone().iter().map(|p| LetterOctave(p.letter(), p.octave()+1)));
            pitches.push(LetterOctave(Letter::E, 4));

            let melody_part = cantus_firmus_maker(&pitches);
            let counterpoint_part = first_species_counterpoint(&melody_part, &pitches);
            let mut parts = vec![
                (0.0, melody_part, InstrumentName::TootHorn),
                (0.0, counterpoint_part, InstrumentName::BassHorn),
            ];
			let mut notes: usize = parts.iter().map(|(_, p, _)| p.len()).sum();

            loop {
				if notes == 0 {
                    let next_start = parts[0].0 + 16.0;
					let melody_part = cantus_firmus_maker(&pitches);
                    let counterpoint_part = first_species_counterpoint(&melody_part, &pitches);
					parts = vec![
						(next_start, melody_part, InstrumentName::TootHorn),
						(next_start, counterpoint_part, InstrumentName::BassHorn),
					];
					notes = parts.iter().map(|(_, p, _)| p.len()).sum();
				}
                let mut min_time = parts[0].0 + parts[0].1[0].1;
                let mut next = 0;
                for i in 1..parts.len() {
                    let part_next = parts[i].0 + parts[i].1[0].1;
                    if part_next < min_time {
                        next = i;
                        min_time = part_next;
                    }
                }
                let (pitch, duration, velocity) = parts[next].1[0];
                let instrument = parts[next].2;
                parts[next].0 += duration;
                parts[next].1.rotate_left(1);
                channel.send((min_time * beat_duration, instrument, pitch, velocity as f32, duration * 0.75 * beat_duration));
				notes -= 1;
            }
        });
    }
}

enum NoteEvent {
    On(f64, InstrumentName, LetterOctave, f32),
    Off(f64, InstrumentName, LetterOctave),
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
            (&NoteEvent::Off(a_time, a_instrument, a_pitch), &NoteEvent::Off(b_time, b_instrument, b_pitch)) => a_time == b_time && a_instrument == b_instrument && a_pitch == b_pitch,
            (&NoteEvent::On(a_time, a_instrument, a_pitch, a_velocity), &NoteEvent::On(b_time, b_instrument, b_pitch, b_velocity)) => a_time == b_time && a_instrument == b_instrument && a_pitch == b_pitch && a_velocity == b_velocity,
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
        .fade(10.0, 500.0)
        .num_voices(3)
   //     .spread(0.2)
     //   .detune(0.2)
        .volume(0.15)
}

fn tap_stick() -> dynamic::Synth {
    let amp_env = Envelope::from(vec!(
           //         Time ,  Amp ,  Curve
           Point::new(0.0  ,  0.0 ,  0.0),
           Point::new(0.01 ,  1.0 ,  0.0),
           Point::new(0.45 ,  1.0 ,  0.0),
           Point::new(0.81 ,  0.8 ,  0.0),
           Point::new(1.0  ,  0.0 ,  0.0),
       ));

    let oscillator = dynamic::Oscillator::new(
        dynamic::oscillator::Waveform::Square,
        //dynamic::oscillator::Amplitude::Envelope(amp_env),
        dynamic::oscillator::Amplitude::Constant(0.7),
        dynamic::oscillator::Frequency::Hz(LetterOctave(Letter::C, 1).hz() as f64),
        dynamic::oscillator::FreqWarp::None,
    );
    let oscillator2 = dynamic::Oscillator::new(
        dynamic::oscillator::Waveform::Noise,
        dynamic::oscillator::Amplitude::Constant(0.7),
        dynamic::oscillator::Frequency::Hz(LetterOctave(Letter::C, 1).hz() as f64),
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
    note_source: Receiver<(f64, InstrumentName, LetterOctave, f32, f64)>,
}

impl Performer {
    pub fn new(composer: Composer, sample_rate: f64) -> Performer {
        let mut synths = HashMap::new();
        synths.insert(InstrumentName::TootHorn, toot_horn());
        synths.insert(InstrumentName::BassHorn, toot_horn());
        synths.insert(InstrumentName::TapStick, tap_stick());

        let (sender, receiver) = sync_channel(100);
        composer.launch_worker(sender);

        Performer {
            synths,
            current_time: 0.0,
            pending_events: BinaryHeap::new(),
            note_source: receiver,
        }
    }

    pub fn is_complete(&self) -> bool {
        false
    }

    fn fill_notes(&mut self) {
        for _ in 0..50 {
            let (onset, instrument, pitch, velocity, duration) = self.note_source.recv().unwrap();
            self.pending_events.push(NoteEvent::On(onset, instrument, pitch, velocity));
            self.pending_events.push(NoteEvent::Off(onset+duration, instrument, pitch));
        }
    }

    pub fn fill_slice(&mut self, buffer: &mut [[f32; 2]], hz: f64) {
        let duration = buffer.len() as f64 / hz;
        let end_time = self.current_time + duration;

        let mut current_sample = 0;
        while current_sample < buffer.len() {
            if self.pending_events.len() < 10 {
                self.fill_notes();
            }
            let next_event_time = self.pending_events.peek().map_or(end_time + 1.0, |e| e.time());
            if next_event_time <= self.current_time {
                // Trigger event
                let event = self.pending_events.pop().unwrap();
                match event {
                    NoteEvent::On(_, instrument, pitch, velocity) => {
                        self.synths.get_mut(&instrument).unwrap().note_on(pitch, velocity);
                    },
                    NoteEvent::Off(_, instrument, pitch) => {
                        self.synths.get_mut(&instrument).unwrap().note_off(pitch);
                    },
                };
            } else {
                // move forward
                let target = next_event_time.min(end_time);
                let gap = target - self.current_time;
                let samples = (gap*hz).ceil() as usize;
                let samples = samples.min(buffer.len()-current_sample);
                for synth in self.synths.values_mut() {
                    synth.fill_slice(&mut buffer[current_sample..current_sample+samples], hz);
                }
                current_sample += samples;
                self.current_time += gap;
            }
        }
    }
}
