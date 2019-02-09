extern crate portaudio;
extern crate sample;

use portaudio as pa;

extern crate goblin_music;

type AudioSample = f32;
type Input = AudioSample;
type Output = AudioSample;

const CHANNELS: i32 = 2;
const FRAMES: u32 = 128;
const SAMPLE_HZ: f64 = 48_000.0;

fn main() {
    run().unwrap()
}

fn run() -> Result<(), pa::Error> {
    let composer = goblin_music::Composer::new();
    let mut performer = goblin_music::Performer::new(composer, SAMPLE_HZ);
    
    // The callback we'll use to pass to the Stream.
    let callback = move |pa::OutputStreamCallbackArgs { buffer, time, .. }| {
        let buffer: &mut [[AudioSample; CHANNELS as usize]] = sample::slice::to_frame_slice_mut(buffer).unwrap();
        sample::slice::equilibrium(buffer);

        performer.fill_slice(buffer, SAMPLE_HZ as f64);

        if performer.is_complete() {
            pa::Complete
        } else {
            pa::Continue
        }
    };

    // Construct PortAudio and the stream.
    let pa = pa::PortAudio::new()?;
    let settings = pa.default_output_stream_settings::<AudioSample>(CHANNELS, SAMPLE_HZ, FRAMES)?;
    let mut stream = pa.open_non_blocking_stream(settings, callback)?;
    stream.start()?;

    // Loop while the stream is active.
    while let Ok(true) = stream.is_active() {
        std::thread::sleep(std::time::Duration::from_millis(16));
    }

    Ok(())
}
