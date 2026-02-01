# RaveR Drum Samples

This directory contains 909-style drum samples used by the raveR package for
deep house drum generation.

## Included Samples

The samples in this directory are **synthesized approximations** of classic
TR-909 drum sounds, generated programmatically for licensing freedom:

| Filename              | Type                | Synthesis Notes                        |
|-----------------------|---------------------|----------------------------------------|
| 909_kick.wav          | Bass drum           | Sine sweep (150->50Hz) + punch transient |
| 909_snare.wav         | Snare drum          | Bandpassed noise + 180Hz sine tone     |
| 909_clap.wav          | Hand clap           | Layered noise bursts + reverb tail     |
| 909_hihat_closed.wav  | Closed hi-hat       | Highpassed noise, tight envelope       |
| 909_hihat_open.wav    | Open hi-hat         | Highpassed noise, gradual decay        |

### Synthesis Details

**Kick (909_kick.wav)**
- Low sine wave with pitch sweep from 150Hz down to 50Hz
- Exponential amplitude decay (~500ms total)
- Added punch transient for attack
- Soft-clipped for analog warmth

**Snare (909_snare.wav)**
- White noise component bandpass filtered (200Hz-8000Hz)
- Sine tone at 180Hz for body
- Fast envelope decay (~300ms)
- Mix: 70% noise, 30% tone

**Clap (909_clap.wav)**
- Multiple noise bursts at 0ms, 8ms, 16ms, 25ms ("hand stack" effect)
- Each burst has ~15ms duration with fast decay
- Bandpass filtered around 3000Hz
- Added reverb-like tail with filtered noise

**Hi-hat Closed (909_hihat_closed.wav)**
- White noise high-passed at 6000Hz
- Very tight envelope (~80ms total)

**Hi-hat Open (909_hihat_open.wav)**
- White noise high-passed at 5000Hz
- Longer decay envelope (~400ms total)

## Regenerating Samples

To regenerate samples (for reproducibility or modification):

```r
source("inst/samples/generate_samples.R")
```

The generation script uses `set.seed(909)` for deterministic output.

## Format Requirements

- **Format:** WAV (uncompressed)
- **Sample rate:** Any rate accepted (will be resampled to 44100 Hz)
- **Bit depth:** Any depth accepted (will be normalized internally)
- **Channels:** Mono or stereo (stereo will be converted to mono)

## Licensing

These synthesized samples are generated programmatically and are part of the
raveR package. They are provided under the same license as the package itself.

For real 909 samples, royalty-free sources include:
- **Drumkito** - https://drumkito.com/
- **BVKER** - https://bfrnd.gumroad.com/l/909pack
- **Samples From Mars** - Occasional free packs
- **Bedroom Producers Blog** - https://bedroomproducersblog.com/

## How Samples Are Loaded

The `raver_load_sample()` function:
1. Reads the WAV file using tuneR
2. Resamples to 44100 Hz if needed
3. Converts stereo to mono if needed
4. Normalizes to 32-bit float format
5. Caches the result for instant subsequent access

If samples are missing, `raver_get_drum_kit()` will return NULL for those
elements and emit a warning. The music generation will fall back to synthesized
drums.
