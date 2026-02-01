# Generate 909-style synthesized drum samples
# These are approximations of the classic TR-909 sound

library(tuneR)

SAMPLE_RATE <- 44100L
BIT_DEPTH <- 32L

# Helper: create wave from samples
make_wave <- function(samples, sample_rate = SAMPLE_RATE) {
  tuneR::Wave(
    left = as.numeric(samples),
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    pcm = FALSE
  )
}

# Helper: normalize wave to peak = 0.9
normalize_wave <- function(wave, peak = 0.9) {
  samples <- wave@left
  max_val <- max(abs(samples))
  if (max_val > 0) {
    samples <- samples * (peak / max_val)
  }
  tuneR::Wave(left = samples, samp.rate = wave@samp.rate, bit = wave@bit, pcm = FALSE)
}

# ============================================================
# 909 KICK - Low sine sweep (150Hz -> 50Hz) with exponential decay
# ============================================================
generate_909_kick <- function(duration_sec = 0.5) {
  n_samples <- as.integer(duration_sec * SAMPLE_RATE)
  t <- seq(0, duration_sec, length.out = n_samples)

  # Pitch envelope: start at 150Hz, decay to 50Hz
  freq_start <- 150
  freq_end <- 50
  pitch_decay <- 30  # Fast pitch decay
  freq <- freq_end + (freq_start - freq_end) * exp(-pitch_decay * t)

  # Generate sine wave with pitch sweep
  phase <- cumsum(freq) / SAMPLE_RATE
  sine <- sin(2 * pi * phase)

  # Amplitude envelope: punchy attack, exponential decay
  amp_decay <- 8
  envelope <- exp(-amp_decay * t)

  # Add punch transient
  punch <- exp(-50 * t) * 0.5

  samples <- (sine * envelope) + punch * sin(2 * pi * 100 * t)

  # Apply soft clipping for warmth
  samples <- tanh(samples * 1.5) / 1.2

  make_wave(samples)
}

# ============================================================
# 909 SNARE - White noise + sine tone, bandpassed, fast decay
# ============================================================
generate_909_snare <- function(duration_sec = 0.3) {
  n_samples <- as.integer(duration_sec * SAMPLE_RATE)
  t <- seq(0, duration_sec, length.out = n_samples)

  # Noise component
  noise <- rnorm(n_samples)

  # Simple bandpass via highpass + lowpass
  # Highpass at ~200Hz
  hp_alpha <- 200 / (200 + SAMPLE_RATE / (2 * pi))
  hp_out <- numeric(n_samples)
  hp_prev <- 0
  for (i in seq_len(n_samples)) {
    hp_out[i] <- (1 - hp_alpha) * hp_prev + (1 - hp_alpha) * (noise[i] - if(i > 1) noise[i-1] else 0)
    hp_prev <- hp_out[i]
  }

  # Lowpass at ~8000Hz
  lp_alpha <- 8000 / (8000 + SAMPLE_RATE / (2 * pi))
  lp_out <- numeric(n_samples)
  for (i in seq_len(n_samples)) {
    lp_out[i] <- lp_alpha * hp_out[i] + (1 - lp_alpha) * (if(i > 1) lp_out[i-1] else 0)
  }

  # Tone component (sine around 180Hz for body)
  tone <- sin(2 * pi * 180 * t)

  # Envelopes
  noise_env <- exp(-15 * t)
  tone_env <- exp(-20 * t)

  # Mix
  samples <- lp_out * noise_env * 0.7 + tone * tone_env * 0.3

  make_wave(samples)
}

# ============================================================
# 909 CLAP - Layered noise bursts with reverb-like tail
# ============================================================
generate_909_clap <- function(duration_sec = 0.35) {
  n_samples <- as.integer(duration_sec * SAMPLE_RATE)
  t <- seq(0, duration_sec, length.out = n_samples)

  samples <- numeric(n_samples)

  # Multiple short noise bursts (clap "hand stack" effect)
  burst_times <- c(0, 0.008, 0.016, 0.025)  # 4 bursts
  burst_duration <- 0.015

  for (burst_start in burst_times) {
    burst_start_sample <- as.integer(burst_start * SAMPLE_RATE) + 1
    burst_end_sample <- min(as.integer((burst_start + burst_duration) * SAMPLE_RATE), n_samples)

    if (burst_start_sample < n_samples && burst_end_sample > burst_start_sample) {
      burst_length <- burst_end_sample - burst_start_sample
      burst <- rnorm(burst_length)

      # Quick decay envelope for each burst
      burst_t <- seq(0, burst_duration, length.out = burst_length)
      burst_env <- exp(-80 * burst_t)

      samples[burst_start_sample:(burst_end_sample-1)] <- samples[burst_start_sample:(burst_end_sample-1)] + burst * burst_env
    }
  }

  # Bandpass filter the result
  lp_alpha <- 3000 / (3000 + SAMPLE_RATE / (2 * pi))
  filtered <- numeric(n_samples)
  for (i in seq_len(n_samples)) {
    filtered[i] <- lp_alpha * samples[i] + (1 - lp_alpha) * (if(i > 1) filtered[i-1] else 0)
  }

  # Reverb-like tail (filtered noise decay)
  tail_noise <- rnorm(n_samples)
  tail_env <- exp(-12 * t)
  tail <- tail_noise * tail_env * 0.15

  # Lowpass the tail
  tail_filtered <- numeric(n_samples)
  lp_tail <- 2000 / (2000 + SAMPLE_RATE / (2 * pi))
  for (i in seq_len(n_samples)) {
    tail_filtered[i] <- lp_tail * tail[i] + (1 - lp_tail) * (if(i > 1) tail_filtered[i-1] else 0)
  }

  samples <- filtered + tail_filtered

  make_wave(samples)
}

# ============================================================
# 909 HI-HAT CLOSED - High-frequency noise, very short decay
# ============================================================
generate_909_hihat_closed <- function(duration_sec = 0.08) {
  n_samples <- as.integer(duration_sec * SAMPLE_RATE)
  t <- seq(0, duration_sec, length.out = n_samples)

  # White noise
  noise <- rnorm(n_samples)

  # High-pass filter (remove low frequencies)
  hp_alpha <- 6000 / (6000 + SAMPLE_RATE / (2 * pi))
  hp_out <- numeric(n_samples)
  hp_prev_in <- 0
  hp_prev_out <- 0
  for (i in seq_len(n_samples)) {
    hp_out[i] <- hp_alpha * (hp_prev_out + noise[i] - hp_prev_in)
    hp_prev_in <- noise[i]
    hp_prev_out <- hp_out[i]
  }

  # Short, tight envelope
  envelope <- exp(-60 * t)

  samples <- hp_out * envelope

  make_wave(samples)
}

# ============================================================
# 909 HI-HAT OPEN - High-frequency noise, longer decay
# ============================================================
generate_909_hihat_open <- function(duration_sec = 0.4) {
  n_samples <- as.integer(duration_sec * SAMPLE_RATE)
  t <- seq(0, duration_sec, length.out = n_samples)

  # White noise
  noise <- rnorm(n_samples)

  # High-pass filter (remove low frequencies)
  hp_alpha <- 5000 / (5000 + SAMPLE_RATE / (2 * pi))
  hp_out <- numeric(n_samples)
  hp_prev_in <- 0
  hp_prev_out <- 0
  for (i in seq_len(n_samples)) {
    hp_out[i] <- hp_alpha * (hp_prev_out + noise[i] - hp_prev_in)
    hp_prev_in <- noise[i]
    hp_prev_out <- hp_out[i]
  }

  # Longer, more gradual envelope
  envelope <- exp(-8 * t)

  samples <- hp_out * envelope

  make_wave(samples)
}

# ============================================================
# Generate and save all samples
# ============================================================
set.seed(909)  # Deterministic generation

samples_dir <- "inst/samples"

# Generate and save each sample
kick <- normalize_wave(generate_909_kick())
tuneR::writeWave(tuneR::normalize(kick, unit = "16"), file.path(samples_dir, "909_kick.wav"), extensible = FALSE)
cat("Generated: 909_kick.wav\n")

snare <- normalize_wave(generate_909_snare())
tuneR::writeWave(tuneR::normalize(snare, unit = "16"), file.path(samples_dir, "909_snare.wav"), extensible = FALSE)
cat("Generated: 909_snare.wav\n")

clap <- normalize_wave(generate_909_clap())
tuneR::writeWave(tuneR::normalize(clap, unit = "16"), file.path(samples_dir, "909_clap.wav"), extensible = FALSE)
cat("Generated: 909_clap.wav\n")

hihat_closed <- normalize_wave(generate_909_hihat_closed())
tuneR::writeWave(tuneR::normalize(hihat_closed, unit = "16"), file.path(samples_dir, "909_hihat_closed.wav"), extensible = FALSE)
cat("Generated: 909_hihat_closed.wav\n")

hihat_open <- normalize_wave(generate_909_hihat_open())
tuneR::writeWave(tuneR::normalize(hihat_open, unit = "16"), file.path(samples_dir, "909_hihat_open.wav"), extensible = FALSE)
cat("Generated: 909_hihat_open.wav\n")

cat("\nAll 909 samples generated successfully!\n")
