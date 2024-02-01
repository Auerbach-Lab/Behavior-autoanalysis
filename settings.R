user_settings = list(
  # The folder where the project subfolders are:
  # Note that the slashes have to be / (not \)
  # MOVED TO UNDERGRADUATE.R FOR EASE OF USE
  # projects_folder = "Z:/Behavior-autoanalysis/",

  # Number of runs per day (probably equals number of active rats)
  runs_per_day = 48,

  # Minimum number of blocks of trials to keep (vs. discard day) not counting the
  # 1st block which will be dropped
  min_blocks = 5,

  # Sensitivity cutoff for determining hearing thresholds
  TH_cutoff = 1.5,

  # Oddball wait interval times (slc) in ms
  oddball_wait_min = 500,
  oddball_wait_max = 1000,

  # Delay window default
  delay_default = "1 4",
  delay_oddball = "1 2",

  # Cutoffs for performance warnings
  minimum_trials = list(
    "Training - Octave" = 264, #?
    "Octave" = 264, #?
    
    "Duration Testing" = 240,

    "Training - Tone" = 50, #?
    "Tone (Standard)" = 240,
    "Tone (Thresholding)" = 240,
    "Tone (Single)" = 240,

    "Training - BBN" = 50,
    "BBN (Standard)" = 240,
    "BBN Mixed Duration" = 330,

    "Training - Gap" = 50,
    "Gap (Standard)" = 240,
    "Gap Mixed Duration" = 330,

    "Training - Oddball" = 240,
    "Oddball (Standard)" = 300,
    "Oddball (Uneven Odds)" = 300,
    "Oddball (Uneven Odds & Catch)" = 300,
    "Oddball (Catch)" = 300,
    "Oddball (Background)" = 300),

  minimum_hit_percent = 0.850,
  maximum_FA_percent = 0.200,
  maximum_attempts_per_trial = 2.0,

  # FA cutoff for analysis
  FA_cutoff = .40,

  # weight change as percent of bodyweight
  maximum_weight_change_daily_percent = 0.05,
  maximum_weight_change_overall_percent = 0.15,

  # positions for the supporting elements in the supervisor spreadsheet
  dynamic_list_length = 7,
  dynamic_col = 56,
  config_row = 1,
  config_col = 64
)
