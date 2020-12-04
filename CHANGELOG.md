# 1.9.5

- Support `time-1.11.1`
  - Add `NFData CalandarDiffDays` instance

# 1.9.4

- Support `time-1.11`
  - `Data.Time.Calendar.Month`
  - `Data.Time.Calendar.Quarter`
  - Pattern synonyms
  - `parseTimeMultipleM` is not backported
  - `Month` is missing `ParseTime` instance
  - Compat extras:
    - Add `Ix`, `Enum`, `NFData` instances to `Month`, `Quarter`, `QuarterOfYear`,
      `CalendarDiffTime` and `DayOfWeek`.

# 1.9.3

- Include `pastMidnight` and `sinceMidnight` aliases (backported from `time-1.10`)
- Support `time-1.10`
