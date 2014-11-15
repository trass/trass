DurationWeeks   n@Int: #{show n} w
DurationDays    n@Int: #{show n} d
DurationHours   n@Int: #{show n} h
DurationMinutes n@Int: #{show n} m
DurationSeconds n@Int: #{show n} s

FullDurationWeeks   n@Int: #{pluralEn n "1 week" "weeks"}
FullDurationDays    n@Int: #{pluralEn n "1 day" "days"}
FullDurationHours   n@Int: #{pluralEn n "1 hour" "hours"}
FullDurationMinutes n@Int: #{pluralEn n "1 minute" "minutes"}
FullDurationSeconds n@Int: #{pluralEn n "1 second" "seconds"}


