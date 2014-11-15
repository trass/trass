DurationWeeks   n@Int: #{show n} н
DurationDays    n@Int: #{show n} д
DurationHours   n@Int: #{show n} ч
DurationMinutes n@Int: #{show n} м
DurationSeconds n@Int: #{show n} с

FullDurationWeeks   n@Int: #{pluralRu n "1 неделя"  "недели"  "недель"}
FullDurationDays    n@Int: #{pluralRu n "1 день"    "дня"     "дней"}
FullDurationHours   n@Int: #{pluralRu n "1 час"     "часа"    "часов"}
FullDurationMinutes n@Int: #{pluralRu n "1 минута"  "минуты"  "минут"}
FullDurationSeconds n@Int: #{pluralRu n "1 секунда" "секунды" "секунд"}
