create table weather (
  _created timestamp default now()::timestamp
  , response jsonb
  , temp numeric(4,1)
)
