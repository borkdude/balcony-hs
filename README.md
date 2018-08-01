
# balcony

This program lets me know if I should water my balcony at night.  It collects
temperature data from https://www.weatherbit.io/ during the day into
PostgreSQL. At the end of the day it sends an e-mail with the average
temperature, if it was over 20 degrees Celcius. I could probably take into
account more data like precipitation but this is a good start.

I implemented this idea before in [Clojure and
ClojureScript](https://github.com/borkdude/balcony), but in those versions I
didn't need the database, since I used a historical API which is not free
anymore and I don't want to pay 35 dollars a month for a reminder about watering
my balcony. Since the requirements changed, I decided to do it in Haskell for
fun and to learn something new.

## Installation

### PostgreSQL

Create a new login user (default name `balcony`) and a database (default name
`balcony`) and execute `ddl.sql` with that user.

### Environment

Set the following environment variables. Those with defaults are optional.

    BALCONY_SMTP_HOST (default smtp.gmail.com)
    BALCONY_SMTP_USER
    BALCONY_SMTP_PASS
    BALCONY_DB_NAME (default balcony)
    BALCONY_DB_HOST (default localhost)
    BALCONY_DB_PORT (default 5432)
    BALCONY_DB_PASS (default empty string)
    BALCONY_WEATHER_API_KEY
    BALCONY_MAIL_FROM
    BALCONY_MAIL_TO (comma separated string)

### Building

    stack build
    stack install

### Cron

Trigger the program via cron. I log every hour from 9 to 18 and send an e-mail
at 19.

    crontab -e
    0 9-18 * * * /usr/bin/env bash -c '. $HOME/.profile ; balcony-exe log'
    0 19 * * * /usr/bin/env bash -c '. $HOME/.profile ; balcony-exe mail'
