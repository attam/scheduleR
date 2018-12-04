# Load package into workspace.
library(gcalendar)

# Provide credentials:
# - Requires a Google APIs project with OAuth 2.0 access
#   and the Google Calendar API enabled.
creds <- GoogleApiCreds(
  userName = "benji.matta@gmail.com", # An optional hint to
  # simplify login.
  appCreds = "~/Documents/CMH/scheduleR/client_id.json" # Location of the JSON file containing your
  # Google APIs project OAuth client ID and
  # secret. Optionally set to a list with named
  # values for client_id and/or client_secret,
  # and/or provide an appname (separate
  # argument) to fetch omitted values from OS
  # environment variables <appname>_CONSUMER_ID
  # and <appname>_CONSUMER_SECRET.
  # Default for appname is "GOOGLE_APIS".
)

# Get a list of your calendars using the credentials you provided,
# then print a summary.
my_cal_list <- gCalendarLists$new(creds = creds)
calendars <- my_cal_list$summary[c("id", "summary", "description")]
print(calendars)

# Get the chosen calendar (contact birthdays).
birthdays_calendar <- gCalendar$new(
  creds = creds,
  id = "#contacts@group.v.calendar.google.com"
)

# Get the events from this chosen calendar.
birthday_events <- birthdays_calendar$events

# Get a summary of the birthdays and print the results.
birthdays <- birthday_events$summary[c("id", "summary", "start")]
print(birthdays)