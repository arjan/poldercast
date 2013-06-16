-define(PROTOCOL_VERSION, 1).

-record(pdc_auth_challenge,
        {
          challenge :: binary()          
        }).

-record(pdc_auth_response,
        {
          response :: binary(),
          protocol_version=?PROTOCOL_VERSION :: integer()
        }).

-record(pdc_closed,
        {
          reason :: binary()
        }).

-record(pdc_error,
        {
          code :: integer(),
          reason :: binary()
        }).
