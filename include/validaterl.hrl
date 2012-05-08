-define(DEFAULT_VALIDATE, fun validaterl:validate/2).

-record(numericality, 
        {
          '$module' = ?DEFAULT_VALIDATE,
          allow_undefined = false :: boolean(),
          allow_null = false :: boolean(),
          allow_string = false :: boolean(),
          allow_empty = false :: boolean(),
          allow_rest = false :: boolean(),
          allow_float = true :: boolean(),
          default = 0 :: number()
        }).

-record(range,
        {
          '$module' = ?DEFAULT_VALIDATE,
          from :: undefined | any(),
          to :: undefined | any(),
          exclusive = false :: boolean()
        }).

-record(format,
        {
          '$module' = ?DEFAULT_VALIDATE,
          allow_undefined = false :: boolean(),
          allow_null = false :: boolean(),
          allow_empty = false :: boolean(),
          re = ".*" :: string(),
          default = "" :: string()
        }).
