-record(numericality, 
        {
          '$module' = validaterl,
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
          '$module' = validaterl,
          from :: undefined | any(),
          to :: undefined | any(),
          exclusive = false :: boolean()
        }).

-record(format,
        {
          '$module' = validaterl,
          allow_undefined = false :: boolean(),
          allow_null = false :: boolean(),
          allow_empty = false :: boolean(),
          re = ".*" :: string(),
          default = "" :: string()
        }).
