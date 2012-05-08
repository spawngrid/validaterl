Validaterl
==========

Validaterl is a simple Erlang library for validating (input) data.

Validation Primitives
---------------------

Using records defined in `validaterl/include/validaterl.hrl` one can check individual values against
validators.

For example:

```erlang
>  validaterl:validate(1, #range{ to = 10 })
true
>  validaterl:validate(1, #range{ to = 0 })
greater
````

Also, if the second argument is not a validator, it will be matched against the first argument:

```erlang
>  validaterl:validate(1, 1).
true
>  validaterl:validate(1, 2).
lesser
>  validaterl:validate(1, 0).
greater
```

Custom Validators
-----------------

One can define custom validators using this layout:

```erlang
-record(my_validator, {
                        '$validator' = fun mymodule:myvalidator/2,
                        ... %% rest of arguments
                      }).
```


Validation Sheets
-----------------

Instead of running individual validations, you can define so called "validation sheets" and test them using
`validaterl:validate/1`.

Validation sheet is a list of validations in the following format:

```erlang
{Name :: any(), Value :: any(), Spec :: spec()}
```

For example:

```erlang
[
 {'user.name', Username, #length{ is = #range{ from = 3, to = 16 } }},
 {'user.email', Email, #length{ is = #range { from = 3, to = 255 } }},
 {'user.age', Age, #numericality{ allow_string = true }}
]
```

Just as an example, if you try to put a string with a non-numeric value into Age, you'll get this:

```erlang
[{'user.age',"wrong",
             #numericality{'$validator' = #Fun<validaterl.validate.2>,
                           allow_undefined = false,allow_null = false,
                           allow_string = true,allow_empty = false,allow_rest = false,
                           allow_float = true,default = 0},
             number_expected}]
```