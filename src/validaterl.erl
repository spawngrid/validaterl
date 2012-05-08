-module(validaterl).
-export([validate/2]).
-include_lib("validaterl/include/validaterl.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(FAILED(X), X).

%% numericality
validate(undefined, #numericality{
           allow_undefined = false
          }) ->
    ?FAILED(undefined_not_allowed);
validate(undefined, #numericality{
           allow_undefined = true,
           default = Default
          } = V) ->
    validate(Default, V);
validate(null, #numericality{
           allow_null = false
          }) ->
    ?FAILED(null_not_allowed);
validate(null, #numericality{
           allow_null = true,
           default = Default
          } = V) ->
    validate(Default, V);
validate(Empty, #numericality{
           allow_string = true,
           allow_empty = false
           }) when Empty == [] orelse Empty == <<>> ->
    ?FAILED(empty_not_allowed);
validate(Empty, #numericality{
           allow_string = true,
           allow_empty = true,
           default = Default
           } = V) when Empty == [] orelse Empty == <<>> ->
    validate(Default, V);
validate(S, #numericality{
           allow_string = false
          }) when is_list(S) orelse is_binary(S) ->
    ?FAILED(string_not_allowed);
validate(S, #numericality{
           allow_string = true,
           allow_rest = Rest
          } = V) when is_list(S) orelse is_binary(S) ->
    Str = to_string(S),
    case string:to_integer(Str) of
        {error, no_integer} ->
            ?FAILED(number_expected);
        {Value, []} ->
           validate(Value, V);
        {Value, _} ->
            case string:to_float(Str) of
                {error, _Error} ->
                    if Rest ->
                            validate(Value, V);
                       true ->
                            ?FAILED(rest_not_allowed)
                    end;
                {ValueF, []} ->
                    validate(ValueF, V);
                {ValueF, _} ->
                    if Rest ->
                            validate(ValueF, V);
                       true ->
                            ?FAILED(rest_not_allowed)
                    end
            end
    end;
    
validate(V, #numericality{ allow_float = false }) when is_float(V) ->
    ?FAILED(float_not_allowed);
validate(V, #numericality{}) when is_integer(V) orelse is_float(V) ->
    true;
validate(_, #numericality{}) ->
    ?FAILED(number_expected);
validate(_, _) ->
    ?FAILED(validator_missing).


to_string(S) when is_list(S) ->
    S;
to_string(S) when is_binary(S) ->
    binary_to_list(S).


-ifdef(TEST).
numericality_test_() ->
    [fun numericality_test_undefined/0,
     fun numericality_test_null/0,
     fun numericality_test_empty/0,
     fun numericality_test_string/0,
     fun numericality_test_allow_float/0,
     fun numericality_test_default/0
    ].

numericality_test_undefined() ->
    ?assertEqual(?FAILED(undefined_not_allowed), validate(undefined, #numericality{})),
    ?assertEqual(?FAILED(undefined_not_allowed), validate(undefined, #numericality{ allow_undefined = false })),
    ?assert(validate(undefined, #numericality{ allow_undefined = true })).

numericality_test_null() ->
    ?assertEqual(?FAILED(null_not_allowed), validate(null, #numericality{})),
    ?assertEqual(?FAILED(null_not_allowed), validate(null, #numericality{ allow_null = false })),
    ?assert(validate(null, #numericality{ allow_null = true })).

numericality_test_empty() ->
    ?assertEqual(?FAILED(string_not_allowed), validate("", #numericality{ allow_empty = true })),
    ?assertEqual(?FAILED(string_not_allowed), validate(<<>>, #numericality{ allow_empty = true })),
    ?assertEqual(true, validate("", #numericality{ allow_empty = true, allow_string = true })),
    ?assertEqual(true, validate(<<>>, #numericality{ allow_empty = true, allow_string = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate("", #numericality{ allow_empty = false, allow_string = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate(<<>>, #numericality{ allow_empty = false, allow_string = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate("", #numericality{ allow_string = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate(<<>>, #numericality{  allow_string = true })).
    
numericality_test_string() ->
    ?assertEqual(true, validate("1", #numericality{ allow_string = true })),
    ?assertEqual(true, validate(<<"1">>, #numericality{ allow_string = true })),
    ?assertEqual(true, validate("1a", #numericality{ allow_string = true, allow_rest = true })),
    ?assertEqual(true, validate(<<"1a">>, #numericality{ allow_string = true, allow_rest = true })),
    ?assertEqual(true, validate("1.1", #numericality{ allow_string = true })),
    ?assertEqual(true, validate(<<"1.1">>, #numericality{ allow_string = true })),
    ?assertEqual(true, validate("1.1a", #numericality{ allow_string = true, allow_rest = true })),
    ?assertEqual(true, validate(<<"1.1a">>, #numericality{ allow_string = true, allow_rest = true })),
    ?assertEqual(?FAILED(number_expected), validate("garbage", #numericality{ allow_string = true })),
    ?assertEqual(?FAILED(number_expected), validate(<<"garbage">>, #numericality{ allow_string = true })).

numericality_test_allow_float() ->
    ?assertEqual(true, validate(1.1, #numericality{})),
    ?assertEqual(?FAILED(float_not_allowed), validate(1.1, #numericality{ allow_float = false})).

numericality_test_default() ->
    ?assertEqual(true, validate(undefined, #numericality{ allow_undefined = true })),
    ?assertEqual(true, validate(null, #numericality{ allow_null = true })),
    ?assertEqual(true, validate("", #numericality{ allow_string = true, allow_empty = true })),
    ?assertEqual(true, validate(<<>>, #numericality{ allow_string = true, allow_empty = true })),

    ?assertEqual(true, validate(undefined, #numericality{ allow_undefined = true, default = 1 })),
    ?assertEqual(true, validate(null, #numericality{ allow_null = true, default = 1 })),
    ?assertEqual(true, validate("", #numericality{ allow_string = true, allow_empty = true, default = 1 })),
    ?assertEqual(true, validate(<<>>, #numericality{ allow_string = true, allow_empty = true, default = 1 })),

    ?assertEqual(?FAILED(number_expected), validate(undefined, #numericality{ allow_undefined = true, default = x })),
    ?assertEqual(?FAILED(number_expected), validate(null, #numericality{ allow_null = true, default = x })),
    ?assertEqual(?FAILED(number_expected), validate("", #numericality{ allow_string = true, allow_empty = true, default = x })),
    ?assertEqual(?FAILED(number_expected), validate(<<>>, #numericality{ allow_string = true, allow_empty = true, default = x })).



-endif.
