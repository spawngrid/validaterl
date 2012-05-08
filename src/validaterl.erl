-module(validaterl).
-export([validate/1, validate/2]).
-include_lib("validaterl/include/validaterl.hrl").
-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(FAILED(X), X).

-type spec() :: #numericality{} | #range{} | #format{}.
-type name() :: any().
-type report() :: any().
-type plan() :: {name(), any(), spec()}.
-type error() :: {name(), report()}.

-spec validate(plan()) -> true | list(error()).
validate(Plan) ->
    lists:filter(fun({_, _, true}) ->
                         false;
                    (_) ->
                         true
                 end,
                 [ {Name, Spec, validate(Value, Spec)} ||
                     {Name, Value, Spec} <- Plan ]).
    

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

validate(_, #range{ from = undefined,
                    to = undefined }) ->
    true;
validate(V, #range{ from = From,
                    to = undefined,
                    exclusive = false}) when From /= undefined andalso
                                             V < From ->
    ?FAILED(lesser);
validate(V, #range{ from = From,
                    to = undefined,
                    exclusive = true}) when From /= undefined andalso 
                                            V =< From ->
    ?FAILED(lesser);
validate(V, #range{ from = undefined,
                    to = To,
                    exclusive = false}) when To /= undefined andalso
                                             V > To ->
    ?FAILED(greater);
validate(V, #range{ from = undefined,
                    to = To,
                    exclusive = true}) when To /= undefined andalso
                                            V >= To ->
    ?FAILED(greater);
validate(V, #range{ from = _From,
                    to = To,
                    exclusive = false}) when To /= undefined andalso
                                             V > To ->
    ?FAILED(greater);
validate(V, #range{ from = From,
                    to = _To,
                    exclusive = false}) when From /= undefined andalso
                                             V < From ->
    ?FAILED(lesser);
validate(V, #range{ from = From,
                    to = _To,
                    exclusive = true}) when From /= undefined andalso
                                             V =< From ->
    ?FAILED(lesser);
validate(V, #range{ from = _From,
                    to = To,
                    exclusive = false}) when To /= undefined andalso 
                                             V > To ->
    ?FAILED(greater);
validate(V, #range{ from = _From,
                    to = To,
                    exclusive = true}) when To /= undefined andalso
                                            V >= To ->
    ?FAILED(greater);
validate(_, #range{}) ->
    true;

validate(undefined, #format{
           allow_undefined = false
          }) ->
    ?FAILED(undefined_not_allowed);
validate(undefined, #format{
           allow_undefined = true,
           default = Default
          } = V) ->
    validate(Default, V);
validate(null, #format{
           allow_null = false
          }) ->
    ?FAILED(null_not_allowed);
validate(null, #format{
           allow_null = true,
           default = Default
          } = V) ->
    validate(Default, V);
validate(Empty, #format{
           allow_empty = false
           }) when Empty == [] orelse Empty == <<>> ->
    ?FAILED(empty_not_allowed);
validate(Str, #format{
           re = Re
          }) when is_list(Str) orelse is_binary(Str) ->
    case re:run(Str, Re) of
        nomatch ->
            ?FAILED(no_match);
        {match, _} ->
            true;
        {error, Error} ->
            ?FAILED(Error)
    end;
validate(_, #format{}) ->
    ?FAILED(string_expected);

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

qc(T) ->
    ?assertEqual(true, proper:quickcheck(T,
                                         [
                                          {numtests, 1000},
                                          {on_output, fun(String, Format) ->
                                                              io:format(user, String, Format)
                                                      end}])).
                                                                  
                                                                  
range_test_() ->
    [{"include",
      fun() -> qc(range_test_inclusive_prop()) end},
     {"exclude",
      fun() -> qc(range_test_exclusive_prop()) end}].


range() ->
    ?SUCHTHAT({from, F, to, T},
              {from, oneof([undefined, integer()]), 
               to, oneof([undefined, integer()])},
              if F == undefined ->
                      true;
                 T == undefined ->
                      true;
                 true ->
                      F =< T
              end).

range_test_inclusive_prop() ->
    ?FORALL({{from, From, to, To}, Random}, {range(),
                                             integer()},
            begin
                Validate = validate(Random, #range{ from = From,
                                                    to = To }),
                if  From /= undefined andalso Random < From ->
                        Validate == ?FAILED(lesser);
                    To /= undefined andalso Random > To ->
                        Validate == ?FAILED(greater);
                    From /= undefined andalso To /= undefined 
                    andalso Random >= From andalso Random =< To ->
                        Validate == true;
                    From == undefined andalso To /= undefined 
                    andalso Random =< To ->
                        Validate == true;
                    To == undefined andalso From /= undefined 
                    andalso Random >= From ->
                        Validate == true;
                    From == undefined andalso To == undefined ->
                        Validate == true;
                    Random < From ->
                        Validate == ?FAILED(lesser);
                    Random > To ->
                        Validate == ?FAILED(greater);
                    true ->
                        false
                end
            end).

range_test_exclusive_prop() ->
    ?FORALL({{from, From, to, To}, Random}, {range(),
                                             integer()},
            begin
                Validate = validate(Random, #range{ from = From,
                                                    to = To,
                                                    exclusive = true}),
                if 
                    From /= undefined andalso Random =< From
                    andalso From == To ->
                        (Validate == ?FAILED(lesser)) orelse
                        (Validate == ?FAILED(greater));
                    From /= undefined andalso Random =< From ->
                        Validate == ?FAILED(lesser);
                    To /= undefined andalso Random >= To
                    andalso From == To ->
                        (Validate == ?FAILED(lesser)) orelse
                        (Validate == ?FAILED(greater));
                    To /= undefined andalso Random >= To ->
                        Validate == ?FAILED(greater);
                    From /= undefined andalso To /= undefined 
                    andalso Random > From andalso Random < To ->
                        Validate == true;
                    From == undefined andalso To /= undefined 
                    andalso Random < To ->
                        Validate == true;
                    To == undefined andalso From /= undefined 
                    andalso Random > From ->
                        Validate == true;
                    From == undefined andalso To == undefined ->
                        Validate == true;
                    Random =< From ->
                        Validate == ?FAILED(lesser);
                    Random >= To ->
                        Validate == ?FAILED(greater);
                    true ->
                        false
                end
            end).                                 

format_test_() ->
    [fun format_test_undefined/0,
     fun format_test_null/0,
     fun format_test_empty/0,
     fun format_test_string/0,
     fun format_test_default/0
    ].

format_test_undefined() ->
    ?assertEqual(?FAILED(undefined_not_allowed), validate(undefined, #format{})),
    ?assertEqual(?FAILED(undefined_not_allowed), validate(undefined, #format{ allow_undefined = false })),
    ?assert(validate(undefined, #format{ allow_undefined = true, allow_empty = true })).

format_test_null() ->
    ?assertEqual(?FAILED(null_not_allowed), validate(null, #format{})),
    ?assertEqual(?FAILED(null_not_allowed), validate(null, #format{ allow_null = false })),
    ?assert(validate(null, #format{ allow_null = true, allow_empty = true })).

format_test_empty() ->
    ?assertEqual(true, validate("", #format{ allow_empty = true })),
    ?assertEqual(true, validate(<<>>, #format{ allow_empty = true })),
    ?assertEqual(?FAILED(empty_not_allowed), validate("", #format{ allow_empty = false })),
    ?assertEqual(?FAILED(empty_not_allowed), validate(<<>>, #format{ allow_empty = false })),
    ?assertEqual(?FAILED(empty_not_allowed), validate("", #format{})),
    ?assertEqual(?FAILED(empty_not_allowed), validate(<<>>, #format{})).
    
format_test_string() ->
    ?assertEqual(true, validate("1", #format{ re = "^1$" })),
    ?assertEqual(?FAILED(no_match), validate("2", #format{ re = "^1$" })).

format_test_default() ->
    ?assertEqual(true, validate(undefined, #format{ allow_undefined = true, allow_empty = true })),
    ?assertEqual(true, validate(null, #format{ allow_null = true, allow_empty = true })),
    ?assertEqual(true, validate("", #format{ allow_empty = true })),
    ?assertEqual(true, validate(<<>>, #format{allow_empty = true })),

    ?assertEqual(?FAILED(string_expected), validate(undefined, #format{ allow_undefined = true, default = x })),
    ?assertEqual(?FAILED(string_expected), validate(null, #format{ allow_null = true, default = x })).

-endif.
