%%-----------------------------------------------------------------
%% This script is executed at server side. The programming language
%% is Erlang.
%% The module must export method "on_event/1" or "on_event/2" . 
%% The on_event/2 should be used if your code needs to keep some state between
%% calls.
%%
%% The callback "on_event" is called by the faceplate only in run-time. 
%% As the trigger might be used timer and/or change of the value of some field 
%% of the tag.
%% If the event is triggered by the timer then the first argument of the on_event 
%% callback will be:
%%    {on_cycle,Cycle} - Cycle is period (ms).
%% If the event is triggered by the change of the value of the linked tag field 
%% then the first argument of the on_event will be:
%%    {tag,TagID,Field,Value} - TagID - OID of the tag
%%                              Field - name of the field
%%                              Value - current value of the field. 
%% The returned value is considered being state and passed as the second argument
%% at the next call.
%%-----------------------------------------------------------------

-module(wacs_models_schedule).

-include("fp_struct.hrl").

-define(COMMON_MODEL_CONTROL_TAG, <<"TAGS/Nodes/model_control">>).
-define(COMMON_MODEL_SETTINGS_TAG, <<"TAGS/Nodes/model_settings">>).

% -record(state,{ tags, time }).

-export([on_event/2]).


on_event(_Event, _State0)->

    {_Date, {Hour, _, _}} = calendar:local_time(),
    
    TriggerStep =
        case fp:get_value(?COMMON_MODEL_CONTROL_TAG,"step", 10000) of
            {ok, Trigger_step0} when is_integer(Trigger_step0)->
                Trigger_step0;
            _-> ok
        end,
    
    StartHour =
        case fp:get_value(?COMMON_MODEL_CONTROL_TAG,"start_hour", 12) of
            {ok, Start_hour0} when is_integer(Start_hour0)->
                Start_hour0;
            _-> ok
        end,
    
    {_, TagControlList} = find_tags(),
    
    ?LOGINFO("DEBUG: Curr_hour ~p, Start_hour ~p",[Hour, StartHour]),
    ?LOGINFO("DEBUG: tag_list ~p",[TagControlList]),
    
    if 
        Hour >= StartHour->
            [begin
                ?LOGINFO("DEBUG: tag ~p",[TagControlId]),
                trigger_tag(?COMMON_MODEL_CONTROL_TAG, ?COMMON_MODEL_SETTINGS_TAG, TagControlId),
                timer:sleep(TriggerStep)
            end
            ||TagControlId <- TagControlList];
        Hour < StartHour ->
            ok = reset_trigger_state();
        true-> ok
    end,
    ok.

find_tags()->
    ResQuery=fp_db:query(<<"get .oid, .name from root where and( .pattern=$oid('/root/.patterns/model_control'), disabled=false, isTriggered=false)">>),
    %for only tag - where disable is false and not triggered yet.
    ResQuery.

trigger_tag( GlobalModelControlTag, GlobalModelSettingsTag, ModelControlTagId )->

    ModelControlTag = fp_db:open(ModelControlTagId,none),
    { ok, ModelPath } = fp_db:read_field(ModelControlTagId, <<".folder">>),
    ModelSettingsTag = <<ModelPath/binary,"/model_settings">>,
    
    
    #{
        <<"task_id">> := TaskId,
        <<"model_uri">> := ModelUri,
        <<"model_point">> := ModelPoint,
        % <<"isTriggered">> := Triggered0,
        <<"task_state">> := TaskState,
        <<"trigger_value">> := TriggerValue0
    } = fp_db:read_fields( fp_db:open(ModelControlTag),[
        <<"task_id">>,
        <<"model_uri">>,
        <<"model_point">>,
        % <<"isTriggered">>,
        <<"task_state">>,
        <<"trigger_value">>
    ]),

    #{
        <<"model_config">>:= ModelConfig,
        <<"data_length">>:= DataLen,
        <<"period_length">>:=PeriodLen,
        <<"granularity">>:=Granularity,
        <<"model_type">>:=ModelType,
        <<"regressor_names">>:=Regressors,
        <<"model_filter">>:=ModelFilter
    } = fp_db:read_fields( fp_db:open(ModelSettingsTag),[
        <<"model_config">>,
        <<"data_length">>,
        <<"period_length">>,
        <<"granularity">>,
        <<"model_type">>,
        <<"regressor_names">>,
        <<"model_filter">>
    ] ),    

    % ?LOGINFO("DEBUG_OF.trigger_tag(): tag path ~ts and tag ~p",[?PATH(Tag), Tag]),
    { ok, Triggered, TriggerValue } = 
            case TaskState of
                {ok, <<"SUCCESS">>}->
                    % Triggered0 = true,
                    % fp_db:edit_object( TagControl, #{ <<"isTriggered">> => true } ),
                    % fp:set_value(TagControl,"trigger_value", 0);
                    {ok, true, TriggerValue0};
                true->
                %% Put information about folder in tag. Then we trigger source, and script extracts values
                    TriggerValue1 = 
                    case TriggerValue0 of
                        {ok, _Value} when is_integer(_Value)-> _Value + 1;
                        _-> 1
                    end,
                    {ok, false, TriggerValue1}
            end,

    fp_db:edit_object(fp_db:open(GlobalModelControlTag),#{
                                                            <<"task_id">> => TaskId,
                                                            <<"model_uri">> => ModelUri,
                                                            <<"model_point">> => ModelPoint,
                                                            <<"trigger_value">> => TriggerValue
                                                        }),
    
    fp_db:edit_object(fp_db:open(GlobalModelSettingsTag),#{
                                                            <<"model_config">>=> ModelConfig,
                                                            <<"data_length">>=> DataLen,
                                                            <<"period_length">>=>PeriodLen,
                                                            <<"granularity">>=>Granularity,
                                                            <<"model_type">>=>ModelType,
                                                            <<"regressor_names">>=>Regressors,
                                                            <<"model_filter">>=>ModelFilter
                                                            }),

    fp_db:edit_object(fp_db:open(ModelControlTag),#{
                                                            <<"trigger_value">> => TriggerValue,
                                                            % <<"task_state">> => TaskState,
                                                            <<"isTriggered">> => Triggered
                                                        }),

    ok.
    
reset_trigger_state() ->
    
    ResQuery=fp_db:query(<<"get .oid from root where and( .pattern=$oid('/root/.patterns/model_control'), disabled=false, isTriggered=true )">>),
    ?LOGINFO("DEBUG: wacs_trigger_MODELLING: Reset state trigger ~p ",[ResQuery ]),
    
    [ begin 
        ModelControlTag = fp_db:open(ModelControlTagId,none),
        ?LOGINFO("DEBUG: wacs_trigger_MODELLING: Reset state trigger  ~ts ",[ ?PATH(ModelControlTagId) ]),
        fp_db:edit_object( ModelControlTag, #{ <<"task_id">> => none } ),
        fp_db:edit_object( ModelControlTag, #{ <<"task_state">> => none } ),
        fp_db:edit_object( ModelControlTag, #{ <<"isTriggered">> => false } )
      end || ModelControlTagId <- ResQuery
    ],
    fp:log_info(<<"DEBUG: wacs_trigger_MODELLING: RESET DONE">>),
    ok.