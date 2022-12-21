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

-module(models_schedule).

-include("fp_struct.hrl").
% -define(COMMON_MODEL_CONTROL_TAG, <<"TAGS/Nodes/model_control">>).
-define(GLOBAL_PATH, <<"/root/PROJECT/TAGS/Nodes">>).
-export([on_event/2]).

on_event(_Event, _State0)->

    % read model settings tag
    GlobalControlTag = <<?GLOBAL_PATH/binary,"/global_model_control">>,
    
    {_Date, {Hour, _, _}} = calendar:local_time(),
    
    { ok, StartHour } =
        case fp:get_value(GlobalControlTag,"start_hour") of
            {ok, Start_hour0 } when is_integer(Start_hour0)->
                { ok, Start_hour0 };
            _-> 
                { ok, Hour }
        end,
    
    {_, TagControlList} = find_tags(),
    
    ?LOGINFO("DEBUG: curr_hour ~p, Start_hour ~p",[Hour, StartHour]),
    ?LOGINFO("DEBUG: tag_list ~p",[TagControlList]),
    
    if 
        Hour >= StartHour->
            [begin
                [TagID0 | _] = TagID,
                ModelControlTag = fp_db:to_path(fp_db:open(TagID0,none)),
                [ModelPath,_,_] = string:replace(ModelControlTag, "/model_control", "", all),
                ?LOGINFO("DEBUG: ModelPath ~p",[ModelPath]),
                trigger_tag(GlobalControlTag, ModelControlTag),
                timer:sleep(5000)
            end
            ||TagID <- TagControlList];
        Hour < StartHour ->
            ok = reset_trigger_state();
        true-> ok
    end,
    ok.

trigger_tag( GlobalModelControlTag, LocalModelControlTag )->
    
    #{
        <<"task_id">> := TaskId,
        <<"model_uri">> := ModelUri,
        <<"model_path">> := ModelPath,
        <<"task_state">> := TaskState,
        <<"periods_archive_name">>:=PeriodArchiveNames,
        <<"data_archive_name">>:=DataArchiveName,
        <<"result_archive_name">>:=ResultArchiveName
    } = fp_db:read_fields( fp_db:open(LocalModelControlTag),[
                                                            <<"task_id">>,
                                                            <<"model_uri">>,
                                                            <<"model_path">>,
                                                            <<"task_state">>,
                                                            <<"periods_archive_name">>,
                                                            <<"data_archive_name">>,
                                                            <<"result_archive_name">>
    ]),

    #{
        <<"trigger">> := TriggerValue0
    } = fp_db:read_fields( fp_db:open( GlobalModelControlTag ),[
        <<"trigger">>
    ]),
    
    ?LOGINFO("DEBUG: Trigger Value: ~p ",[ TriggerValue0 ]),
    ?LOGINFO("DEBUG: TaskState: ~p ",[ TaskState ]),
        
    % ?LOGINFO("DEBUG: PeriodArchiveNames: ~p ",[ PeriodArchiveNames ]),
    
    { ok, Triggered, TriggerValue } = 
        case TaskState of
            <<"SUCCESS">> ->
                {ok, true, TriggerValue0};
            _->
                %% Put information about folder in tag. Then we trigger source, and script extracts values
                TriggerValue1 = 
                    case TriggerValue0 of
                        _Value when is_integer(_Value)->_Value + 1;
                        _-> 
                            1
                    end,
                {ok, false, TriggerValue1}
        end,
    
    % edit global tag
    ?LOGINFO("DEBUG: Edit global tag"),
    fp_db:edit_object(fp_db:open(GlobalModelControlTag),#{
                                                            <<"task_id">>=>TaskId,
                                                            <<"model_uri">>=>ModelUri,
                                                            <<"model_path">>=>ModelPath,
                                                            <<"trigger">>=>TriggerValue,
                                                            <<"task_state">>=>TaskState,
                                                            <<"periods_archive_name">>=>PeriodArchiveNames,
                                                            <<"data_archive_name">>=>DataArchiveName,
                                                            <<"result_archive_name">>=>ResultArchiveName                                                            
                                                        }),


    fp_db:edit_object(fp_db:open(LocalModelControlTag),#{
                                                            <<"triggered">>=>Triggered
                                                    }),
    ok.
    
% , disabled=false, triggered=false
find_tags()->
    ResQuery=fp_db:query(<<"get .oid, .name from root where and( .pattern=$oid('/root/.patterns/model_control'), disabled=false)">>),
    %for only tag - where disable is false and not triggered yet.
    ResQuery.
    
reset_trigger_state() ->
    
    ResQuery=fp_db:query(<<"get .oid from root where and( .pattern=$oid('/root/.patterns/model_control'), disabled=false, triggered=true )">>),
    ?LOGINFO("DEBUG: wacs_trigger_MODELLING: Reset state trigger ~p ",[ResQuery ]),
    
    [ begin 
        ModelControlTag = fp_db:open(ModelControlTagId,none),
        ?LOGINFO("DEBUG: wacs_trigger_MODELLING: Reset state trigger  ~ts ",[ ?PATH(ModelControlTagId) ]),
        
        fp_db:edit_object( ModelControlTag, #{ <<"task_id">> => none } ),
        fp_db:edit_object( ModelControlTag, #{ <<"task_state">> => none } ),
        fp_db:edit_object( ModelControlTag, #{ <<"triggered">> => false } )
      end || ModelControlTagId <- ResQuery
    ],
    fp:log_info(<<"DEBUG: wacs_trigger_MODELLING: RESET DONE">>),
    ok.