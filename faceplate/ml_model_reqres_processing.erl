% %%-----------------------------------------------------------------
% %% This script is executed at server side. The programming language
% %% is Erlang.
% %% The module must export method "on_event/1" or "on_event/2" . 
% %% The on_event/2 should be used if your code needs to keep some state between
% %% calls.
% %%
% %% The callback "on_event" is called by the faceplate only in run-time. 
% %% As the trigger might be used timer and/or change of the value of some field 
% %% of the tag.
% %% If the event is triggered by the timer then the first argument of the on_event 
% %% callback will be:
% %%    {on_cycle,Cycle} - Cycle is period (ms).
% %% If the event is triggered by the change of the value of the linked tag field 
% %% then the first argument of the on_event will be:
% %%    {tag,TagID,Field,Value} - TagID - OID of the tag
% %%                              Field - name of the field
% %%                              Value - current value of the field. 
% %% The returned value is considered being state and passed as the second argument
% %% at the next call.
% %%-----------------------------------------------------------------

-module(ml_model_reqres_processing).

-include("fp_struct.hrl").

-export([on_event/2]).

-export([
    request/2,
    response/1
]).

-define(HOUR, 3600). %In Second

on_event(_vent, State)->
   % DUMMY
   State.

next_ts(TS, Cycle) ->
    (TS div Cycle) * Cycle + Cycle.

request(Ts, #{ "task_id":=TaskId, "model_path":=ModelPath, "model_uri":=ModelUri0, "periods_archive_name":=FutureArchive0, "data_archive_name":=HistoryArchive0 })->
    
    % Path settings
    ArchivesPath = <<ModelPath/binary,"/archivesP/">>,
    FuturePath = <<ModelPath/binary,"/futureP/">>,
    ?LOGINFO("REQ: ArchivesPath ~p, FuturePath ~p",[ArchivesPath, FuturePath]),
    
    % read model settings tag
    ModelSettingsTagPath = <<ModelPath/binary,"/model_settings">>,
    
    #{
        <<"model_config">>:= Config0,
        <<"data_length">>:= History0,
        <<"period_length">>:=Future0,
        <<"granularity">>:=Granularity0,
        <<"model_type">>:=ModelType,
        <<"model_features">>:=ModelFeatures0
    } = fp_db:read_fields( fp_db:open(ModelSettingsTagPath),[
        <<"model_config">>,
        <<"data_length">>,
        <<"period_length">>,
        <<"granularity">>,
        <<"model_type">>,
        <<"model_features">>
    ] ),
    
    FutureArchive1 = 
        case FutureArchive0 of
            _Name0 when is_list(_Name0)->_Name0;
            true -> string:tokens(FutureArchive0, ",");
            _-> []
        end,
        
    % fp:log(info, "PeriodArchiveNames: ~p, DataArchiveName: ~p", [PeriodArchiveNames, DataArchiveName]),
    ?LOGINFO("REQ: DataArchiveName ~p, PeriodArchiveNames ~p",[HistoryArchive0, FutureArchive0]),
    % fp:log(info, "TaskId: ~p ", [TaskId]),
    
    ModelPoint = lists:last(binary:split(ModelPath, <<"/">>, [global])),
    % ?LOGINFO("REQ: Take ModelPoint from ModelPath ~p",[ModelPoint]),
    
    ModelUri = case ModelUri0 of 
        { ok, ModelUri0 } -> ModelUri0;
        _-> null
    end,
    
    Req =
        case TaskId of
            undefined->
                % Check settings
                
                History = if is_number(History0)->History0; true-> 96 * ?HOUR end,
                Future = if is_number(Future0)->Future0; true-> 48 * ?HOUR end,
                Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
                
                % The time point to which read the history 
                To = next_ts( Ts, Granularity*1000 ) - Granularity*1000,  %in ms
                
                HistoryArchive = [ [<<ArchivesPath/binary,(unicode:characters_to_binary(A))/binary>>,<<"avg">>] || A <- [HistoryArchive0] ],
                
                ?LOGINFO("REQ: History archive name ~p",[HistoryArchive]),
                
                HistoryDataset = fp:archives_get_periods(#{
                    <<"step_unit">> => <<"second">>,
                    <<"step_size">> => Granularity,
                    <<"step_count">> => History div Granularity,
                    <<"end">> => To
                }, HistoryArchive),
                
                % ?LOGINFO("REQ: History dataset ~p",[HistoryDataset]),
                FutureDataset = 
                    if is_list(FutureArchive1)->
                            FutureArchive = [ [<<FuturePath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- FutureArchive1 ],
                            % ?LOGINFO("REQ: Future period dataset archive ~p",[FutureArchive]),
                            FutureDataset0 = fp:archives_get_periods(#{
                                <<"step_unit">> => <<"second">>,
                                <<"step_size">> => Granularity,
                                <<"step_count">> => Future div Granularity,
                                <<"start">> => To
                            }, FutureArchive),
                            
                            fp_db:to_json(term, FutureDataset0);
                        true->
                            null
                    end,
                % ?LOGINFO("REQ: Future period dataset ~p",[FutureDataset]),
                
                #{
                    "model_type"=> ModelType,
                    "model_point"=> ModelPoint,     
                    "task_id" =>  null,      
                    "model_uri"=>  ModelUri,  
                    "model_config" => Config0,
                    "metadata"=> #{
                        "model_features"=> ModelFeatures0,
                        "regressor_names"=> null
                        },              
                    "period"=> FutureDataset,
                    "dataset"=> fp_db:to_json(term, HistoryDataset)
                };
                
            TaskId ->
                
                fp:log(info,"DEBUG In case of TaskId is not empty: ~p", [TaskId]),
                #{
                    "model_type"=> null,
                    "model_point"=> ModelPoint,     
                    "task_id" => TaskId,      
                    "model_uri"=>  null,  
                    "model_config" => null,
                    "metadata"=> null,              
                    "period"=> null,
                    "dataset"=> null
                }
        end,
    fp:log(info,"DEBUG Request"),
    Req.


response(#{
    "result":= Result,
    "ts":= Ts,
    "task_state":= TaskState,
    "task_id":= TaskId,
    "model_point":= ModelPoint,
    "model_uri":= ModelUri
})->
    ModelPath = <<"/root/PROJECT/TAGS/Nodes/", ModelPoint/binary>>,
    
    % read model settings tag
    % ModelSettingsTagPath = <<ModelPath/binary,"/model_settings">>,
    
    % update model control tag
    ModelControlTagPath = <<ModelPath/binary,"/model_control">>,
    
    #{
        <<"result_archive_name">>:=ResultArchiveName
    } = fp_db:read_fields( fp_db:open(ModelControlTagPath),[
        <<"result_archive_name">>
    ] ),

    ArchivePredict = 
        <<ModelPath/binary, "/futureP/", (unicode:characters_to_binary(hd([ResultArchiveName])))/binary>>,
    
    % always return task state and task id
    try
        fp:set_value(ModelControlTagPath, "task_id", TaskId),
        fp:set_value(ModelControlTagPath, "task_state", TaskState),
        fp:set_value(ModelControlTagPath, "task_exec",Ts),
        fp:set_value(ModelControlTagPath, "model_url",ModelUri)
    catch
        Error0 -> ?LOGINFO("DEBUG wacs_ml Result :  ERROR ~p ",[Error0])
    end,
    
    case Result of _Result when is_map(_Result) ->
        Predict0 = maps:get(<<"prediction">>, _Result, none),
        case Predict0 of 
            _Values when is_list(_Values)->
                Predict = [{floor(TS) *1000, V}||[TS, V]<-_Values],
                try
                    ResIns = fp_archive:insert_values( ArchivePredict, Predict ),
                    fp:log(info,"DEBUG ML: fp_archive:insert_values: ~p ~p", [Predict, ResIns])
                catch
                    Error1 -> ?LOGINFO("DEBUG ERROR write to archives: ~p ",[Error1])
                end;
            _->
                ok
        end
    end,
    ok.