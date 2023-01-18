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

-module(ml_model_request_processing).

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

request(Ts, #{ "task_id":=TaskId, "model_path":=ModelPath, "model_uri":=ModelUri0, "input_archive":=InputArchive0 })->
    
    % Path settings
    InputArchivesPath = <<ModelPath/binary,"/archivesP/">>,
    OutputArchivePath = <<ModelPath/binary,"/futureP/">>,
    ?LOGINFO("REQ: ArchivesPath ~p, FuturePath ~p",[InputArchivesPath, OutputArchivePath]),
    
    % read model settings tag
    ModelSettingsTagPath = <<ModelPath/binary,"/model_settings">>,
    
    #{
        <<"model_config">>:= Config0,
        <<"input_window">>:= InputWindow0,
        <<"output_window">>:=OutputWindow0,
        <<"granularity">>:=Granularity0,
        <<"model_type">>:=ModelType,
        <<"model_features">>:=ModelFeatures0
    } = fp_db:read_fields( fp_db:open(ModelSettingsTagPath),[
        <<"model_config">>,
        <<"input_window">>,
        <<"output_window">>,
        <<"granularity">>,
        <<"model_type">>,
        <<"model_features">>
    ] ),
    
    % InputArchive1 = 
    %     case InputArchive0 of
    %         _Name0 when is_list(_Name0)->_Name0;
    %         true -> string:tokens(RelatedArchive0, ",");
    %         _-> []
    %     end,
    
    ModelPoint = lists:last(binary:split(ModelPath, <<"/">>, [global])),
    
    ModelUri = case ModelUri0 of 
        { ok, ModelUri0 } -> ModelUri0;
        _-> null
    end,
    
    Req =
        case TaskId of
            undefined->
                % Check settings
                
                InputWindow = if is_number(InputWindow0)->HisInputWindow0tory0; true-> 96 * ?HOUR end,
                OutputWindow = if is_number(OutputWindow0)->OutputWindow0; true-> 48 * ?HOUR end,
                Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
                
                % The time point to which read the history 
                To = next_ts( Ts, Granularity*1000 ) - Granularity*1000,  %in ms
                
                % InputArchive = [ [<<ArchivesPath/binary,(unicode:characters_to_binary(A))/binary>>,<<"avg">>] || A <- [InputArchive0] ],
                % ?LOGINFO("REQ: History archive name ~p",[InputArchive0]),
                
                % InputDataset = fp:archives_get_periods(#{
                %     <<"step_unit">> => <<"second">>,
                %     <<"step_size">> => Granularity,
                %     <<"step_count">> => History div Granularity,
                %     <<"end">> => To
                % }, InputArchive),
                
                % ?LOGINFO("REQ: History dataset ~p",[HistoryDataset]),
                InputDataset = 
                    if is_list(InputArchive1)->
                            InputArchive = [ [<<FuturePath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- InputArchive1 ],
                            InputData = fp:archives_get_periods(#{
                                <<"step_unit">> => <<"second">>,
                                <<"step_size">> => Granularity,
                                <<"step_count">> => InputWindow div Granularity,
                                <<"start">> => To
                            }, InputArchive),
                            fp_db:to_json(term, InputData)   
                    true->
                        null
                    end,
                
                #{
                    "model_type"=> ModelType,
                    "model_point"=> ModelPoint,     
                    "task_id" =>  null,      
                    "model_uri"=>  ModelUri,  
                    "model_config" => Config0,
                    "metadata"=> #{
                        },              
                    "dataset"=> InputDataset
                };
                
            TaskId ->

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
    Req.


response(#{
    "result":= Result0,
    "ts":= Ts,
    "task_status":= TaskStatus,
    "task_id":= TaskId,
    "model_point":= ModelPoint,
    "model_uri":= ModelUri
})->
    ModelPath = <<"/root/PROJECT/TAGS/Nodes", ModelPoint/binary>>,
    ModelControlTagPath = <<ModelPath/binary,"/model_control">>,
    
    #{
        <<"output_archive">>:=OutputArchive0
    } = fp_db:read_fields( fp_db:open(ModelControlTagPath),[
        <<"output_archive">>
    ] ),

    OutputArchive = 
        <<ModelPath/binary, "/futureP/", (unicode:characters_to_binary(hd([OutputArchive0])))/binary>>,
    
    % always return task state and task id
    try
        fp:set_value(ModelControlTagPath, "task_id", TaskId),
        fp:set_value(ModelControlTagPath, "task_state", TaskStatus),
        fp:set_value(ModelControlTagPath, "task_exec",Ts),
        fp:set_value(ModelControlTagPath, "model_url",ModelUri)
    catch
        Error0 -> ?LOGINFO("DEBUG wacs_ml Result :  ERROR ~p ",[Error0])
    end,
    
    Result = 
        case Result0 of Result0 when is_map(Result0) ->
                maps:get(<<"result">>, Result0, none);
            true ->
                none
        end,

    case Result of 
        Result when is_list(Result)->
            Predict = [{floor(TS) *1000, V}||[TS, V]<-Result],
            try
                ResIns = fp_archive:insert_values( OutputArchive, Predict ),
                fp:log(info,"DEBUG ML: fp_archive:insert_values: ~p ~p", [Predict, ResIns])
            catch
                Error1 -> ?LOGINFO("DEBUG ERROR write to archives: ~p ",[Error1])
            end;
        true ->
            fp:log(info,"DEBUG ML: Data is not a list: ~p, ~p", [not is_list(Predict), Predict])
        _->
            ok
    end,
    ok.