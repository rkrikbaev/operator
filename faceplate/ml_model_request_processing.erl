
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
    
    ModelPoint = lists:last(binary:split(ModelPath, <<"/">>, [global])),
    
    Req =
        case TaskId of
            undefined->
                ?LOGINFO("REQ: TaskId: ~p", [TaskId]),
                % Path settings
                InputArchivesPath = <<ModelPath/binary,"/archivesP/">>,
                OutputArchivePath = <<ModelPath/binary,"/futureP/">>,
                ?LOGINFO("REQ: ArchivesPath ~p, FuturePath ~p",[InputArchivesPath, OutputArchivePath]),
                
                % read model settings tag
                ModelSettingsTagPath = <<ModelPath/binary,"/model_settings">>,
                
                #{
                    % <<"model_config">>:= Config0,
                    <<"input_window">>:= InputWindow0,
                    <<"output_window">>:= OutputWindow0,
                    <<"granularity">>:=Granularity0,
                    <<"model_type">>:=ModelType
                } = fp_db:read_fields( fp_db:open(ModelSettingsTagPath),[
                    % <<"model_config">>,
                    <<"input_window">>,
                    <<"output_window">>,
                    <<"granularity">>,
                    <<"model_type">>
                ] ),
                
                {ok, InputArchive1} = 
                    case InputArchive0 of
                        InputArchive0 when is_list(InputArchive0)->
                            {ok, InputArchive0 };
                        true -> 
                            { ok, string:tokens(InputArchive0, ",") };
                        _-> 
                            { error, none }
                    end,
                
                ModelUri = fp_lib:from_json(ModelUri0),
                
                Config = #{
                    <<"input_window">>=>InputWindow0, 
                    <<"output_window">>=>OutputWindow0,
                    <<"granularity">>=>Granularity0
                },
                
                ?LOGINFO("REQ: ModelUri value ~p",[ModelUri]),
                ?LOGINFO("REQ: Config value ~p",[Config]),
                
                % Check settings

                InputWindow = if is_number(InputWindow0)->( InputWindow0 + 1 ) * ?HOUR; true-> 96 * ?HOUR end,
                Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
                
                % The time point to which read the history 
                To = next_ts( Ts, Granularity * 1000) - Granularity * 1000,  %in ms
                
                InputDataset = 
                    if is_list(InputArchive1)->
                            InputArchive = [ [<<InputArchivesPath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- InputArchive1 ],
                            ?LOGINFO("REQ: InputArchive list ~p",[InputArchive]),
                            ?LOGINFO("REQ: Granularity ~p",[Granularity]),
                            ?LOGINFO("REQ: InputWindow ~p",[InputWindow]),
                            ?LOGINFO("REQ: From (Ts) ~p, round to (To) ~p",[ To, Ts ]),
                            InputData = fp:archives_get_periods(#{
                                <<"step_unit">> => <<"second">>,
                                <<"step_size">> => Granularity,
                                <<"step_count">> => InputWindow div Granularity,
                                <<"end">> => To
                            }, InputArchive),
                            fp_db:to_json(term, InputData);
                    true->
                        none
                    end,
                ?LOGINFO("REQ: Size of dataset ~p",[length(InputDataset)]),
                ?LOGINFO("REQ: Config value ~p, ModelUri value ~p",[Config, ModelUri]),
                #{
                    "model_type"=> ModelType,
                    "model_point"=> ModelPoint,     
                    "task_id" => null,      
                    "model_uri"=> ModelUri,  
                    "model_config"=> Config,
                    "metadata"=> null,             
                    "dataset"=> InputDataset,
                    "period"=>null
                };
            TaskId ->
                ?LOGINFO("REQ: TaskId: ~p", [TaskId]),
                #{
                    "model_type"=> null,
                    "model_point"=> ModelPoint,     
                    "task_id" => TaskId,      
                    "model_uri"=>  null,  
                    "model_config" => null,
                    "metadata"=> null,              
                    "dataset"=> null,
                    "period"=>null
                }
        end,
    Req.

response(#{
    "result":= Result,
    "ts":= Ts,
    "task_status":= TaskStatus,
    "task_id":= TaskId,
    "model_point":= ModelPoint,
    "model_uri":= ModelUri
})->
    ModelPath = <<"/root/PROJECT/TAGS/Nodes/", ModelPoint/binary>>,
    ModelControlTagPath = <<ModelPath/binary,"/model_control">>,
    ?LOGINFO("REQ: Model Control Tag Path: ~p", [ModelControlTagPath]),
    
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
        fp:set_value(ModelControlTagPath, "task_status", TaskStatus),
        fp:set_value(ModelControlTagPath, "task_exec",Ts),
        fp:set_value(ModelControlTagPath, "model_url",ModelUri)
    catch
        Error0 -> ?LOGINFO("DEBUG wacs_ml Result :  ERROR ~p ",[Error0])
    end,
    
    % Result = 
    %     case Result0 of Result0 when is_map(Result0) ->
    %             maps:get(<<"result">>, Result0, none);
    %         true ->
    %             none
    %     end,

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
            fp:log(info,"DEBUG ML: Data is not a list");
        _-> 
            ok
    end,
    ok.