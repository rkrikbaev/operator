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

-module(wacs_ml_model_query).
-include("fp_struct.hrl").
-export([on_event/2]).
-export([
    request/3,
    response/2,
    send_request/3
]).

-define(HOUR, 3600). %In Second

on_event(_vent, State)->
   % DUMMY
   State.


next_ts(TS, Cycle) ->
    (TS div Cycle) * Cycle + Cycle.


request(Consumer, Ts, #{
    "data_name" := Archives0,
    "period_name":=Period0,
    "task_id":=Task_id0,
    "model_uri":=Model_uri0,
    "model_point":=Model_point0
    })->
    
    % fp:log(info, "Consumer path: ~p", [Consumer]),
    
    % Path settings
    ArchivesPath = <<Consumer/binary,"/archivesP/">>,
    FuturePath = <<Consumer/binary,"/futureP/">>,
    
    % read model settings tag
    TagPath = <<Consumer/binary,"/model_settings">>,
    % fp:log(info, "Tag model_settings: ~p", [TagPath]),
    
    % #{
    %     <<"config">>:= Config0,
    %     <<"history_length">>:= History0,
    %     <<"future_length">>:=Future0,
    %     <<"granularity">>:=Granularity0,
    %     <<"model_type">>:=Model_type0,
    %     <<"regressor_names">>:=Regressor_names0,
    %     <<"model_features">>:=Model_features0
    % } = fp_db:read_fields( fp_db:open(TagPath),[
    %     <<"config">>,
    %     <<"history_length">>,
    %     <<"future_length">>,
    %     <<"granularity">>,
    %     <<"model_type">>,
    %     <<"regressor_names">>,
    %     <<"model_features">>
    % ] ),
    
    Model_uri = case Model_uri0 of
                    none->
                        null;
                    _-> Model_uri0
                end,
    
    ResReq =
        if Task_id0 /= none ->
                % Check settings
                History = if is_number(History0)->History0; true-> 96 * ?HOUR end,
                Future = if is_number(Future0)->Future0; true-> 48 * ?HOUR end,
                Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
                
                % The time point to which read the history 
                To = next_ts( Ts, Granularity*1000 ) - Granularity*1000,  %in ms
                
                Archives = [ [<<ArchivesPath/binary,(unicode:characters_to_binary(A))/binary>>,<<"avg">>] || A <- Archives0 ],
                
                % fp:log(info,"DEBUG MLService: Archives: ~p", [Archives]),
                % fp:log(info,"DEBUG MLService: TEST_POINT: ~p ~p ~p", [ Granularity, History, To]),
                
                HistoryDataset0 = fp:archives_get_periods(#{
                    <<"step_unit">> => <<"second">>, %
                    <<"step_size">> => Granularity,
                    <<"step_count">> => History div Granularity,
                    <<"end">> => To
                }, Archives),
                
                % fp:log(info,"DEBUG MLService: History: ~p", [HistoryDataset0]),
                
                FuturePeriod = 
                if is_list(Period0)->
                        Period = [ [<<FuturePath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- Period0 ],
                        FuturePeriod0 = fp:archives_get_periods(#{
                            <<"step_unit">> => <<"second">>,
                            <<"step_size">> => Granularity,
                            <<"step_count">> => Future div Granularity,
                            <<"start">> => To
                        }, Period),
                        fp_db:to_json(term, FuturePeriod0);
                    true->
                        null
                end,
        
                if Archives0 =:= [ "q_load_sc" ] -> <<Model_point0/binary,<<"_Q">>/binary >>;
                    true -> <<Model_point0/binary,<<"_P">>/binary >>
                end,

                    #{
                        "model_type"=> null,
                        "model_point"=> null,     
                        "task_id" =>  Task_id0,      
                        "model_uri"=>  null,  
                        "model_config" => null,
                        "metadata"=> null,              
                        "period"=> null,
                        "dataset"=> null
                    };
                true ->
                    #{
                        "model_type"=> Model_type0,
                        "model_point"=> Model_point0,     
                        "task_id" =>  null,      
                        "model_uri"=>  Model_uri,  
                        "model_config" => Config0,
                        "metadata"=> #{
                            "model_features"=> Model_features0,
                            "regressor_names"=> Regressor_names0
                            },              
                        "period"=> FuturePeriod,
                        "dataset"=> fp_db:to_json(term, HistoryDataset0)
                    }
                end,

    end,
    
    ResReq.


response(Consumer, #{
    "result" := Result,
    "archives" := Archives,
    "time_exec":= Time_execution,
    "task_status":= Task_status,
    "task_id":= Task_id
})->
    TagPath = <<Consumer/binary,"/model_control/">>,
    % fp:log(info,"DEBUG MLService: TaskID ~p",[Task_id]),
    % fp:log(info,"DEBUG MLService: TagPath ~p",[TagPath]),
    case Result of
        Result when is_map(Result) ->
        
            #{<<"granularity">>:=Granularity0} = fp_db:read_fields( fp_db:open(<<Consumer/binary,"/model_settings">>),[<<"granularity">>] ),
            Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
            
            % fp:log(info,"DEBUG MLService: Result ~p",[Result]),
            Model_uri = maps:get(<<"model_uri">>, Result, none),
            Predict0 = maps:get(<<"prediction">>, Result, none),
            
            % fp:log(info,"DEBUG MLService: Predict ~p",[Predict0]),
            
            Predict1 = 
                case Predict0 of 
                    Predict0 when is_list(Predict0)->
                            [{floor(TS) *1000, V}||[TS, V]<-Predict0];
                    _->
                        ok
            end,
            case Predict1 of
                    Predict1 when is_list(Predict1)->
                        
                        Predict = Predict1,
                        fp:log(info,"DEBUG MLService: Predict1 ~p",[Predict]),  
                        ArchivePredict = 
                            <<Consumer/binary, "/futureP/", (unicode:characters_to_binary(hd(Archives)))/binary>>,
                        
                        try
                            ResIns = fp_archive:insert_values( ArchivePredict, Predict ),
                            fp:log(info,"DEBUG MLService: TagPath2: ~p", [ResIns]),
                            fp:set_value(TagPath, "task_id",Task_id),
                            fp:set_value(TagPath, "task_status",Task_status),
                            fp:set_value(TagPath, "task_exec",Time_execution),
                            fp:set_value(TagPath, "model_url",Model_uri)
                        catch
                            Error1 -> ?LOGINFO("DEBUG wacs_ml RESPONSE :  ERROR ~p ",[Error1])
                        end;
                    none->
                        ok
            end;
        _->
            fp:log(info,"DEBUG MLService: Write Task_ID ~p",[Task_id]), 
            try
                fp:set_value(TagPath, "task_id", Task_id),
                fp:set_value(TagPath, "task_status", Task_status)
            catch
                Error0 -> ?LOGINFO("DEBUG wacs_ml RESPONSE :  ERROR ~p ",[Error0])
            end
    end,
  ok.