
-module(preset_ml_model_config).

-include("fp_struct.hrl").

% -define(PROTOTYPE_TAG, <<"/root/PROJECT/TAGS/Nodes/Agadyr/model_control_p">>). %In Second

-export([on_event/2]).

on_event({on_cycle,_Cycle},_State)->

    % read prototype tag
    %  ModelControlTag = <<ModelPath/binary,"/model_control">>,
    
    % #{
    %     % <<"task_id">> := TaskId,
    %     % <<"model_uri">> := ModelURI,
    %     <<"input_archive">>:=InputArchive,
    %     <<"output_archive">>:=OutputArchive,
    %     <<"model_config">>:= ModelConfig,
    %     <<"model_type">>:=ModelType,
    %     <<"start_hour">>:=StartHour
    % } = fp_db:read_fields( fp_db:open(?PROTOTYPE_TAG), [
    %     % <<"task_id">>,
    %     <<"model_uri">>,
    %     <<"input_archive">>,
    %     <<"output_archive">>,
    %     <<"model_config">>,
    %     <<"model_type">>,
    %     <<"start_hour">>]),  

    {_, TagList} = find_tags(),
    ?LOGINFO("REQ: Find Model tags: ~p", [TagList]),
    
    % setup control tags
    StartHour = 3,

    ModelType = <<"tf_model">>,
    
    ModelConfig0 = #{
        <<"granularity">>=>3600,
        <<"inputWindow">>=>96,
        <<"outputWindow">>=>48
    },
    
    ModelConfig = fp_lib:to_json(ModelConfig0),
    
    % ExperimentID = <<"571625146127493926">>,
    % RunID = <<"6776c0c6dda044bd8f120d2875463883">>,
    % ModelURI = <<"{ \"experiment_id\":", ExperimentID, "\"run_id\":", RunID }">>,
    % ModelURI = <<"{ \"experiment_id\":\"571625146127493926\", \"run_id\": \"6776c0c6dda044bd8f120d2875463883\" }">>,
    
    [begin
      
        [Tag0 | _] = Tag,
        
        ModelConfigTag = fp_db:to_path(fp_db:open(Tag0,none)),

        [_|Tokens] = binary:split(ModelConfigTag,<<"/">>,[global]),
        
        { ModelParentPath0, [TagName0 | _] } = lists:split(length(Tokens) - 1, Tokens),
        ?LOGINFO("REQ: ModelParentPath0: ~p", [ModelParentPath0]),
        ?LOGINFO("REQ: TagName0: ~p", [TagName0]),
        
        { _, ModelPoint0 } = lists:split(3, ModelParentPath0),
        ModelPoint1 = fp_lib:join_binary(ModelPoint0, <<"_">>),
        ?LOGINFO("REQ: ModelPoint1: ~p", [ModelPoint1]),
        
        [ _, P ] = binary:split(TagName0, <<".">>),
        ?LOGINFO("REQ: Parameter: ~p", [P]),
        
        InputArchive = <<P/binary, "_sc">>,
        OutputArchive = <<P/binary, "_baseline">>, 
        
        ModelParentPath1 = fp_lib:join_binary(ModelParentPath0, <<"/">>),
        ModelParentPath = <<"/", ModelParentPath1/binary>>,
        ?LOGINFO("RES: ModelParentPath: ~p", [ModelParentPath]),
        
        ModelPoint = string:lowercase(<<ModelPoint1/binary, ".", P/binary>>),
        ?LOGINFO("RES: ModelPoint: ~p", [ModelPoint]),
        
        fp_db:edit_object(fp_db:open(ModelConfigTag),#{
                                                        <<"model_path">>=>ModelParentPath,
                                                        <<"model_point">>=>ModelPoint,
                                                        <<"output_archive">>=>OutputArchive,
                                                        <<"input_archive">>=>InputArchive,
                                                        <<"start_hour">>=>StartHour,
                                                        <<"model_config">>=>ModelConfig,
                                                        <<"model_type">>=>ModelType,
                                                        <<"preset">>=>false
                                                    })

    end || Tag <- TagList],

    ok.

% find all tags   
find_tags()->
    ResQuery=fp_db:query(<<"get .oid, .name from root where and( .pattern=$oid('/root/.patterns/model_control'), preset=true)">>),
    %for only tag - where disable is false and not triggered yet.
    ?LOGINFO("REQ: Find Model tags: ~p", [ResQuery]),
    ResQuery.