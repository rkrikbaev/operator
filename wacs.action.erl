fun( #{ "task_id":=TaskId, "result":=Result, "task_status":=TaskStatus, "time_exec":=TimeExecution, "model_path":=ModelPath, "archive_name":=ArchiveName, "period":=Period} = Input, Output, Context)->
    
    % Path = get(path),
    
    fp:log(info,"DEBUG MLService: Input ~p",[Input]),
    
    % Result = maps:get("result", Input),
    % TaskStatus = maps:get("task_status",Input),
    % TimeExecution = maps:get("time_exec",Input),
    % TaskId = maps:get("task_id",Input),
    
    wacs_ml2:response(Consumer = ModelPath, #{
                            "result" => Result,
                            "archives" => ArchiveName,
                            "task_status" => TaskStatus,
                            "time_exec" => TimeExecution,
                            "task_id"=> TaskId
                        }),
    ok
end.