fun( #{ "task_id":=TaskId, "result":=Result, "task_state":=TaskState, "ts":=Ts, "model_point":=ModelPoint, "model_uri":=ModelUri } = Input, Output, Context)->
    wacs_models_query:response(#{
                            "result"=>Result,
                            "task_state"=>TaskState,
                            "ts"=>Ts,
                            "task_id"=>TaskId,
                            "model_point"=>ModelPoint,
                            "model_uri"=>ModelUri
                        }),
    ok
end.