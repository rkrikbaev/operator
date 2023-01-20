fun( #{ "task_id":=TaskId, "model_path":=ModelPath, "model_uri":=ModelUri, "input_archive":=InputArchive }=Vars, _Input, _From, To)->
    fp:log(info,"DEBUG ML: Request in IoT biding. TaskId: ~p, ModelPath: ~p, ModelUri: ~p, InputArchive: ~p", [TaskId, ModelPath, ModelUri, InputArchive]),
    Request = ml_model_request_processing:request(Ts=To,
                                                  Vars#{ 
                                                        "task_id"=>TaskId, 
                                                        "model_path"=>ModelPath, 
                                                        "model_uri"=>ModelUri,
                                                        "input_archive"=>["p_load_sc"]
                                                }),
    Request
end.