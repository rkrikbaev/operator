fun( #{ "task_id":=TaskId, "model_path":=ModelPath, "model_uri":=ModelUri, "periods_archive_name":=PeriodArchiveNames,"data_archive_name":=DataArchiveName }=Vars, _Input, _From, To)->
    fp:log(info,"DEBUG ML: Request in biding ~p, ~p, ~p, ~p, ~p", [TaskId, ModelPath, ModelUri, PeriodArchiveNames, DataArchiveName]),
    Request = wacs_models_query:request(Ts=To, Vars#{ "task_id"=>TaskId, "model_path"=>ModelPath, "model_uri"=>ModelUri, "periods_archive_name"=>PeriodArchiveNames,"data_archive_name"=>DataArchiveName }),
    Request
end.