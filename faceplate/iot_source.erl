fun( #{ "task_id":=TaskId, "model_path":=ModelPath, "model_uri":=ModelUri, "periods_archive_name":=PeriodArchiveNames,"data_archive_name":=DataArchiveName }=Vars, _Input, _From, To)->
    fp:log(info,"DEBUG MLService: At the source: ~p", [Vars]),
    
    Request = wacs_models_query:request(Ts=To, Vars#{ "task_id"=>TaskId, "model_path"=>ModelPath, "model_uri"=>ModelUri, "periods_archive_name"=>PeriodArchiveNames,"data_archive_name"=>DataArchiveName }),

    fp:log(info,"DEBUG MLService: Request from the Source!!!!: ~p", [Request]),
    % ModelPath,
    Request
end.


% JSON

{
    "model_type": $model_type,
    "model_point": $model_point,
    "task_id": $task_id,
    "model_uri": $model_uri,
    "metadata": {
        "model_features": $model_features,
        "regressor_names": $regressor_names
        },
    "period": $period,
    "dataset": $dataset,
    "model_config": $model_config
}