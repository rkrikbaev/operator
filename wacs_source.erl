fun( #{
    "model_path":=ModelPath, 
    "data_name":=DataArchive, 
    "period_name":=PeriodArchives, 
    "task_id":=TaskId, 
    "model_uri" := ModelUri,
    "model_point" := ModelPoint}=Vars, _Input, _From, To)->

    % % Archive = [ "p_load_sc" ],
    % Period  = [  ],
    % % Path = fp_db:to_path(FolderID),
    
    % % put(names, [ "predict_p_load_sc" ]),
    % % put(names, [ ArchiveName ]),
    % % put(path, ModelPath),
    
    Request = wacs_ml2:request( _Consumer = ModelPath, To, Vars#{
                                                                    "data_name" => DataArchive,
                                                                    "period_name" => PeriodArchives,
                                                                    "task_id" => TaskId,
                                                                    "model_uri" => ModelUri,
                                                                    "model_point" => ModelPoint
                                                                }),
    ModelPath,
    Request

end.