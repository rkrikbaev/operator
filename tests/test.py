import os




def get_model(modelhub, exp_id, run_id=None, timestamp = 0):

    if run_id is None:

        os.chdir(f'{modelhub}/{exp_id}')

        all_folders = [ x for x in os.listdir('.') if os.path.isdir(x) ]

        for folder in all_folders:
            tokens = folder.split('@')  
            if len(tokens) == 2:
                if int(timestamp) <= int(tokens[0]):
                    timestamp = tokens[0]
                    run_id = folder
    
        if run_id is None: 
            raise RuntimeError('Din not find any directory')
        else:
            print('Variable "run_id" is None so take latest saved model')

    path = f'{modelhub}/{exp_id}/{run_id}'
    
    if os.path.isdir(path): 
        return path
    else:
        raise RuntimeError(f'Could not find model by {path}')

exp_id = '1'
modelhub = '/Users/rustamkrikbayev/operator/jupyter/project/mlruns'
# run_id='1674891914@0db7560f0d80470a9565d7faa3f79324'

latest_model = get_model(modelhub, exp_id)


print(latest_model)

