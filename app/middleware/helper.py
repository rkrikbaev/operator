import yaml

def load_config(file)->dict:
    
    context = {}

    try:
        with open(file, 'r') as fl:
            context = yaml.safe_load(fl)
    except:
        pass

    return context