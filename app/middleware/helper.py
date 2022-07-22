import logging, yaml, os
import logging.config



def logger(name=__name__) -> None:
    
    file_path = os.path.join(os.getcwd(), 'app/config/logger_config.yaml')
    

    try:
        with open(file_path, 'r') as fl:
            config =  yaml.safe_load(fl)
            logging.config.dictConfig(config) 
                       
    except Exception as exc:
        print(exc)

    # formatter = logging.Formatter('%(asctime)s - [%(levelname)s] - %(name)s - (%(filename)s).%(funcName)s(%(lineno)d) - %(message)s')
    
    # file_handler = logging.FileHandler('app/logs/app.log', mode='a', encoding=None, delay=False)

    # file_handler.setLevel(logging.DEBUG)

    # file_handler.setFormatter(formatter)

    # logger = logging.getLogger(name) 
    # logger.addHandler(file_handler)
    logger = logging.getLogger(name)
    return logger
