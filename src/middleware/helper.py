import logging, yaml, os
import logging.config

class Logger():
    # file_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'config/logger_config.yaml')
    def __init__(self, file_path, name=__name__) -> None:
        
        try:
            with open(file_path, 'r') as fl:
                config =  yaml.safe_load(fl)
                logging.config.dictConfig(config)      
        except:
            pass
        finally:
            return logging.getLogger(name)


#------------logger_config------------

logger = logging.getLogger(__name__)

formatter = logging.Formatter(f'%(asctime)s - [%(levelname)s] - %(name)s - (%(filename)s).%(funcName)s(%(lineno)d) - %(message)s')
  
file_handler = logging.FileHandler('src/logs/app.log', mode='a', encoding=None, delay=False)

file_handler.setLevel(logging.DEBUG)

file_handler.setFormatter(formatter)

logger.addHandler(file_handler)

#------------logger_config------------