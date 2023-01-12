import logging, os, sys
import logging.config
from logging.handlers import RotatingFileHandler
from pathlib import Path
import configparser

config = configparser.ConfigParser()
path_abs = Path(__file__).parent.absolute()
path_root, _ = os.path.split(path_abs)

log_dir = os.path.join(path_root, 'logs')
if not os.path.exists(log_dir):
  os.makedirs(log_dir)

PATH_TO_CONFG = os.path.join(path_root, 'main.config')

config.read_file(open(PATH_TO_CONFG))

LOG_LEVEL = config.get('APP', 'LOG_LEVEL')
if LOG_LEVEL==None: LOG_LEVEL='INFO'

def get_logger(name='root', loglevel='INFO'):
  logger = logging.getLogger(name)

  # if logger 'name' already exists, return it to avoid logging duplicate
  # messages by attaching multiple handlers of the same type
  if logger.handlers:
    return logger
  # if logger 'name' does not already exist, create it and attach handlers
  else:
    # set logLevel to loglevel or to INFO if requested level is incorrect
    loglevel = getattr(logging, loglevel.upper(), logging.INFO)
    logger.setLevel(loglevel)

    formate = '%(asctime)s [%(levelname)s] %(filename)-8s:: %(lineno)d : %(message)s'
    formate_date = '%Y-%m-%dT%T%Z'
    formatter = logging.Formatter(formate, formate_date)

    terminal_handler = logging.StreamHandler()
    terminal_handler.setLevel(logging.DEBUG)
    terminal_handler.setFormatter(formatter)
    logger.addHandler(terminal_handler)

    file_handler = RotatingFileHandler(filename=f'{log_dir}/log.log', mode='a', encoding=None, delay=False, maxBytes=5*1024*1024, backupCount=2)
    file_handler.setLevel(logging.ERROR)
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)

    if logger.name == 'root':
      logger.warning('Running: %s %s',
                     os.path.basename(sys.argv[0]),
                     ' '.join(sys.argv[1:]))
    return logger
