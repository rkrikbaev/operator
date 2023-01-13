import logging, os, sys
import logging.config
from logging.handlers import RotatingFileHandler
from pathlib import Path

# read application configuration
import configparser
config = configparser.ConfigParser()

BASE_PATH = os.getcwd()

CONFIG_FILE = os.path.join(BASE_PATH, 'main.config')
if not CONFIG_FILE:
  CONFIG_FILE = os.environ.get('CONFIG_FILE')

config.read_file(open(CONFIG_FILE))

LOG_LEVEL = config.get('APP', 'LOG_LEVEL')
if LOG_LEVEL==None: LOG_LEVEL='INFO'


LOG_PATH = BASE_PATH + config.get('APP', 'LOG_PATH')
TRACKING_SERVER = config.get('MLFLOW', 'TRACKING_SERVER')

# logger configuretion
def get_logger(name='root', loglevel='INFO'):

  LOG_PATH  = os.path.join(os.getcwd(), 'logs')
  if not os.path.exists(LOG_PATH):
    os.makedirs(LOG_PATH)
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

    file_handler = RotatingFileHandler(filename=f'{LOG_PATH}/log.log', mode='a', encoding=None, delay=False, maxBytes=5*1024*1024, backupCount=2)
    file_handler.setLevel(logging.ERROR)
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)

    if logger.name == 'root':
      logger.warning('Running: %s %s',
                     os.path.basename(sys.argv[0]),
                     ' '.join(sys.argv[1:]))
    return logger
