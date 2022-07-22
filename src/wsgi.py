from wsgiref.simple_server import make_server

from app import api
# from middleware.helper import logger

import logging, yaml, os
import logging.config

#------------logger_config------------

logger = logging.getLogger(__name__)

formatter = logging.Formatter(f'%(asctime)s - [%(levelname)s] - %(name)s - (%(filename)s).%(funcName)s(%(lineno)d) - %(message)s')
  
file_handler = logging.FileHandler('src/logs/app.log', mode='a', encoding=None, delay=False)

file_handler.setLevel(logging.DEBUG)

file_handler.setFormatter(formatter)

logger.addHandler(file_handler)

#------------logger_config------------

if __name__ == "__main__":

    logger.debug("Start wsgi web-server")
    with make_server("", 8025, api) as httpd:
        # Serve until process is killed
        
        httpd.serve_forever()
        logger.debug("Listening Port 8015...")