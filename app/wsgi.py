from logging import Logger
from wsgiref.simple_server import make_server

from app import api
from helper import get_logger

logger = get_logger(name=__name__, loglevel='DEBUG')

if __name__ == "__main__":
    
    with make_server("", 8015, api) as httpd:
        # Serve until process is killed
        logger.debug("Start wsgi web-server")
        logger.debug("Listening Port 8015...")
        
        httpd.serve_forever()
        