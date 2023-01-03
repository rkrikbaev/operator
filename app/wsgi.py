from wsgiref.simple_server import make_server

from app import api
from helper import get_logger, LOG_LEVEL

logger = get_logger(name=__name__, loglevel=LOG_LEVEL)

if __name__ == "__main__":
    
    with make_server("", 8015, api) as httpd:

        # Serve until process is killed
        logger.info("Start wsgi web-server")
        logger.info("Listening Port 8015...")
        
        httpd.serve_forever()
        