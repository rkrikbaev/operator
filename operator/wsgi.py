from wsgiref.simple_server import make_server

from api import api
from utils import get_logger, LOG_LEVEL

logger = get_logger(name=__name__, loglevel=LOG_LEVEL)

if __name__ == "__main__":
    
    port = 8015
    with make_server("", 8015, api) as httpd:

        # Serve until process is killed
        logger.info("Start wsgi web-server")
        logger.info(f"Listening Port {port}...")
        
        httpd.serve_forever()
        