"""
Example Application

Usage:
    falcon-server [options]

Options:
    -h --help                   Show this screen.
"""

from wsgiref.simple_server import make_server

import os
from docopt import docopt
from config.logger import logger


import falcon
from resources.resources import *
from middleware.context import ContextMiddleware

class Server(falcon.API):

    def __init__(self, config):
        
        super(Server, self).__init__(
            middleware=[ContextMiddleware()]
        )

        # Create resources
        action = Prediction(config)

        # Build routes
        self.add_route("/action", action)
        # self.add_route("/test", test)

if __name__ == "__main__":

    docopt(__doc__)

    DOCKER_ENGINE_REM_HOST = os.getenv('DOCKER_ENGINE_REM_HOST', default='127.0.0.1')
    DOCKER_ENGINE_REM_PORT = os.getenv('DOCKER_ENGINE_REM_PORT', default='4242')
    APP_HOST = os.getenv('APP_HOST', default='127.0.0.1')

    TENSORFLOW_IMAGE = os.getenv('TENSORFLOW_IMAGE', default='tensorflow/serving:2.4.1')
    PROPHET_IMAGE = os.getenv('PROPHET_IMAGE', default='rkrikbaev/service-prophet:v1.1.3')
    PATH_TO_MODELS = os.getenv('PATH_TO_MODELS', default='/tmp/models/')
    PATH_TO_SOURCE = os.getenv('PATH_TO_SOURCE', default='/tmp/source')
    PATH_TO_DEST = '/application'
    
    # DOCKER_ENGINE_ADD = f'{DOCKER_ENGINE_REM_HOST}:{DOCKER_ENGINE_REM_PORT}'
    
    # if DOCKER_ENGINE_REM_HOST == '127.0.0.1':
    DOCKER_ENGINE_ADD = 'unix://var/run/docker.sock'

    config = {
        'docker_engine_address': DOCKER_ENGINE_ADD,
        'app_host': APP_HOST,
        'prophet': {
            'app_internal_port': '8005/tcp',
            'image': PROPHET_IMAGE,
            'volumes': {PATH_TO_SOURCE: {"bind": PATH_TO_DEST, "mode": "rw"}},
            'detach': True
        },
        'tensorflow': {
            'image': TENSORFLOW_IMAGE,
            'volumes': {PATH_TO_MODELS: {"bind": PATH_TO_DEST, "mode": "rw"}},
            'detach': True            
        }
    }

    api_app = Server(config)

    with make_server("", 8001, api_app) as httpd:
        logger.debug("Serving on port 8001...")
        httpd.serve_forever()
