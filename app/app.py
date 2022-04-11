import falcon

from resources.service import Predict, Health

import logging
# from middleware.helper import load_config

logger = logging.getLogger('Main application')
logger.setLevel(logging.DEBUG)

# config = load_config(file='app/config/service.yaml')

api = falcon.App()

api.add_route("/action", Predict())
api.add_route("/health", Health())