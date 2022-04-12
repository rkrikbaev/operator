import falcon
from resources.service import Predict, Health

import logging

logger = logging.getLogger('Main application')
logger.setLevel(logging.DEBUG)

api = falcon.App()

api.add_route("/action", Predict())
api.add_route("/health", Health())
