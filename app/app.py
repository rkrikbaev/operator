import falcon
from resources.service import Predict, Health

from middleware.logger import logger

api = falcon.App()

api.add_route("/action2", Predict())
api.add_route("/health", Health())
logger.debug('service started')