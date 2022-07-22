import falcon
from resources.model import Predict, Health
from middleware.helper import logger

api = falcon.App()

api.add_route('/predict/{task_id}', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')