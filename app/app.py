import falcon
from resources.model import Predict, Health

from middleware.helper import get_logger
logger = get_logger(name=__name__, loglevel='DEBUG')

api = falcon.App()

api.add_route('/predict/{task_id}', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')