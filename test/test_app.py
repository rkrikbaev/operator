import falcon
from falcon import testing
import msgpack
import pytest

from operator import run


@pytest.fixture
def client():
    return testing.TestClient(run)


# pytest will inject the object returned by the "client" function
# as an additional parameter.
def test_action1(client):
    doc = {
        'state': 'ok'
    }

    response = client.simulate_get('/action1')
    result_doc = msgpack.unpackb(response.content, raw=False)

    assert result_doc == doc
    assert response.status == falcon.HTTP_OK