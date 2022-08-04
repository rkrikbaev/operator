import falcon
from falcon import testing

import pytest
from app import api
# from app import create_server
# from app import run_server

@pytest.fixture
def client():
    return testing.TestClient(api)

# pytest will inject the object returned by the "client" function
# as an additional parameter.

def test_action(client):

    doc = {
        'state': 'ok'
    }

    # response = client.simulate_get('/action')
    
    response = client.simulate_get('/health')
    assert response.status_code == 200


    response = client.simulate_get('/action')
    # result_doc = msgpack.unpackb(response.content, raw=False)
    # assert result_doc == doc
    assert response.status_code == falcon.HTTP_201

    # assert result_doc == doc
    # assert response.status == falcon.HTTP_OK