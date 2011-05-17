import os
import sys
import json
import httplib2
import pprint

SERVER = "192.168.0.195"
PORT = "8080"
BASE_URL = "http://%s:%s" % (SERVER, PORT)
USERNAME = "user0"
PASSWORD = "pass0"
API_KEY = "29ab4d98-f2e0-4e9a-aa50-18acbdb9f1f0"

def create_condition_report(h, username=USERNAME, password=PASSWORD):
    document_blob = {"title": "Title of the art",
                     "type": "Painting"}
    data = {"api_version": "1",
            "username": username,
            "password": password,
            "api_key": API_KEY,
            "operation_type": "create_condition_report",
            "document_blob": document_blob}
    print json.dumps(data)
    resp, content = h.request("%s/create_condition_report" % (BASE_URL, ),
                              "POST",
                              body=json.dumps(data),
                              headers={"content-type": "application/json"})
    import pdb; pdb.set_trace()

if __name__ == "__main__":
    h = httplib2.Http()
    httplib2.debuglevel = 1    
    create_condition_report(h)
    #create_condition_report(h, password="garbage")
    
    