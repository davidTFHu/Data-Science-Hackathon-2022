import pandas as pd
import numpy as np

import requests
import re

token = {'Authorization': ''}  # fill in valid token


def get_url(name, extensions=[]):
    url = f"https://fount.wppbav.com/api/v1/{name}?per_page=100"
    for extension in extensions:
        url = f"{url}&{extension}"
    return url


def get_pages(url, token):
    response = requests.get(url, headers=token)
    last_page_str = re.findall("&page=\d+", response.json()["links"]["last"])[0]
    last_page = re.findall("\d+", last_page_str)[0]
    last_page = int(last_page)
    pages = np.arange(1, last_page+1, 1)
    return pages


def get_json(url, token, pages):

    json_data = []

    for page_no in pages:
        print(f"Page {page_no} of {pages[-1]}")

        # Get url of page
        page_url = f"{url}&page={page_no}"

        # Call to API
        response = requests.get(page_url, headers=token)
        json_data += response.json()["data"]

    return json_data


def get_bav_ids_for_endpoint(endpoint, extensions=[], var_names=["id", "name"]):

    url = get_url(name=endpoint, extensions=extensions)
    pages = get_pages(url, token)
    json = get_json(url, token, pages)
    data = pd.DataFrame([{var: data[var] for var in var_names} for data in json])
    filename = f"bav_ids_{endpoint}.csv"
    data.to_csv(filename)
    print(filename)
    return(data)
