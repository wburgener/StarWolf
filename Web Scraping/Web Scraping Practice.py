import ssl
from urllib.request import urlopen as uReq
from bs4 import BeautifulSoup as soup

context = ssl._create_unverified_context()
my_url = "https://www.newegg.com/Video-Cards-Video-Devices/Category/ID-38?Tpk=graphics%20card"
uClient = uReq(my_url, context=context)
page_html = uClient.read()
uClient.close()

page_soup = soup(page_html, "html.parser")
# print(page_soup.h1)
# print(page_soup.p)
containers = page_soup.findAll("div", {"class":"item-container"})
# print(len(containers))
# container = containers[0]
# print(container)

for container in containers:
    brand = container.div.div.a.img["title"]
    title_container = container.findAll("a", {"class":"item-title"})
    shipping_container = container.findAll("li", {"class": "price-ship"})
    print(brand, ",", title_container[0].text, ",", shipping_container[0].text.strip())
