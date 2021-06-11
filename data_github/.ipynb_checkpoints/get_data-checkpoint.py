# -*- coding: utf-8 -*-
"""
Created on Thu Mar  5 20:42:13 2020

@author: 陈畅
"""

import requests


def get_dis_tm(origin, destination):
    url = 'https://restapi.amap.com/v3/direction/walking?'
    key = 'dde6f59b0f409f4e2b7d3e5b4fb87ca5' 
    link = '{}origin={}&destination={}&key={}'.format(url, origin ,destination , key)
    response = requests.get(link)
    dis, tm = 999999, 999999
    if response.status_code == 200:
        results = response.json()
        if results['status'] == '1':
            dis = int(results['route']['paths'][0]['distance'])
            tm = int(results['route']['paths'][0]['duration'])
        else:
            print(link)
    return dis, tm


park = []
street_office = []

with open('park.txt', 'r',encoding="utf-8") as f1:
    l1 = f1.readlines()
    for i in range (1,len(l1)):
        p = l1[i].split()
        pxy = p[2]+','+p[3]
        park.append([p[0],pxy])
        

with open('street_office.txt','r',encoding="utf-8") as f2: 
    l2 = f2.readlines() 
    for i in range (1,len(l2)):
        s = l2[i].split(',')
        sxy = str(round(float(s[7]),6))+','+str(round(float(s[8]),6))
        street_office.append([s[3],sxy])


'''
task = street_office[76] #共77个  
file_name = task[0]+'.txt'
dis_tm = []

with open(file_name, 'w') as f3:
    for j in range(len(park)):
        origin = task[1]
        destination = park[j][1]
        dis,tm = get_dis_tm(origin,destination)
        dis_tm.append(park[j][0]+' '+str(dis)+' '+str(tm)+'\n')
    f3.writelines(dis_tm)
'''    
   
for i in range(101,107):
    task = park[i] #共107个  
    file_name = task[0]+'.txt'
    dis_tm = []

    with open(file_name, 'w') as f3:
        for j in range(len(street_office)):
            origin = task[1]
            destination = street_office[j][1]
            dis,tm = get_dis_tm(origin,destination)
            dis_tm.append(street_office[j][0]+' '+str(dis)+' '+str(tm)+'\n')
        f3.writelines(dis_tm)