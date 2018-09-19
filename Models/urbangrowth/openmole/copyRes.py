import sys,os,shutil

# copy latest generation for each run in a folder

SOURCE=sys.argv[1]
TARGET=sys.argv[2]

def getLatestGen(dir):
    files = os.listdir(dir)
    print(sorted(map(lambda s:int(s.split('population')[1].split('.')[0]),files),reverse=True)[0])
    return(str(sorted(map(lambda s : int(s.split('population')[1].split('.')[0]),files),reverse=True)[0]))


sourcedirs = os.listdir(SOURCE)
targetdirs = os.listdir(TARGET)

for dir in sourcedirs :
    if not os.path.isdir(TARGET+'/'+dir):
        os.mkdir(TARGET+'/'+dir)
    gen = getLatestGen(SOURCE+'/'+dir)
    if not os.path.isfile(TARGET+'/'+dir+'/population'+gen+'.csv'):
        shutil.copy(SOURCE+'/'+dir+'/population'+gen+'.csv',TARGET+'/'+dir+'/population'+gen+'.csv')
