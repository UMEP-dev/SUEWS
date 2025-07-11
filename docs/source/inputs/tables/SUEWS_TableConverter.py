#!/usr/bin/env python
########################################################
# Table Converter for SUEWS
# Ting Sun, ting.sun@reading.ac.uk
# Yihao Tang, Yihao.Tang@student.reading.ac.uk
# history:
# TS, 13 Oct 2017: initial version
# YT, 01 Jun 2018: added the chained conversion
########################################################

import numpy as np
import os.path
import os
from fnmatch import fnmatch
from shutil import copyfile
import f90nml
import shutil
from collections import defaultdict
import pandas as pd
from heapq import heappop, heappush


# ignore warnings raised by numpy when reading-in -9 lines
import warnings

warnings.filterwarnings("ignore")
########################################################


# load the rule file
rules = np.genfromtxt("rules.csv", dtype=np.ndarray, delimiter=",", names=True)


########################################################
# define action functions:
# the current supported actions:
# rename, delete, add, move

# rename:
# # rename file
# def rename_file(fromFile,toFile):
#     pass


# rename variable
def rename_var(toFile, toVar, toCol, toVal):
    # if namelist:
    if toFile.endswith(".nml"):
        print(toFile, toVar, toVal)
        rename_var_nml(toFile, toVar, toVal)
    else:
        dataX = np.genfromtxt(
            toFile,
            dtype=np.ndarray,
            skip_header=1,
            comments="!",
            names=True,
            invalid_raise=False,
        )
        # generate headers
        header = np.array(dataX.dtype.names)
        header[header == toVar] = toVal
        headerLine = (
            " ".join(str(i + 1) for i in np.arange(len(dataX[0])))
            + "\n"
            + " ".join(header)
        )

        # convert to a more handy array
        dataX = np.array(dataX.tolist()).astype(str)

        # NB: caveat: comment info will be dropped, needs to be recovered
        np.savetxt(
            toFile, dataX, fmt="%s", header=headerLine, footer="-9\n-9", comments=""
        )

        # print toVar + ' has been renamed to ' + toVal + '!'
        return


def rename_var_nml(toFile, toVar, toVal):
    nml = f90nml.read(toFile)
    title = list(nml.keys())[0]
    if toVar.lower() in list(nml[title].keys()):
        nml[title][toVal] = nml[title].pop(toVar)
    else:
        print(toVar + " does not exist!")
    nml.write(toFile, force=True)


# delete:
# delete variable
def delete_var(toFile, toVar, toCol, toVal):
    if toFile.endswith(".nml"):
        delete_var_nml(toFile, toVar, toVal)
    else:
        dataX = np.genfromtxt(
            toFile, dtype=np.ndarray, skip_header=1, comments="!", invalid_raise=False
        )

        # convert to a more handy array
        dataX = np.array(dataX.tolist()).astype(str)
        # print dataX

        # position of columns to delete
        posDel = np.where(dataX[0] == toVar)
        # print posDel
        dataX = np.delete(dataX, posDel, axis=1)

        # dataX[0] = [str(i + 1) for i in np.arange(len(dataX[0]))]
        headerLine = " ".join(str(i + 1) for i in np.arange(len(dataX[0])))
        # NB: caveat: comment info will be dropped, needs to be recovered
        np.savetxt(
            toFile, dataX, fmt="%s", header=headerLine, footer="-9\n-9", comments=""
        )

        # print toVar + ' has been deleted!'
        return


def delete_var_nml(toFile, toVar, toVal):
    nml = f90nml.read(toFile)
    toVarX = toVar.lower()
    title = list(nml.keys())[0]
    if toVarX in list(nml[title].keys()):
        nml[title].pop(toVarX)
    else:
        print(toVar + " does not exist!")
    nml.write(toFile, force=True)


# add:
# add variable(s) to a file
def add_var(toFile, toVar, toCol, toVal):
    # toFile missing
    if not (os.path.isfile(toFile)):
        # create toFile
        dataX = [1]
        np.savetxt(
            toFile, dataX, header="1\nCode", footer="-9\n-9", fmt="%s", comments=""
        )

    # if namelist:
    if toFile.endswith(".nml"):
        add_var_nml(toFile, toVar, toVal)
    else:
        # load original data from toFile
        dataX = np.genfromtxt(
            toFile,
            dtype=np.ndarray,
            skip_header=1,
            comments="!",
            names=True,
            invalid_raise=False,
        )
        # print dataX.dtype.names
        header = np.array(dataX.dtype.names).astype("S140")
        # print header.dtype, header
        # print toVar,int(toCol) - 1
        header = np.insert(header, int(toCol) - 1, toVar)
        # print header
        headerLine = (
            " ".join(str(i + 1) for i in np.arange(len(header)))
            + "\n"
            + " ".join(header)
        )
        # print dataX.shape
        # if toVar exists:
        if toVar in dataX.dtype.names:
            return toVar + " exists! Stop."
        else:
            # convert to a more handy array
            # print np.array(dataX.tolist()).astype(str).shape
            dataX = np.array(dataX.tolist()).astype(str)
            dataX = dataX.reshape((-1, len(header) - 1))
            # construct new column
            colNew = np.empty(dataX.shape[0], dtype=np.object)
            # colNew[0] = toCol
            # colNew[1] = toVar
            colNew = toVal

            # insert one column
            # print colNew
            # print int(toCol) - 1
            dataX = np.insert(dataX, int(toCol) - 1, colNew, axis=1)
            # # recover header positions
            # dataX[0] = np.arange(len(dataX[0])) + 1

            # NB: caveat: comment info will be dropped, needs to be recovered
            np.savetxt(
                toFile, dataX, header=headerLine, footer="-9\n-9", fmt="%s", comments=""
            )
            # print toVar + ' has been added!'
            return


def add_var_nml(toFile, toVar, toVal):
    nml = f90nml.read(toFile)
    toVarX = toVar.lower()
    title = list(nml.keys())[0]
    if not (toVarX in list(nml[title].keys())):
        nml[title][toVarX] = toVal
    else:
        print(toVar + " exists!")
    nml.write(toFile, force=True)


# # change:
# # change a variable to a target value
# def change_var(toFile, toCol, toRow, toVal):
#     # if namelist:
#     if toFile.endswith('.nml'):
#         change_var_nml(toFile, toVar, toVal)
#     else:
#         # load original data from toFile
#         dataX = np.genfromtxt(toFile, dtype=np.ndarray, skip_header=1,
#                               comments='!', names=True, invalid_raise=False)
#         # generate headers
#         headerLine = ' '.join(
#             str(i + 1) for i
#             in np.arange(len(dataX[0]))) + '\n' + ' '.join(dataX.dtype.names)
#
#         # convert to a more handy array
#         dataX = np.array(dataX.tolist()).astype(str)
#
#         # change
#         # print np.array(toRow)-1, np.array(toCol)-1
#         # print dataX[np.array(toRow)-1, np.array(toCol)-1]
#         dataX[np.array(toRow) - 1, np.array(toCol) - 1] = toVal
#         # print dataX[np.array(toRow)-1, np.array(toCol)-1]
#
#         # NB: caveat: comment info will be dropped, needs to be recovered
#         np.savetxt(toFile, dataX, fmt='%s', header=headerLine,
#                    footer='-9\n-9', comments='')
#         return


def change_var_nml(toFile, toVar, toVal):
    nml = f90nml.read(toFile)
    nml[toVar] = toVal
    nml.write(toFile)


# NOT USED CURRENTLY
# move:
# move variables from one file to another
# def move_var(fromFile, fromVar, toCol, toFileCode):
#     # get row code and column values from fromFile
#     # load original data from fromFile
#     data0 = np.genfromtxt(fromFile, dtype=np.ndarray, skip_header=1,
#                           comments='!', names=True, invalid_raise=False)
#
#     # parse:
#     # toFile: target file
#     # toCode: column name in fromFile to locate toCodeVal
#     [toFile, toCode] = toFileCode.split(':')
#     # toCodeVal: line code in toFile that should be aligned with value in
#     # fromFile
#     toCodeVal = np.array(np.unique(data0[[toCode, fromVar]]).tolist())
#
#     # load data from toFile
#     print toFile
#     dataX = np.genfromtxt(toFile, dtype=np.ndarray, skip_header=1,
#                           comments='!', names=True, invalid_raise=False)
#
#     # get to toRow number in toFile array
#     toRow = np.ravel(np.where(dataX['Code'] == toCodeVal[:, 0])) + 1
#
#     # conform toCol to pythonic index
#     # toCol=toCol+1
#
#     # store original info of row and column
#
#     # add variable/row to toFile with default value -999
#     toVar = fromVar  # same variable name as this is moving
#     add_var(toFile, toVar, toCol, '-999')
#
#     # change moved variable to the original value
#     toVal = toCodeVal[:, 1]
#     change_var(toFile, toCol, toRow, toVal)
#
#     # delete volumns of fromFile that have been moved
#     delete_var(fromFile, fromVar, toCol, toVal)
#
#     return


# a single conversion between two versions
def SUEWS_Converter_single(fromDir, toDir, fromVer, toVer):
    # copy files in fromDir to toDir, only: *.nml, SUEWS_*.txt
    if os.path.exists(toDir) is False:
        os.mkdir(toDir)
    fileList = []
    for fileX in os.listdir(fromDir):
        if any(fnmatch(fileX, p) for p in ["SUEWS*.txt", "*.nml"]):
            fileList.append(fileX)

    for fileX in fileList:
        copyfile(os.path.join(fromDir, fileX), os.path.join(toDir, fileX))

    # list all files involved in the given conversion
    posRules = np.unique(
        np.where(np.array(rules[["From", "To"]].tolist()) == [fromVer, toVer])[0]
    )
    filesToConvert = np.unique(rules["File"][posRules])

    for fileX in filesToConvert:
        actionList = rules[posRules].compress(rules["File"][posRules] == fileX, axis=0)
        actionList = np.array(actionList.tolist())[:, 2:].astype("S140")
        # prepend toDir to fileX
        actionList[:, 1] = os.path.join(toDir, fileX)
        # print actionList
        SUEWS_Converter_file(os.path.join(toDir, fileX), actionList)


def SUEWS_Converter_file(fileX, actionList):
    # actionList:[Action,File,Variable,Column,Value]
    # for a given fileX, action order:
    # 1. rename
    # 2. delete
    # 3. move
    # 4. add
    order = {"Rename": 1, "Delete": 2, "Move": 3, "Add": 4}
    todoList = np.array([
        np.concatenate(([order[x[0]]], x)).tolist() for x in actionList
    ])
    # print todoList
    # sort by Column number, then by Action order in actionList; also expand
    # dtype size
    todoList = todoList[np.lexsort((todoList[:, 4].astype(int), todoList[:, 0]))][
        :, 1:
    ].astype("S140")

    # print todoList,fileX
    # correct file names with proper path
    todoList[:, 1] = fileX
    # print todoList,fileX
    for action in todoList:
        # print action
        SUEWS_Converter_action(*action)


def SUEWS_Converter_action(action, toFile, var, col, val):
    print(action, toFile, var, col, val)
    actionFunc = {"Rename": rename_var, "Delete": delete_var, "Add": add_var}
    actionFunc[action](toFile, var, col, val)

    print(action + " " + var + " for " + toFile + ": done!")
    return


def dijkstra(edges, f, t):
    g = defaultdict(list)
    for l, r, c in edges:
        g[l].append((c, r))
    q, seen = [(0, f, ())], set()

    while q:
        (cost, v1, path) = heappop(q)

        if v1 not in seen:
            seen.add(v1)
            path = (v1, path)
            if v1 == t:
                return (cost, path)
            for c, v2 in g.get(v1, ()):
                if v2 not in seen:
                    heappush(q, (cost + c, v2, path))

    return float("inf")


def version_list(fromVer, toVer):
    edges = []
    a = pd.read_csv("rules.csv")
    v_from = np.unique(a["From"])
    for i in v_from:
        df = a[a["From"] == i]
        for k in np.unique(df["To"]):
            edges.append((i, k, 1))

    s = dijkstra(edges, fromVer, toVer)
    chain_ver = []
    while s:
        chain_ver.append(s[0])
        s = s[1]
    return chain_ver


# a chained conversion across multiole versions
def SUEWS_Converter_chain(fromDir, toDir, fromVer, toVer):
    chain_ver = version_list(fromVer, toVer)
    temDir_1 = temDir_2 = fromDir
    i = chain_ver[0]
    # Create temporary folders
    if os.path.exists("tem1") is False:
        os.mkdir("tem1")
    if os.path.exists("tem2") is False:
        os.mkdir("tem2")

    # Indirect version conversion process
    while i > 1:
        if i % 2:
            temDir_2 = "tem2"
            SUEWS_Converter_single(temDir_1, temDir_2, chain_ver[i + 1], chain_ver[i])
            temDir_1 = "tem1"
            # Remove input temporary folders
            shutil.rmtree("tem1", ignore_errors=True)

        else:
            temDir_1 = "tem1"
            SUEWS_Converter_single(temDir_2, temDir_1, chain_ver[i + 1], chain_ver[i])
            temDir_2 = "tem2"
            # Remove input temporary folders
            shutil.rmtree("tem2", ignore_errors=True)
            # this loop always break in this part
        i -= 1

    SUEWS_Converter_single(temDir_1, toDir, chain_ver[2], chain_ver[1])
    # Remove temporary folders
    shutil.rmtree("tem1", ignore_errors=True)
    shutil.rmtree("tem2", ignore_errors=True)


if __name__ == "__main__":
    # example:
    fromDir = "2016a"
    toDir = "2018a"
    fromVer = "2016a"
    toVer = "2018a"
    SUEWS_Converter_chain(fromDir, toDir, fromVer, toVer)
