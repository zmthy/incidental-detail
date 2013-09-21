import maya.standalone as std
std.initialize(name='python')

import pymel.core as pmc

import basic

def createScene(fileName):
    pmc.newFile(new = True, force = True)
    basic.generate()
    pmc.saveAs("/Volumes/Scratch/Workflow/incidental-detail/src/test.ma", type = "mayaAscii")

createScene("test")